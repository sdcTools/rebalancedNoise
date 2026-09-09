# Generate deterministic hash from dim_list, sensitive_params and rounding
# flag for table identification
.get_table_hash <- function(dim_list, sensitive_params = list(), round = FALSE) {
  # Extract sorted dimension names for consistent hashing
  dim_names <- sort(names(dim_list))
  # Create deterministic string representation of dimensions
  hash_input_dims <- paste(dim_names, collapse = "|")
  # Create deterministic string representation of sensitive_params
  # Sort parameter names for consistency
  param_names <- sort(names(sensitive_params))
  hash_input_params <- paste(
    sapply(
      param_names,
      function(p) {
        val <- sensitive_params[[p]]
        if (is.list(val)) {
          # Handle nested lists (e.g., nk_rule)
          paste(
            sort(names(val)),
            sapply(val, function(v) paste(v, collapse = ",")),
            collapse = ":"
          )
        } else {
          paste(val, collapse = ",")
        }
      },
      USE.NAMES = TRUE
    ),
    collapse = "|"
  )
  # Combine dimensions, params and rounding flag
  hash_input <- paste(
    hash_input_dims,
    hash_input_params,
    paste0("round=", as.character(isTRUE(round))),
    sep = "::"
  )
  # Generate xxhash64 hash
  xxhashlite::xxhash(hash_input)
}

# Validate export data structure and contents
.validate_export_data <- function(export_data) {
  if (!inherits(export_data, "rebalancedNoise_ExportData")) {
    cli::cli_abort("Object must have class 'rebalancedNoise_ExportData'")
  }

  required <- c("microdata", "sensitive_params", "rebal_status")
  missing <- setdiff(required, names(export_data))
  if (length(missing) > 0) {
    cli::cli_abort("Export object missing required elements: {.val {missing}}")
  }

  # Validate microdata structure
  req_cols <- c("direction", "noise_multiplier")
  missing_cols <- setdiff(req_cols, names(export_data$microdata))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Microdata missing columns: {.val {missing_cols}}")
  }

  # Validate direction values
  if (!all(export_data$microdata$direction %in% c(1, -1))) {
    cli::cli_abort(
      "Exported microdata has invalid direction values (must be 1 or -1)"
    )
  }

  # Validate direction_rebalanced values (if present)
  if ("direction_rebalanced" %in% names(export_data$microdata)) {
    if (
      !all(export_data$microdata$direction_rebalanced %in% c(1, -1))
    ) {
      cli::cli_abort(
        "Exported microdata has invalid direction_rebalanced values (must be 1 or -1)"
      )
    }
  }

  # Validate noise_multiplier
  if (any(export_data$microdata$noise_multiplier < 0)) {
    cli::cli_abort("Exported microdata has negative noise_multiplier values")
  }

  return(invisible(TRUE))
}

# Internal helper: Compute cell-level sensitivity from microdata
# Handles both n_threshold-only and dominance rules
# Returns data.table with (strID, is_sens_<target_var>)
.compute_cell_sensitivity <- function(
  microdata,
  sensitive_params,
  target_var,
  n_threads = 1L
) {
  sens_col <- paste0("is_sens_", target_var)

  # Check if we have strID in microdata
  if (!"strID" %in% names(microdata)) {
    cli::cli_abort(
      "{.arg microdata} must contain {.field strID} column."
    )
  }

  # Fast-path for n_threshold only (no dominance rules)
  is_only_n <- (is.null(sensitive_params$p_rule) ||
    sensitive_params$p_rule == 0) &&
    (is.null(sensitive_params$nk_rule$n) || sensitive_params$nk_rule$n == 0)

  if (is_only_n) {
    n_thresh <- as.integer(sensitive_params$n_threshold %||% 0)
    # Count observations per strID (this is n_obs for each cell)
    cell_counts <- microdata[, .(n_obs = .N), by = strID]
    # Mark sensitive cells: n_obs <= n_threshold means sensitive
    cell_counts[, (sens_col) := n_obs <= n_thresh]
    return(cell_counts)
  }

  # Full sensitivity checking with dominance rules
  # Sort by strID and target_var (descending within each cell)
  setorderv(microdata, c("strID", target_var), c(1, -1))
  group_starts <- which(!duplicated(microdata$strID)) - 1

  # Compute sensitivity at record level
  is_sens_vec <- check_sensitivity_cpp(
    vals = microdata[[target_var]],
    ids = as.character(microdata$strID),
    group_starts = group_starts,
    n_threshold = as.integer(sensitive_params$n_threshold %||% 0),
    p_rule = as.double(sensitive_params$p_rule %||% 0),
    nk_n = as.integer(sensitive_params$nk_rule$n %||% 0),
    nk_k = as.double(sensitive_params$nk_rule$k %||% 0),
    n_threads = as.integer(n_threads)
  )

  # Add sensitivity flag to microdata
  microdata[, (sens_col) := is_sens_vec]

  # Aggregate to cell level: cell is sensitive if any record is sensitive
  sens_lookup <- microdata[, .(is_sens = any(get(sens_col))), by = strID]
  setnames(sens_lookup, "is_sens", sens_col)

  return(sens_lookup)
}

# Internal helper: Identify base cells (leaf nodes) in a hierarchy
# Returns data.table with (strID, is_base_cell)
.identify_base_cells <- function(prob_object, dim_names, data_summary) {
  struct_mapping <- data_summary[, .SD, .SDcols = c(dim_names, "strID")]

  min_info <- lapply(prob_object@dimInfo@dimInfo, function(x) {
    data.table(
      code = slot(x, "codesOriginal"),
      is_minimal = slot(x, "codesMinimal")
    )
  })

  is_base_dt <- copy(struct_mapping)
  for (d in dim_names) {
    is_base_dt <- merge(
      is_base_dt,
      min_info[[d]],
      by.x = d,
      by.y = "code",
      all.x = TRUE
    )
    setnames(is_base_dt, "is_minimal", paste0("is_min_", d))
  }

  min_cols <- paste0("is_min_", dim_names)
  is_base_dt[,
    is_base_cell := rowSums(.SD == TRUE) == length(dim_names),
    .SDcols = min_cols
  ]

  return(is_base_dt[, .(strID, is_base_cell)])
}

# Internal helper: Full rebalancing workflow
# Main helper function that performs rebalancing and returns updated data
.perform_ezs_rebalancing <- function(
  data,
  dimList,
  num_var,
  sensitive_params,
  n_threads = 1L
) {
  # Create structural mapping using sdcTable
  prob_object <- sdcTable::makeProblem(
    data = data,
    dimList = dimList,
    numVarInd = num_var
  )

  # Extract the table skeleton
  data_summary <- as.data.table(
    sdcProb2df(
      prob_object,
      addDups = TRUE,
      addNumVars = TRUE,
      dimCodes = "original"
    )
  )
  setnames(data_summary, "freq", "n_obs")

  dim_names <- names(dimList)
  # Deduplicate dimension combinations to avoid Cartesian product with bogus codes
  struct_mapping <- unique(data_summary[, .SD, .SDcols = c(dim_names, "strID")], by = dim_names)

  # Identify base cells (leaf nodes) across all dimensions
  base_cells <- .identify_base_cells(prob_object, dim_names, data_summary)
  base_cell_ids <- base_cells[is_base_cell == TRUE, as.integer(strID)]

  # Join mapping back to microdata (drop stale strID from a previous run,
  # e.g. when rebalancing is performed again on already-rebalanced microdata)
  if ("strID" %in% names(data)) {
    data[, strID := NULL]
  }
  data <- merge(data, struct_mapping, by = dim_names, all.x = TRUE)
  # Keep strID integer for fast grouping during rebalancing; the character
  # representation is restored at the end of this function
  data[, strID := as.integer(strID)]

  # Compute sensitivity using microdata (handles both n_threshold and dominance rules)
  sens_lookup <- .compute_cell_sensitivity(
    microdata = data,
    sensitive_params = sensitive_params,
    target_var = num_var[1],
    n_threads = n_threads
  )

  # Rebalance all base cells in one vectorized pass (C++ kernel, OpenMP)
  data[, direction_rebalanced := as.double(direction)] # Original directions

  if (length(base_cell_ids) > 0) {
    # Only base cells can trigger rebalancing; aggregates are excluded
    sens_col_name <- paste0("is_sens_", num_var[1])
    base_sens <- sens_lookup[
      strID %in% base_cell_ids,
      .(strID, is_sens = get(sens_col_name))
    ]

    # Group base-cell records by strID via positional indexing (no row
    # copy); group_starts follows the convention of check_sensitivity_cpp()
    base_idx <- which(data$strID %in% base_cell_ids)
    sids <- data$strID[base_idx]
    ord <- order(sids)
    sids_sorted <- sids[ord]
    group_starts <- which(!duplicated(sids_sorted)) - 1L
    group_ids <- sids_sorted[group_starts + 1L]

    cell_is_sens <- base_sens$is_sens[match(group_ids, base_sens$strID)]
    if (any(is.na(cell_is_sens))) {
      cli::cli_abort(c(
        "x" = "Sensitivity status missing for one or more base cells.",
        "i" = "{sum(is.na(cell_is_sens))} base cell(s) have no sensitivity entry."
      ))
    }

    sub_idx <- base_idx[ord]
    dir_sorted <- rebalance_cells_cpp(
      orig = as.double(data[[num_var[1]]][sub_idx]),
      mult = as.double(data$noise_multiplier[sub_idx]),
      dirs = as.double(data$direction_rebalanced[sub_idx]),
      is_sens_by_cell = cell_is_sens,
      group_starts = as.integer(group_starts),
      n_threads = as.integer(n_threads)
    )

    # Undo the sorting permutation and write back by row position
    new_dirs <- rep_len(data$direction_rebalanced[base_idx], length(ord))
    new_dirs[ord] <- dir_sorted
    data[base_idx, direction_rebalanced := new_dirs]
  }

  # Restore the character strID representation used by the public API
  data[, strID := as.character(strID)]

  # Hard gate: rebalanced directions must be exactly -1 or +1
  # (NA, NaN and Inf fail %in% and are caught here as well)
  bad_dir <- !data$direction_rebalanced %in% c(1, -1)
  if (any(bad_dir)) {
    offenders <- unique(data$direction_rebalanced[bad_dir])
    cli::cli_abort(c(
      "x" = "Rebalancing produced invalid {.val direction_rebalanced} values.",
      "i" = "{sum(bad_dir)} record(s) with NA/NaN or non-integerish directions.",
      "!" = "Offending values: {.val {offenders[1:min(3, length(offenders))]}}"
    ))
  }
  data[, direction_rebalanced := as.integer(direction_rebalanced)]

  return(data)
}
