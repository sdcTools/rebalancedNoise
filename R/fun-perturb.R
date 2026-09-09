# Core perturbation + tabulation logic, shared between the functional API
# (rn_perturb) and the R6 engine ($perturb).

# Perturb variables and tabulate through the hierarchy.
# Processes all variables in a single call to
# avoid redundant aggregation of large tables.
# Returns list(table = <result data.table>, base_cells = <data.table>)
.perturb_tabulate <- function(
  microdata,
  dim_list,
  variables,
  sensitive_params,
  round = FALSE,
  n_threads = 1L
) {
  dt <- copy(microdata)
  variables <- unique(variables)
  dim_names <- names(dim_list)

  # Compute initial and final perturbation for all variables in one pass
  all_num_vars <- character(0)
  for (tv in variables) {
    init_name <- paste0(tv, "_init")
    p_name <- paste0(tv, "_pert")
    dt[, (init_name) := get(tv) * (1 + direction * noise_multiplier)]
    dt[, (p_name) := get(tv) * (1 + direction_rebalanced * noise_multiplier)]
    if (round) {
      dt[, (init_name) := round(get(init_name))]
      dt[, (p_name) := round(get(p_name))]
    }
    all_num_vars <- c(all_num_vars, tv, init_name, p_name)
  }

  # Compute full table with all numeric variables
  prob_object <- sdcTable::makeProblem(
    data = dt,
    dimList = dim_list,
    numVarInd = all_num_vars
  )

  full_res <- as.data.table(
    sdcProb2df(
      prob_object,
      addDups = TRUE,
      addNumVars = TRUE,
      dimCodes = "original"
    )
  )
  full_res$sdcStatus <- NULL
  setnames(full_res, "freq", "n_obs")

  # Re-join microdata with new strID from this perturb call's dim_list
  # Deduplicate dimension combinations to avoid Cartesian product with bogus codes
  struct_mapping <- unique(
    full_res[, .SD, .SDcols = c(dim_names, "strID")],
    by = dim_names
  )
  dt <- merge(
    dt,
    struct_mapping,
    by = dim_names,
    all.x = TRUE,
    suffixes = c("", ".new")
  )
  if ("strID.new" %in% names(dt)) {
    dt[, strID := as.character(strID.new)]
    dt[, strID.new := NULL]
  }

  # Identify base cells
  base_cells <- .identify_base_cells(prob_object, dim_names, full_res)
  # Deduplicate base_cells by strID to avoid Cartesian product
  if (nrow(base_cells) > 0) {
    base_cells <- base_cells[, .SD[1], by = "strID"]
  }

  # Merge base cells into full_res once (applies to all variables)
  full_res <- merge(full_res, base_cells, by = "strID", all.x = TRUE)
  full_res[is.na(is_base_cell), is_base_cell := FALSE]

  # Sensitivity flags: with n_threshold only, a cell is sensitive if its
  # record count <= threshold), so it is computed once for all variables.
  # Dominance rules require the per-variable C++ check because
  # check_sensitivity_cpp() takes a single variable.
  sens_cols <- paste0("is_sens_", variables)
  is_only_n <- (is.null(sensitive_params$p_rule) ||
    sensitive_params$p_rule == 0) &&
    (is.null(sensitive_params$nk_rule$n) || sensitive_params$nk_rule$n == 0)

  if (is_only_n) {
    n_thresh <- as.integer(sensitive_params$n_threshold %||% 0)
    sens_vec <- !is.na(full_res$n_obs) &
      full_res$n_obs <= n_thresh &
      full_res$is_base_cell
    full_res[, (sens_cols) := FALSE]
    full_res[sens_vec, (sens_cols) := TRUE]
  } else {
    # Assign by strID match to avoid one full-table merge per variable
    for (tv in variables) {
      sens_result <- .compute_cell_sensitivity(
        microdata = dt,
        sensitive_params = sensitive_params,
        target_var = tv,
        n_threads = n_threads
      )
      sens_col_name <- paste0("is_sens_", tv)
      full_res[, (sens_col_name) := sens_result[[sens_col_name]][
        match(full_res$strID, sens_result$strID)
      ]]
      full_res[is.na(get(sens_col_name)), (sens_col_name) := FALSE]
    }
  }

  # Only base cells are sensitive; aggregates are FALSE
  if (!all(full_res$is_base_cell)) {
    full_res[is_base_cell == FALSE, (sens_cols) := FALSE]
  }

  # Rename is_base_cell to is_internal for user-facing output
  setnames(full_res, "is_base_cell", "is_internal")

  # Deduplicate by dimension columns to remove hierarchy-level duplicates
  # sdcProb2df with addDups=TRUE creates multiple rows for the same dimension
  # value when it appears at different hierarchy levels (e.g., b, b01, b001)
  # All these rows share the same strID, so we keep one row per dimension combo
  if (nrow(full_res) > 0) {
    full_res <- full_res[, .SD[1], by = dim_names]
  }

  # Reorder columns
  .reorder_var_columns(full_res, dim_names)

  list(table = full_res, base_cells = base_cells[, .(strID, is_base_cell)])
}

# Utility Fn: All result-table columns belonging to a variable.
.var_cols <- function(variables) {
  unlist(lapply(variables, function(tv) {
    c(tv, paste0(tv, "_init"), paste0(tv, "_pert"), paste0("is_sens_", tv))
  }))
}

# Utility Fn: Build per-variable metadata for result entries.
.var_metadata <- function(variables, sensitive_params) {
  setNames(
    lapply(variables, function(tv) {
      list(
        sensitive_params = sensitive_params,
        is_sens_col = paste0("is_sens_", tv)
      )
    }),
    variables
  )
}

# Internal helper: Reorder result columns as dims + meta + grouped by variable.
# Each variable group consists of <v>, <v>_init, <v>_pert and is_sens_<v>.
.reorder_var_columns <- function(full_res, dim_names) {
  all_cols <- names(full_res)

  # Detect variable names from the value column suffixes and the
  # is_sens_<v> prefixes
  all_vars <- union(
    unique(sub("_init$|_pert$", "", grep("_init$|_pert$", all_cols, value = TRUE))),
    unique(sub("^is_sens_", "", grep("^is_sens_", all_cols, value = TRUE)))
  )

  # Build ordered column list
  ordered_cols <- c(dim_names, "n_obs", "is_internal")
  for (v in all_vars) {
    ordered_cols <- c(ordered_cols, intersect(.var_cols(v), all_cols))
  }

  setcolorder(full_res, ordered_cols)
  full_res
}

# Internal helper: Merge columns of an additional variable into an existing
# result table. Extracted from the three duplicated merge blocks in the
# R6 $perturb() method.
.merge_var_into_table <- function(
  existing_table,
  full_res,
  dim_names,
  cols_to_add,
  reorder = TRUE
) {
  # Deduplicate dimension combinations to avoid Cartesian product with bogus codes
  full_res_subset <- unique(
    full_res[, .SD, .SDcols = c(dim_names, cols_to_add)],
    by = dim_names
  )

  # Merge, keeping all existing columns and adding new ones
  merged <- merge(
    existing_table,
    full_res_subset,
    by = dim_names,
    all.x = TRUE
  )

  # Remove duplicate is_internal if it exists (keep the original)
  if ("is_internal.y" %in% names(merged)) {
    merged[, is_internal.y := NULL]
  }

  if (reorder) {
    merged <- .reorder_var_columns(merged, dim_names)
  }

  merged
}

# Internal helper: validate perturb arguments (shared by rn_perturb and R6)
.validate_perturb_args <- function(microdata, dim_list, variables, round) {
  if (!is.list(dim_list) || is.null(names(dim_list))) {
    cli::cli_abort(
      "{.arg dim_list} must be a {.strong named} list of hierarchies."
    )
  }

  missing_dims <- setdiff(names(dim_list), names(microdata))
  if (length(missing_dims) > 0) {
    cli::cli_abort(
      "Dimension {.val {missing_dims}} in {.arg dim_list} not found in data."
    )
  }

  if (!is.character(variables) || length(variables) == 0) {
    cli::cli_abort(
      "{.arg variables} must be a non-empty character vector."
    )
  }

  for (tv in variables) {
    if (!(tv %in% names(microdata))) {
      cli::cli_abort(
        "Variable {.var {tv}} not found in data."
      )
    }
  }

  if (!is.logical(round) || length(round) != 1 || is.na(round)) {
    cli::cli_abort("{.arg round} must be a single logical value.")
  }

  invisible(TRUE)
}

#' Perturb and Tabulate Variables (Functional API)
#'
#' @description
#' Execute the EZS perturbation for specific variables using rebalanced
#' directions and tabulate the results through the given hierarchy.
#' Requires a rebalanced object from [rn_rebalance()] (or an imported state
#' with completed rebalancing).
#'
#' @param x An `rn_rebalanced` object (from [rn_rebalance()]) or an
#'   `rn_initialized` object imported from an export with completed
#'   rebalancing.
#' @param dim_list Named list of hierarchies defining the table structure
#'   for this perturbation.
#' @param variables Character vector of variable name(s) to perturb.
#' @param round Logical. If TRUE, the perturbed values are rounded to
#'   whole numbers with `round()` before the tables are computed. This
#'   makes all published cell values integers. Rounding can add up to 0.5
#'   per record, so cell totals may shift a little. Default is `FALSE`.
#'
#' @return An object of class `rn_perturbed`, usable with [rn_format()] and
#'   [rn_summarize()]. Convert to a plain `data.table` (wide format) via
#'   `as.data.table()`.
#'
#' @export
#' @examples
#' old_log <- Sys.getenv("SDC_LOG_LEVEL")
#' Sys.setenv(SDC_LOG_LEVEL = "OFF")
#' dt <- data.table::data.table(
#'   country = sample(c("AT", "DE", "NL"), 100, replace = TRUE),
#'   turnover = runif(100, 10, 1000),
#'   direction = sample(c(1, -1), 100, replace = TRUE),
#'   noise_multiplier = 0.05
#' )
#' dims <- list(country = sdcHierarchies::hier_create("Total",
#'   nodes = c("AT", "DE", "NL")
#' ))
#' res <- dt |>
#'   rn_init(sensitive_params = list(n_threshold = 3)) |>
#'   rn_rebalance(dim_list = dims, num_var = "turnover") |>
#'   rn_perturb(dim_list = dims, variables = "turnover")
#' data.table::as.data.table(res)
#' Sys.setenv(SDC_LOG_LEVEL = old_log)
rn_perturb <- function(x, dim_list, variables, round = FALSE) {
  if (inherits(x, "rn_initialized")) {
    x <- .promote_rn_rebalanced(x, call = parent.frame())
  }
  .assert_rn_class(
    x,
    "rn_rebalanced",
    hint = "Call {.fn rn_rebalance} before {.fn rn_perturb}, or use {.fn rn_init} on an exported object with completed rebalancing."
  )

  microdata <- x$microdata
  sensitive_params <- x$meta$sensitive_params
  n_threads <- x$meta$n_threads

  .validate_perturb_args(microdata, dim_list, variables, round)

  table_hash <- .get_table_hash(dim_list, sensitive_params, round)

  # Batch tabulation: single makeProblem/sdcProb2df for all variables
  res <- .perturb_tabulate(
    microdata = microdata,
    dim_list = dim_list,
    variables = variables,
    sensitive_params = sensitive_params,
    round = round,
    n_threads = n_threads
  )

  .new_rn_perturbed(
    table = res$table,
    hash = table_hash,
    dim_list = dim_list,
    sensitive_params = sensitive_params,
    round = round,
    base_cells = res$base_cells,
    variables = .var_metadata(unique(variables), sensitive_params)
  )
}
