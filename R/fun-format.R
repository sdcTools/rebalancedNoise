# Result formatting, shared between the functional API (rn_format) and the
# R6 engine ($get_results).

# Internal helper: process a stored wide result table into wide/long output.
# Extracted from the R6 get_results() method.
.process_result_table <- function(table, stored_vars, format = "wide", variables = NULL) {
  # Determine which variables to include
  if (is.null(variables)) {
    target_vars <- names(stored_vars)
  } else {
    # Validate requested variables
    missing <- setdiff(variables, names(stored_vars))
    if (length(missing) > 0) {
      available <- paste(names(stored_vars), collapse = ", ")
      cli::cli_abort(c(
        "x" = "Variable(s) {.val {missing}} not found in table.",
        "i" = "Available variables: {.val {available}}"
      ))
    }
    target_vars <- variables
  }

  if (format == "long") {
    all_cols <- names(table)

    # Filter to only requested variables
    target_vars_full <- target_vars
    target_vars <- target_vars_full[target_vars_full %in% all_cols]

    if (length(target_vars) > 0) {
      # Get all value columns for requested variables
      value_cols <- unlist(lapply(target_vars, function(v) {
        c(v, paste0(v, "_init"), paste0(v, "_pert"))
      }))
      value_cols <- intersect(value_cols, all_cols)

      # Get is_sens columns for requested variables
      sens_cols <- paste0("is_sens_", target_vars_full)
      sens_cols <- intersect(sens_cols, all_cols)

      # Get all is_sens columns
      all_sens_cols <- grep("^is_sens_", all_cols, value = TRUE)

      # Columns to remove: non-requested value columns + non-requested is_sens columns
      all_vars_in_table <- unique(
        gsub(
          "_init$|_pert$", "",
          grep("_init$|_pert$", all_cols, value = TRUE)
        )
      )
      all_value_cols <- unlist(lapply(all_vars_in_table, function(v) {
        c(v, paste0(v, "_init"), paste0(v, "_pert"))
      }))
      all_value_cols <- intersect(all_value_cols, all_cols)

      value_to_remove <- setdiff(all_value_cols, value_cols)
      sens_to_remove <- setdiff(all_sens_cols, sens_cols)
      cols_to_remove <- c(value_to_remove, sens_to_remove)

      # Get id columns (dimension columns + metadata + is_sens for requested vars)
      id_cols <- setdiff(all_cols, c(all_value_cols, sens_to_remove))

      # Melt all value columns
      res_long <- data.table::melt(
        table,
        id.vars = id_cols,
        measure.vars = value_cols,
        variable.name = "temp_var",
        value.name = "value"
      )

      # Extract variable name and measure type
      res_long[, var_name := gsub("_init$|_pert$", "", temp_var)]
      res_long[, measure_type := ifelse(grepl("_init$", temp_var), "init",
                                 ifelse(grepl("_pert$", temp_var), "pert", "orig"))]

      # Cast to wide format per variable
      res_long[, temp_var := NULL]
      res_long <- data.table::dcast(
        res_long,
        ... ~ measure_type,
        value.var = "value"
      )
      setnames(res_long, c("orig", "init", "pert"),
               c("val_orig", "val_pert_init", "val_pert"))

      # Add is_sensitive column by looking up the correct is_sens column
      res_long[, is_sensitive := NA]
      for (v in target_vars) {
        sens_col <- paste0("is_sens_", v)
        if (sens_col %in% names(res_long)) {
          res_long[var_name == v, is_sensitive := get(sens_col)]
          res_long[, (sens_col) := NULL]
        }
      }

      # Rename var_name to variable
      setnames(res_long, "var_name", "variable")

      # Calculate percentage differences
      res_long[,
        diff_init_pct := round(
          (val_pert_init - val_orig) / val_orig * 100,
          digits = 3
        )
      ]
      res_long[,
        diff_final_pct := round(
          (val_pert - val_orig) / val_orig * 100,
          digits = 3
        )
      ]

      # Clean up: remove strID if present
      if ("strID" %in% names(res_long)) {
        setorder(res_long, strID)
        res_long[, strID := NULL]
      }

      return(res_long)
    }
  }

  # Wide format: remove columns for non-requested variables
  if (length(target_vars) < length(names(stored_vars))) {
    # Get all columns related to variables to remove
    vars_to_remove <- setdiff(names(stored_vars), target_vars)
    if (length(vars_to_remove) > 0) {
      cols_to_remove <- unlist(lapply(vars_to_remove, function(v) {
        c(v, paste0(v, "_init"), paste0(v, "_pert"), paste0("is_sens_", v))
      }))
      cols_to_remove <- intersect(cols_to_remove, names(table))
      if (length(cols_to_remove) > 0) {
        table[, (cols_to_remove) := NULL]
      }
    }
  }

  # Remove strID if present
  if ("strID" %in% names(table)) {
    setorder(table, strID)
    table[, strID := NULL]
  }

  table
}

#' Format a Perturbed Result Table (Functional API)
#'
#' @description
#' Retrieve a perturbed result table in wide or long format. This is the
#' functional counterpart of the R6 `$get_results()` method.
#'
#' @param x An `rn_perturbed` object (from [rn_perturb()] or
#'   `sdc$get_table()`).
#' @param format Character, either "wide" (default) or "long".
#' @param variables Character vector of variable names to include. If NULL,
#'   includes all variables.
#'
#' @return A `data.table` in wide or long format.
#'
#' @export
#' @examples
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
#' rn_format(res, format = "long")
rn_format <- function(x, format = "wide", variables = NULL) {
  .assert_rn_class(
    x,
    "rn_perturbed",
    hint = "Call {.fn rn_perturb} first to create a result table."
  )

  if (!format %in% c("wide", "long")) {
    cli::cli_abort(
      "{.arg format} must be either {.val wide} or {.val long}."
    )
  }

  .process_result_table(
    copy(x$table),
    x$variables,
    format = format,
    variables = variables
  )
}
