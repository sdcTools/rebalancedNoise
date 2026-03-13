#' Perturbative Statistical Disclosure Control (EZS Method)
#'
#' @description
#' The `rn_setup` function and the `rebalancedNoise` class provide the
#' framework for applying the **EZS perturbation method** to magnitude tables as
#' described in the paper
#' [Using Perturbative Methods for Magnitude Tables in Statistical Disclosure Control.](https://unece.org/sites/default/files/2025-10/SDC2025_Sf_Sweden_Almberg_D.pdf)
#' by Sabolová, R., Tepe, Ö., Adriansson, N., & Almberg, L.-E. (2025).
#'
#' This approach applies record-level noise with a dynamic rebalancing algorithm
#' to preserve data quality in non-sensitive cells while ensuring additive
#' consistency within a table hierarchy.
#'
#' @param data A `data.table` or `data.frame` containing microdata.
#' @param dim_list A named list of hierarchies (e.g., created via `sdcHierarchies`).
#' @param num_vars A character vector of numerical variables to be perturbed.
#' @param sensitive_params A list of SDC rules. Supported elements:
#'   * `n_threshold`: Minimum number of observations (default: 3).
#'   * `p_rule`: The p-percent rule value.
#'   * `nk_rule`: A list with `n` and `k` for dominance rules.
#' @param n_threads Integer specifying the number of threads for C++ OpenMP
#'   parallelization used for the rebalancing procedure.
#'   If `NULL`, the engine resolves the thread count in the following priority:
#'   1. `options("rn_threads")`
#'   2. `Sys.getenv("rn_threads")`
#'   3. Fallback: `max(1, parallel::detectCores() - 1)`.
#' @details
#' The EZS method functions in two stages:
#'
#' 1. **Initial Perturbation:** Each record is assigned a fixed noise multiplier
#'    and a direction (+1/-1) based on a permanent random number or hash.
#' 2. **Dynamic Rebalancing:** In non-sensitive cells (those not flagged by
#'    dominance or threshold rules), the directions of records are adjusted
#'    to minimize the "running noise total," effectively "balancing" the
#'    perturbation towards the original cell total.
#'
#' For sensitive cells, rebalancing is disabled to ensure the protection
#' level remains at the intended fixed noise level.
#'
#' @section Methods:
#' ### `perturb(var, force = FALSE)`
#' Runs the EZS perturbation algorithm on the numerical variable `var`.
#'
#' ### `get_results(target_var = NULL, format = "wide")`
#' Retrieves aggregated results.
#' * format **"wide"**: Each row is a cell; columns are prefixed with the variable name.
#' * format **"long"**: Standardized names (`val_orig`, `val_pert`) with a `variable` column.
#' * Metadata: Includes `is_internal` (non-aggregate/internal cell) and `is_sens` (sensitivity flag).
#' * Deviations `{var}_diff_init_pct` and `{var}_diff_final_pct` are rounded to 3 digits.
#'
#' ### `summarize(target_var)`
#' Performs a statistical diagnostic of the perturbation impact for `target_var`.
#'
#' The output is divided into three groups:
#' * **OVERALL**: Performance across the entire table.
#' * **NON-SENSITIVE**: Efficiency metrics for cells subject to rebalancing.
#' * **SENSITIVE**: Protection metrics for cells with fixed noise.
#'
#' For each group, it displays:
#' * **Key Metrics**: nrCells, Mean Absolute Percentage Error (MAPE), Initial MAPE, and Noise Reduction (%).
#' * **Percentiles**: Detailed distribution (Min to Max) for Relative (%) and Absolute (Units)
#'   deviations. For rebalanced groups, a *"Before vs. After"* comparison of
#'   the relative distributions to visualize the "shrinking" effect of the algorithm is shown.
#'
#' @return
#' * `rn_setup()`: Returns a new `rebalancedNoise` R6 object.
#' * `rebalancedNoise`: An `R6` class object (accessible via `rn_setup`).
#'
#' @export
#' @rdname rn_setup
#' @references
#' Sabolová, R., Tepe, Ö., Adriansson, N., & Almberg, L.-E. (2025).
#' *Using Perturbative Methods for Magnitude Tables in Statistical Disclosure Control*.
#' Paper presented at the UNECE Expert Meeting on Statistical Data Confidentiality,
#' October 15–17, 2025, Barcelona, Spain.
#' [PDF Link](https://unece.org/sites/default/files/2025-10/SDC2025_Sf_Sweden_Almberg_D.pdf)
#' @examples
#' \dontrun{
#' # Optional: Disable logging
#' Sys.setenv(SDC_LOG_LEVEL = "OFF")
#'
#' # Set threads via environment variable
#' # Alternatively, one can also use: options(rn_threads = 4)
#' Sys.setenv(rn_threads = 4)
#'
#' # Generate dummy data
#' N <- 100
#' countries <- c("AT", "DE", "NL", "SE", "FR", "IT")
#' set.seed(1)
#' dt <- data.table(
#'   country = sample(countries, N, replace = TRUE),
#'   turnover = runif(N, 10, 1000),
#'   direction = sample(c(1, -1), N, replace = TRUE),
#'   noise_multiplier = 0.05
#' )
#'
#' # Define simple hierarchy
#' dims <- list(
#'   country = sdcHierarchies::hier_create("Total", nodes = countries)
#' )
#'
#' # Initialize the object
#' # Note that setting Argument `n_threads` overrides previously set global settings
#' sdc <- rn_setup(
#'   data = dt,
#'   dim_list = dims,
#'   num_vars = "turnover",
#'   sensitive_params  = list(n_threshold = 15),
#'   n_threads = 3
#' )
#'
#' # To re-enable logging, set the level back to "INFO"
#' # This will show cli alerts and progress bars again
#' Sys.setenv(SDC_LOG_LEVEL = "INFO")
#'
#' # Run Perturbation
#' sdc$perturb("turnover")
#'
#' # Retrieve Results (per default: wide-format)
#' sdc$get_results("turnover")
#'
#' # Long-Format is possible too
#' res <- sdc$get_results("turnover", format = "long")
#'
#' # Subset to aggregate results (marginal totals)
#' res[is_internal == FALSE]
#'
#' # Get only internal cells that were sensitive
#' res[is_internal == TRUE & is_sens == TRUE]
#'
#' # Summarize results
#' sdc$summarize("turnover")
#' }
rn_setup <- function(data, dim_list, num_vars, sensitive_params = list(n_threshold = 3),  n_threads = NULL) {
  # Sanity Checks
  # Check data type
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame or data.table, not {.cls {class(data)}}.")
  }

  # Check req. variables exist
  missing_vars <- setdiff(num_vars, names(data))
  if (length(missing_vars) > 0) {
    cli::cli_abort(c(
      "x" = "The following variables are missing from {.arg data}:",
      "i" = "{.val {missing_vars}}"
    ))
  }

  # Check for mandatory columns in EZS (direction & noise_multiplier)
  req_cols <- c("direction", "noise_multiplier")
  missing_req <- setdiff(req_cols, names(data))
  if (length(missing_req) > 0) {
    cli::cli_abort(c(
      "x" = "EZS method requires specific columns in the microdata:",
      "i" = "Missing: {.val {missing_req}}",
      "*" = "Ensure {.code direction} (1/-1) and {.code noise_multiplier} are present."
    ))
  }

  # Check 'direction' values (must be 1 or -1)
  dir_vals <- data[["direction"]]
  if (!is.numeric(dir_vals) || !all(dir_vals %in% c(1, -1))) {
    cli::cli_abort(c(
      "x" = "Column {.code direction} contains invalid values.",
      "i" = "Only {.val {c(1, -1)}} are allowed.",
      "!" = "Found values like: {.val {unique(dir_vals)[1:min(3, length(unique(dir_vals)))]}}."
    ))
  }

  # Check 'noise_multiplier' is positive
  if (!is.numeric(data[["noise_multiplier"]]) || any(data[["noise_multiplier"]] < 0)) {
    cli::cli_abort("{.code noise_multiplier} must be a positive numeric column.")
  }

  # Check the hierarchy-definition
  if (!is.list(dim_list) || is.null(names(dim_list))) {
    cli::cli_abort("{.arg dim_list} must be a {.strong named} list of hierarchies.")
  }

  # Check hierarchy names match data
  missing_dims <- setdiff(names(dim_list), names(data))
  if (length(missing_dims) > 0) {
    cli::cli_abort("Dimension {.val {missing_dims}} in {.arg dim_list} not found in {.arg data}.")
  }
  rebalancedNoise$new(
    data = data,
    dimList = dim_list,
    numVars = num_vars,
    sensitive_params = sensitive_params
  )
}
