#' Rebalance Perturbation Directions (Functional API)
#'
#' @description
#' Perform rebalancing ONCE on a detailed table structure and store updated
#' `direction_rebalanced` values in the microdata. The result can be reused
#' for an arbitrary number of perturbation calls via [rn_perturb()].
#'
#' @param x An `rn_initialized` object (from [rn_init()]) or an
#'   `rn_rebalanced` object (to rebalance again on a different structure).
#' @param dim_list Named list of hierarchies defining the detailed table
#'   structure for rebalancing
#'   (e.g., `list(nace = hier_detailed, bgkl = hier_detailed)`).
#' @param num_var Name of the numerical variable to rebalance.
#'   Currently only a single variable is supported.
#'
#' @return An object of class `rn_rebalanced`, usable with [rn_perturb()].
#'   Convert back to a plain `data.table` via `as.data.table()`.
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
#' state <- rn_init(dt, sensitive_params = list(n_threshold = 3))
#' rebal <- rn_rebalance(state, dim_list = dims, num_var = "turnover")
#' rebal
rn_rebalance <- function(x, dim_list, num_var) {
  if (inherits(x, "rn_rebalanced")) {
    microdata <- x$microdata
    sensitive_params <- x$meta$sensitive_params
    n_threads <- x$meta$n_threads
  } else if (inherits(x, "rn_initialized")) {
    microdata <- x$microdata
    sensitive_params <- x$sensitive_params
    n_threads <- x$n_threads
  } else {
    .assert_rn_class(
      x,
      "rn_initialized",
      hint = "Call {.fn rn_init} before {.fn rn_rebalance}."
    )
  }

  # Validate dim_list
  if (!is.list(dim_list) || is.null(names(dim_list))) {
    cli::cli_abort(
      "{.arg dim_list} must be a {.strong named} list of hierarchies."
    )
  }

  # Validate num_var
  if (!is.character(num_var) || length(num_var) != 1) {
    cli::cli_abort("{.arg num_var} must be a single character string.")
  }

  # Check hierarchy names match data
  missing_dims <- setdiff(names(dim_list), names(microdata))
  if (length(missing_dims) > 0) {
    cli::cli_abort(
      "Dimension {.val {missing_dims}} in {.arg dim_list} not found in data."
    )
  }

  # Check num_var exists in data
  if (!(num_var %in% names(microdata))) {
    cli::cli_abort(
      "Variable {.val {num_var}} not found in data."
    )
  }

  .rn_log_info("Performing rebalancing on detailed table structure...")

  updated_microdata <- .perform_ezs_rebalancing(
    data = microdata,
    dimList = dim_list,
    numVars = num_var,
    sensitive_params = sensitive_params,
    n_threads = n_threads
  )

  .rn_log_success(
    "Rebalancing complete. Updated {.val direction_rebalanced} column."
  )

  .new_rn_rebalanced(
    microdata = updated_microdata,
    meta = list(
      dim_list = dim_list,
      num_var = num_var,
      sensitive_params = sensitive_params,
      n_threads = n_threads
    )
  )
}
