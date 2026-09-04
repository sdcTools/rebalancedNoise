#' Initialize Rebalancing State (Functional API)
#'
#' @description
#' Validates input data and prepares it for rebalancing and perturbation.
#' This is the functional counterpart of the internal setup performed by
#' [rn_setup()]; the R6 engine reuses this function internally.
#'
#' @details
#' `rn_init()` accepts three kinds of input:
#' * A plain `data.frame`/`data.table` containing the EZS columns
#'   `direction` (1/-1) and `noise_multiplier` (>= 0).
#' * A file path to an exported `rebalancedNoise_ExportData` object (RDS).
#' * An in-memory `rebalancedNoise_ExportData` object (e.g. from
#'   `sdc$export()`).
#'
#' @param data Input microdata containing `direction` and `noise_multiplier`
#'   columns, a file path to an export object, or an export object itself.
#' @param sensitive_params List of SDC rules (e.g., `list(n_threshold = 3, p_rule = 10)`).
#'   If `data` is an export object, this will be overridden by the exported value.
#' @param n_threads Integer specifying threads for parallel sensitivity checking
#'   and rebalancing. Supports `options(rn_threads = X)` or `Sys.setenv(rn_threads = X)`.
#'
#' @return An object of class `rn_initialized`, usable with [rn_rebalance()]
#'   and (when imported from a completed rebalancing) directly with [rn_perturb()].
#'   Convert back to a plain `data.table` via `as.data.table()`.
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
#' state <- rn_init(dt, sensitive_params = list(n_threshold = 3))
#' state
#' Sys.setenv(SDC_LOG_LEVEL = old_log)
rn_init <- function(
  data,
  sensitive_params = list(n_threshold = 3),
  n_threads = NULL
) {
  .rn_log_info("Initialization started...")

  # Retrieve n_threads from multiple sources
  resolved_threads <- n_threads %||%
    getOption("rn_threads") %||%
    Sys.getenv("rn_threads")

  if (!is.null(resolved_threads) && length(resolved_threads) > 1L) {
    cli::cli_abort("{.arg n_threads} must be a single value.")
  }

  # NULL or empty string -> fallback to parallel::detectCores()
  if (
    is.null(resolved_threads) ||
      length(resolved_threads) == 0L ||
      (!is.na(resolved_threads) && resolved_threads == "")
  ) {
    resolved_threads <- max(1, parallel::detectCores() - 1)
  }

  # validate n_threads (NA and non-positive values warn and fall back to 1)
  resolved_threads <- suppressWarnings(as.integer(resolved_threads))
  if (is.na(resolved_threads) || resolved_threads < 1) {
    cli::cli_warn(
      "{.arg n_threads} must be a positive integer. Falling back to {.val 1}."
    )
    resolved_threads <- 1
  }

  # Check if data is a file path (auto-import)
  if (is.character(data) && length(data) == 1 && file.exists(data)) {
    export_data <- readRDS(data)
    if (!inherits(export_data, "rebalancedNoise_ExportData")) {
      cli::cli_abort(
        "File does not contain valid 'rebalancedNoise_ExportData' object."
      )
    }
    # Validate and use exported data
    .validate_export_data(export_data)
    microdata <- export_data$microdata
    if (!is.null(export_data$sensitive_params)) {
      sensitive_params <- export_data$sensitive_params
    }
    rebal_status <- export_data$rebal_status
    result_tables <- export_data$result_tables %||% list()
  } else if (inherits(data, "rebalancedNoise_ExportData")) {
    # Direct export object
    .validate_export_data(data)
    rebal_status <- data$rebal_status
    result_tables <- data$result_tables %||% list()
    microdata <- data$microdata
    if (!is.null(data$sensitive_params)) {
      sensitive_params <- data$sensitive_params
    }
  } else {
    # Regular data - no import status
    rebal_status <- NULL
    result_tables <- list()
    microdata <- data
    # Sanity Checks for regular data
    # Check data type
    if (!is.data.frame(microdata)) {
      cli::cli_abort(
        "{.arg data} must be a data.frame or data.table, not {.cls {class(data)}}."
      )
    }

    # Check for mandatory columns in EZS (direction & noise_multiplier)
    req_cols <- c("direction", "noise_multiplier")
    missing_req <- setdiff(req_cols, names(microdata))
    if (length(missing_req) > 0) {
      cli::cli_abort(c(
        "x" = "EZS method requires specific columns in the microdata:",
        "i" = "Missing: {.val {missing_req}}",
        "*" = "Ensure {.code direction} (1/-1) and {.code noise_multiplier} are present."
      ))
    }

    # Check 'direction' values (must be 1 or -1)
    dir_vals <- microdata[["direction"]]
    if (!is.numeric(dir_vals) || !all(dir_vals %in% c(1, -1))) {
      cli::cli_abort(c(
        "x" = "Column {.code direction} contains invalid values.",
        "i" = "Only {.val {c(1, -1)}} are allowed.",
        "!" = "Found values like: {.val {unique(dir_vals)[1:min(3, length(unique(dir_vals)))]}}."
      ))
    }

    # Check 'noise_multiplier' is positive
    if (
      !is.numeric(microdata[["noise_multiplier"]]) ||
        any(microdata[["noise_multiplier"]] < 0)
    ) {
      cli::cli_abort(
        "{.code noise_multiplier} must be a positive numeric column."
      )
    }
  }

  dt <- as.data.table(copy(microdata))

  # Add internal record ID for tracking
  dt[, record_id := .I]

  # Initialize status tracking
  if (is.null(rebal_status)) {
    rebal_status <- list(
      done = FALSE,
      dim_list = NULL,
      params = NULL,
      num_var = NULL
    )
  }

  .rn_log_success("Initialization complete.")

  .new_rn_initialized(
    microdata = dt,
    sensitive_params = sensitive_params,
    n_threads = resolved_threads,
    rebal_status = rebal_status,
    result_tables = result_tables
  )
}
