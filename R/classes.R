# S3 infrastructure for the functional API pipeline:
#   rn_init()      -> "rn_initialized"
#   rn_rebalance() -> "rn_rebalanced"
#   rn_perturb()   -> "rn_perturbed"
#
# The objects are wrapper lists (robust against class loss in downstream
# data.table operations) and follow the same pattern as the existing
# "rebalancedNoise_ExportData" class.

# Utility for NULL handling (package-wide)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Constructor helpers ------------------------------------------------------------

.new_rn_initialized <- function(
  microdata,
  sensitive_params,
  n_threads,
  rebal_status,
  result_tables = list()
) {
  structure(
    list(
      microdata = microdata,
      sensitive_params = sensitive_params,
      n_threads = n_threads,
      rebal_status = rebal_status,
      result_tables = result_tables
    ),
    class = "rn_initialized"
  )
}

.new_rn_rebalanced <- function(microdata, meta) {
  structure(
    list(microdata = microdata, meta = meta),
    class = "rn_rebalanced"
  )
}

.new_rn_perturbed <- function(
  table,
  hash,
  dim_list,
  sensitive_params,
  round,
  base_cells,
  variables
) {
  structure(
    list(
      hash = hash,
      dim_list = dim_list,
      sensitive_params = sensitive_params,
      round = round,
      base_cells = base_cells,
      table = table,
      variables = variables
    ),
    class = "rn_perturbed"
  )
}

# Promote an "rn_initialized" object (imported from an export with completed
# rebalancing) to an "rn_rebalanced" object using the stored rebalancing meta.
.promote_rn_rebalanced <- function(x, arg = "x", call = parent.frame()) {
  if (
    !isTRUE(x$rebal_status$done) ||
      !"direction_rebalanced" %in% names(x$microdata)
  ) {
    cli::cli_abort(
      c(
        "x" = "{.arg {arg}} is not rebalanced yet.",
        "i" = "Call {.fn rn_rebalance} before {.fn rn_perturb}."
      ),
      call = call
    )
  }
  .new_rn_rebalanced(
    microdata = x$microdata,
    meta = list(
      dim_list = x$rebal_status$dim_list,
      num_var = x$rebal_status$num_var,
      sensitive_params = x$sensitive_params,
      n_threads = x$n_threads
    )
  )
}

# Ensure a stored result-table entry carries the "rn_perturbed" class
# (needed for result tables restored from older exports).
.as_rn_perturbed <- function(entry) {
  if (inherits(entry, "rn_perturbed") || !is.list(entry) ||
      is.null(entry$table)) {
    return(entry)
  }
  class(entry) <- "rn_perturbed"
  entry
}

# Strict class assertion ---------------------------------------------------------

.assert_rn_class <- function(x, cls, arg = "x", hint = NULL, call = parent.frame()) {
  if (!inherits(x, cls)) {
    cli::cli_abort(
      c(
        "x" = "{.arg {arg}} must be a {.cls {cls}} object, not {.cls {class(x)}}.",
        "i" = hint
      ),
      call = call
    )
  }
  invisible(TRUE)
}

# Logging helpers (shared between functional API and R6 class) --------------------

.rn_log_info <- function(msg, envir = parent.frame()) {
  if (Sys.getenv("SDC_LOG_LEVEL") != "OFF") {
    cli::cli_alert_info(msg, .envir = envir)
  }
}

.rn_log_success <- function(msg, envir = parent.frame()) {
  if (Sys.getenv("SDC_LOG_LEVEL") != "OFF") {
    cli::cli_alert_success(msg, .envir = envir)
  }
}

# Print methods -------------------------------------------------------------------

#' @export
#' @method print rn_initialized
print.rn_initialized <- function(x, ...) {
  cli::cat_line(cli::style_bold("<rn_initialized>"))
  cli::cat_line(
    "  microdata: ",
    nrow(x$microdata),
    " records x ",
    length(names(x$microdata)),
    " columns"
  )
  cli::cat_line(
    "  sensitive_params: ",
    paste(
      names(x$sensitive_params),
      sapply(x$sensitive_params, function(v) paste(v, collapse = ",")),
      sep = " = ",
      collapse = ", "
    ) %||%
      "none"
  )
  cli::cat_line("  n_threads: ", x$n_threads)
  cli::cat_line(
    "  rebalanced: ",
    isTRUE(x$rebal_status$done)
  )
  invisible(x)
}

#' @export
#' @method print rn_rebalanced
print.rn_rebalanced <- function(x, ...) {
  cli::cat_line(cli::style_bold("<rn_rebalanced>"))
  cli::cat_line(
    "  microdata: ",
    nrow(x$microdata),
    " records x ",
    length(names(x$microdata)),
    " columns"
  )
  cli::cat_line("  num_var: ", x$meta$num_var %||% "<unknown>")
  cli::cat_line(
    "  dim_list: ",
    paste(names(x$meta$dim_list), collapse = " x ") %||% "<unknown>"
  )
  cli::cat_line("  n_threads: ", x$meta$n_threads)
  invisible(x)
}

#' @export
#' @method print rn_perturbed
print.rn_perturbed <- function(x, ...) {
  cli::cat_line(cli::style_bold("<rn_perturbed>"))
  cli::cat_line(
    "  table: ",
    nrow(x$table),
    " cells x ",
    length(names(x$table)),
    " columns"
  )
  cli::cat_line(
    "  dimensions: ",
    paste(names(x$dim_list), collapse = " x")
  )
  cli::cat_line(
    "  variables: ",
    paste(names(x$variables), collapse = ", ")
  )
  cli::cat_line(
    "  round: ",
    isTRUE(x$round),
    " | hash: ",
    substr(as.character(x$hash), 1, 12)
  )
  invisible(x)
}

# Conversion methods ----------------------------------------------------------------

#' @export
#' @method as.data.table rn_initialized
as.data.table.rn_initialized <- function(x, ...) {
  dt <- copy(x$microdata)
  dt[, record_id := NULL]
  dt
}

#' @export
#' @method as.data.table rn_rebalanced
as.data.table.rn_rebalanced <- function(x, ...) {
  dt <- copy(x$microdata)
  if ("record_id" %in% names(dt)) {
    dt[, record_id := NULL]
  }
  dt
}

#' @export
#' @method as.data.table rn_perturbed
as.data.table.rn_perturbed <- function(x, ...) {
  copy(x$table)
}
