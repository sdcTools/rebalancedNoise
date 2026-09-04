#' Initialize rebalancedNoise SDC Engine
#'
#' @description
#' Initialize the SDC engine. No hierarchy definition needed at setup.
#' Internally, `rn_setup()` calls [rn_init()] and passes the resulting
#' state object to the R6 engine.
#' @param data Input microdata containing `direction` and `noise_multiplier` columns.
#'   Alternatively, can be a file path to an exported `rebalancedNoise_ExportData` object
#'   or an export object itself for reusing previously rebalanced microdata.
#' @param sensitive_params List of SDC rules (e.g., `list(n_threshold = 3, p_rule = 10)`).
#'   If `data` is an export object, this will be overridden by the exported value.
#' @param n_threads Integer specifying threads for parallel sensitivity checking and rebalancing.
#'   Supports `options(rn_threads = X)` or `Sys.setenv(rn_threads = X)`.

#' @return
#' * `rn_setup()`: Returns a new `rebalancedNoise` R6 object.
#' * `rebalancedNoise`: An `R6` class object (accessible via `rn_setup`).
#'
#' @export
#' @rdname rn_setup
#' @seealso [rn_init()], [rn_rebalance()], [rn_perturb()], [rn_format()],
#'   [rn_summarize()] for the functional API.
#' @references
#' Sabolová, R., Tepe, Ö., Adriansson, N., & Almberg, L.-E. (2025).
#' *Using Perturbative Methods for Magnitude Tables in Statistical Disclosure Control*.
#' Paper presented at the UNECE Expert Meeting on Statistical Data Confidentiality,
#' October 15–17, 2025, Barcelona, Spain.
#' [PDF Link](https://unece.org/sites/default/files/2025-10/SDC2025_Sf_Sweden_Almberg_D.pdf)
#' @examples
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
#' dt <- data.table::data.table(
#'   country = sample(countries, N, replace = TRUE),
#'   turnover = runif(N, 10, 1000),
#'   workers = sample(0:50, N, replace = TRUE),
#'   direction = sample(c(1, -1), N, replace = TRUE),
#'   noise_multiplier = 0.05
#' )
#'
#' # Define hierarchies
#' dims_rebalance <- list(
#'   country = sdcHierarchies::hier_create("Total", nodes = countries)  # Detailed
#' )
#' dims_table <- list(
#'   country = sdcHierarchies::hier_create("Total", nodes = countries)  # Aggregated
#' )
#'
#' # Initialize the object (no num_vars needed!)
#' # Note that setting Argument `n_threads` overrides previously set global settings
#' sdc <- rn_setup(
#'   data = dt,
#'   sensitive_params  = list(n_threshold = 15),
#'   n_threads = 3
#' )
#'
#' # Perform rebalancing ONCE on detailed structure (specify variable here)
#' sdc$rebalance(dim_list = dims_rebalance, num_var = "turnover")
#'
#' # Get microdata (without internal record_id)
#' microdata <- sdc$get_microdata()
#'
#' # Get microdata with internal record_id
#' microdata_with_id <- sdc$get_microdata(include_record_id = TRUE)
#'
#' # To re-enable logging, set the level back to "INFO"
#' # This will show cli alerts and progress bars again
#' Sys.setenv(SDC_LOG_LEVEL = "INFO")
#'
#' # Run Perturbation for different table structures
#' # Single variable
#' sdc$perturb(dim_list = dims_table, variables = "turnover", name = "table_a")
#'
#' # Multiple variables in one call
#' sdc$perturb(dim_list = dims_table, variables = c("turnover", "workers"), name = "table_b")
#'
#' # Round perturbed values to whole numbers so all published cells are integers
#' sdc$perturb(
#'   dim_list = dims_table, variables = "turnover",
#'   name = "table_rounded", round = TRUE
#' )
#'
#' # List all perturbed tables
#' tables <- sdc$list_tables()
#'
#' # Retrieve Results (per default: wide-format)
#' sdc$get_results("table_a")
#'
#' # Long-Format is possible too
#' res <- sdc$get_results("table_a", format = "long")
#'
#' # Get all results as named list
#' sdc$get_results()
#'
#' # Summarize results
#' sdc$summarize(table = "table_a", target_vars = "turnover")
#'
#' # Bridge to the functional API (S3 objects)
#' state <- sdc$get_state()
#' res_s3 <- rn_perturb(state, dim_list = dims_table, variables = "workers")
#' data.table::as.data.table(res_s3)
rn_setup <- function(
  data,
  sensitive_params = list(n_threshold = 3),
  n_threads = NULL
) {
  state <- rn_init(
    data = data,
    sensitive_params = sensitive_params,
    n_threads = n_threads
  )
  rebalancedNoise$new(state)
}
