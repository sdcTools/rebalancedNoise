#' @keywords internal
#' @import R6
#' @import data.table
#' @import sdcTable
#' @import progress
#' @useDynLib rebalancedNoise, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom sdcHierarchies hier_create
#' @importFrom parallel detectCores
#' @importFrom Rcpp sourceCpp
#' @useDynLib rebalancedNoise, .registration = TRUE
"_PACKAGE"

utils::globalVariables(c(
  ".",
  "strID",
  "n_obs",
  "is_base_cell",
  "is_sens",
  "direction_rebalanced",
  "direction",
  "record_id",
  "noise_multiplier",
  "original_idx",
  "impact",
  "slot"
))
