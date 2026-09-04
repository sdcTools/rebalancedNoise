#' @keywords internal
#' @import R6
#' @import data.table
#' @import sdcTable
#' @useDynLib rebalancedNoise, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom sdcHierarchies hier_create
#' @importFrom parallel detectCores
#' @importFrom stats quantile
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
  "slot",
  "var_name",
  "temp_var",
  "measure_type",
  "is_sensitive",
  "val_orig",
  "val_pert",
  "val_pert_init",
  "diff_init_pct",
  "diff_final_pct",
  "is_internal",
  "is_internal.y",
  "strID.new",
  "pert_val",
  "dir_used"
))
