# Internal helper: Perturb using existing direction_rebalanced (no rebalancing)
# Simple perturbation: val_pert = val_orig * (1 + direction * noise_multiplier)
.perturb_with_direction <- function(
  data,
  target_var,
  direction_col = "direction_rebalanced",
  noise_col = "noise_multiplier"
) {
  if (!(direction_col %in% names(data))) {
    cli::cli_abort(c(
      "x" = "Column {.val {direction_col}} not found in data.",
      "i" = "Has {.fn rebalance} been called?"
    ))
  }

  # Simple perturbation using existing directions
  pert_vals <- data[[target_var]] *
    (1 + data[[direction_col]] * data[[noise_col]])
  return(pert_vals)
}

# Internal helper: Aggregate perturbed values through hierarchy
# Uses sdcTable to propagate results
.aggregate_through_hierarchy <- function(
  base_data,
  dimList,
  target_var,
  pert_init_col,
  pert_col
) {
  all_target_cols <- c(target_var, pert_init_col, pert_col)

  # Create temporary problem for aggregation
  tmp_prob <- sdcTable::makeProblem(
    data = as.data.frame(base_data),
    dimList = dimList,
    numVarInd = all_target_cols
  )

  # Aggregate through hierarchy
  full_res <- as.data.table(
    sdcProb2df(
      tmp_prob,
      addDups = TRUE,
      addNumVars = TRUE,
      dimCodes = "original"
    )
  )

  return(full_res)
}
