library(data.table)

test_that("multiple variables accumulate in same table", {
  dt <- create_test_data()
  dt[, assets := runif(100, 50, 5000)]
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = c("turnover", "assets"), name = "multi")

  results <- sdc$get_results("multi")

  # Check both variables exist
  expect_true("turnover" %in% names(results))
  expect_true("assets" %in% names(results))

  # Check columns for both variables
  expect_true("turnover" %in% names(results))
  expect_true("turnover_init" %in% names(results))
  expect_true("turnover_pert" %in% names(results))
  expect_true("assets" %in% names(results))
  expect_true("assets_init" %in% names(results))
  expect_true("assets_pert" %in% names(results))
})

test_that("column ordering is correct after merging", {
  dt <- create_test_data(n_rows = 100)
  dt[, assets := runif(100, 50, 5000)]
  dims <- create_dims_region_only()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = c("turnover", "assets"), name = "ordered")

  results <- sdc$get_results("ordered")

  # Check column order: dims -> meta -> variables grouped
  cols <- names(results)

  # Dimensions first
  expect_equal(cols[1], "REGION")

  # Then metadata (strID removed from user-facing output)
  expect_false("strID" %in% cols)
  expect_true("n_obs" %in% cols)
  expect_true("is_internal" %in% cols)

  # Then turnover columns together
  turnover_idx <- which(grepl("^turnover", cols))
  assets_idx <- which(grepl("^assets", cols))

  # All turnover columns should come before assets columns
  expect_true(max(turnover_idx) < min(assets_idx))
})

test_that("3-way perturbation columns are created", {
  dt <- create_test_data(n_rows = 100)
  dims <- create_dims_region_only()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "three_way")

  results <- sdc$get_results("three_way")

  # Check all three versions exist
  expect_true("turnover" %in% names(results))
  expect_true("turnover_init" %in% names(results))
  expect_true("turnover_pert" %in% names(results))

  # Check that init and pert are different
  expect_false(all(results$turnover_init == results$turnover_pert))
})

test_that("long format includes init columns", {
  dt <- create_test_data(n_rows = 100)
  dims <- create_dims_region_only()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "long_test")

  res_long <- sdc$get_results("long_test", format = "long")

  # Check long format columns
  expect_true("val_orig" %in% names(res_long))
  expect_true("val_pert_init" %in% names(res_long))
  expect_true("val_pert" %in% names(res_long))
  expect_true("diff_init_pct" %in% names(res_long))
  expect_true("diff_final_pct" %in% names(res_long))
})
