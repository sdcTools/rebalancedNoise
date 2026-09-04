library(data.table)

test_that("summarize shows overall statistics", {
  dt <- create_test_data()
  dims <- create_dims_region_only()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "test_table")

  # Test summarize returns correct structure
  result <- sdc$summarize(table = "test_table", target_vars = "turnover")

  expect_type(result, "list")
  expect_named(result, c("overall", "meta"))
  expect_true(result$meta$sensitivity_rate >= 0 && result$meta$sensitivity_rate <= 100)
})

test_that("summarize handles no sensitive cells", {
  dt <- create_test_data()
  dims <- create_dims_region_only()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 100))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "test_table")

  # Should not error
  result <- sdc$summarize(table = "test_table", target_vars = "turnover")
  expect_type(result, "list")
})

test_that("summarize returns list for multiple variables", {
  dt <- create_test_data()
  dt[, assets := runif(100, 50, 5000)]
  dims <- create_dims_region_only()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = c("turnover", "assets"), name = "multi_table")

  # Summarize all variables
  result <- sdc$summarize(table = "multi_table")

  expect_type(result, "list")
  expect_named(result, c("turnover", "assets"))
  expect_true(all(c("overall", "meta") %in% names(result$turnover)))
})

test_that("summarize returns metadata correctly", {
  dt <- create_test_data()
  dims <- create_dims_region_only()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "test_table")

  result <- sdc$summarize(table = "test_table", target_vars = "turnover")

  # Check metadata fields
  expect_named(
    result$meta,
    c(
      "sensitivity_rate",
      "n_total_cells",
      "n_sensitive_cells",
      "n_internal_cells",
      "noise_reduction_pct"
    )
  )

  # Check that noise_reduction is calculated
  expect_true(!is.na(result$meta$noise_reduction_pct))
})

test_that("summarize handles all sensitive cells", {
  dt <- create_test_data()
  dims <- create_dims_region_only()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 1))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "test_table")

  # Should not error
  result <- sdc$summarize(table = "test_table", target_vars = "turnover")
  expect_type(result, "list")
})
