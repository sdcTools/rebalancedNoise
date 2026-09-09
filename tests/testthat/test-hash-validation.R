library(data.table)

test_that("hash validation prevents conflicting hierarchies", {
  dt <- create_test_data()
  dims1 <- create_dims_region_only()
  dims2 <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc$rebalance(dim_list = dims1, num_var = "turnover")

  # First perturb with dims1
  sdc$perturb(dim_list = dims1, variables = "turnover", name = "test")

  # Try to use same name with different hierarchy - should error
  expect_error(
    sdc$perturb(dim_list = dims2, variables = "turnover", name = "test"),
    regexp = "Table name.*is already in use"
  )
})

test_that("different names allow different hierarchies", {
  dt <- create_test_data()
  dims1 <- create_dims_region_only()
  dims2 <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc$rebalance(dim_list = dims1, num_var = "turnover")

  # Should work with different names
  sdc$perturb(dim_list = dims1, variables = "turnover", name = "table1")
  sdc$perturb(dim_list = dims2, variables = "turnover", name = "table2")

  results <- sdc$get_results()
  expect_true("table1" %in% names(results))
  expect_true("table2" %in% names(results))
})

test_that("same hierarchy with same name works (idempotent)", {
  dt <- create_test_data(n_rows = 100)
  dims <- create_dims_region_only()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  # First perturb
  sdc$perturb(dim_list = dims, variables = "turnover", name = "idempotent")

  # Second perturb with same name and same hierarchy should work (but skip due to cache)
  expect_message(
    sdc$perturb(dim_list = dims, variables = "turnover", name = "idempotent"),
    "already calculated"
  )

  # Force should recalculate (but may produce messages)
  sdc$perturb(dim_list = dims, variables = "turnover", name = "idempotent", force = TRUE)
})

test_that("imported result tables are recognized as cached", {
  dt <- create_test_data()
  dims <- create_dims_region_only()

  sdc1 <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc1$rebalance(dim_list = dims, num_var = "turnover")
  sdc1$perturb(dim_list = dims, variables = "turnover", name = "tbl")

  sdc2 <- rn_setup(data = sdc1$export(include_results = TRUE))

  # Re-perturbing an existing variable must hit the cache, not re-merge
  expect_message(
    sdc2$perturb(dim_list = dims, variables = "turnover", name = "tbl"),
    "already calculated"
  )
  cols <- names(sdc2$get_results("tbl"))
  expect_false(any(grepl("\\.(x|y)$", cols)))
})

test_that("force = TRUE replaces columns instead of duplicating them", {
  dt <- create_test_data()
  dims <- create_dims_region_only()

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "forced")

  res_before <- sdc$get_results("forced")
  expect_message(
    sdc$perturb(
      dim_list = dims, variables = "turnover",
      name = "forced", force = TRUE
    ),
    "Added"
  )
  res_after <- sdc$get_results("forced")

  expect_setequal(names(res_after), names(res_before))
  expect_false(any(grepl("\\.(x|y)$", names(res_after))))
  expect_equal(
    res_after[order(REGION)],
    res_before[order(REGION)]
  )
  # Long format must still work after forcing
  res_long <- sdc$get_results("forced", format = "long")
  expect_true("val_pert" %in% names(res_long))
})

test_that("hash includes sensitive_params", {
  dt <- create_test_data(n_rows = 100)
  dims <- create_dims_region_only()

  sdc1 <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 3))
  sdc1$rebalance(dim_list = dims, num_var = "turnover")
  sdc1$perturb(dim_list = dims, variables = "turnover", name = "params3")

  sdc2 <- rn_setup(data = dt, sensitive_params = create_sensitive_params(n_threshold = 5))
  sdc2$rebalance(dim_list = dims, num_var = "turnover")

  # Different sensitive_params means different hash, so different name works
  expect_message(
    sdc2$perturb(dim_list = dims, variables = "turnover", name = "params5"),
    "Created new table"
  )

  # Verify both objects work independently
  expect_true("params3" %in% names(sdc1$get_results()))
  expect_true("params5" %in% names(sdc2$get_results()))
})
