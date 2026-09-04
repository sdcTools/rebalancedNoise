test_that("second rebalance() errors with hint to reset()", {
  dims <- create_dims()
  sdc <- rn_setup(data = create_test_data(), sensitive_params = create_sensitive_params(3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  expect_error(
    sdc$rebalance(dim_list = dims, num_var = "turnover"),
    "reset"
  )
})

test_that("reset() clears rebalancing artifacts and restores microdata", {
  dt <- create_test_data()
  dims <- create_dims()
  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  expect_true("direction_rebalanced" %in% names(sdc$get_microdata()))

  sdc$reset()

  md <- sdc$get_microdata()
  expect_false("direction_rebalanced" %in% names(md))
  expect_false("strID" %in% names(md))
  expect_false(any(grepl("^is_sens_", names(md))))
  expect_setequal(names(md), names(dt))
  expect_equal(md, dt, ignore_attr = TRUE)
})

test_that("reset() clears results, caches and state", {
  dims <- create_dims()
  sdc <- rn_setup(data = create_test_data(), sensitive_params = create_sensitive_params(3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "tbl")

  sdc$reset()

  expect_error(sdc$get_results(), "No results found")
  expect_error(sdc$get_table("tbl"), "not found")
  expect_s3_class(sdc$get_state(), "rn_initialized")
  expect_false(isTRUE(sdc$get_state()$rebal_status$done))
})

test_that("perturb() after reset() requires rebalance() again", {
  dims <- create_dims()
  sdc <- rn_setup(data = create_test_data(), sensitive_params = create_sensitive_params(3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "tbl")

  sdc$reset()

  expect_error(
    sdc$perturb(dim_list = dims, variables = "turnover", name = "tbl"),
    "rebalance"
  )
})

test_that("full fresh start after reset() works with different structure", {
  dt <- create_test_data(n_rows = 200)
  dims <- create_dims()
  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "tbl")

  sdc$reset()
  sdc$rebalance(dim_list = create_dims_region_only(), num_var = "turnover")
  sdc$perturb(
    dim_list = create_dims_region_only(),
    variables = "turnover",
    name = "tbl"
  )

  res <- sdc$get_results("tbl")
  expect_true("turnover_pert" %in% names(res))
  expect_false("INDUSTRY" %in% names(res))
})

test_that("reset() with new sensitive_params is used by rebalance()", {
  dt <- create_test_data()
  dims <- create_dims()

  # reference run with new params in a fresh object
  sdc_new <- rn_setup(data = dt, sensitive_params = create_sensitive_params(0))
  sdc_new$rebalance(dim_list = dims, num_var = "turnover")
  reb_expected <- sdc_new$get_state()$microdata$direction_rebalanced

  sdc <- rn_setup(data = dt, sensitive_params = create_sensitive_params(3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$reset(sensitive_params = create_sensitive_params(0))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  expect_identical(sdc$sensitive_params, create_sensitive_params(0))
  expect_identical(
    sdc$get_state()$microdata$direction_rebalanced,
    reb_expected
  )
})

test_that("reset() rejects non-list sensitive_params", {
  sdc <- rn_setup(data = create_test_data(), sensitive_params = create_sensitive_params(3))
  expect_error(sdc$reset(sensitive_params = 42), "sensitive_params")
})

test_that("export() after reset() reports rebalancing as not done", {
  dims <- create_dims()
  sdc <- rn_setup(data = create_test_data(), sensitive_params = create_sensitive_params(3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "tbl")

  sdc$reset()
  exported <- sdc$export(include_results = TRUE)

  expect_false(isTRUE(exported$rebal_status$done))
  expect_length(exported$result_tables, 0)
  expect_false("direction_rebalanced" %in% names(exported$microdata))
})

test_that("re-imported completed state requires reset() before rebalance()", {
  dims <- create_dims()
  sdc <- rn_setup(data = create_test_data(), sensitive_params = create_sensitive_params(3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc2 <- rn_setup(sdc$export(include_results = FALSE))

  expect_error(sdc2$rebalance(dim_list = dims, num_var = "turnover"), "reset")

  sdc2$reset()
  expect_no_error(sdc2$rebalance(dim_list = dims, num_var = "turnover"))
})
