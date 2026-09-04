library(data.table)

test_that("direction_rebalanced is strictly integerish (+/-1) after rebalance", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  md <- sdc$get_microdata()
  dirs <- md$direction_rebalanced

  expect_type(dirs, "integer")
  expect_false(any(is.na(dirs)))
  expect_false(any(is.nan(dirs)))
  expect_false(any(is.infinite(dirs)))
  expect_true(all(dirs %in% c(1L, -1L)))
})

test_that("direction_rebalanced is strictly integerish with varied multipliers", {
  dt <- create_test_data_countries()
  dims <- create_dims_countries()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  md <- sdc$get_microdata()
  dirs <- md$direction_rebalanced

  expect_type(dirs, "integer")
  expect_true(all(dirs %in% c(1L, -1L)))
})

test_that("rebalancing handles zero values and zero noise_multiplier without NaN", {
  dt <- create_test_data()
  # Regression: back-solving (pert/orig - 1)/mult produced NaN for
  # zero values and zero noise multipliers
  dt[1:5, turnover := 0]
  dt[6:10, noise_multiplier := 0]
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  md <- sdc$get_microdata()
  dirs <- md$direction_rebalanced

  expect_type(dirs, "integer")
  expect_false(any(is.na(dirs)))
  expect_false(any(is.nan(dirs)))
  expect_true(all(dirs %in% c(1L, -1L)))

  # Perturbed results must stay finite
  sdc$perturb(dim_list = dims, variables = "turnover", name = "zero_check")
  res <- sdc$get_results("zero_check")
  expect_false(any(is.na(res$turnover_pert)))
  expect_false(any(is.nan(res$turnover_pert)))
})

test_that("import rejects export with corrupt direction_rebalanced", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc1 <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc1$rebalance(dim_list = dims, num_var = "turnover")
  exported <- sdc1$export()

  # Corrupt the rebalanced directions
  exported$microdata[1, direction_rebalanced := NaN]
  expect_error(
    rn_setup(data = exported),
    "invalid direction_rebalanced"
  )

  exported <- sdc1$export()
  exported$microdata[1, direction_rebalanced := 0]
  expect_error(
    rn_setup(data = exported),
    "invalid direction_rebalanced"
  )
})
