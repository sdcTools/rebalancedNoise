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

test_that(".rebalance_cell returns exact +/-1 directions", {
  cell <- data.table(
    orig = c(100, 200, 50),
    noise_multiplier = c(0.05, 0.05, 0.05),
    direction_rebalanced = c(1, -1, 1)
  )
  res <- rebalancedNoise:::.rebalance_cell(cell, "orig", is_sensitive = FALSE)

  expect_type(res$dir, "double")
  expect_true(all(res$dir %in% c(1, -1)))
  expect_length(res$dir, 3)
  expect_length(res$pert_val, 3)
})

test_that(".rebalance_cell keeps cell sum balanced for non-sensitive cells", {
  cell <- data.table(
    orig = c(100, 200, 50, 300),
    noise_multiplier = rep(0.1, 4),
    direction_rebalanced = rep(1, 4)
  )
  res <- rebalancedNoise:::.rebalance_cell(cell, "orig", is_sensitive = FALSE)

  noise <- sum(res$pert_val - cell$orig)
  # Greedy balancing bounds residual noise by the smallest impact
  expect_lte(abs(noise), min(cell$orig * cell$noise_multiplier))
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
