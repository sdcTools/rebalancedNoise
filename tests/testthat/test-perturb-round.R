library(data.table)

test_that("round = FALSE (default) keeps fractional perturbed values", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "no_round")

  res <- sdc$get_results("no_round")
  expect_true(any(res$turnover_pert != round(res$turnover_pert)))
})

test_that("round = TRUE yields integer perturbed values (wide format)", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(
    dim_list = dims, variables = "turnover",
    name = "rounded", round = TRUE
  )

  res <- sdc$get_results("rounded")

  expect_true(all(res$turnover_pert == round(res$turnover_pert)))
  expect_true(all(res$turnover_init == round(res$turnover_init)))
  # Original (unperturbed) values are NOT rounded
  expect_true(any(res$turnover != round(res$turnover)))
  # Rounding adds at most 0.5 per contributing record to a cell total
  sdc$perturb(dim_list = dims, variables = "turnover", name = "unrounded")
  res_un <- sdc$get_results("unrounded")
  expect_true(all(abs(res$turnover_pert - res_un$turnover_pert) <= 0.5 * res$n_obs))
})

test_that("round = TRUE yields integer perturbed values (long format)", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(
    dim_list = dims, variables = "turnover",
    name = "rounded_long", round = TRUE
  )

  res <- sdc$get_results("rounded_long", format = "long")

  expect_true(all(res$val_pert == round(res$val_pert)))
  expect_true(all(res$val_pert_init == round(res$val_pert_init)))
})

test_that("round = TRUE works for multiple variables", {
  dt <- create_test_data()
  dt[, assets := runif(100, 50, 5000)]
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(
    dim_list = dims, variables = c("turnover", "assets"),
    name = "rounded_multi", round = TRUE
  )

  res <- sdc$get_results("rounded_multi")

  for (v in c("turnover", "assets")) {
    expect_true(all(res[[paste0(v, "_pert")]] ==
                      round(res[[paste0(v, "_pert")]])))
    expect_true(all(res[[paste0(v, "_init")]] ==
                      round(res[[paste0(v, "_init")]])))
  }
})

test_that("round flag is part of the table hash", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "tbl")

  # Same name, different round flag -> hash collision
  expect_error(
    sdc$perturb(
      dim_list = dims, variables = "turnover",
      name = "tbl", round = TRUE
    ),
    "already in use"
  )

  # Re-running with identical arguments uses the cache (no error)
  expect_no_error(
    sdc$perturb(dim_list = dims, variables = "turnover", name = "tbl")
  )
})

test_that("invalid round argument is rejected", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  expect_error(
    sdc$perturb(
      dim_list = dims, variables = "turnover",
      name = "bad1", round = "yes"
    ),
    "single logical"
  )
  expect_error(
    sdc$perturb(
      dim_list = dims, variables = "turnover",
      name = "bad2", round = NA
    ),
    "single logical"
  )
  expect_error(
    sdc$perturb(
      dim_list = dims, variables = "turnover",
      name = "bad3", round = c(TRUE, FALSE)
    ),
    "single logical"
  )
})

test_that("summarize works on rounded tables", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(
    dim_list = dims, variables = "turnover",
    name = "rounded_sum", round = TRUE
  )

  result <- sdc$summarize(table = "rounded_sum", target_vars = "turnover")

  expect_true(is.finite(result$overall$mape_final))
  expect_true(result$meta$n_total_cells > 0)
})
