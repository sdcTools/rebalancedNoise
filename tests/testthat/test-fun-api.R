test_that("functional pipeline end-to-end works", {
  dt <- create_test_data(n_rows = 200)
  dims <- create_dims()

  res <- rn_perturb(
    rn_rebalance(
      rn_init(dt, sensitive_params = create_sensitive_params(3)),
      dim_list = dims,
      num_var = "turnover"
    ),
    dim_list = dims,
    variables = "turnover"
  )

  expect_s3_class(res, "rn_perturbed")
  wide <- rn_format(res)
  expect_s3_class(wide, "data.table")
  expect_true(all(c("REGION", "INDUSTRY", "n_obs", "is_internal",
                    "turnover", "turnover_init", "turnover_pert",
                    "is_sens_turnover") %in% names(wide)))

  long <- rn_format(res, format = "long")
  expect_true(all(c("variable", "val_orig", "val_pert", "val_pert_init",
                    "is_sensitive") %in% names(long)))
})

test_that("functional results are identical to R6 engine results", {
  dt <- create_test_data(n_rows = 200)
  dims <- create_dims()
  params <- create_sensitive_params(3)

  sdc <- rn_setup(data = dt, sensitive_params = params)
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = c("turnover"), name = "t1")
  res_r6 <- sdc$get_results("t1")

  res_fun <- rn_format(
    rn_perturb(
      rn_rebalance(
        rn_init(dt, sensitive_params = params),
        dim_list = dims,
        num_var = "turnover"
      ),
      dim_list = dims,
      variables = "turnover"
    )
  )

  expect_equal(res_fun, res_r6, ignore_attr = TRUE)
})

test_that("multiple variables in one rn_perturb call", {
  dt <- create_test_data_countries()
  dims <- create_dims_countries()

  res <- dt |>
    rn_init(sensitive_params = create_sensitive_params(3)) |>
    rn_rebalance(dim_list = dims, num_var = "turnover") |>
    rn_perturb(dim_list = dims, variables = c("turnover", "workers"))

  expect_s3_class(res, "rn_perturbed")
  expect_setequal(names(res$variables), c("turnover", "workers"))

  wide <- rn_format(res)
  expect_true(all(c(
    "turnover", "turnover_init", "turnover_pert",
    "workers", "workers_init", "workers_pert"
  ) %in% names(wide)))

  # variable filtering works via rn_format
  only_tw <- rn_format(res, variables = "turnover")
  expect_false("workers_pert" %in% names(only_tw))
})

test_that("rn_summarize works on functional results", {
  dt <- create_test_data(n_rows = 200)
  dims <- create_dims()

  res <- dt |>
    rn_init(sensitive_params = create_sensitive_params(3)) |>
    rn_rebalance(dim_list = dims, num_var = "turnover") |>
    rn_perturb(dim_list = dims, variables = "turnover")

  s <- rn_summarize(res, target_vars = "turnover")
  expect_true(all(c("overall", "meta") %in% names(s)))
  expect_true(all(c("count", "mape_init", "mape_final") %in% names(s$overall)))

  s_all <- rn_summarize(res)
  expect_named(s_all, "turnover")
})

test_that("strict class checks reject invalid inputs", {
  dt <- create_test_data(n_rows = 50)
  dims <- create_dims()

  # rn_rebalance needs rn_initialized
  expect_error(
    rn_rebalance(dt, dim_list = dims, num_var = "turnover"),
    "rn_init"
  )

  state <- rn_init(dt, sensitive_params = create_sensitive_params(3))

  # rn_perturb requires rebalancing first
  expect_error(
    rn_perturb(state, dim_list = dims, variables = "turnover"),
    "rn_rebalance"
  )

  # rn_perturb rejects plain data.frames
  dt_reb <- dt
  dt_reb[, direction_rebalanced := direction]
  expect_error(
    rn_perturb(dt_reb, dim_list = dims, variables = "turnover"),
    "rn_rebalanced"
  )

  reb <- rn_rebalance(state, dim_list = dims, num_var = "turnover")

  # rn_format / rn_summarize reject anything not rn_perturbed
  expect_error(rn_format(reb$microdata), "rn_perturb")
  expect_error(rn_summarize(reb$microdata), "rn_perturb")
})

test_that("rn_rebalance validates arguments", {
  state <- rn_init(create_test_data(), sensitive_params = create_sensitive_params(3))
  dims <- create_dims()

  expect_error(rn_rebalance(state, dim_list = as.list(1:2), num_var = "turnover"),
    "named"
  )
  expect_error(rn_rebalance(state, dim_list = dims, num_var = 42),
    "single character"
  )
  expect_error(rn_rebalance(state, dim_list = dims, num_var = "nope"),
    "not found"
  )
  bad_dims <- list(NOT_IN_DATA = dims$REGION)
  expect_error(rn_rebalance(state, dim_list = bad_dims, num_var = "turnover"),
    "not found"
  )
})

test_that("rn_perturb validates arguments", {
  reb <- rn_init(create_test_data(), sensitive_params = create_sensitive_params(3)) |>
    rn_rebalance(dim_list = create_dims(), num_var = "turnover")

  expect_error(rn_perturb(reb, dim_list = create_dims(), variables = character(0)),
    "non-empty character"
  )
  expect_error(rn_perturb(reb, dim_list = create_dims(), variables = "nope"),
    "not found"
  )
  expect_error(
    rn_perturb(reb, dim_list = create_dims(), variables = "turnover", round = NA),
    "single logical"
  )
})

test_that("round = TRUE yields integers in functional API", {
  dt <- create_test_data(n_rows = 200)
  dims <- create_dims()

  res <- dt |>
    rn_init(sensitive_params = create_sensitive_params(3)) |>
    rn_rebalance(dim_list = dims, num_var = "turnover") |>
    rn_perturb(dim_list = dims, variables = "turnover", round = TRUE)

  wide <- rn_format(res)
  expect_equal(wide$turnover_pert, round(wide$turnover_pert))
})

test_that("functional API roundtrips through export object", {
  dt <- create_test_data(n_rows = 200)
  dims <- create_dims()
  params <- create_sensitive_params(3)

  sdc <- rn_setup(data = dt, sensitive_params = params)
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  exported <- sdc$export(include_results = FALSE)

  # imported state can be rebalanced/perturbed functionally
  res <- exported |>
    rn_init() |>
    rn_perturb(dim_list = dims, variables = "turnover")
  expect_s3_class(res, "rn_perturbed")

  # and after re-rebalancing
  res2 <- exported |>
    rn_init() |>
    rn_rebalance(dim_list = dims, num_var = "turnover") |>
    rn_perturb(dim_list = dims, variables = "turnover")
  expect_s3_class(res2, "rn_perturbed")

  # file-based import
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf))
  sdc$export(include_results = FALSE, file = tf)
  res3 <- rn_init(tf) |>
    rn_perturb(dim_list = dims, variables = "turnover")
  expect_s3_class(res3, "rn_perturbed")
})
