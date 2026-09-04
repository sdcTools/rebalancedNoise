test_that("rn_init returns rn_initialized with expected fields", {
  state <- rn_init(create_test_data(), sensitive_params = create_sensitive_params(3))

  expect_s3_class(state, "rn_initialized")
  expect_true(all(c(
    "microdata", "sensitive_params", "n_threads",
    "rebal_status", "result_tables"
  ) %in% names(state)))
  expect_true("record_id" %in% names(state$microdata))
  expect_false(isTRUE(state$rebal_status$done))
})

test_that("rn_rebalance returns rn_rebalanced with meta", {
  dims <- create_dims()
  reb <- rn_init(create_test_data(), sensitive_params = create_sensitive_params(3)) |>
    rn_rebalance(dim_list = dims, num_var = "turnover")

  expect_s3_class(reb, "rn_rebalanced")
  expect_true("direction_rebalanced" %in% names(reb$microdata))
  expect_true(all(reb$microdata$direction_rebalanced %in% c(1, -1)))
  expect_identical(reb$meta$dim_list, dims)
  expect_identical(reb$meta$num_var, "turnover")
})

test_that("print methods do not error", {
  dims <- create_dims()
  state <- rn_init(create_test_data(), sensitive_params = create_sensitive_params(3))
  reb <- rn_rebalance(state, dim_list = dims, num_var = "turnover")
  res <- rn_perturb(reb, dim_list = dims, variables = "turnover")

  expect_output(print(state), "rn_initialized")
  expect_output(print(reb), "rn_rebalanced")
  expect_output(print(res), "rn_perturbed")
})

test_that("as.data.table conversions return plain data.tables", {
  dims <- create_dims()
  state <- rn_init(create_test_data(), sensitive_params = create_sensitive_params(3))
  reb <- rn_rebalance(state, dim_list = dims, num_var = "turnover")
  res <- rn_perturb(reb, dim_list = dims, variables = "turnover")

  dt_state <- as.data.table(state)
  expect_identical(class(dt_state), c("data.table", "data.frame"))
  expect_false("record_id" %in% names(dt_state))

  dt_reb <- as.data.table(reb)
  expect_identical(class(dt_reb), c("data.table", "data.frame"))
  expect_true("direction_rebalanced" %in% names(dt_reb))

  dt_res <- as.data.table(res)
  expect_identical(class(dt_res), c("data.table", "data.frame"))
  expect_true("turnover_pert" %in% names(dt_res))
})

test_that("R6 get_state() bridges into functional API", {
  dt <- create_test_data(n_rows = 200)
  dims <- create_dims()
  params <- create_sensitive_params(3)

  # before rebalancing: rn_initialized
  sdc <- rn_setup(data = dt, sensitive_params = params)
  expect_s3_class(sdc$get_state(), "rn_initialized")

  # after rebalancing: rn_rebalanced, directly usable with rn_perturb
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  state <- sdc$get_state()
  expect_s3_class(state, "rn_rebalanced")

  res <- rn_perturb(state, dim_list = dims, variables = "turnover")
  wide_fun <- rn_format(res)
  sdc$perturb(dim_list = dims, variables = "turnover", name = "cmp")
  expect_equal(wide_fun, sdc$get_results("cmp"), ignore_attr = TRUE)
})

test_that("R6 get_table() bridges into rn_summarize and rn_format", {
  dims <- create_dims_region_only()
  sdc <- rn_setup(data = create_test_data(), sensitive_params = create_sensitive_params(3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "tbl")

  obj <- sdc$get_table("tbl")
  expect_s3_class(obj, "rn_perturbed")

  # equivalence with R6 outputs
  expect_equal(rn_format(obj), sdc$get_results("tbl"), ignore_attr = TRUE)

  s_fun <- rn_summarize(obj)
  s_r6 <- sdc$summarize(table = "tbl")
  expect_equal(s_fun, s_r6)

  expect_error(sdc$get_table("nope"), "not found")
})

test_that("get_table() returns a defensive copy", {
  dims <- create_dims_region_only()
  sdc <- rn_setup(data = create_test_data(), sensitive_params = create_sensitive_params(3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "tbl")

  obj <- sdc$get_table("tbl")
  n_before <- nrow(sdc$get_results("tbl"))
  obj$table[1, turnover_pert := NA_real_]
  expect_equal(nrow(sdc$get_results("tbl")), n_before)
  expect_false(anyNA(sdc$get_results("tbl")$turnover_pert))
})

test_that("export/import keeps rn_perturbed class usable", {
  dims <- create_dims_region_only()
  sdc <- rn_setup(data = create_test_data(), sensitive_params = create_sensitive_params(3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "tbl")

  exported <- sdc$export(include_results = TRUE)
  sdc2 <- rn_setup(exported)
  obj <- sdc2$get_table("tbl")
  expect_s3_class(obj, "rn_perturbed")
  expect_no_error(rn_format(obj))
  expect_no_error(rn_summarize(obj))
})
