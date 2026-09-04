library(data.table)

# Tests for get_microdata()
test_that("get_microdata returns data.table without record_id by default", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  microdata <- sdc$get_microdata()

  expect_s3_class(microdata, "data.table")
  expect_false("record_id" %in% names(microdata))
  expect_true("direction" %in% names(microdata))
  expect_true("direction_rebalanced" %in% names(microdata))
  expect_true("noise_multiplier" %in% names(microdata))
})

test_that("get_microdata includes record_id when requested", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  microdata <- sdc$get_microdata(include_record_id = TRUE)

  expect_s3_class(microdata, "data.table")
  expect_true("record_id" %in% names(microdata))
  expect_equal(nrow(microdata), nrow(dt))
})

test_that("get_microdata returns copy, not reference", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  microdata <- sdc$get_microdata()
  original_value <- microdata[1, turnover]

  # Modify the returned data
  microdata[1, turnover := 9999]

  # Internal data should be unchanged
  microdata2 <- sdc$get_microdata()
  expect_equal(microdata2[1, turnover], original_value)
})

test_that("get_microdata preserves all original columns", {
  dt <- create_test_data()
  dt[, extra_col := "test"]
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  microdata <- sdc$get_microdata()

  expect_true("extra_col" %in% names(microdata))
  expect_true(all(c("REGION", "INDUSTRY", "turnover") %in% names(microdata)))
})

# Tests for list_tables()
test_that("list_tables returns empty data.table when no perturbations", {
  dt <- create_test_data()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))

  # cli::cli_alert_warning prints to console but doesn't use R's warning()
  # So we just check the result
  result <- sdc$list_tables()

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) == 0)
  expect_true(all(c("table_name", "dimensions", "variables") %in% names(result)))
})

test_that("list_tables returns correct columns", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "table1")

  result <- sdc$list_tables()

  expect_s3_class(result, "data.table")
  expect_true("table_name" %in% names(result))
  expect_true("dimensions" %in% names(result))
  expect_true("variables" %in% names(result))
})

test_that("list_tables shows correct dimension labels", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "table1")

  result <- sdc$list_tables()

  expect_true(nrow(result) == 1)
  expect_true(result[1, dimensions] == "REGION x INDUSTRY")
})

test_that("list_tables shows correct variable list", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "table1")

  result <- sdc$list_tables()

  expect_true("turnover" %in% result[1, variables])
})

test_that("list_tables handles multiple tables", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "table1")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "table2")

  result <- sdc$list_tables()

  expect_true(nrow(result) == 2)
  expect_true(all(c("table1", "table2") %in% result$table_name))
})

test_that("list_tables handles multiple variables per table", {
  dt <- create_test_data()
  dt[, assets := runif(100, 50, 5000)]
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = c("turnover", "assets"), name = "multi_var")

  result <- sdc$list_tables()

  expect_true(nrow(result) == 1)
  expect_true(grepl("turnover", result[1, variables]))
  expect_true(grepl("assets", result[1, variables]))
})

test_that("list_tables returns correct number of rows", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  # No tables yet
  expect_true(nrow(sdc$list_tables()) == 0)

  # Add one table
  sdc$perturb(dim_list = dims, variables = "turnover", name = "table1")
  expect_true(nrow(sdc$list_tables()) == 1)

  # Add another table
  sdc$perturb(dim_list = dims, variables = "turnover", name = "table2")
  expect_true(nrow(sdc$list_tables()) == 2)
})
