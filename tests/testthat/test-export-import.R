library(data.table)

test_that("export returns valid object when no file", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  exported <- sdc$export()

  expect_s3_class(exported, "rebalancedNoise_ExportData")
  expect_true(exported$rebal_status$done)
  expect_true("direction_rebalanced" %in% names(exported$microdata))
  expect_equal(exported$sensitive_params$n_threshold, 3)
  expect_true("export_metadata" %in% names(exported))
  expect_true("timestamp" %in% names(exported$export_metadata))
  expect_true("package_version" %in% names(exported$export_metadata))
})

test_that("export saves to file when file given", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")

  temp_file <- tempfile(fileext = ".rds")
  result <- sdc$export(file = temp_file)

  expect_s3_class(result, "rebalancedNoise_Class")
  expect_true(file.exists(temp_file))

  # Verify file content
  exported <- readRDS(temp_file)
  expect_s3_class(exported, "rebalancedNoise_ExportData")
  expect_true(exported$rebal_status$done)

  unlink(temp_file)
})

test_that("export with results includes tables", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "table1")

  temp_file <- tempfile(fileext = ".rds")
  sdc$export(include_results = TRUE, file = temp_file)

  exported <- readRDS(temp_file)
  expect_true("table1" %in% names(exported$result_tables))
  expect_true(!is.null(exported$result_meta))
  expect_true(length(exported$result_meta) > 0)

  unlink(temp_file)
})

test_that("export without results excludes tables", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc$rebalance(dim_list = dims, num_var = "turnover")
  sdc$perturb(dim_list = dims, variables = "turnover", name = "table1")

  temp_file <- tempfile(fileext = ".rds")
  sdc$export(include_results = FALSE, file = temp_file)

  exported <- readRDS(temp_file)
  expect_null(exported$result_tables)
  expect_null(exported$result_meta)

  unlink(temp_file)
})

test_that("rn_setup imports from export object", {
  dt <- create_test_data()
  dims <- create_dims()

  # Create and export
  sdc1 <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc1$rebalance(dim_list = dims, num_var = "turnover")
  exported <- sdc1$export()

  # Import via rn_setup with export object
  sdc2 <- rn_setup(data = exported)

  # Verify rebalancing status is preserved (access via private)
  expect_true(sdc2$.__enclos_env__$private$rebal_status$done)
  expect_true("direction_rebalanced" %in% names(sdc2$.__enclos_env__$private$microdata))
  expect_equal(sdc2$sensitive_params$n_threshold, 3)
})

test_that("import from export object overrides sensitive_params with exported values", {
  dt <- create_test_data()
  dims <- create_dims()

  # Distinct non-default parameters so an ignored override is detectable
  params <- list(n_threshold = 7, p_rule = 90)
  sdc1 <- rn_setup(data = dt, sensitive_params = params)
  sdc1$rebalance(dim_list = dims, num_var = "turnover")
  exported <- sdc1$export()

  # Import without passing sensitive_params: exported values must win over
  # the default list(n_threshold = 3)
  sdc2 <- rn_setup(data = exported)
  expect_equal(sdc2$sensitive_params$n_threshold, 7)
  expect_equal(sdc2$sensitive_params$p_rule, 90)

  # Same for the functional entry point
  state <- rn_init(exported)
  expect_equal(state$sensitive_params$n_threshold, 7)
  expect_equal(state$sensitive_params$p_rule, 90)
})

test_that("rn_setup imports from file path", {
  dt <- create_test_data()
  dims <- create_dims()

  # Create and export to file
  sdc1 <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc1$rebalance(dim_list = dims, num_var = "turnover")

  temp_file <- tempfile(fileext = ".rds")
  sdc1$export(file = temp_file)

  # Import via rn_setup with file path
  sdc2 <- rn_setup(data = temp_file)

  # Verify rebalancing status is preserved
  expect_true(sdc2$.__enclos_env__$private$rebal_status$done)
  expect_true("direction_rebalanced" %in% names(sdc2$.__enclos_env__$private$microdata))

  unlink(temp_file)
})

test_that("imported data can be used for perturb", {
  dt <- create_test_data()
  dims <- create_dims()

  # Create and export
  sdc1 <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc1$rebalance(dim_list = dims, num_var = "turnover")

  temp_file <- tempfile(fileext = ".rds")
  sdc1$export(file = temp_file)

  # Import and perturb
  sdc2 <- rn_setup(data = temp_file)
  sdc2$perturb(dim_list = dims, variables = "turnover", name = "new_table")

  # Verify result exists
  results <- sdc2$get_results("new_table")
  expect_true("turnover" %in% names(results))
  expect_true("turnover_init" %in% names(results))
  expect_true("turnover_pert" %in% names(results))

  unlink(temp_file)
})

test_that("imported data can access existing results", {
  dt <- create_test_data()
  dims <- create_dims()

  # Create, perturb and export with results
  sdc1 <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc1$rebalance(dim_list = dims, num_var = "turnover")
  sdc1$perturb(dim_list = dims, variables = "turnover", name = "table1")

  temp_file <- tempfile(fileext = ".rds")
  sdc1$export(file = temp_file)

  # Import and access results
  sdc2 <- rn_setup(data = temp_file)
  results <- sdc2$get_results("table1")

  expect_true(!is.null(results))
  expect_true("turnover" %in% names(results))

  unlink(temp_file)
})

test_that("import validates export structure", {
  # Create invalid export object
  invalid_export <- list(microdata = create_test_data())
  class(invalid_export) <- "rebalancedNoise_ExportData"

  expect_error(rn_setup(data = invalid_export), "missing required elements")
})

test_that("import validates microdata columns", {
  dt <- create_test_data()
  # Remove required column
  dt[, direction := NULL]

  invalid_export <- list(
    microdata = dt,
    sensitive_params = list(n_threshold = 3),
    rebal_status = list(done = FALSE, dim_list = NULL, params = NULL)
  )
  class(invalid_export) <- "rebalancedNoise_ExportData"

  expect_error(rn_setup(data = invalid_export), "missing columns")
})

test_that("import validates direction values", {
  dt <- create_test_data()
  dt[, direction := 0] # Invalid direction

  invalid_export <- list(
    microdata = dt,
    sensitive_params = list(n_threshold = 3),
    rebal_status = list(done = FALSE, dim_list = NULL, params = NULL)
  )
  class(invalid_export) <- "rebalancedNoise_ExportData"

  expect_error(rn_setup(data = invalid_export), "invalid direction")
})

test_that("import validates noise_multiplier", {
  dt <- create_test_data()
  dt[, noise_multiplier := -0.05] # Negative

  invalid_export <- list(
    microdata = dt,
    sensitive_params = list(n_threshold = 3),
    rebal_status = list(done = FALSE, dim_list = NULL, params = NULL)
  )
  class(invalid_export) <- "rebalancedNoise_ExportData"

  expect_error(rn_setup(data = invalid_export), "negative noise")
})

test_that("non-export file raises error", {
  dt <- create_test_data()
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(dt, temp_file) # Save regular data, not export

  expect_error(rn_setup(data = temp_file), "does not contain valid")

  unlink(temp_file)
})

test_that("export preserves dim_list in rebal_status", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc1 <- rn_setup(data = dt, sensitive_params = list(n_threshold = 3))
  sdc1$rebalance(dim_list = dims, num_var = "turnover")

  exported <- sdc1$export()

  expect_true(!is.null(exported$rebal_status$dim_list))
  expect_true("REGION" %in% names(exported$rebal_status$dim_list))
  expect_true("INDUSTRY" %in% names(exported$rebal_status$dim_list))
})

test_that("export preserves sensitive_params in rebal_status", {
  dt <- create_test_data()
  dims <- create_dims()

  sdc1 <- rn_setup(data = dt, sensitive_params = list(n_threshold = 5, p_rule = 10))
  sdc1$rebalance(dim_list = dims, num_var = "turnover")

  exported <- sdc1$export()

  expect_true(!is.null(exported$rebal_status$params))
  expect_equal(exported$rebal_status$params$n_threshold, 5)
  expect_equal(exported$rebal_status$params$p_rule, 10)
})
