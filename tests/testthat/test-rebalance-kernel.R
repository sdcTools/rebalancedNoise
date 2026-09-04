library(data.table)

test_that("kernel returns exactly +/-1 directions for non-sensitive cells", {
  set.seed(21)
  sizes <- rep(25, 40)
  starts0 <- as.integer(cumsum(c(0, sizes[-length(sizes)])))
  cell <- data.table(
    orig = round(runif(sum(sizes), 0, 500), 2),
    noise_multiplier = 0.05,
    direction_rebalanced = sample(c(1, -1), sum(sizes), replace = TRUE)
  )
  dir <- rebalancedNoise:::rebalance_cells_cpp(
    cell$orig, cell$noise_multiplier, cell$direction_rebalanced,
    rep(FALSE, 40), starts0, 1L
  )
  expect_true(all(dir %in% c(1, -1)))
  expect_length(dir, sum(sizes))
})

test_that("greedy pass keeps the residual cell noise within the largest impact", {
  set.seed(22)
  sizes <- sample(2:15, 50, replace = TRUE)
  starts0 <- as.integer(cumsum(c(0, sizes[-length(sizes)])))
  orig <- round(runif(sum(sizes), 1, 500), 2)
  cell <- data.table(
    orig = orig,
    noise_multiplier = 0.1,
    direction_rebalanced = rep(1L, sum(sizes))
  )
  dir <- rebalancedNoise:::rebalance_cells_cpp(
    cell$orig, cell$noise_multiplier, cell$direction_rebalanced,
    rep(FALSE, 50), starts0, 1L
  )
  noise <- orig * dir * cell$noise_multiplier
  cell_noises <- split(noise, rep(seq_along(sizes), sizes))
  for (cn in cell_noises) {
    expect_lte(abs(sum(cn)), max(abs(cn)))
  }
})

test_that("sensitive cells keep their fixed directions exactly", {
  set.seed(23)
  cell <- data.table(
    orig = runif(20, 1, 500),
    noise_multiplier = 0.05,
    direction_rebalanced = sample(c(1, -1), 20, replace = TRUE)
  )
  kern <- rebalancedNoise:::rebalance_cells_cpp(
    cell$orig, cell$noise_multiplier, cell$direction_rebalanced,
    TRUE, 0L, 1L
  )
  expect_identical(as.double(kern), as.double(cell$direction_rebalanced))
})

test_that("single-record cells apply their own direction", {
  kern <- rebalancedNoise:::rebalance_cells_cpp(50, 0.05, -1, TRUE, 0L, 1L)
  expect_identical(as.double(kern), -1)
})

test_that("multi-threaded kernel output matches single-threaded output", {
  set.seed(24)
  sizes <- rep(25, 40)
  starts0 <- as.integer(cumsum(c(0, sizes[-length(sizes)])))
  cell <- data.table(
    orig = round(runif(sum(sizes), 0, 500), 2),
    noise_multiplier = 0.05,
    direction_rebalanced = sample(c(1, -1), sum(sizes), replace = TRUE)
  )
  sens <- rep(c(TRUE, FALSE), 20)
  k1 <- rebalancedNoise:::rebalance_cells_cpp(
    cell$orig, cell$noise_multiplier, cell$direction_rebalanced,
    sens, starts0, 1L
  )
  k4 <- rebalancedNoise:::rebalance_cells_cpp(
    cell$orig, cell$noise_multiplier, cell$direction_rebalanced,
    sens, starts0, 4L
  )
  expect_identical(k1, k4)
})
