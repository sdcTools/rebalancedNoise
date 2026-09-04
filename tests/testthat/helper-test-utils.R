# Test utilities for rebalancedNoise package
# This file provides helper functions and data fixtures for unit tests

library(data.table)

#' Create standard test microdata
#'
#' Generates reproducible test data with REGION, INDUSTRY, turnover, direction,
#' and noise_multiplier columns.
#'
#' @param n_rows Number of rows to generate (default: 100)
#' @param seed Random seed for reproducibility (default: 42)
#' @return data.table with test microdata
create_test_data <- function(n_rows = 100, seed = 42) {
  set.seed(seed)
  data.table(
    REGION = rep(c("North", "South"), length.out = n_rows),
    INDUSTRY = rep(c("Tech", "Mfg", "Retail"), length.out = n_rows),
    turnover = runif(n_rows, 10, 1000),
    direction = sample(c(1, -1), n_rows, replace = TRUE),
    noise_multiplier = 0.05
  )
}

#' Create alternate test microdata (6 countries, 4 types)
#'
#' Generates test data with country and type dimensions for different test scenarios.
#'
#' @param n_rows Number of rows to generate (default: 500)
#' @param seed Random seed for reproducibility (default: 1)
#' @return data.table with test microdata
create_test_data_countries <- function(n_rows = 500, seed = 1) {
  countries <- c("AT", "DE", "NL", "SE", "FR", "IT")
  types <- LETTERS[1:4]
  set.seed(seed)
  data.table(
    country = sample(countries, n_rows, replace = TRUE),
    type = sample(types, n_rows, replace = TRUE),
    turnover = runif(n_rows, 10, 1000),
    workers = sample(0:50, n_rows, replace = TRUE),
    direction = sample(c(1, -1), n_rows, replace = TRUE),
    noise_multiplier = round(runif(n_rows, min = 0.75, max = 1.25), digits = 2)
  )
}

#' Create standard dimension hierarchies (REGION x INDUSTRY)
#'
#' @return Named list of sdcHierarchies hierarchies
create_dims <- function() {
  list(
    REGION = sdcHierarchies::hier_create("Total", nodes = c("North", "South")),
    INDUSTRY = sdcHierarchies::hier_create("Total", nodes = c("Tech", "Mfg", "Retail"))
  )
}

#' Create country-type dimension hierarchies
#'
#' @return Named list of sdcHierarchies hierarchies
create_dims_countries <- function() {
  list(
    country = sdcHierarchies::hier_create("Total", nodes = c("AT", "DE", "NL", "SE", "FR", "IT")),
    type = sdcHierarchies::hier_create("Total", nodes = LETTERS[1:4])
  )
}

#' Create single-dimension hierarchy (REGION only)
#'
#' @return Named list with single REGION hierarchy
create_dims_region_only <- function() {
  list(REGION = sdcHierarchies::hier_create("Total", nodes = c("North", "South")))
}

#' Create single-dimension hierarchy (INDUSTRY only)
#'
#' @return Named list with single INDUSTRY hierarchy
create_dims_industry_only <- function() {
  list(INDUSTRY = sdcHierarchies::hier_create("Total", nodes = c("Tech", "Mfg", "Retail")))
}

#' Create test sensitive_params list
#'
#' @param n_threshold n-threshold value (default: 3)
#' @param p_rule p-rule value (optional)
#' @return List of SDC parameters
create_sensitive_params <- function(n_threshold = 3, p_rule = NULL) {
  params <- list(n_threshold = n_threshold)
  if (!is.null(p_rule)) {
    params$p_rule <- p_rule
  }
  params
}
