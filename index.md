# rebalancedNoise: Perturbative Statistical Disclosure Control for Magnitude Tables

**rebalancedNoise** implements the **EZS method** for magnitude tables
as described in the Paper [*Using Perturbative Methods for Magnitude
Tables in Statistical Disclosure
Control*](https://unece.org/sites/default/files/2025-10/SDC2025_Sf_Sweden_Almberg_D.pdf)
from Sabolová et al. (2025). It provides a high-performance framework
for applying record-level noise with a dynamic rebalancing algorithm
that preserves data quality in non-sensitive cells while ensuring
additive consistency across complex hierarchies.

## Key Features

- **Hierarchy Integrity:** Ensures that all hierarchical aggregates
  remain perfectly additive after perturbation.
- **Dynamic Rebalancing:** Minimizes noise in non-sensitive cells by
  “balancing” noise directions within hierarchical nodes.
- **High Performance:** Rebalancing algorithm is implemented in **C++
  with OpenMP** support for fast processing of large inputs.
- **Built-in Diagnostics:** Powerful `$summarize()` tools to evaluate
  rebalancing efficiency and data utility via high-density statistical
  reports.
- **Two Interchangeable Interfaces:** A stateful R6 engine for
  interactive table production and a functional, pipe-friendly API for
  scripts and reproducible pipelines.
- **Seamless Integration:** Designed to work directly with `data.table`
  and `sdcHierarchies`.

## Installation

``` r

# Install from Github
devtools::install_github("sdcTools/rebalancedNoise")
```

## Quick Start

The same workflow, first with the R6 engine, then with the functional
interface:

``` r

library(rebalancedNoise)
library(data.table)

# Generate dummy data
N <- 100
countries <- c("AT", "DE", "NL", "SE", "FR", "IT")
set.seed(1)
dt <- data.table(
  country = sample(countries, N, replace = TRUE),
  turnover = runif(N, 10, 1000),
  direction = sample(c(1, -1), N, replace = TRUE),
  noise_multiplier = 0.05
)

# Define hierarchy
dims <- list(
  country = sdcHierarchies::hier_create("Total", nodes = countries)
)

# Option 1: R6 engine
sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 5))
sdc$rebalance(dim_list = dims, num_var = "turnover")
sdc$perturb(dim_list = dims, variables = "turnover", name = "table_a")
sdc$summarize(table = "table_a")
sdc$get_results("table_a", format = "long") # or "wide"

# Option 2: functional interface
res <- dt |>
  rn_init(sensitive_params = list(n_threshold = 5)) |>
  rn_rebalance(dim_list = dims, num_var = "turnover") |>
  rn_perturb(dim_list = dims, variables = "turnover")

rn_summarize(res)
rn_format(res, format = "long") # or "wide"
```

## Documentation

Two vignettes document the package in both styles:

``` r

# Overview of the method and both interfaces, with parallel examples
vignette("getting-started", package = "rebalancedNoise")

# Detailed workflows: multiple tables, linear combinations, caching,
# rounding, evaluation, export and import, parallelization
vignette("workflows", package = "rebalancedNoise")
```
