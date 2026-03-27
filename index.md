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
- **Seamless Integration:** Designed to work directly with `data.table`
  and `sdcHierarchies`.

## Installation

``` r
# Install from Github
devtools::install_github("sdcTools/rebalancedNoise")
```

## Quick Start

``` r
library(rebalancedNoise)

# Generate Dummy-Data
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

# Initialize the Object
sdc <- rn_setup(
    data = dt, 
    dim_list = dims, 
    num_vars = c("turnover"),
    sensitive_params = list(n_threshold = 5)
)

# Perturb the numerical variable
sdc$perturb("turnover")

# Show summarize-statistics (initial vs. final perturbation)
sdc$summarize("turnover")

# extract results
sdc$get_results("turnover", format = "long") # or "wide"
```

## Documentation

For a detailed walkthrough of the method, multi-variable handling, and
parallelization settings (via
[`options()`](https://rdrr.io/r/base/options.html) or environment
variables), please see the full package vignette:

``` r
vignette("getting-started", package = "rebalancedNoise")
```
