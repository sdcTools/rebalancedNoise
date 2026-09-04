# rebalancedNoise 0.2.0

## New Features
- New `round` argument in `$perturb()` (default `FALSE`): rounds perturbed microdata values with `round()` before tabulation so all published cell values are whole numbers
- `$export()` method: Save rebalanced state and results to RDS file or export object
- `rn_setup()` auto-detection: Import from export objects or file paths
- `$get_microdata()`: Extract microdata with direction columns and noise multipliers
- `$list_tables()`: List all perturbed tables with dimensions and variables

## Documentation
- Vignette sections on export/import workflow and utility methods

## Bug Fixes
- `direction_rebalanced` is now guaranteed to be strictly integer (`-1`/`+1`): directions are taken from the balancing algorithm instead of being back-solved from perturbed values, which previously produced `NaN` for zero values or zero noise multipliers and floating-point drift
- `$rebalance()` aborts with a clear error if any rebalanced direction is invalid (NA/NaN/Inf/non-integer)
- Import validation now also checks `direction_rebalanced` values in export objects

## Testing
- Implemented some unit-tests

# rebalancedNoise 0.1.1
- Initial ordering in `$rebalance()` is now based on impact (`abs(orig * mult)`)

# rebalancedNoise 0.1.0

- **Initial release** of the `rebalancedNoise` package.
- Implementation of the **EZS method** (Sabolova et al., 2025) for magnitude tables.
- Contains the **dynamic rebalancing algorithm** to minimize noise in non-sensitive cells and preserve additive consistency across hierarchies.
- Added **`rebalancedNoise` R6 class** for integrated SDC workflows.
  - Method **`$perturb()`**: Perturbs a numeric variable and performs rebalancing
  - Method **`get_results()`**: Allows retrieval of results
    + Supports both **Wide and Long output formats** 
    + Standardized column naming convention (e.g., `[var]_orig`, `[var]_pert`).
    + Includes metadata variables: `is_internal` to identify internal and `is_sens` to track sensitivity status.
    + Computes percentage deviations before and after the rebalancing step.
- Added **`rn_setup()`** to initialize a `rebalancedNoise` object with support for multiple numerical variables.
- Added a package vignette to get started `vignette("getting-started", package = "rebalancedNoise")`
- **Performance:** Integrated OpenMP support to accelerate the rebalancing procedure written in C++.
  + can be configured using argument `n_threads` in `rn_setup()` or  `options(rn_threads)` / `Sys.getenv("rn_threads")`
