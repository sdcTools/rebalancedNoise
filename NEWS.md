# rebalancedNoise 0.3.0

## New Features
- Functional (pipe) API with typed S3 objects: `rn_init()` (`rn_initialized`), `rn_rebalance()` (`rn_rebalanced`), and `rn_perturb()` (`rn_perturbed`). Each step strictly validates its input class and returns an object consumable by the next step
- `rn_format()`: Retrieve a `rn_perturbed` result table in wide or long format (functional counterpart of `$get_results()`)
- `rn_summarize()`: Summarize a `rn_perturbed` result table (functional counterpart of `$summarize()`)
- `print()` and `as.data.table()` methods for all three pipeline classes
- New R6 bridge methods `get_state()` (returns `rn_rebalanced`/`rn_initialized`) and `get_table(name)` (returns `rn_perturbed`) to feed R6 engine state into the functional API and vice versa
- `rn_setup()` and the R6 `$initialize()` now reuse the exported `rn_init()` internally
- New R6 method `$reset()`: returns the object to its initialized state (strips `direction_rebalanced`, `strID` and `is_sens_*` columns, restores original row order, clears cached results; optionally replaces `sensitive_params`)

## Breaking Changes
- `$rebalance()` (R6) can only be performed once per object; calling it again aborts with a hint to call `$reset()` first. The functional `rn_rebalance()` remains stateless and can be applied repeatedly

## Bug Fixes
- Calling `rn_rebalance()` a second time on already-rebalanced data no longer fails with "object 'strID' not found"; a stale `strID` column from a previous run is dropped before the structural merge

## Internals
- R6 engine and functional API share the same core: perturbation/tabulation (`.perturb_tabulate()`), variable merging (`.merge_var_into_table()`), formatting (`.process_result_table()`), and summaries (`.summarize_entry()`)
- Removed deprecated internal `.compute_sensitivity()`; single package-wide definition of `%||%`

## Documentation
- Vignettes split into two: "Getting Started with rebalancedNoise" (overview of the method and both interfaces with parallel R6 and functional examples) and "Perturbation Workflows" (detailed features shown in both styles: multiple tables, linear combinations, naming and caching, rounding, evaluation, export and import, parallelization)

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
