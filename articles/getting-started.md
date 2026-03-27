# Getting Started with rebalancedNoise

## The EZS Method

The `rebalancedNoise` package implements the EZS method as described by
Sabolová et al. ([2025](#ref-Sabolova2025)). It applies record-level
noise with a dynamic rebalancing algorithm to preserve data quality in
non-sensitive cells while ensuring statistical disclosure control (SDC)
across complex hierarchies.

## Example with Multiple Variables

The engine can process multiple numerical variables. Initializing them
together ensures the underlying SDC problem structure is consistent.

``` r
library(rebalancedNoise)
#> Welcome to rebalancedNoise v0.1.0 - Perturbation for Magnitude Tables
library(data.table)
library(sdcHierarchies)
#> Loading required package: shinythemes
#> Package 'sdcHierarchies' 0.23.0 has been loaded.

# Generate dummy microdata
set.seed(1)
dt <- data.table(
  ID = 1:200,
  REGION = sample(c("North", "South"), 200, replace = TRUE),
  INDUSTRY = sample(c("Tech", "Mfg", "Retail"), 200, replace = TRUE),
  turnover = runif(200, 10, 1000),
  assets = runif(200, 50, 5000),
  direction = sample(c(1, -1), 200, replace = TRUE),
  noise_multiplier = 0.05 
)

# Define hierarchies
dims <- list(
  REGION = hier_create("Total", nodes = c("North", "South")),
  INDUSTRY = hier_create("Total", nodes = c("Tech", "Mfg", "Retail"))
)

# Initialize and perturb
sdc <- rn_setup(
  data = dt, 
  dim_list = dims, 
  num_vars = c("turnover", "assets"), 
  sensitive_params = list(n_threshold = 5)
)
#> ℹ Initialization started...
#> ℹ Creating structural mapping via sdcTable...
#> ℹ Identifying base cells (leaf nodes)...
#> ✔ Initialization complete.

sdc$perturb("turnover")
#> ℹ Only n_threshold active.
#> ℹ Starting rebalancing for 6 base cells...
#> ℹ Aggregating results through hierarchy via sdcTable...
#> ✔ Perturbation for `turnover` complete.
sdc$perturb("assets")
#> ℹ Only n_threshold active.
#> ℹ Starting rebalancing for 6 base cells...
#> ℹ Aggregating results through hierarchy via sdcTable...
#> ✔ Perturbation for `assets` complete.
```

## Flexible Output Formats

The `get_results()` method provides two primary ways to view your data.

### Long Format

Ideal for automated processing or visualization with `ggplot2`. It uses
standardized value columns and a `variable` column to distinguish
between `turnover` and `assets`.

``` r
res_long <- sdc$get_results(format = "long")

# Standardized names: val_orig, val_pert, diff_final_pct
head(res_long[, .(variable, REGION, val_orig, val_pert, is_sens)])
#>    variable REGION  val_orig  val_pert is_sens
#>      <char> <char>     <num>     <num>  <lgcl>
#> 1: turnover  Total 105576.98 105578.42   FALSE
#> 2: turnover  Total  42444.12  42442.35   FALSE
#> 3: turnover  Total  32198.80  32199.71   FALSE
#> 4: turnover  Total  30934.06  30936.36   FALSE
#> 5: turnover  North  52147.99  52148.29   FALSE
#> 6: turnover  North  22106.82  22103.80   FALSE
```

### Wide Format (Default)

The wide format pivots the data so that each hierarchical cell occupies
a single row. Variable names are used as prefixes for the values.

``` r
res_wide <- sdc$get_results(format = "wide")

# Clean prefixes: [var]_orig, [var]_pert, [var]_is_sens
head(res_wide[, .(REGION, INDUSTRY, turnover_orig, turnover_pert, assets_orig, assets_pert)])
#> Key: <REGION, INDUSTRY>
#>    REGION INDUSTRY turnover_orig turnover_pert assets_orig assets_pert
#>    <char>   <char>         <num>         <num>       <num>       <num>
#> 1:  North      Mfg      12708.55      12710.00    84917.51    84905.71
#> 2:  North   Retail      17332.62      17334.48    83773.87    83770.42
#> 3:  North     Tech      22106.82      22103.80   102334.98   102331.48
#> 4:  North    Total      52147.99      52148.29   271026.35   271007.62
#> 5:  South      Mfg      19490.24      19489.70    97824.43    97812.75
#> 6:  South   Retail      13601.44      13601.88    54236.11    54243.34

# Identify cells where rebalancing was most effective for turnover
res_wide[turnover_is_sens == FALSE & abs(turnover_diff_final_pct) < 0.01]
#> Key: <REGION, INDUSTRY, n_obs, is_internal>
#>    REGION INDUSTRY n_obs is_internal assets_orig turnover_orig assets_pert_init
#>    <char>   <char> <num>      <lgcl>       <num>         <num>            <num>
#> 1:  North    Total   102       FALSE   271026.35      52147.99        269608.41
#> 2:  South      Mfg    37        TRUE    97824.43      19490.24         96425.48
#> 3:  South   Retail    24        TRUE    54236.11      13601.44         55105.22
#> 4:  South     Tech    37        TRUE    80672.60      20337.30         80934.48
#> 5:  South    Total    98       FALSE   232733.14      53428.99        232465.17
#> 6:  Total      Mfg    63       FALSE   182741.93      32198.80        181491.84
#> 7:  Total   Retail    57       FALSE   138009.98      30934.06        137812.79
#> 8:  Total     Tech    80       FALSE   183007.58      42444.12        182768.95
#> 9:  Total    Total   200       FALSE   503759.48     105576.98        502073.58
#>    turnover_pert_init assets_pert turnover_pert assets_is_sens turnover_is_sens
#>                 <num>       <num>         <num>         <lgcl>           <lgcl>
#> 1:           52083.31   271007.62      52148.29          FALSE            FALSE
#> 2:           19270.13    97812.75      19489.70          FALSE            FALSE
#> 3:           13895.88    54243.34      13601.88          FALSE            FALSE
#> 4:           20289.75    80671.44      20338.55          FALSE            FALSE
#> 5:           53455.76   232727.53      53430.13          FALSE            FALSE
#> 6:           31999.18   182718.46      32199.71          FALSE            FALSE
#> 7:           31183.22   138013.76      30936.36          FALSE            FALSE
#> 8:           42356.67   183002.92      42442.35          FALSE            FALSE
#> 9:          105539.07   503735.14     105578.42          FALSE            FALSE
#>    assets_diff_init_pct turnover_diff_init_pct assets_diff_final_pct
#>                   <num>                  <num>                 <num>
#> 1:               -0.523                 -0.124                -0.007
#> 2:               -1.430                 -1.129                -0.012
#> 3:                1.602                  2.165                 0.013
#> 4:                0.325                 -0.234                -0.001
#> 5:               -0.115                  0.050                -0.002
#> 6:               -0.684                 -0.620                -0.013
#> 7:               -0.143                  0.805                 0.003
#> 8:               -0.130                 -0.206                -0.003
#> 9:               -0.335                 -0.036                -0.005
#>    turnover_diff_final_pct
#>                      <num>
#> 1:                   0.001
#> 2:                  -0.003
#> 3:                   0.003
#> 4:                   0.006
#> 5:                   0.002
#> 6:                   0.003
#> 7:                   0.007
#> 8:                  -0.004
#> 9:                   0.001
```

## Output Variable Description

The following variables are included in the results to help interpret
the perturbation quality:

| Variable               | Description                                                              |
|:-----------------------|:-------------------------------------------------------------------------|
| `n_obs`                | Number of microdata records contributing to the specific cell.           |
| `is_internal`          | `TRUE` for base cells (leaf nodes); `FALSE` for hierarchical aggregates. |
| `[var]_orig`           | The original aggregated value before perturbation.                       |
| `[var]_pert`           | The final perturbed and rebalanced value.                                |
| `[var]_is_sens`        | `TRUE` if the specific variable was flagged as sensitive in that cell.   |
| `[var]_diff_final_pct` | The percentage deviation between original and final values.              |

## Evaluation of Results

Before finalizing your tables, use the `$summarize()` method to evaluate
the impact of the perturbation. This provides a high-density diagnostic
report across three groups: **Overall**, **Non-Sensitive** (Rebalanced),
and **Sensitive** (Fixed Noise).

``` r
# Display statistics for the turnover variable
sdc$summarize("turnover")
#> 
#> ── EZS Perturbation Summary: "turnover" ────────────────────────────────────────
#> 
#> ── OVERALL (All Cells) ──
#> 
#> ℹ nrCells: 12 | MAPE: 0.005% (Initial: 0.498%) | Noise Reduction: 98.9%
#> Percentiles (Min | 1% | 5% | 25% | 50% | 75% | 95% | 99% | Max):
#> Relative (%) - Initial (Balanced):
#> -1.129 | -1.073 | -0.849 | -0.241 | -0.152 | 0.078 | 1.417 | 2.015 | 2.165
#> Relative (%) - Final (Rebalanced):
#> -0.014 | -0.013 | -0.008 | 0 | 0.002 | 0.006 | 0.011 | 0.011 | 0.011
#> Absolute (Units) - Final:
#> -3.014 | -2.877 | -2.329 | 0.086 | 1.029 | 1.443 | 2.058 | 2.251 | 2.299
#> 
#> ── NON-SENSITIVE (Rebalanced) ──
#> 
#> ℹ nrCells: 12 | MAPE: 0.005% (Initial: 0.498%) | Noise Reduction: 98.9%
#> Percentiles (Min | 1% | 5% | 25% | 50% | 75% | 95% | 99% | Max):
#> Relative (%) - Initial (Balanced):
#> -1.129 | -1.073 | -0.849 | -0.241 | -0.152 | 0.078 | 1.417 | 2.015 | 2.165
#> Relative (%) - Final (Rebalanced):
#> -0.014 | -0.013 | -0.008 | 0 | 0.002 | 0.006 | 0.011 | 0.011 | 0.011
#> Absolute (Units) - Final:
#> -3.014 | -2.877 | -2.329 | 0.086 | 1.029 | 1.443 | 2.058 | 2.251 | 2.299
```

### How to Interpret the Summary

- **MAPE (Initial vs. Final):** In the Non-Sensitive group, the
  *“Initial” MAPE* reflects the noise before rebalancing. The *“Final”
  MAPE* should be significantly lower, demonstrating the algorithm’s
  effectiveness.
- **Percentile Comparison:** By comparing the *“Initial”* and *“Final”
  Relative (%) distributions*, you can visualize the *“shrinking”*
  effect. A successful run will show the interquartile range (`25%` to
  `75%`) moving towards `0`.
- **Tail Risks:** Monitor the `1%` and `99%` percentiles. These identify
  if specific cells in the hierarchy received disproportionately high
  noise, which can occasionally occur in very small or isolated nodes.
- **Absolute Impact:** The *“Absolute (Units)”* distribution translates
  these percentages into the original scale of your data, helping you
  assess the actual impact on table utility.

## Performance and Parallelization

The EZS engine utilizes C++ with OpenMP for computationally intensive
tasks. The number of threads (`n_threads`) is resolved during
`rn_setup()` and remains fixed for the life of the object.

The engine resolves the thread count using the following priority:

1.  **Function Argument:** `rn_setup(..., n_threads = 4)`
2.  **Global Option:** `options(rn_threads = 4)`
3.  **Environment Variable:** `Sys.setenv(rn_threads = "4")`
4.  **Automatic Fallback:** `parallel::detectCores() - 1` (Minimum of 1)

``` r
# Example: Setting a session-wide preference
options(rn_threads = 4)

# Initialize - this object is now locked to 4 threads
sdc <- rn_setup(dt, dims, "turnover")
#> ℹ Initialization started...
#> ℹ Creating structural mapping via sdcTable...
#> ℹ Identifying base cells (leaf nodes)...
#> ✔ Initialization complete.
```

## Summary of Logic

- **Hierarchy Integrity:** Aggregations across the hierarchy remain
  additive.
- **Automatic Cleanup:** Internal SDC identifiers are removed, leaving
  only the dimensions and results.
- **Calculated Deviations:** Percentage deviations are rounded to 3
  decimal places for immediate quality checks.

## References

Sabolová, Radka, Özlem Tepe, Nils Adriansson, and Lars-Erik Almberg.
2025. “Using Perturbative Methods for Magnitude Tables in Statistical
Disclosure Control.” In *Proceedings of the Expert Meeting on
Statistical Data Confidentiality*. Barcelona, Spain: UNECE.
<https://unece.org/sites/default/files/2025-10/SDC2025_Sf_Sweden_Almberg_D.pdf>.
