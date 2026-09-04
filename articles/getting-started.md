# Getting Started with rebalancedNoise

## The EZS Method

The `rebalancedNoise` package implements the EZS method as described by
Sabolová et al. ([2025](#ref-Sabolova2025)). The method adds
record-level noise to numeric survey variables. Every record carries a
random direction (either 1 or -1) and a noise multiplier. Cells
identified as sensitive keep their fixed direction, which guarantees
protection. In non-sensitive cells the algorithm adjusts the directions
so that the noise cancels out within table hierarchies. The result is
disclosure control with low distortion and additive consistency across
all aggregation levels.

## Two Ways to Work

The package offers two interchangeable interfaces. Both produce
identical results and can be mixed freely.

1.  The **R6 engine**. `rn_setup()` creates one object that keeps all
    state. You work with methods like `$rebalance()` and `$perturb()`.
    Tables are stored under names, results are cached, and one
    rebalancing serves any number of publication tables.
2.  The **functional interface**. Plain functions take a typed object
    and return a new one. You chain them with the pipe operator `|>`:
    `rn_init()` returns an `rn_initialized` object, `rn_rebalance()`
    returns an `rn_rebalanced` object, and `rn_perturb()` returns an
    `rn_perturbed` object. There is no hidden state, so each pipeline is
    self-contained.

The table below maps the corresponding entry points.

| Task | R6 engine | Functional interface |
|:---|:---|:---|
| Initialize | `rn_setup()` | `rn_init()` |
| Rebalance | `sdc$rebalance()` | `rn_rebalance()` |
| Perturb | `sdc$perturb()` | `rn_perturb()` |
| Retrieve a table | `sdc$get_results()` | `rn_format()` |
| Evaluate a table | `sdc$summarize()` | `rn_summarize()` |
| Microdata with directions | `sdc$get_microdata()` | `as.data.table(state)` |
| Reuse in another session | `sdc$export()` | [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) and `rn_init()` |
| Start over after rebalancing | `sdc$reset()` | rebalance again, no reset needed |
| Overview of stored tables | `sdc$list_tables()` | not needed, you hold the objects |

The R6 engine suits interactive work where you create many tables from
the same rebalanced microdata and want caching and naming support. The
functional interface suits scripts and reproducible pipelines because
each step is an explicit, testable function call. The vignette
[`vignette("workflows", package = "rebalancedNoise")`](https://sdctools.github.io/rebalancedNoise/articles/workflows.md)
covers all details of both styles.

## Setup

The following data is used throughout this vignette. It contains two
dimensions (country and industry) and three numeric variables (turnover,
workers, assets). Country probabilities are unequal on purpose so that
some cells hold fewer than five records and are flagged as sensitive.

``` r

library(rebalancedNoise)
#> Welcome to rebalancedNoise 0.3.0.
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%
library(sdcHierarchies)
#> Loading required package: shinythemes
#> Package 'sdcHierarchies' 0.23.1 has been loaded.

set.seed(1)
N <- 300
countries <- c("AT", "DE", "NL", "SE", "FR", "IT")
industries <- c("Tech", "Mfg", "Retail", "Services")

country_probs <- c(0.25, 0.25, 0.20, 0.15, 0.10, 0.05)

dt <- data.table(
  country = sample(countries, N, replace = TRUE, prob = country_probs),
  industry = sample(industries, N, replace = TRUE),
  turnover = runif(N, 1000, 10000),
  workers = sample(10:500, N, replace = TRUE),
  assets = runif(N, 10000, 100000),
  direction = sample(c(1, -1), N, replace = TRUE),
  noise_multiplier = sample(seq(0.75, 1.25, by = 0.01), N, replace = TRUE)
)

# Detailed hierarchy for rebalancing (country x industry)
dims_detailed <- list(
  country = hier_create("Total", nodes = countries),
  industry = hier_create("Total", nodes = industries)
)

# Coarse hierarchies for publication tables
dims_country_only <- list(
  country = hier_create("Total", nodes = countries)
)

dims_industry_only <- list(
  industry = hier_create("Total", nodes = industries)
)

head(dt)
#>    country industry turnover workers   assets direction noise_multiplier
#>     <char>   <char>    <num>   <int>    <num>     <num>            <num>
#> 1:      AT     Tech 8328.266     257 94908.25         1             0.80
#> 2:      AT     Tech 9358.995      89 97960.83         1             0.85
#> 3:      NL   Retail 2327.329     478 13497.99         1             0.86
#> 4:      FR     Tech 7748.395      53 62895.50        -1             1.05
#> 5:      DE   Retail 9780.916     469 12701.05        -1             0.94
#> 6:      FR     Tech 9773.132     487 60875.31        -1             1.15

# Cell sizes, the smallest cells will be sensitive
dt[, .N, by = .(country, industry)][order(N)]
#>     country industry     N
#>      <char>   <char> <int>
#>  1:      IT     Tech     2
#>  2:      IT   Retail     2
#>  3:      IT      Mfg     3
#>  4:      IT Services     3
#>  5:      FR Services     5
#>  6:      DE      Mfg     7
#>  7:      FR     Tech     8
#>  8:      SE   Retail     8
#>  9:      SE Services     8
#> 10:      FR   Retail     8
#> 11:      SE      Mfg     9
#> 12:      FR      Mfg    11
#> 13:      NL     Tech    11
#> 14:      NL Services    12
#> 15:      AT      Mfg    15
#> 16:      SE     Tech    16
#> 17:      NL   Retail    17
#> 18:      DE   Retail    17
#> 19:      NL      Mfg    17
#> 20:      AT   Retail    19
#> 21:      DE Services    23
#> 22:      DE     Tech    25
#> 23:      AT Services    25
#> 24:      AT     Tech    29
#>     country industry     N
#>      <char>   <char> <int>
```

Every input record must provide the two EZS columns `direction` (1 or
-1) and `noise_multiplier` (0 or greater). Set
`Sys.setenv(SDC_LOG_LEVEL = "OFF")` if you do not want the engine to
print status messages.

## The Same Workflow Twice

The workflow has three steps: initialize the data, rebalance once on the
detailed structure, and perturb a publication table at a coarser level.
The two following sections run exactly this workflow, first with the R6
engine and then with the functional interface.

### Using the R6 engine

``` r

# One object holds all state
sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 5))
#> ℹ Initialization started...
#> ✔ Initialization complete.

# Rebalance once on the detailed structure
sdc$rebalance(dim_list = dims_detailed, num_var = "turnover")
#> ℹ Performing rebalancing on detailed table structure...
#> ✔ Rebalancing complete. Updated "direction_rebalanced" column.

# Perturb a publication table at country level, stored under a name
sdc$perturb(dim_list = dims_country_only, variables = "turnover", name = "table_a")
#> ✔ Created new table "table_a" with variable `turnover`.

# Read the stored table
res_r6 <- sdc$get_results("table_a")
head(res_r6)
#>    country n_obs is_internal  turnover turnover_init turnover_pert
#>     <char> <num>      <lgcl>     <num>         <num>         <num>
#> 1:   Total   300       FALSE 1651113.7     1853460.8     1679243.9
#> 2:      AT    88        TRUE  544080.2      631265.0      548794.9
#> 3:      DE    72        TRUE  335581.4      281955.3      334307.4
#> 4:      NL    57        TRUE  313695.9      372977.6      316609.1
#> 5:      SE    41        TRUE  216463.0      289693.4      215228.8
#> 6:      FR    32        TRUE  197918.8      200338.4      187072.6
#>    is_sens_turnover
#>              <lgcl>
#> 1:            FALSE
#> 2:            FALSE
#> 3:            FALSE
#> 4:            FALSE
#> 5:            FALSE
#> 6:            FALSE
```

### Using the functional interface

``` r

# Each step returns a typed object, the pipe passes it to the next step
res_fun <- dt |>
  rn_init(sensitive_params = list(n_threshold = 5)) |>
  rn_rebalance(dim_list = dims_detailed, num_var = "turnover") |>
  rn_perturb(dim_list = dims_country_only, variables = "turnover")
#> ℹ Initialization started...
#> ✔ Initialization complete.
#> ℹ Performing rebalancing on detailed table structure...
#> ✔ Rebalancing complete. Updated "direction_rebalanced" column.

# The pipeline result is an rn_perturbed object
res_fun
#> <rn_perturbed>
#>   table: 7 cells x 8 columns
#>   dimensions: country
#>   variables: turnover
#>   round: FALSE | hash: 8087832319fd

# Convert it to a data.table in wide format
res_fun_wide <- rn_format(res_fun)
head(res_fun_wide)
#>    country n_obs is_internal  turnover turnover_init turnover_pert
#>     <char> <num>      <lgcl>     <num>         <num>         <num>
#> 1:   Total   300       FALSE 1651113.7     1853460.8     1679243.9
#> 2:      AT    88        TRUE  544080.2      631265.0      548794.9
#> 3:      DE    72        TRUE  335581.4      281955.3      334307.4
#> 4:      NL    57        TRUE  313695.9      372977.6      316609.1
#> 5:      SE    41        TRUE  216463.0      289693.4      215228.8
#> 6:      FR    32        TRUE  197918.8      200338.4      187072.6
#>    is_sens_turnover
#>              <lgcl>
#> 1:            FALSE
#> 2:            FALSE
#> 3:            FALSE
#> 4:            FALSE
#> 5:            FALSE
#> 6:            FALSE
```

Both routes compute the same table, and the results are identical:

``` r

identical(as.data.frame(res_r6), as.data.frame(res_fun_wide))
#> [1] TRUE
```

The intermediate object `rn_perturbed` carries the hierarchy, the SDC
parameters, and a hash, so the pipeline stays reproducible without any
external bookkeeping. Each function checks the class of its input
strictly and rejects wrong types with a hint about the required
preceding step. A plain `data.table`, even one that contains a
`direction_rebalanced` column, is not accepted by `rn_perturb()`.

## Reading the Results

Both interfaces provide the same output formats, and the resulting
`data.table` columns are the same in both cases. Use `sdc$get_results()`
or `rn_format()` with `format = "wide"` (the default) or
`format = "long"`.

### Wide Format

Wide format has one row per table cell. Every perturbed variable
contributes a group of columns.

``` r

head(res_r6)
#>    country n_obs is_internal  turnover turnover_init turnover_pert
#>     <char> <num>      <lgcl>     <num>         <num>         <num>
#> 1:   Total   300       FALSE 1651113.7     1853460.8     1679243.9
#> 2:      AT    88        TRUE  544080.2      631265.0      548794.9
#> 3:      DE    72        TRUE  335581.4      281955.3      334307.4
#> 4:      NL    57        TRUE  313695.9      372977.6      316609.1
#> 5:      SE    41        TRUE  216463.0      289693.4      215228.8
#> 6:      FR    32        TRUE  197918.8      200338.4      187072.6
#>    is_sens_turnover
#>              <lgcl>
#> 1:            FALSE
#> 2:            FALSE
#> 3:            FALSE
#> 4:            FALSE
#> 5:            FALSE
#> 6:            FALSE
```

### Long Format

Long format has one row per cell and variable, with standardized column
names. It is convenient for plotting and further processing.

``` r

head(sdc$get_results("table_a", format = "long"))
#>    country n_obs is_internal variable val_pert_init  val_orig  val_pert
#>     <char> <num>      <lgcl>   <char>         <num>     <num>     <num>
#> 1:   Total   300       FALSE turnover     1853460.8 1651113.7 1679243.9
#> 2:      AT    88        TRUE turnover      631265.0  544080.2  548794.9
#> 3:      DE    72        TRUE turnover      281955.3  335581.4  334307.4
#> 4:      NL    57        TRUE turnover      372977.6  313695.9  316609.1
#> 5:      SE    41        TRUE turnover      289693.4  216463.0  215228.8
#> 6:      FR    32        TRUE turnover      200338.4  197918.8  187072.6
#>    is_sensitive diff_init_pct diff_final_pct
#>          <lgcl>         <num>          <num>
#> 1:        FALSE        12.255          1.704
#> 2:        FALSE        16.024          0.867
#> 3:        FALSE       -15.980         -0.380
#> 4:        FALSE        18.898          0.929
#> 5:        FALSE        33.830         -0.570
#> 6:        FALSE         1.223         -5.480
head(rn_format(res_fun, format = "long"))
#>    country n_obs is_internal variable val_pert_init  val_orig  val_pert
#>     <char> <num>      <lgcl>   <char>         <num>     <num>     <num>
#> 1:   Total   300       FALSE turnover     1853460.8 1651113.7 1679243.9
#> 2:      AT    88        TRUE turnover      631265.0  544080.2  548794.9
#> 3:      DE    72        TRUE turnover      281955.3  335581.4  334307.4
#> 4:      NL    57        TRUE turnover      372977.6  313695.9  316609.1
#> 5:      SE    41        TRUE turnover      289693.4  216463.0  215228.8
#> 6:      FR    32        TRUE turnover      200338.4  197918.8  187072.6
#>    is_sensitive diff_init_pct diff_final_pct
#>          <lgcl>         <num>          <num>
#> 1:        FALSE        12.255          1.704
#> 2:        FALSE        16.024          0.867
#> 3:        FALSE       -15.980         -0.380
#> 4:        FALSE        18.898          0.929
#> 5:        FALSE        33.830         -0.570
#> 6:        FALSE         1.223         -5.480
```

### Column Reference

| Format | Column | Content |
|:---|:---|:---|
| both | `[dim]` | dimension value, one column per dimension |
| both | `n_obs` | number of records contributing to the cell |
| both | `is_internal` | `TRUE` for base cells, `FALSE` for aggregates |
| wide | `[var]` | unperturbed original value |
| wide | `[var]_init` | perturbation with the original random directions |
| wide | `[var]_pert` | final perturbed value after rebalancing |
| wide | `is_sens_[var]` | `TRUE` if the cell is sensitive for that variable |
| long | `variable` | name of the variable of this row |
| long | `val_orig`, `val_pert_init`, `val_pert` | the three values of `[var]`, `[var]_init`, and `[var]_pert` |
| long | `is_sensitive` | `TRUE` if the cell is sensitive for this variable |
| long | `diff_init_pct`, `diff_final_pct` | percentage deviation of the initial and final values from the original |

The comparison between the three versions of each variable is what makes
the method auditable. `[var]` is the truth, `[var]_init` shows what
plain perturbation would have produced, and `[var]_pert` shows the final
published value.

You can restrict the output to selected variables with the `variables`
argument, which both `sdc$get_results()` and `rn_format()` accept. A
variable that is not part of the table causes an error that lists the
available ones.

## Combining Both Interfaces

The two interfaces are interchangeable at every point in the workflow.
An R6 object can hand its state to the functional interface, and an R6
result table can be evaluated with the functional tools.

``` r

# Feed the R6 engine state into the functional pipeline
state <- sdc$get_state()  # rn_rebalanced
t_b <- rn_perturb(state, dim_list = dims_industry_only, variables = "workers")
head(rn_format(t_b))
#>    industry n_obs is_internal workers workers_init workers_pert is_sens_workers
#>      <char> <num>      <lgcl>   <num>        <num>        <num>          <lgcl>
#> 1:    Total   300       FALSE   73686     83914.68     76617.98           FALSE
#> 2:     Tech    91        TRUE   20800     25302.16     21261.32           FALSE
#> 3:      Mfg    62        TRUE   15925     18689.39     16782.93           FALSE
#> 4:   Retail    71        TRUE   16140     17937.37     16788.53           FALSE
#> 5: Services    76        TRUE   20821     21985.76     21785.20           FALSE

# Evaluate a stored R6 table with the functional summary
rn_summarize(sdc$get_table("table_a"))
#> 
#> ── EZS Perturbation Summary: "rn_perturbed" ────────────────────────────────────
#> 
#> ── Variable: "turnover" ──
#> 
#> ── OVERALL
#> ℹ nrCells: 7 | MAPE (Initial): 25.181% | MAPE (Final): 12.57% | Noise Reduction: 50.08%
#> Percentiles - Relative (%) Initial:
#> -15.98 | -14.948 | -10.819 | 6.739 | 16.024 | 26.364 | 64.789 | 75.403 | 78.057
#> Percentiles - Relative (%) Final:
#> -5.48 | -5.185 | -4.007 | -0.475 | 0.867 | 1.316 | 55.151 | 73.476 | 78.057
#> Percentiles - Absolute (Units) Final:
#> -10846.157 | -10271.829 | -7974.517 | -1254.123 | 2913.184 | 16422.458 |
#> 32138.769 | 33513.132 | 33856.722
#> 
#> ── Summary Statistics
#> • Sensitivity Rate: 0% (0/7 cells)
#> • Internal Cells: 6 (85.7%)
#> • Noise Reduction: 50.08%
```

The reverse direction works as well. An object created with `rn_init()`
can be passed to `rebalancedNoise$new()` to continue with R6 methods.

## Next Steps

The vignette
[`vignette("workflows", package = "rebalancedNoise")`](https://sdctools.github.io/rebalancedNoise/articles/workflows.md)
describes the remaining features in both styles: multiple variables and
multiple tables from one rebalancing, rebalancing on linear
combinations, table naming and caching in the R6 engine, rounding,
evaluation with `summarize()`, export and import, and parallelization.

## References

Sabolová, Radka, Özlem Tepe, Nils Adriansson, and Lars-Erik Almberg.
2025. “Using Perturbative Methods for Magnitude Tables in Statistical
Disclosure Control.” *Proceedings of the Expert Meeting on Statistical
Data Confidentiality* (Barcelona, Spain).
<https://unece.org/sites/default/files/2025-10/SDC2025_Sf_Sweden_Almberg_D.pdf>.
