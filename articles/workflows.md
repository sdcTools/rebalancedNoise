# Perturbation Workflows

## Overview

This vignette covers the detailed features of `rebalancedNoise` after
the basic workflow. Every feature is shown in both interfaces: the
stateful R6 engine (`rn_setup()` plus `$` methods) and the functional
interface (`rn_init()` plus piped function calls). The basics of both
styles are explained in
[`vignette("getting-started", package = "rebalancedNoise")`](https://sdctools.github.io/rebalancedNoise/articles/getting-started.md).

## Setup

The same simulated data is used throughout this vignette. Country
probabilities are unequal so that some cells hold fewer than five
records.

``` r

library(rebalancedNoise)
#> Welcome to rebalancedNoise 0.3.1.
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

dims_detailed <- list(
  country = hier_create("Total", nodes = countries),
  industry = hier_create("Total", nodes = industries)
)

dims_country_only <- list(
  country = hier_create("Total", nodes = countries)
)

dims_industry_only <- list(
  industry = hier_create("Total", nodes = industries)
)
```

## One Rebalancing, Many Tables

Sensitive cell detection runs once during rebalancing and is reused by
every following perturbation. Rebalancing is the expensive step, so the
choice of the detailed hierarchy deserves attention. Publication tables
can then use any coarser hierarchy and any numeric variable without
further rebalancing.

The chunks below set up the two core objects that the rest of this
vignette builds on. Both objects rebalance `turnover` on the detailed
structure (country x industry) and create one country-level table.

``` r

sdc <- rn_setup(data = dt, sensitive_params = list(n_threshold = 5))
#> ℹ Initialization started...
#> ✔ Initialization complete.
sdc$rebalance(dim_list = dims_detailed, num_var = "turnover")
#> ℹ Performing rebalancing on detailed table structure...
#> ✔ Rebalancing complete. Updated "direction_rebalanced" column.
sdc$perturb(dim_list = dims_country_only, variables = "turnover", name = "table_a")
#> ✔ Created new table "table_a" with variables `turnover`.
```

``` r

state_fun <- dt |>
  rn_init(sensitive_params = list(n_threshold = 5)) |>
  rn_rebalance(dim_list = dims_detailed, num_var = "turnover")
#> ℹ Initialization started...
#> ✔ Initialization complete.
#> ℹ Performing rebalancing on detailed table structure...
#> ✔ Rebalancing complete. Updated "direction_rebalanced" column.

t_a <- rn_perturb(state_fun, dim_list = dims_country_only, variables = "turnover")
```

Additional tables reuse the same rebalanced state. In the R6 engine
every table is stored under a name inside the object:

``` r

# A second table, different hierarchy, two variables in one call
sdc$perturb(
  dim_list = dims_industry_only,
  variables = c("turnover", "assets"),
  name = "table_b"
)
#> ✔ Created new table "table_b" with variables `turnover` and `assets`.
res_b <- sdc$get_results("table_b")
head(res_b[, list(
  industry, turnover, turnover_init, turnover_pert,
  assets, assets_init, assets_pert
)])
#>    industry  turnover turnover_init turnover_pert   assets assets_init
#>      <char>     <num>         <num>         <num>    <num>       <num>
#> 1:    Total 1651113.7     1853460.8     1679243.9 15895256    17764751
#> 2:     Tech  526319.4      571895.9      539107.1  4897189     5397934
#> 3:      Mfg  323879.3      405202.8      333368.4  3231023     3207915
#> 4:   Retail  411855.2      471077.7      417148.2  3691171     5013046
#> 5: Services  389059.7      405284.4      389620.2  4075872     4145856
#>    assets_pert
#>          <num>
#> 1:    16007592
#> 2:     5283449
#> 3:     3009194
#> 4:     3846979
#> 5:     3867971

# All stored tables at once, returned as a named list
names(sdc$get_results())
#> [1] "table_a" "table_b"
```

In the functional interface you keep the objects yourself, for example
in a named list. The `rn_rebalanced` object `state_fun` can be perturbed
as often as needed:

``` r

t_b <- rn_perturb(
  state_fun,
  dim_list = dims_industry_only,
  variables = c("turnover", "assets")
)

tables <- list(table_a = rn_format(t_a), table_b = rn_format(t_b))
names(tables)
#> [1] "table_a" "table_b"
```

There is no automatic caching in the functional interface. A repeated
call to `rn_perturb()` recomputes the table. In the R6 engine a repeated
call with the same name and the same parameters is recognized and
skipped.

## Sensitive Cell Detection

The results carry the sensitivity flags that the rebalancing step
derived from `sensitive_params`. Cells with few contributing records are
flagged, and their noise direction stays fixed while the algorithm
optimizes all other directions.

``` r

res_a <- sdc$get_results("table_a")
res_a[, list(country, n_obs, is_internal, is_sens_turnover)][order(n_obs)]
#>    country n_obs is_internal is_sens_turnover
#>     <char> <num>      <lgcl>           <lgcl>
#> 1:      IT    10        TRUE            FALSE
#> 2:      FR    32        TRUE            FALSE
#> 3:      SE    41        TRUE            FALSE
#> 4:      NL    57        TRUE            FALSE
#> 5:      DE    72        TRUE            FALSE
#> 6:      AT    88        TRUE            FALSE
#> 7:   Total   300       FALSE            FALSE
```

The `n_obs` column counts the records contributing to each cell. The
`is_internal` column marks base cells, and `is_sens_turnover` reports
the sensitivity decision for that variable. In long format the same
information appears in the standardized `is_sensitive` column.

## Table Naming and Caching in the R6 Engine

Every R6 result table is identified by its name and by a hash of the
hierarchy, the SDC parameters, and the rounding flag. The hash enables
two protections.

Repeating an identical call recomputes nothing:

``` r

sdc$list_tables()
#>    table_name dimensions        variables
#>        <char>     <char>           <char>
#> 1:    table_a    country         turnover
#> 2:    table_b   industry turnover, assets

# Same name, same hash: the calculation is skipped
sdc$perturb(dim_list = dims_country_only, variables = "turnover", name = "table_a")
#> ℹ Table "table_a" for `turnover` already calculated. Skipping.
```

Reusing a name for a different table structure raises an error instead
of silently overwriting a stored result:

``` r

# Different hierarchy under an existing name: error
try(sdc$perturb(dim_list = dims_industry_only, variables = "turnover", name = "table_a"))
#> Error in sdc$perturb(dim_list = dims_industry_only, variables = "turnover",  : 
#>   ✖ Table name "table_a" is already in use.
#> ℹ Use a different table name for this hierarchy definition.

# Different structures need different names
sdc$perturb(dim_list = dims_industry_only, variables = "turnover", name = "table_c")
#> ✔ Created new table "table_c" with variables `turnover`.
sdc$list_tables()
#>    table_name dimensions        variables
#>        <char>     <char>           <char>
#> 1:    table_a    country         turnover
#> 2:    table_b   industry turnover, assets
#> 3:    table_c   industry         turnover
```

The functional interface has no table registry. You store each
`rn_perturbed` object under a name of your own choosing, so collisions
cannot occur.

## Rounding to Whole Numbers

Some publications require integer values. The `round` argument,
available in `sdc$perturb()` and `rn_perturb()`, rounds the perturbed
values on the microdata level with R’s
[`round()`](https://rdrr.io/r/base/Round.html) before the tables are
computed. Because rounding happens before tabulation, every published
value is a whole number.

``` r

sdc$perturb(
  dim_list = dims_country_only, variables = "turnover",
  name = "table_rounded", round = TRUE
)
#> ✔ Created new table "table_rounded" with variables `turnover`.
res_rounded <- sdc$get_results("table_rounded")
head(res_rounded[, list(country, turnover, turnover_pert)])
#>    country  turnover turnover_pert
#>     <char>     <num>         <num>
#> 1:   Total 1651113.7       1679249
#> 2:      AT  544080.2        548800
#> 3:      DE  335581.4        334305
#> 4:      NL  313695.9        316612
#> 5:      SE  216463.0        215232
#> 6:      FR  197918.8        187070

all(res_rounded$turnover_pert == round(res_rounded$turnover_pert))
#> [1] TRUE
```

``` r

t_rounded <- rn_perturb(
  state_fun,
  dim_list = dims_country_only, variables = "turnover", round = TRUE
)
res_rounded_fun <- rn_format(t_rounded)
all(res_rounded_fun$turnover_pert == round(res_rounded_fun$turnover_pert))
#> [1] TRUE
```

Four points to keep in mind:

- Only the perturbed columns (`[var]_init` and `[var]_pert`) are
  rounded. The original values in `[var]` stay unchanged.
- Rounding adds up to 0.5 per record, so cell totals can shift a little.
- R’s [`round()`](https://rdrr.io/r/base/Round.html) rounds halves
  toward the nearest even number, so 2.5 becomes
  2.  Exact halves rarely occur in perturbed data.
- The round flag is part of the R6 table hash. Reusing a table name with
  a different `round` value raises a collision error.

## Rebalancing on Linear Combinations

A single variable is not always the right basis for direction
optimization. If turnover, workers, and assets matter equally,
rebalancing on only one of them protects the others less. In that case,
build a weighted linear combination of standardized variables and use it
as the rebalancing target. The algorithm then protects all variables
together.

Missing values need attention here. A missing variable should neither
add to nor subtract from the combined score, and dropping records with
missing values wastes information. The recommended solution is to
replace missing values with 0 before scaling. After centering, 0 equals
the mean, so missing values contribute neutrally.

``` r

# Work on a copy so that dt stays complete
dt2 <- copy(dt)
dt2[sample(1:N, 20), assets := NA]
sum(is.na(dt2$assets))
#> [1] 20

# Standardize turnover and workers, replace NA with 0 before scaling assets
dt2[, turnover_std := scale(turnover)]
dt2[, workers_std := scale(workers)]
dt2[, assets_std := scale(ifelse(is.na(assets), 0, assets))]

# Weighted combination: 30 percent turnover, 30 percent workers, 40 percent assets
dt2[, rebal_combined := 0.3 * turnover_std + 0.3 * workers_std + 0.4 * assets_std]
sum(is.na(dt2$rebal_combined))
#> [1] 0
```

The combined variable is used only in the rebalancing step. The
perturbation still operates on the original variables.

``` r

sdc_lc <- rn_setup(data = dt2, sensitive_params = list(n_threshold = 5))
#> ℹ Initialization started...
#> ✔ Initialization complete.
sdc_lc$rebalance(dim_list = dims_detailed, num_var = "rebal_combined")
#> ℹ Performing rebalancing on detailed table structure...
#> ✔ Rebalancing complete. Updated "direction_rebalanced" column.
sdc_lc$perturb(
  dim_list = dims_country_only,
  variables = c("turnover", "workers", "assets"),
  name = "multi_var_table"
)
#> ✔ Created new table "multi_var_table" with variables `turnover`, `workers`, and `assets`.
res_lc <- sdc_lc$get_results("multi_var_table")
head(res_lc[, list(country, turnover, turnover_pert, workers, workers_pert)])
#>    country  turnover turnover_pert workers workers_pert
#>     <char>     <num>         <num>   <num>        <num>
#> 1:   Total 1651113.7     1720381.2   73686     75565.00
#> 2:      AT  544080.2      586227.6   22973     26204.30
#> 3:      DE  335581.4      329979.7   17957     13920.51
#> 4:      NL  313695.9      295206.6   12488     11133.28
#> 5:      SE  216463.0      208358.1    8834      9326.71
#> 6:      FR  197918.8      223378.2    8502      9135.63
```

``` r

res_lc_fun <- dt2 |>
  rn_init(sensitive_params = list(n_threshold = 5)) |>
  rn_rebalance(dim_list = dims_detailed, num_var = "rebal_combined") |>
  rn_perturb(
    dim_list = dims_country_only,
    variables = c("turnover", "workers", "assets")
  )
#> ℹ Initialization started...
#> ✔ Initialization complete.
#> ℹ Performing rebalancing on detailed table structure...
#> ✔ Rebalancing complete. Updated "direction_rebalanced" column.
head(rn_format(res_lc_fun)[, list(country, turnover, turnover_pert)])
#>    country  turnover turnover_pert
#>     <char>     <num>         <num>
#> 1:   Total 1651113.7     1720381.2
#> 2:      AT  544080.2      586227.6
#> 3:      DE  335581.4      329979.7
#> 4:      NL  313695.9      295206.6
#> 5:      SE  216463.0      208358.1
#> 6:      FR  197918.8      223378.2
```

## Evaluating Results

Before releasing tables, inspect the perturbation impact. The R6 method
`$summarize()` and the function `rn_summarize()` report the same
statistics.

``` r

sdc$summarize(table = "table_a", target_vars = "turnover")
#> 
#> ── EZS Perturbation Summary: "table_a" ─────────────────────────────────────────
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

``` r

rn_summarize(t_a)
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

How to read the report:

- **MAPE** is the mean absolute percentage error between original and
  perturbed values. MAPE (Initial) refers to the noise with the original
  random directions, before rebalancing. MAPE (Final) refers to the
  published values. A lower final value shows the effect of the
  rebalancing.
- **Noise Reduction** states by how many percent the final MAPE is below
  the initial MAPE.
- **Percentiles** list nine levels of the deviation distribution: 0, 1,
  5, 25, 50, 75, 95, 99, and 100 percent. The first two rows are
  relative deviations in percent, initial and final. The third row gives
  absolute deviations in the original unit of the variable. Check the 1
  and 99 percent levels for extreme deviations in single cells.
- **Sensitivity Rate** states which share of cells was flagged as
  sensitive. These cells received fixed noise for disclosure protection.

Both functions return the statistics invisibly as a named list, so you
can store and process them. With a single `target_vars` entry the list
contains the elements `overall` and `meta`, with one element per
variable otherwise:

``` r

result_a <- sdc$summarize(table = "table_a", target_vars = "turnover")
#> 
#> ── EZS Perturbation Summary: "table_a" ─────────────────────────────────────────
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
names(result_a)
#> [1] "overall" "meta"
names(result_a$overall)
#> [1] "count"       "mape_init"   "mape_final"  "q_rel_init"  "q_rel_final"
#> [6] "q_abs_final"
names(result_a$meta)
#> [1] "sensitivity_rate"    "n_total_cells"       "n_sensitive_cells"  
#> [4] "n_internal_cells"    "noise_reduction_pct"

result_b <- sdc$summarize(table = "table_b", target_vars = c("turnover", "assets"))
#> 
#> ── EZS Perturbation Summary: "table_b" ─────────────────────────────────────────
#> 
#> ── Variable: "turnover" ──
#> 
#> ── OVERALL
#> ℹ nrCells: 5 | MAPE (Initial): 12.914% | MAPE (Final): 1.699% | Noise Reduction: 86.85%
#> Percentiles - Relative (%) Initial:
#> 4.17 | 4.35 | 5.068 | 8.659 | 12.255 | 14.379 | 22.963 | 24.68 | 25.109
#> Percentiles - Relative (%) Final:
#> 0.144 | 0.19 | 0.372 | 1.285 | 1.704 | 2.43 | 2.83 | 2.91 | 2.93
#> Percentiles - Absolute (Units) Final:
#> 560.488 | 749.785 | 1506.976 | 5292.931 | 9489.092 | 12787.7 | 25061.708 |
#> 27516.509 | 28130.21
#> 
#> ── Summary Statistics
#> • Sensitivity Rate: 0% (0/5 cells)
#> • Internal Cells: 4 (80%)
#> • Noise Reduction: 86.85%
#> 
#> ── EZS Perturbation Summary: "table_b" ─────────────────────────────────────────
#> 
#> ── Variable: "assets" ──
#> 
#> ── OVERALL
#> ℹ nrCells: 5 | MAPE (Initial): 12.046% | MAPE (Final): 4.956% | Noise Reduction: 58.85%
#> Percentiles - Relative (%) Initial:
#> -0.715 | -0.618 | -0.229 | 1.717 | 10.225 | 11.761 | 31.002 | 34.85 | 35.812
#> Percentiles - Relative (%) Final:
#> -6.866 | -6.795 | -6.513 | -5.101 | 0.707 | 4.221 | 7.154 | 7.74 | 7.887
#> Percentiles - Absolute (Units) Final:
#> -221829.755 | -221272.605 | -219044.006 | -207901.009 | 112336.321 | 155807.158
#> | 340169.373 | 377041.816 | 386259.927
#> 
#> ── Summary Statistics
#> • Sensitivity Rate: 0% (0/5 cells)
#> • Internal Cells: 4 (80%)
#> • Noise Reduction: 58.85%
names(result_b)
#> [1] "turnover" "assets"
```

## Export and Import

Rebalancing large datasets takes time. The rebalanced state can be
stored and loaded again so that later sessions only pay for the
perturbations.

### Using the R6 engine

`$export()` writes the rebalanced microdata, the SDC parameters, the
rebalancing status, and, optionally, the cached result tables.
`rn_setup()` recognizes an export object or the path to an RDS file and
restores the full state.

``` r

export_file <- file.path(tempdir(), "rebalanced_data.rds")
sdc$export(file = export_file)
#> ✔ Exported to /tmp/RtmpF4ntCf/rebalanced_data.rds

# In a later session: load and continue without rebalancing again
sdc2 <- rn_setup(export_file)
#> ℹ Initialization started...
#> ✔ Initialization complete.
sdc2$list_tables()
#>       table_name dimensions        variables
#>           <char>     <char>           <char>
#> 1:       table_a    country         turnover
#> 2:       table_b   industry turnover, assets
#> 3:       table_c   industry         turnover
#> 4: table_rounded    country         turnover

# New tables from the imported state, no recomputation of the directions
sdc2$perturb(dim_list = dims_industry_only, variables = "workers", name = "table_d")
#> ✔ Created new table "table_d" with variables `workers`.
sdc2$list_tables()
#>       table_name dimensions        variables
#>           <char>     <char>           <char>
#> 1:       table_a    country         turnover
#> 2:       table_b   industry turnover, assets
#> 3:       table_c   industry         turnover
#> 4: table_rounded    country         turnover
#> 5:       table_d   industry          workers
```

Pass `include_results = FALSE` to export only the rebalanced microdata,
which produces a smaller file. On import, `rn_setup()` validates the
object and rejects files with missing columns or invalid direction
values.

### Using the functional interface

An `rn_rebalanced` object contains everything that is needed to perturb
further tables, so a plain
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) is enough:

``` r

state_file <- file.path(tempdir(), "rebalanced_state.rds")
saveRDS(state_fun, state_file)

state_re <- readRDS(state_file)
t_re <- rn_perturb(state_re, dim_list = dims_industry_only, variables = "workers")
head(rn_format(t_re))
#>    industry n_obs is_internal workers workers_init workers_pert is_sens_workers
#>      <char> <num>      <lgcl>   <num>        <num>        <num>          <lgcl>
#> 1:    Total   300       FALSE   73686     83914.68     76617.98           FALSE
#> 2:     Tech    91        TRUE   20800     25302.16     21261.32           FALSE
#> 3:      Mfg    62        TRUE   15925     18689.39     16782.93           FALSE
#> 4:   Retail    71        TRUE   16140     17937.37     16788.53           FALSE
#> 5: Services    76        TRUE   20821     21985.76     21785.20           FALSE
```

The R6 export object can also be fed directly into the functional
interface, which makes the two persistence formats interchangeable:

``` r

rn_init(sdc$export(include_results = FALSE))
#> ℹ Initialization started...
#> ✔ Initialization complete.
#> <rn_initialized>
#>   microdata: 300 records x 10 columns
#>   sensitive_params: n_threshold = 5
#>   n_threads: 3
#>   rebalanced: TRUE
```

## Utility Methods

### Microdata with Directions

`sdc$get_microdata()` returns the microdata including `direction`,
`direction_rebalanced`, and `noise_multiplier`. Set
`include_record_id = TRUE` to add the internal record identifier that
the rebalancing uses for tracking. The functional counterpart is
[`as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html)
applied to an `rn_rebalanced` object.

``` r

head(sdc$get_microdata())
#> Key: <country, industry>
#>    country industry turnover workers   assets direction noise_multiplier  strID
#>     <char>   <char>    <num>   <int>    <num>     <num>            <num> <char>
#> 1:      AT      Mfg 6713.882     434 13327.64         1             0.78    102
#> 2:      AT      Mfg 8189.331     374 24854.73         1             1.17    102
#> 3:      AT      Mfg 8452.675      63 71095.67        -1             1.00    102
#> 4:      AT      Mfg 7932.028      36 56080.61         1             1.20    102
#> 5:      AT      Mfg 2082.540     385 16656.75         1             0.76    102
#> 6:      AT      Mfg 6432.361     137 57557.15        -1             0.89    102
#>    direction_rebalanced
#>                   <int>
#> 1:                   -1
#> 2:                    1
#> 3:                   -1
#> 4:                   -1
#> 5:                   -1
#> 6:                    1
head(sdc$get_microdata(include_record_id = TRUE))
#> Key: <country, industry>
#>    country industry turnover workers   assets direction noise_multiplier
#>     <char>   <char>    <num>   <int>    <num>     <num>            <num>
#> 1:      AT      Mfg 6713.882     434 13327.64         1             0.78
#> 2:      AT      Mfg 8189.331     374 24854.73         1             1.17
#> 3:      AT      Mfg 8452.675      63 71095.67        -1             1.00
#> 4:      AT      Mfg 7932.028      36 56080.61         1             1.20
#> 5:      AT      Mfg 2082.540     385 16656.75         1             0.76
#> 6:      AT      Mfg 6432.361     137 57557.15        -1             0.89
#>    record_id  strID direction_rebalanced
#>        <int> <char>                <int>
#> 1:        28    102                   -1
#> 2:        62    102                    1
#> 3:        73    102                   -1
#> 4:        81    102                   -1
#> 5:       113    102                   -1
#> 6:       122    102                    1

# Functional equivalent
head(as.data.table(state_fun))
#> Key: <country, industry>
#>    country industry turnover workers   assets direction noise_multiplier  strID
#>     <char>   <char>    <num>   <int>    <num>     <num>            <num> <char>
#> 1:      AT      Mfg 6713.882     434 13327.64         1             0.78    102
#> 2:      AT      Mfg 8189.331     374 24854.73         1             1.17    102
#> 3:      AT      Mfg 8452.675      63 71095.67        -1             1.00    102
#> 4:      AT      Mfg 7932.028      36 56080.61         1             1.20    102
#> 5:      AT      Mfg 2082.540     385 16656.75         1             0.76    102
#> 6:      AT      Mfg 6432.361     137 57557.15        -1             0.89    102
#>    direction_rebalanced
#>                   <int>
#> 1:                   -1
#> 2:                    1
#> 3:                   -1
#> 4:                   -1
#> 5:                   -1
#> 6:                    1
```

### Listing Stored Tables

`sdc$list_tables()` shows the name, the dimensions, and the variables of
every table stored in the R6 object.

``` r

sdc$list_tables()
#>       table_name dimensions        variables
#>           <char>     <char>           <char>
#> 1:       table_a    country         turnover
#> 2:       table_b   industry turnover, assets
#> 3:       table_c   industry         turnover
#> 4: table_rounded    country         turnover
```

### Starting Over with reset()

Rebalancing can run once per R6 object and once per imported export
state. A second call raises an error. `$reset()` returns the object to
its initialized state: it removes the rebalancing artifacts
(`direction_rebalanced`, `strID`, and `is_sens_*` columns), restores the
original row order, and clears all cached tables. Optionally it accepts
new SDC parameters.

``` r

sdc_rst <- rn_setup(dt, sensitive_params = list(n_threshold = 5))
#> ℹ Initialization started...
#> ✔ Initialization complete.
sdc_rst$rebalance(dim_list = dims_detailed, num_var = "turnover")
#> ℹ Performing rebalancing on detailed table structure...
#> ✔ Rebalancing complete. Updated "direction_rebalanced" column.
sdc_rst$perturb(dim_list = dims_detailed, variables = "turnover", name = "t1")
#> ✔ Created new table "t1" with variables `turnover`.

# A second rebalance() on the same object is rejected
try(sdc_rst$rebalance(dim_list = dims_country_only, num_var = "turnover"))
#> Error in sdc_rst$rebalance(dim_list = dims_country_only, num_var = "turnover") : 
#>   ✖ Rebalancing has already been performed on this object.
#> ℹ Call `reset()` first to start over with a different structure or variable.

# Reset, optionally with new parameters, then start again
sdc_rst$reset(sensitive_params = list(n_threshold = 3))
#> ✔ Reset complete. Call `rebalance()` to start over.
sdc_rst$rebalance(dim_list = dims_country_only, num_var = "turnover")
#> ℹ Performing rebalancing on detailed table structure...
#> ✔ Rebalancing complete. Updated "direction_rebalanced" column.
sdc_rst$perturb(dim_list = dims_country_only, variables = "turnover", name = "t1")
#> ✔ Created new table "t1" with variables `turnover`.
sdc_rst$list_tables()
#>    table_name dimensions variables
#>        <char>     <char>    <char>
#> 1:         t1    country  turnover
```

After `$reset()` the object behaves like a freshly initialized one. The
functional interface needs no equivalent: `rn_rebalance()` is stateless
and can be applied to an `rn_rebalanced` object again at any time.

## Performance and Parallelization

The sensitivity check during rebalancing is the most time-consuming step
and runs in C++ with OpenMP. It is used in `$rebalance()` and
`rn_rebalance()`, in `$perturb()` with `strID`, and in `rn_perturb()`
when the sensitivity flags of a result table are computed. The
perturbation step itself runs single-threaded.

The number of threads is resolved once in `rn_setup()` or `rn_init()`
and then stays fixed for the object or state. Besides the `n_threads`
argument there, the value can also be set globally with
`options(rn_threads = 4)` or via the environment variable `rn_threads`,
for example with `Sys.setenv(rn_threads = "4")`. The argument takes
precedence over the option, and the option takes precedence over the
environment variable. Without any of these settings, the default is the
number of detected cores minus one, with a minimum of 1. This setting
applies to both interfaces.

## Summary

Every feature of `rebalancedNoise` is reachable through both interfaces,
and the interfaces can be combined at any stage of a workflow. The R6
engine adds naming, caching, and state management for interactive table
production. The functional interface provides explicit, composable steps
for scripts and reproducible pipelines. Choose by working style, not by
capability.
