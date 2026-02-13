# Convert census tract aggregates to an H3 grid using CNEFE points

`tracts_to_h3()` performs a dasymetric interpolation with the following
steps:

1.  census tract totals are allocated to CNEFE dwelling points inside
    each tract;

2.  allocated values are aggregated to an H3 grid at a user-defined
    resolution.

The function uses DuckDB with the spatial and H3 extensions for the
heavy work.

## Usage

``` r
tracts_to_h3(
  code_muni,
  year = 2022,
  h3_resolution = 9,
  vars = c("pop_ph", "pop_ch"),
  cache = TRUE,
  verbose = TRUE
)
```

## Arguments

- code_muni:

  Integer. Seven-digit IBGE municipality code.

- year:

  Integer. The CNEFE data year. Currently only 2022 is supported.
  Defaults to 2022.

- h3_resolution:

  Integer. H3 resolution (0 to 15). Defaults to 9.

- vars:

  Character vector. Names of tract-level variables to interpolate.
  Supported variables:

  - `pop_ph`: population in private households (*Domicílios
    particulares*).

  - `pop_ch`: population in collective households (*Domicílios
    coletivos*).

  - `male`: total male population.

  - `female`: total female population.

  - `age_0_4`, `age_5_9`, `age_10_14`, `age_15_19`, `age_20_24`,
    `age_25_29`, `age_30_39`, `age_40_49`, `age_50_59`, `age_60_69`,
    `age_70m`: population by age group.

  - `race_branca`, `race_preta`, `race_amarela`, `race_parda`,
    `race_indigena`: population by race/color (*cor ou raça*).

  - `n_resp`: number of household heads (*Pessoas responsáveis por
    domicílios*).

  - `avg_inc_resp`: average income of the household heads.

  For a reference table mapping these variable names to the official
  IBGE census tract codes and descriptions, see
  [tracts_variables_ref](https://pedreirajr.github.io/cnefetools/reference/tracts_variables_ref.md).

  Allocation rules:

  - `pop_ph` is allocated only to private dwellings.

  - `pop_ch` is allocated only to collective dwellings.

  - `n_resp` is allocated only to private dwellings (same rule as
    `pop_ph`).

  - Demographic variables (`male`, `female`, `age_*`, `race_*`) are
    allocated to private dwellings when the tract has any; if the tract
    has zero private dwellings but has collective dwellings, they are
    allocated to collective.

  - `avg_inc_resp` is assigned (not split) to each private dwelling
    point; tracts with no private dwellings receive no allocation.

- cache:

  Logical. Whether to use the existing package cache for assets and
  CNEFE zips.

- verbose:

  Logical. Whether to print step messages and timing.

## Value

An `sf` object (CRS 4326) with an H3 grid and the requested interpolated
variables.

## Examples

``` r
# \donttest{
# Interpolate population to H3 hexagons
hex_pop <- tracts_to_h3(
  code_muni = 2929057,
  vars = c("pop_ph", "pop_ch")
)
#> ℹ Processing code 2929057
#> ℹ Step 1/6: connecting to DuckDB and loading extensions...
#> ✔ Spatial extension loaded
#> ℹ Step 1/6: connecting to DuckDB and loading extensions...
#> ✔ Step 1/6 (DuckDB ready) [267ms]
#> 
#> ℹ Step 2/6: preparing census tracts in DuckDB...
#> ℹ Downloading sc_29.parquet from GitHub release
#> ℹ Downloading "sc_29.parquet"...
#> ℹ Downloading sc_29.parquet from GitHub release
#>   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |=========                                                             |  12%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |=============                                                         |  18%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  45%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  48%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |=====================================                                 |  52%  |                                                                              |======================================                                |  54%  |                                                                              |=======================================                               |  55%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |============================================                          |  62%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |================================================                      |  68%  |                                                                              |=================================================                     |  69%  |                                                                              |==================================================                    |  71%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |==========================================================            |  82%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  88%  |                                                                              |===============================================================       |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  95%  |                                                                              |====================================================================  |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |======================================================================| 100%
#> ✔ Downloading sc_29.parquet from GitHub release [1.6s]
#> 
#> ℹ Step 2/6: preparing census tracts in DuckDB...
#> ✔ Step 2/6 (Tracts ready) [1.8s]
#> 
#> ℹ Step 3/6: preparing CNEFE points in DuckDB...
#> ℹ Using cached file: /home/runner/.cache/R/cnefetools/2929057_SAO_FELIX_DO_CORIBE.zip
#> ℹ Step 3/6: preparing CNEFE points in DuckDB...
#> ✔ Step 3/6 (CNEFE points ready) [280ms]
#> 
#> ℹ Step 4/6: spatial join (points to tracts) and allocation prep...
#> ✔ Step 4/6 (Join and allocation) [82ms]
#> 
#> ℹ Step 5/6: aggregating allocated values to H3 cells...
#> ✔ Step 5/6 (Hex aggregation) [15ms]
#> 
#> ℹ Step 6/6: building H3 grid and joining results...
#> ✔ Step 6/6 (sf output) [159ms]
#> 
#> 
#> ── Dasymetric interpolation diagnostics ──
#> 
#> ── Stage 1: Tracts → CNEFE points 
#> ! Unallocated total for population from private households (pop_ph): 0 of 15183
#>   (0.00%)
#> ! Unallocated total for population from collective households (pop_ch): 0 of 0
#>   (0.00%)
#> ! Unmatched CNEFE points (no tract): 4 of 6949 points (0.06% of total points)
#> ! Tracts with NA totals: pop_ch in 1 of 40 tracts (2.50% of total tracts)
#> 
#> ── Stage 2: CNEFE points → H3 hexagons 
#> ℹ CNEFE points mapped to H3 cells: 6945 of 6945 allocated points (100.00%)
# }
```
