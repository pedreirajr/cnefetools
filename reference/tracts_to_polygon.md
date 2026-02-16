# Convert census tract aggregates to user-provided polygons using CNEFE points

`tracts_to_polygon()` performs a dasymetric interpolation with the
following steps:

1.  census tract totals are allocated to CNEFE dwelling points inside
    each tract;

2.  allocated values are aggregated to user-provided polygons
    (neighborhoods, administrative divisions, custom areas, etc.).

The function uses DuckDB with spatial extensions for the heavy work.

## Usage

``` r
tracts_to_polygon(
  code_muni,
  polygon,
  year = 2022,
  vars = c("pop_ph", "pop_ch"),
  crs_output = NULL,
  cache = TRUE,
  verbose = TRUE
)
```

## Arguments

- code_muni:

  Integer. Seven-digit IBGE municipality code.

- polygon:

  An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) object
  with polygon geometries (POLYGON or MULTIPOLYGON). The function will
  automatically align CRS and issue a warning reporting the percentage
  of the polygon area that falls outside the municipality.

- year:

  Integer. The CNEFE data year. Currently only 2022 is supported.
  Defaults to 2022.

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

- crs_output:

  The CRS for the output object. Default is `NULL`, which uses the
  original CRS of the `polygon` argument. Can be an EPSG code (e.g.,
  4326, 31983) or any CRS object accepted by
  [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html).

- cache:

  Logical. Whether to use the existing package cache for assets and
  CNEFE zips.

- verbose:

  Logical. Whether to print step messages and timing.

## Value

An `sf` object with the user-provided polygons and the requested
interpolated variables. The output CRS matches the original `polygon`
CRS (or `crs_output` if specified).

## Examples

``` r
# \donttest{
# Interpolate population to user-provided polygons (neighborhoods of Lauro de Freitas-BA)
# Using geobr to download neighborhood boundaries
library(geobr)
nei_ldf <- subset(
  read_neighborhood(year = 2022),
  code_muni == 2919207
)
#> Using year/date 2022
poly_pop <- tracts_to_polygon(
  code_muni = 2919207,
  polygon = nei_ldf,
  vars = c("pop_ph", "pop_ch")
)
#> ℹ Processing municipality code 2919207...
#> ℹ Step 1/6: aligning CRS...
#> ℹ Input CRS: "EPSG:4674" | Output CRS: "EPSG:4674"
#> ℹ Step 1/6: aligning CRS...
#> ✔ Step 1/6 (CRS alignment) [39ms]
#> 
#> ℹ Step 2/6: connecting to DuckDB and loading extensions...
#> ✔ Spatial extension loaded
#> ℹ Step 2/6: connecting to DuckDB and loading extensions...
#> ✔ Step 2/6 (DuckDB ready) [254ms]
#> 
#> ℹ Step 3/6: preparing census tracts in DuckDB...
#> ℹ Using cached file: sc_29.parquet
#> ℹ Step 3/6: preparing census tracts in DuckDB...
#> ✔ Step 3/6 (Tracts ready) [209ms]
#> 
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...
#> ℹ Using cached file: /home/runner/.cache/R/cnefetools/2919207_LAURO_DE_FREITAS.zip
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...
#> ✔ Step 4/6 (CNEFE points ready) [829ms]
#> 
#> ℹ Step 5/6: spatial join (points to tracts) and allocation...
#> ✔ Step 5/6 (Join and allocation) [1.1s]
#> 
#> ℹ Step 6/6: aggregating allocated values to polygons...
#> ✔ Step 6/6 (Polygon aggregation) [26ms]
#> 
#> 
#> ── Dasymetric interpolation diagnostics ──
#> 
#> ── Stage 1: Tracts → CNEFE points 
#> ! Unallocated total for population from private households (pop_ph): 0 of
#>   202583 (0.00%)
#> ! Unallocated total for population from collective households (pop_ch): 0 of
#>   317 (0.00%)
#> ! Unmatched CNEFE points (no tract): 186 of 95739 points (0.19% of total
#>   points)
#> ! Tracts with NA totals: pop_ph in 4 of 354 tracts (1.13% of total tracts);
#>   pop_ch in 6 of 354 tracts (1.69% of total tracts).
#> 
#> ── Stage 2: CNEFE points → Polygons 
#> ℹ Polygon coverage: 95488 of 95553 allocated points captured (99.93%)
# }
```
