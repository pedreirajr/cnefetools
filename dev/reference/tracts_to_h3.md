# Convert census tract aggregates to an H3 grid using CNEFE points

`tracts_to_h3()` performs a dasymetric interpolation with the following
steps:

1.  census tract totals are allocated to CNEFE dwelling points inside
    each tract;

2.  allocated values are aggregated to an H3 grid at a user-defined
    resolution.

The function uses DuckDB with the spatial and H3 extensions for the
heavy work.

Unlike
[`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_counts.md)
and
[`compute_lumi()`](https://pedreirajr.github.io/cnefetools/dev/reference/compute_lumi.md),
this function does not expose a `backend` argument and relies on DuckDB
exclusively. The dominant cost here is a spatial overlay between the
full CNEFE point set of the municipality and the census tract polygons,
which is a different workload from the tabular aggregation those other
functions perform. Running that overlay in R would take prohibitively
long in medium and large municipalities, so a pure-R fallback would
offer users a path that does not finish rather than a slower one.

## Usage

``` r
tracts_to_h3(
  code_muni,
  year = 2022,
  h3_resolution = 9,
  vars = c("pop_ph", "pop_ch"),
  scope = c("muni", "geom"),
  cache = TRUE,
  cache_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- code_muni:

  Numeric or character. One or more seven-digit IBGE municipality codes,
  for example `2927408` or `c(2927408, 2919207)`. Repeated codes are
  dropped. With several codes, they're interpolated together (see
  "Several municipalities"). Always required, since the grid is built
  from the boundaries of these municipalities.

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
  [tracts_variables_ref](https://pedreirajr.github.io/cnefetools/dev/reference/tracts_variables_ref.md).

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

- scope:

  Character. Which CNEFE addresses (and census tracts) the function
  works with. `"muni"` (the default, and the faster option) uses every
  address and tract of the municipalities in `code_muni`. `"geom"` uses
  the addresses that fall inside the H3 cells, whichever municipality
  they belong to, so the municipalities the grid reaches are read as
  well. The H3 grid holds the cells whose center falls inside the
  boundaries of `code_muni`, plus the neighboring cells that intersect
  those boundaries. See the section "Border cells at the edge of the
  region".

- cache:

  Logical. If `TRUE` (default), the downloaded files are kept in the
  user cache directory and reused in later calls. These are the CNEFE
  file, the census tracts and the boundary of each municipality read.
  Only the tracts of those municipalities are downloaded, not the whole
  state. If `FALSE`, temporary files are used and deleted after the
  call. See
  [`clear_cache_muni()`](https://pedreirajr.github.io/cnefetools/dev/reference/clear_cache_muni.md),
  [`clear_cache_tracts()`](https://pedreirajr.github.io/cnefetools/dev/reference/clear_cache_tracts.md)
  and
  [`clear_cache_boundaries()`](https://pedreirajr.github.io/cnefetools/dev/reference/clear_cache_boundaries.md)
  to delete the cached files.

- cache_dir:

  Character. Directory to use for cached downloads. If `NULL` (default),
  the `CNEFETOOLS_CACHE_DIR` environment variable is used when it is
  set, otherwise
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) with
  `which = "cache"`. Use this to point large downloads at a secondary
  drive or a shared volume.

- verbose:

  Logical. Whether to print step messages and timing.

## Value

An `sf` object (CRS 4326) with an H3 grid and the requested interpolated
variables.

## Details

### Several municipalities

Pass a vector of codes to interpolate a region that spans more than one
municipality in a single call. The tracts and the CNEFE points of all of
them go through one allocation, each point is matched only to the tracts
of its own municipality, and the grid is built over the union of the
boundaries, so every cell appears once.

Calling the function once per municipality and binding the results isn't
the same. A cell on a shared border belongs to the grid of both
municipalities, so it comes back from both calls, each with only its own
side (under the default `scope = "muni"`), and dropping the repeated
rows loses one side. Totals (every variable except `avg_inc_resp`) add
up, so separate outputs can be combined by summing them by `id_hex`.
`avg_inc_resp` can't, because it's an average over the allocated points,
and two averages don't give the average of the pooled points. Use a
vector of codes instead.

### Border cells at the edge of the region

Under the default `scope = "muni"`, only the addresses and tracts of the
municipalities in `code_muni` are read. A cell on the outer edge of the
requested area, one that also overlaps a municipality you did not
request, therefore describes only the part of it that lies inside the
area: its values (totals and `avg_inc_resp`) leave out the addresses of
the neighbor. Adding municipalities moves the edge outward but does not
remove it.

Under `scope = "geom"`, the municipalities that the grid reaches are
read too, and every cell receives the allocation of all the dwellings
inside it, so the edge is complete. A message lists the neighboring
municipalities read. The CNEFE file of each neighbor is read whole,
since the files come one per municipality, but only the neighbor's
tracts with at least one dwelling inside the grid enter the allocation.
Those tracts enter with all their dwellings, so the result matches a
call that lists the neighbors in `code_muni`, restricted to the cells of
this grid. It's still slower than `"muni"`, which is why it isn't the
default.

### Addresses outside the H3 grid

The grid is built from the 2022 IBGE municipal boundaries, at full
resolution, plus the ring of cells along them, but each dwelling point
goes to the cell of its own coordinates. A point geocoded beyond that
ring, for example on the coast or on a small island the boundary doesn't
follow, lands in a cell outside the grid, and what was allocated to it
isn't in the output. The diagnostics report how many allocated points of
the municipalities in `code_muni` this affects and how much of each
total they carried, so the output can be reconciled with the tract
totals. Under `scope = "geom"`, the points of the neighboring
municipalities that fall beyond the grid are expected and not reported.

## Examples

``` r
# \donttest{
# Interpolate population to H3 hexagons
hex_pop <- tracts_to_h3(
  code_muni = 2929057,
  vars = c("pop_ph", "pop_ch"),
  cache = FALSE
)
#> ℹ Processing code 2929057
#> ℹ Step 1/6: connecting to DuckDB and loading extensions...
#> ✔ Step 1/6 (DuckDB ready) [3.4s]
#> 
#> ℹ Step 2/6: preparing census tracts in DuckDB...
#> ✔ Step 2/6 (Tracts ready) [845ms]
#> 
#> ℹ Step 3/6: preparing CNEFE points in DuckDB...
#> ✔ Step 3/6 (CNEFE points ready) [998ms]
#> 
#> ℹ Step 4/6: spatial join (points to tracts) and allocation prep...
#> ✔ Step 4/6 (Join and allocation) [68ms]
#> 
#> ℹ Step 5/6: aggregating allocated values to H3 cells...
#> ✔ Step 5/6 (Hex aggregation) [13ms]
#> 
#> ℹ Step 6/6: joining results to the H3 grid...
#> ✔ Step 6/6 (sf output) [85ms]
#> 
#> 
#> ── Dasymetric interpolation diagnostics ──
#> 
#> ── Stage 1: Tracts → CNEFE points 
#> ! Unallocated total for population from private households (pop_ph): 0 of 15183
#>   (0.00%)
#> ! Unallocated total for population from collective households (pop_ch): 0 of 0
#>   (0.00%)
#> ! Unmatched CNEFE points of the municipalities in `code_muni` (no tract): 4 of
#>   6949 points (0.06%)
#> ! Tracts with NA totals: pop_ch in 1 of 40 tracts (2.50% of the tracts read)
#> 
#> ── Stage 2: CNEFE points → H3 hexagons 
#> ℹ Allocated points of the municipalities in `code_muni` inside the grid: 6945
#>   of 6945 (100.00%)
# }
```
