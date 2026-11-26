# Convert census tract aggregates to user-provided polygons using CNEFE points

`tracts_to_polygon()` performs a dasymetric interpolation with the
following steps:

1.  census tract totals are allocated to CNEFE dwelling points inside
    each tract;

2.  allocated values are aggregated to user-provided polygons
    (neighborhoods, administrative divisions, custom areas, etc.).

The function uses DuckDB with spatial extensions for the heavy work.

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
tracts_to_polygon(
  code_muni = NULL,
  polygon,
  year = 2022,
  vars = c("pop_ph", "pop_ch"),
  crs_output = NULL,
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
  dropped. With several codes, they're interpolated together, in a
  single allocation (see "Several municipalities").

  Required under `scope = "muni"` (the default) and ignored under
  `scope = "geom"`, which finds the municipalities that the polygons
  intersect (see "Municipalities detected from the polygons").

- polygon:

  An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) object
  with polygon geometries (POLYGON or MULTIPOLYGON), in any CRS.
  Features with the same geometry as an earlier feature are dropped,
  with a message, and the first one is kept. Under `scope = "muni"`, the
  diagnostics report the share of the allocated points of the
  municipalities in `code_muni` that falls inside the polygons. When
  polygons overlap, the diagnostics open with how many dwellings fall in
  more than one of them, since their allocated values are added to each.

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

- crs_output:

  The CRS for the output object. Default is `NULL`, which uses the
  original CRS of the `polygon` argument. Can be an EPSG code (e.g.,
  4326, 31983) or any CRS object accepted by
  [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html).

- scope:

  Character. Which CNEFE addresses (and census tracts) the function
  works with. `"muni"` (the default, and the faster option) uses every
  address and tract of the municipalities in `code_muni`. `"geom"` uses
  the addresses that fall inside the polygons in `polygon`, whichever
  municipality they belong to, so the municipalities those polygons
  reach are read as well. See the section "Polygons at the edge of the
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

An `sf` object with the user-provided polygons and the requested
interpolated variables. The output CRS matches the original `polygon`
CRS (or `crs_output` if specified).

## Details

### Several municipalities

Pass a vector of codes to interpolate a region that spans more than one
municipality in a single call. The tracts and the CNEFE points of all of
them go through one allocation, each point is matched only to the tracts
of its own municipality, and the result has one row per polygon,
whichever municipality the polygon crosses.

Calling the function once per municipality and binding the results isn't
the same. A polygon that crosses a municipal border gets, in each call,
only the part of the allocation that comes from the municipality of that
call. Totals (every variable except `avg_inc_resp`) add up, so separate
outputs can be combined by summing them by polygon. `avg_inc_resp`
can't, because it's an average over the allocated points, and two
averages don't give the average of the pooled points. Use a vector of
codes instead.

### Polygons at the edge of the region

Under the default `scope = "muni"`, only the addresses and tracts of the
municipalities in `code_muni` are used. A polygon that extends beyond
them receives only the part of the allocation that comes from the
municipalities requested, so its totals and `avg_inc_resp` describe a
fraction of it. Use `scope = "geom"` for a complete result, which reads
every municipality the polygons reach.

### Municipalities detected from the polygons

With `scope = "geom"`, the function finds the municipalities that the
polygons intersect, using the 2022 IBGE municipal boundaries, and uses
all of their tracts and addresses, even when they belong to different
states. A message lists them, and a `code_muni` passed in this case is
ignored.

Polygons from another source rarely follow the IBGE boundaries exactly,
so a polygon along a municipal border can touch a neighboring
municipality by a few meters. That neighbor is then downloaded and read
too. The values are not affected, since only the points inside the
polygons are aggregated, but use `scope = "muni"` with `code_muni`
whenever you know which municipalities the polygons belong to, which is
faster.

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
#> ℹ Using year/date 2022
poly_pop <- tracts_to_polygon(
  code_muni = 2919207,
  polygon = nei_ldf,
  vars = c("pop_ph", "pop_ch"),
  cache = FALSE
)
#> ℹ Processing municipality code 2919207...
#> ℹ Step 1/6: aligning CRS...
#> ℹ Input CRS: "EPSG:4674" | Output CRS: "EPSG:4674"
#> ℹ Step 1/6: aligning CRS...

#> ✔ Step 1/6 (CRS alignment) [28ms]
#> 
#> ℹ Step 2/6: connecting to DuckDB and loading extensions...
#> ✔ Step 2/6 (DuckDB ready) [324ms]
#> 
#> ℹ Step 3/6: preparing census tracts in DuckDB...
#> ✔ Step 3/6 (Tracts ready) [645ms]
#> 
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [444ms]
#> 
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...

#> ✔ Step 4/6 (CNEFE points ready) [2.3s]
#> 
#> ℹ Step 5/6: spatial join (points to tracts) and allocation...
#> ✔ Step 5/6 (Join and allocation) [733ms]
#> 
#> ℹ Step 6/6: aggregating allocated values to polygons...
#> ✔ Step 6/6 (Polygon aggregation) [22ms]
#> 
#> 
#> ── Dasymetric interpolation diagnostics ──
#> 
#> ! Overlapping polygons: 61 of the 95492 CNEFE dwellings inside the polygons
#>   (0.064%) fall in more than one polygon, so the values allocated to each are
#>   added to every polygon that contains it, and the totals summed over the
#>   polygons exceed those of the tracts.
#> 
#> ── Stage 1: Tracts → CNEFE points 
#> ! Unallocated total for population from private households (pop_ph): 0 of
#>   202583 (0.00%)
#> ! Unallocated total for population from collective households (pop_ch): 0 of
#>   317 (0.00%)
#> ! Unmatched CNEFE points of the municipalities in `code_muni` (no tract): 186
#>   of 95739 points (0.19%)
#> ! Tracts with NA totals: pop_ph in 4 of 354 tracts (1.13% of the tracts read);
#>   pop_ch in 6 of 354 tracts (1.69% of the tracts read)
#> 
#> ── Stage 2: CNEFE points → Polygons 
#> ℹ Polygon coverage: 95492 of 95553 allocated points of the municipalities in
#>   `code_muni` captured (99.94%)

# With scope = "geom", the municipalities the polygons intersect are used
poly_pop_geom <- tracts_to_polygon(polygon = nei_ldf, scope = "geom", cache = FALSE)
#> ℹ The polygons intersect 4 municipalities:
#>   Camaçari (2905701), Lauro de Freitas (2919207), Salvador (2927408), Simões
#>   Filho (2930709)
#> ℹ Use `scope = "muni"` with `code_muni` to read only the municipalities you
#>   choose.
#> ℹ Processing municipality code 2905701, 2919207, 2927408, and 2930709...
#> ℹ Step 1/6: aligning CRS...
#> ℹ Input CRS: "EPSG:4674" | Output CRS: "EPSG:4674"
#> ℹ Step 1/6: aligning CRS...

#> ✔ Step 1/6 (CRS alignment) [30ms]
#> 
#> ℹ Step 2/6: connecting to DuckDB and loading extensions...
#> ✔ Step 2/6 (DuckDB ready) [319ms]
#> 
#> ℹ Step 3/6: preparing census tracts in DuckDB...
#> ✔ Step 3/6 (Tracts ready) [1.2s]
#> 
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...
#> ℹ Municipality 1 of 4: 2905701
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2905701_CAMACARI.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [934ms]
#> 
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...

#> ℹ Municipality 2 of 4: 2919207
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [457ms]
#> 
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...

#> ℹ Municipality 3 of 4: 2927408
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2927408_SALVADOR.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [6.7s]
#> 
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...

#> ℹ Municipality 4 of 4: 2930709
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2930709_SIMOES_FILHO.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [247ms]
#> 
#> ℹ Step 4/6: preparing CNEFE points in DuckDB...

#> ✔ Step 4/6 (CNEFE points ready) [37.8s]
#> 
#> ℹ Step 5/6: spatial join (points to tracts) and allocation...
#> ✔ Step 5/6 (Join and allocation) [17.4s]
#> 
#> ℹ Step 6/6: aggregating allocated values to polygons...
#> ✔ Step 6/6 (Polygon aggregation) [23ms]
#> 
#> 
#> ── Dasymetric interpolation diagnostics ──
#> 
#> ! Overlapping polygons: 61 of the 95514 CNEFE dwellings inside the polygons
#>   (0.064%) fall in more than one polygon, so the values allocated to each are
#>   added to every polygon that contains it, and the totals summed over the
#>   polygons exceed those of the tracts.
#> 
#> ── Stage 1: Tracts → CNEFE points 
#> ! Unallocated total for population from private households (pop_ph): 5058 of
#>   3024198 (0.17%)
#> ! Unallocated total for population from collective households (pop_ch): 235 of
#>   4204 (5.59%)
#> ! Unmatched CNEFE points of the municipalities in `code_muni` (no tract): 301
#>   of 1541623 points (0.02%)
#> ! Tracts with NA totals: pop_ph in 89 of 5831 tracts (1.53% of the tracts
#>   read); pop_ch in 141 of 5831 tracts (2.42% of the tracts read)
#> ! Tracts with no eligible dwellings: pop_ph in 9 of 5831 tracts (0.15% of the
#>   tracts read); pop_ch in 15 of 5831 tracts (0.26% of the tracts read)
#> 
#> ── Stage 2: CNEFE points → Polygons 
#> ℹ Allocated points inside the polygons: 95514, from the 4 municipalities the
#>   polygons intersect
# }
```
