# cnefetools: Tools for working with Brazilian CNEFE address data

[![R-CMD-check](https://github.com/pedreirajr/cnefetools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pedreirajr/cnefetools/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://pedreirajr.github.io/cnefetools/LICENSE.md)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

**{cnefetools}** provides helper functions to work with the 2022
Brazilian National Address File for Statistical Purposes (*Cadastro
Nacional de Endereços para Fins Estatísticos*, CNEFE), an address-level
dataset released by the Brazilian Institute of Geography and Statistics
(*Instituto Brasileiro de Geografia e Estatística*, IBGE).

## Installation

``` r
# install.packages("pak")
pak::pak("pedreirajr/cnefetools")
```

## Basic usage

``` r
library(cnefetools)
library(dplyr)

# Example: read CNEFE data for a municipality (Salvador) as an Arrow table
tab_ssa <- read_cnefe(2927408, cache = TRUE)

# Convert to a data frame for inspection
df_ssa <- tab_ssa %>%
  as.data.frame()

head(df_ssa)
```

## Spatial output and quick map

If you prefer to work with spatial objects,
[`read_cnefe()`](https://pedreirajr.github.io/cnefetools/reference/read_cnefe.md)
can return an `sf` object by setting `output = "sf"`. The example below
reads CNEFE data for Salvador, filters only religious facilities
(`COD_ESPECIE == 8`), and plots them with `ggplot2`:

``` r
library(cnefetools)
library(sf)
library(ggplot2)
library(dplyr)

# Read CNEFE data as sf (Salvador, IBGE code = 2927408)
tab_ssa_sf <- read_cnefe(code_muni = 2927408, output = "sf", cache = TRUE)

# Keep only religious facilities (COD_ESPECIE == 8)
temples_ssa <- tab_ssa_sf %>%
  dplyr::filter(COD_ESPECIE == 8)

ggplot() +
  geom_sf(data = temples_ssa, size = 0.3, alpha = 0.6) +
  coord_sf() +
  theme_minimal()
```

> **Warning**
>
> For large municipalities, CNEFE may contain more than 1 million
> address points. Plotting all coordinates at once can be slow and
> memory intensive, so consider filtering by area of interest or
> sampling points before creating maps.

## Caching behavior

By default, `cache = TRUE` stores the downloaded ZIP file in a
user-level cache directory specific to this package. If you prefer to
avoid persistent caching, set:

``` r
tab_ssa <- read_cnefe(code_muni = 2927408, cache = FALSE)
```

In this case, the ZIP file is stored in a temporary location and removed
after reading.

## Accessing official CNEFE documentation

**{cnefetools}** includes local copies of the official methodological
note and the variable dictionary for the 2022 CNEFE released by IBGE.

You can open these documents in your default PDF and spreadsheet viewers
with:

``` r
# Open the official methodological note (PDF)
cnefe_doc()

# Open the official variable dictionary (Excel)
cnefe_dictionary()
```

## Hexagon-level address counts with `hex_cnefe_counts()`

The
[`hex_cnefe_counts()`](https://pedreirajr.github.io/cnefetools/reference/hex_cnefe_counts.md)
function aggregates CNEFE address points into an H3 hexagonal grid of
any resolution and returns an `sf` object with one row per hexagon. It
includes:

- `id_hex`: H3 cell identifier.
- `addr_type1` … `addr_type8`: counts by address category (see
  [`?hex_cnefe_counts`](https://pedreirajr.github.io/cnefetools/reference/hex_cnefe_counts.md)
  for the mapping).
- `geometry`: hexagon geometry (CRS 4326).

### Choosing a backend (performance)

[`hex_cnefe_counts()`](https://pedreirajr.github.io/cnefetools/reference/hex_cnefe_counts.md)
supports two backends via the `backend` argument:

- `backend = "duckdb"` (default): uses DuckDB with the community `h3`
  extension to compute the H3 cell id and aggregate counts in SQL. This
  is substantially faster for large municipalities because it avoids
  materializing all points as an `sf` object in R.
- `backend = "r"`: uses `sf` + `h3jsr` in R. This is useful when you
  prefer a pure-R workflow, but it can be much slower for very large
  cities.

To use `backend = "duckdb"`, you need `duckdb` installed. On first use,
DuckDB may download or install the `h3` and `zipfs` extensions (DuckDB
community extensions). Subsequent runs reuse the installed extensions.

Below is an example for São Paulo (IBGE code 3550308) at H3 resolution
9:

``` r
library(cnefetools)
library(sf)
library(ggplot2)
library(dplyr)

# Fast path (default): DuckDB backend
hex_sp <- hex_cnefe_counts(code_muni = 3550308, h3_resolution = 9, backend = "duckdb", verbose = TRUE)

# Pure-R path (can be slower for large cities)
# hex_sp_r <- hex_cnefe_counts(code_muni = 3550308, h3_resolution = 9, backend = "r", verbose = TRUE)
```

You can visualize the spatial distribution of private households
(`addr_type1`) with `ggplot2`:

``` r
ggplot(hex_sp) +
  geom_sf(aes(fill = addr_type1), color = NA) +
  scale_fill_viridis_c(option = "magma", trans = "sqrt") +
  coord_sf() +
  labs(
    fill = "Count",
    title = "Private households (addr_type1)",
    subtitle = "São Paulo (IBGE 3550308), H3 resolution 9"
  ) +
  theme_minimal()
```

## Land-use mix indicators with `compute_lumi()`

The
[`compute_lumi()`](https://pedreirajr.github.io/cnefetools/reference/compute_lumi.md)
function computes land-use mix indicators on an H3 hexagonal grid at any
user-defined resolution for any Brazilian municipality covered by the
2022 CNEFE dataset ([Pedreira Junior et al.,
2025](https://engrxiv.org/preprint/view/5975/version/7846)). Internally,
it: (i) reads and preprocesses address points from CNEFE; (ii) assigns
each point to an H3 cell; (iii) aggregates residential and
non-residential addresses per hexagon; and (iv) computes the following
indicators:

- `p_res`: proportion of residential addresses in each hexagon.
- `ei`: Entropy Index (EI), ranging from 0 (single use) to 1 (maximum
  mix), with the maximum attained at `p_res` = 0.5.
- `hhi`: Herfindahl–Hirschman Index (HHI), ranging from 0.5 (maximum mix
  at `p_res` = 0.5) to 1 (single use).
- `bal`: Balance Index (BAL), ranging from 0 (single use) to 1 (maximum
  balance), where balance is defined relative to the citywide
  residential proportion (i.e., the maximum occurs when `p_res` equals
  the citywide residential share).
- `hhi_adp`: adapted HHI, which adds directionality and takes values in
  \[-1, 1\], where negative values indicate non-residential dominance
  and positive values indicate residential dominance (0 indicates a
  50/50 split).
- `bgbi`: Bidirectional Global-centered Balance Index (BGBI), which
  measures the deviation of the local residential share (`p_res`) from
  the citywide residential proportion, also in \[-1, 1\].

### Choosing a backend (performance)

Like
[`hex_cnefe_counts()`](https://pedreirajr.github.io/cnefetools/reference/hex_cnefe_counts.md),
[`compute_lumi()`](https://pedreirajr.github.io/cnefetools/reference/compute_lumi.md)
supports a `backend` argument. Below is an example for Fortaleza (IBGE
code 2304400) at H3 resolution 8:

``` r
library(cnefetools)
library(sf)
library(ggplot2)
library(dplyr)

# Compute land use mix indicators (recommended for large cities)
lumi_for <- compute_lumi(code_muni = 2304400, h3_resolution = 8, backend = "duckdb", verbose = TRUE)
```

You can visualize the spatial distribution of the BGBI indicator with
`ggplot2`:

``` r
ggplot(lumi_for) +
  geom_sf(aes(fill = bgbi), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  coord_sf() +
  labs(
    fill = "BGBI",
    title = "Bidirectional Global-centered Balance Index (BGBI)",
    subtitle = "Fortaleza (IBGE 2304400), H3 resolution 8"
  ) +
  theme_minimal()
```

## Dasymetric interpolation from census tracts to H3 with `tracts_to_h3()`

[`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_h3.md)
implements a dasymetric interpolation workflow that transfers census
tract totals to address-level points (CNEFE), and then aggregates the
allocated values to an H3 grid.

### What the function does

1.  **Prepare inputs in DuckDB**
    - Downloads (once) and caches a UF-specific Parquet file with 2022
      census tract geometries and tract-level aggregates.
    - Reads CNEFE points for the requested municipality (from the cached
      municipal ZIP).
    - Builds tract geometries (`MULTIPOLYGON`) and point geometries from
      coordinates.
2.  **Stage 1: census tracts to CNEFE points**
    - Spatially assigns each CNEFE point to a census tract.
    - For each tract and dwelling type, computes how many eligible
      dwellings exist in the tract and allocates tract totals across
      points.
3.  **Stage 2: CNEFE points to H3**
    - Assigns each point to an H3 cell at the chosen resolution and sums
      allocated values per cell.
    - Builds an H3 hexagon `sf` layer and joins the aggregated values.

The output is an `sf` object (CRS 4326) with one row per H3 hexagon,
`id_hex`, and the interpolated variables you requested.

### Variables available for interpolation

At the moment,
[`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_h3.md)
can interpolate the variables below (all sourced from tract-level
aggregates), by choosing which ones to compute via the `vars` argument:

- `n_inhab_p`: total population in **private households** (domicílios
  particulares);

- `n_inhab_c`: total population in **collective households** (domicílios
  coletivos);

- `n_resp`: number of household heads (*Pessoas responsáveis*);

- Sex totals: `male` (total male population) and `female` (total female
  population);

- Age groups : `age_0_4`, `age_5_9`, `age_10_14`, `age_15_19` -
  `age_20_24`, `age_25_29`, `age_30_39`, `age_40_49`, `age_50_59`,
  `age_60_69` and `age_70m` (70+);

- Income: `avg_inc_resp`: average income of the responsible person (mean
  at tract level).

#### Allocation rules (important)

- For **count-type totals** (population by sex/age, `n_resp`, and the
  population totals):
  - values are allocated across **private household points** within each
    tract when private dwellings exist;
  - if a tract has **no private dwellings**, the value is allocated
    across **collective household points** (if any);
  - if a tract has **no eligible dwellings** (or the tract total is
    `NA`), that tract total is **not allocated**, and it is reported in
    the diagnostics.
- For `avg_inc_resp`:
  - the tract value is assigned directly to each eligible dwelling point
    (private, or collective only when there are no private dwellings in
    that tract);
  - when `avg_inc_resp` is `NA`, the function reports how many tracts
    had missing values.

### Diagnostics and timing

[`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_h3.md)
prints an execution timing summary (when `verbose = TRUE`) and may emit
a warning with a diagnostic report. The diagnostics report includes, for
each requested variable:

- unallocated tract totals (with the percentage relative to the tract
  total for that municipality);
- tracts with `NA` totals;
- tracts with no eligible dwellings (so nothing could be allocated);
- unmatched CNEFE points (points that did not fall in any tract).

### Example: Recife (2611606)

``` r
library(cnefetools)
library(dplyr)
library(ggplot2)

# Interpolate to H3 for Recife at resolution 9
# Example vars: private-household population and mean income of the responsible person
rec_hex <- tracts_to_h3(
  code_muni = 2611606,
  h3_resolution = 9,
  vars = c("n_inhab_p", "avg_inc_resp"),
  cache = TRUE,
  verbose = TRUE
)

# Map private-household population
ggplot(rec_hex) +
  geom_sf(aes(fill = n_inhab_p), color = NA) +
  scale_fill_viridis_c(trans = "sqrt") +
  coord_sf() +
  labs(
    title = "Dasymetric interpolation to H3 using CNEFE (Recife)",
    subtitle = "Private-household population (n_inhab_p), H3 resolution 9",
    fill = "Population"
  ) +
  theme_minimal()

# Map avg income of the household head
ggplot(rec_hex) +
  geom_sf(aes(fill = avg_inc_resp), color = NA) +
  scale_fill_viridis_c() +
  coord_sf() +
  labs(
    title = "Dasymetric interpolation to H3 using CNEFE (Recife)",
    subtitle = "Average income of the responsible person (avg_inc_resp), H3 resolution 9",
    fill = "Income"
  ) +
  theme_minimal()
```

## Citation

If you use **{cnefetools}** in your work, please cite the associated
preprint:

> Pedreira Jr., J. U.; Louro, T. V.; Assis, L. B. M.; Brito, P. L.
> *Measuring land use mix with address-level census data* (2025).
> engrXiv. <https://engrxiv.org/preprint/view/5975>

## Roadmap

The current version focuses on efficient download, caching, and reading
of municipality-level CNEFE files, plus scalable H3-based analytics
(counts, land-use mix, and tract-to-hex dasymetric interpolation).

Planned next steps include:

- Expanding the set of tract-level variables available for dasymetric
  interpolation.
- More diagnostics and summary helpers (for example, returning a
  diagnostics object as an attribute).
- Support alternative aggregate data sources and target geographic
  output (other than H3), when available.
