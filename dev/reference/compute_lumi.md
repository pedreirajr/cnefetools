# Compute land-use mix indicators on a spatial grid

`compute_lumi()` reads CNEFE records for a given municipality, assigns
each address point to spatial units (either H3 hexagonal cells or
user-provided polygons), and computes the residential proportion
(`p_res`) and land-use mix indices, such as the Entropy Index (`ei`),
the Herfindahl-Hirschman Index (`hhi`), the Balance Index (`bal`), the
Index of Concentration at Extremes (`ice`), the adapted HHI (`hhi_adp`),
and the Bidirectional Global-centered Index (`bgbi`), following the
methodology proposed in Pedreira Junior et al. (2026).

## Usage

``` r
compute_lumi(
  code_muni,
  year = 2022,
  polygon_type = lifecycle::deprecated(),
  polygon = NULL,
  crs_output = NULL,
  h3_resolution = 9,
  verbose = TRUE,
  cache = TRUE,
  cache_dir = NULL,
  backend = c("duckdb", "r")
)
```

## Arguments

- code_muni:

  Integer. Seven-digit IBGE municipality code.

- year:

  Integer. The CNEFE data year. Currently only 2022 is supported.
  Defaults to 2022.

- polygon_type:

  **\[deprecated\]** The aggregation mode is now inferred from
  `polygon`: leave it `NULL` for an H3 grid, or pass an
  [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) object
  for user polygons. Passing `polygon_type` still works and warns.

- polygon:

  An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) object
  with polygon geometries. Supplying it switches the output from an H3
  grid to these polygons. A warning is issued reporting the percentage
  of CNEFE points covered by the polygon area. If no CNEFE points fall
  within the polygon, an error is raised.

- crs_output:

  The CRS for the output object. Only used when `polygon_type = "user"`.
  Default is `NULL`, which uses the original CRS of the `polygon`
  argument. Can be an EPSG code (e.g., 4326, 31983) or any CRS object
  accepted by
  [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html).

- h3_resolution:

  Integer. H3 grid resolution (default: 9). Only used when
  `polygon_type = "hex"`.

- verbose:

  Logical; if `TRUE`, prints messages and timing information.

- cache:

  Logical. If `TRUE` (default), the downloaded data is stored as a
  gzipped CSV in the user cache directory and reused in future calls. If
  `FALSE`, a temporary file is used and deleted after the call.

- cache_dir:

  Character. Directory to use for cached downloads. If `NULL` (default),
  the `CNEFETOOLS_CACHE_DIR` environment variable is used when it is
  set, otherwise
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) with
  `which = "cache"`. Use this to point large downloads at a secondary
  drive or a shared volume.

- backend:

  Character. `"duckdb"` (default) uses DuckDB with the H3 extension,
  reading the cached gzipped CSV directly. `"r"` computes H3 in R using
  h3jsr instead, and needs no DuckDB extension.

  `"r"` exists for environments where DuckDB extensions cannot be
  installed, such as some restricted computing clusters. It is **not**
  the lighter option: it materialises the filtered address table in R
  memory, so its footprint grows with the municipality, while DuckDB
  aggregates in a streaming fashion and stays nearly flat. On São Paulo
  (5.7 million addresses) the measured peak is about 8.4 GB under `"r"`
  against 0.6 GB under `"duckdb"`, alongside being roughly 15 times
  slower.

  If the constraint is memory rather than installability, keep the
  DuckDB backend and cap it with the `cnefetools.duckdb_config` option
  instead. See
  [`?cnefetools`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefetools-package.md)
  for that option, and the benchmark article at
  <https://pedreirajr.github.io/cnefetools/articles/bench_duckdb.html>
  for the measurements.

## Value

An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) object
containing:

- When `polygon_type = "hex"`::

  - `id_hex`: H3 cell identifier

  - `p_res`, `ei`, `hhi`, `bal`, `ice`, `hhi_adp`, `bgbi`: land-use mix
    indicators

  - `geometry`: hexagon geometry (CRS 4326)

- When `polygon_type = "user"`::

  - Original columns from `polygon`

  - `p_res`, `ei`, `hhi`, `bal`, `ice`, `hhi_adp`, `bgbi`: land-use mix
    indicators

  - `geometry`: polygon geometry (in the original or `crs_output` CRS)

## Details

### Binary land-use classification

The indices computed here rest on a binary split. An address is counted
as residential when `COD_ESPECIE == 1` (private household), and as
non-residential otherwise. This follows the formulation of the indices
as published in Pedreira Junior et al. (2026), where the measures are
defined and empirically validated on that two-category basis.

### Exclusion of buildings under construction

`compute_lumi()` drops records with `COD_ESPECIE == 7` (building under
construction or renovation), because such records describe a
transitional state rather than a realised land use. Note that
[`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_counts.md)
does **not** apply this exclusion and reports these records as
`addr_type7`.

### The citywide baseline P

The `bgbi` index is referenced against a citywide residential share P.
It is the only index computed here that uses such a baseline, the others
being computed entirely within each spatial unit. Two properties of P
are worth stating.

First, P is computed from CNEFE address-type counts rather than from
census population, so it describes the distribution of address types and
not the distribution of residents.

Second, P is always computed over the full municipality, including when
`polygon_type = "user"`, so it does not adapt to the area the supplied
polygons happen to cover. This is intended, as P describes the context
the addresses sit in, which is the municipality, and a sub-area of a
city is still part of that wider context. A baseline recomputed over the
sub-area would measure something different, namely mix relative to the
sub-area itself rather than relative to the city.

## References

Pedreira Junior, J. U.; Louro, T. V.; Assis, L. B. M.; Brito, P. L.;
Bomfim, F. G. (2026). BGBI: A citywide-referenced and bidirectional land
use mix index for planning and policy evaluation. *Land Use Policy*,
169, 108135. https://doi.org/10.1016/j.landusepol.2026.108135

Pedreira Jr., J. U.; Louro, T. V.; Assis, L. B. M.; Brito, P. L.
Measuring land use mix with address-level census data (2025). *engrXiv*.
https://engrxiv.org/preprint/view/5975 (preprint, where the adapted HHI
(`hhi_adp`) is documented)

Booth, A.; Crouter, A. C. (Eds.). (2001). *Does It Take a Village?
Community Effects on Children, Adolescents, and Families*. Psychology
Press.

Song, Y.; Merlin, L.; Rodriguez, D. (2013). Comparing measures of urban
land use mix. *Computers, Environment and Urban Systems*, 42, 1–13.
https://doi.org/10.1016/j.compenvurbsys.2013.08.001

## Examples

``` r
# \donttest{
# Compute land-use mix indices on H3 hexagons
lumi <- compute_lumi(code_muni = 2929057, cache = FALSE)
#> ℹ Processing municipality code 2929057...
#> ℹ Step 1/3: Ensuring the CNEFE data file...
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2929057_SAO_FELIX_DO_CORIBE.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [45ms]
#> 
#> ℹ Step 1/3: Ensuring the CNEFE data file...
#> ✔ Step 1/3 (CNEFE data ready) [533ms]
#> 
#> ℹ Step 2/3: Counting addresses per H3 cell...
#> ✔ Step 2/3 (Addresses counted) [165ms]
#> 
#> ℹ Step 3/3: Building grid and computing LUMI...
#> ✔ Step 3/3 (Land use mix indices computed) [3.2s]
#> 

# Compute land-use mix indices on user-provided polygons (neighborhoods of Lauro de Freitas-BA)
# Using geobr to download neighborhood boundaries
library(geobr)
nei_ldf <- subset(
  read_neighborhood(year = 2022),
  code_muni == 2919207
)
#> ℹ Using year/date 2022
lumi_poly <- compute_lumi(
  code_muni = 2919207,
  polygon_type = "user",
  polygon = nei_ldf,
  cache = FALSE
)
#> Warning: The `polygon_type` argument of `compute_lumi()` is deprecated as of cnefetools
#> 0.3.0.
#> The aggregation mode is now inferred from `polygon`.
#> ℹ Pass an <sf> object to `polygon` for user polygons, or leave it `NULL` for an
#>   H3 grid.
#> ℹ The deprecated feature was likely used in the cnefetools package.
#>   Please report the issue at <https://github.com/pedreirajr/cnefetools/issues>.
#> ℹ Processing municipality code 2919207...
#> ℹ Step 1/3: Ensuring data and preparing polygon...
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [742ms]
#> 
#> ℹ Step 1/3: Ensuring data and preparing polygon...
#> ✔ Step 1/3 (Data and polygon ready) [2.3s]
#> 
#> ℹ Step 2/3: Counting addresses per polygon...
#> ✔ Step 2/3 (Addresses counted) [1.1s]
#> 
#> ℹ Step 3/3: Computing land use mix indices...
#> Warning: Polygon coverage: "99.7%" of CNEFE points captured.
#> ℹ 106975 of 107244 points are within the provided polygon.
#> ℹ 269 points fell outside the polygon and were not counted.
#> ✔ Step 3/3 (Land use mix indices computed) [57ms]
#> 
# }
```
