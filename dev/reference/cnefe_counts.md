# Count CNEFE address species on a spatial grid

`cnefe_counts()` reads CNEFE records for a given municipality, assigns
each address point to spatial units (either H3 hexagonal cells or
user-provided polygons), and returns per-unit counts of `COD_ESPECIE` as
`addr_type1` to `addr_type8`.

## Usage

``` r
cnefe_counts(
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

  Character. `"duckdb"` (default) uses DuckDB with the H3 extension, and
  the spatial extension as well when `polygon` is supplied. `"r"` uses
  h3jsr and sf in R instead, and needs no DuckDB extension.

  `"r"` exists for environments where DuckDB extensions cannot be
  installed, such as some restricted computing clusters. It is **not**
  the lighter option: it materialises the filtered address table in R
  memory, so its footprint grows with the municipality, while DuckDB
  aggregates in a streaming fashion and stays nearly flat. On São Paulo
  (5.7 million addresses) the measured peak is about 9 GB under `"r"`
  against 0.7 GB under `"duckdb"`, alongside being roughly 13 times
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

- `id_hex` (when `polygon_type = "hex"`): H3 cell identifier

- Original columns from `polygon` (when `polygon_type = "user"`)

- `addr_type1` ... `addr_type8`: counts per address type

- `geometry`: polygon geometry

When `polygon_type = "user"`, the output CRS matches the original
`polygon` CRS (or `crs_output` if specified).

## Details

The counts in the columns `addr_type1` to `addr_type8` correspond to:

- `addr_type1`: Private household (Domicílio particular)

- `addr_type2`: Collective household (Domicílio coletivo)

- `addr_type3`: Agricultural establishment (Estabelecimento
  agropecuário)

- `addr_type4`: Educational establishment (Estabelecimento de ensino)

- `addr_type5`: Health establishment (Estabelecimento de saúde)

- `addr_type6`: Establishment for other purposes (Estabelecimento de
  outras finalidades)

- `addr_type7`: Building under construction or renovation (Edificação em
  construção ou reforma)

- `addr_type8`: Religious establishment (Estabelecimento religioso)

All eight types are reported. In particular, `addr_type7` is retained
here, whereas
[`compute_lumi()`](https://pedreirajr.github.io/cnefetools/dev/reference/compute_lumi.md)
excludes it when computing land-use mix indices.

## See also

[`compute_lumi()`](https://pedreirajr.github.io/cnefetools/dev/reference/compute_lumi.md)
for land-use mix indices on the same spatial units.

## Examples

``` r
# \donttest{
# Count addresses per H3 hexagon (resolution 9)
hex_counts <- cnefe_counts(code_muni = 2929057, cache = FALSE)
#> ℹ Step 1/3: Ensuring the CNEFE data file...
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2929057_SAO_FELIX_DO_CORIBE.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [47ms]
#> 
#> ℹ Step 1/3: Ensuring the CNEFE data file...
#> ✔ Step 1/3 (CNEFE data ready) [1.7s]
#> 
#> ℹ Step 2/3: Building full H3 grid over municipality boundary...
#> ✔ Step 2/3 (H3 grid built) [6.6s]
#> 
#> ℹ Step 3/3: Counting address species per hexagon...
#> ✔ Step 3/3 (Addresses counted) [539ms]
#> 

# Count addresses per user-provided polygon (neighborhoods of Lauro de Freitas-BA)
# Using geobr to download neighborhood boundaries
library(geobr)
nei_ldf <- subset(
  read_neighborhood(year = 2022),
  code_muni == 2919207
)
#> ℹ Using year/date 2022
hex_counts <- cnefe_counts(
  code_muni = 2919207,
  polygon_type = "user",
  polygon = nei_ldf,
  cache = FALSE
)
#> Warning: The `polygon_type` argument of `cnefe_counts()` is deprecated as of cnefetools
#> 0.3.0.
#> The aggregation mode is now inferred from `polygon`.
#> ℹ Pass an <sf> object to `polygon` for user polygons, or leave it `NULL` for an
#>   H3 grid.
#> ℹ The deprecated feature was likely used in the cnefetools package.
#>   Please report the issue at <https://github.com/pedreirajr/cnefetools/issues>.
#> ℹ Step 1/2: Ensuring data and preparing polygon...
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [566ms]
#> 
#> ℹ Step 1/2: Ensuring data and preparing polygon...
#> ✔ Step 1/2 (Data and polygon ready) [3.1s]
#> 
#> ℹ Step 2/2: Counting addresses per polygon...
#> Warning: Polygon coverage: "99.7%" of CNEFE points captured.
#> ℹ 111100 of 111385 points are within the provided polygon.
#> ℹ 285 points fell outside the polygon and were not counted.
#> ✔ Step 2/2 (Addresses counted) [1.1s]
#> 
# }
```
