# Count CNEFE address species on a spatial grid

`cnefe_counts()` reads CNEFE records for one or more municipalities,
assigns each address point to spatial units (either H3 hexagonal cells
or user-provided polygons), and returns per-unit counts of `COD_ESPECIE`
as `addr_type1` to `addr_type8`.

## Usage

``` r
cnefe_counts(
  code_muni = NULL,
  year = 2022,
  polygon_type = lifecycle::deprecated(),
  polygon = NULL,
  crs_output = NULL,
  h3_resolution = 9,
  scope = c("muni", "geom"),
  verbose = TRUE,
  cache = TRUE,
  cache_dir = NULL,
  backend = c("duckdb", "r")
)
```

## Arguments

- code_muni:

  Numeric or character. One or more seven-digit IBGE municipality codes,
  for example `2927408` or `c(2927408, 2919207)`. Repeated codes are
  dropped. With several codes, their addresses are counted together (see
  "Several municipalities").

  Required for the H3 grid, which is built from the boundaries of these
  municipalities. With `polygon`, it's required under `scope = "muni"`
  and ignored under `scope = "geom"`, which finds the municipalities
  that the polygons intersect (see "Municipalities detected from the
  polygons").

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
  with polygon geometries. When it's supplied, the counts are returned
  for these polygons instead of an H3 grid. Features with the same
  geometry as an earlier feature are dropped, with a message, and the
  first one is kept. Under `scope = "muni"`, a warning reports how many
  addresses of the municipalities in `code_muni` fall outside the
  polygons. When polygons overlap, a warning also reports how many
  addresses fall in more than one of them, since those are counted in
  each. If no address falls inside the polygons, the function stops with
  an error.

- crs_output:

  The CRS for the output object. Only used when `polygon` is supplied.
  Default is `NULL`, which uses the original CRS of the `polygon`
  argument. Can be an EPSG code (e.g., 4326, 31983) or any CRS object
  accepted by
  [`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html).

- h3_resolution:

  Integer. H3 grid resolution (default: 9). Only used for the H3 grid,
  so it is ignored when `polygon` is supplied.

- scope:

  Character. Which CNEFE addresses the function works with. `"muni"`
  (the default, and the faster option) uses every address of the
  municipalities in `code_muni`. `"geom"` uses the addresses that fall
  inside the output geometries (the H3 cells, or the polygons in
  `polygon`), whichever municipality they belong to, so the
  municipalities those geometries reach are read as well. The H3 grid
  holds the cells whose center falls inside the boundaries of
  `code_muni`, plus the neighboring cells that intersect those
  boundaries. See the section "Border cells at the edge of the region".

- verbose:

  Logical; if `TRUE`, prints messages and timing information.

- cache:

  Logical. If `TRUE` (default), the downloaded files are kept in the
  user cache directory and reused in later calls. These are the CNEFE
  file of each municipality, stored as a gzipped CSV, and the municipal
  boundaries used to build the H3 grid or to find the municipalities
  that the polygons reach, one small file per municipality. If `FALSE`,
  temporary files are used and deleted after the call. See
  [`clear_cache_muni()`](https://pedreirajr.github.io/cnefetools/dev/reference/clear_cache_muni.md)
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

- backend:

  Character. `"duckdb"` (default) uses DuckDB with the H3 extension, and
  the spatial extension as well when `polygon` is supplied. `"r"` uses
  h3jsr and sf in R instead, and needs no DuckDB extension for the
  counts. The municipal boundaries of the H3 grid are still downloaded
  through DuckDB's httpfs extension the first time, on either backend,
  and if that fails the simplified geobr boundaries are used instead.

  `"r"` exists for environments where DuckDB extensions cannot be
  installed, such as some restricted computing clusters. It is **not**
  the lighter option: it materializes the filtered address table in R
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

- `id_hex` (when `polygon` is `NULL`): H3 cell identifier

- Original columns from `polygon` (when `polygon` is supplied)

- `addr_type1` ... `addr_type8`: counts per address type

- `geometry`: polygon geometry

When `polygon` is supplied, the output CRS matches the original
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

### Several municipalities

Pass a vector of codes to cover a region that spans more than one
municipality. The records of all of them are counted together on one
grid built over the union of the boundaries, so every cell appears once.

Calling the function once per municipality and binding the results isn't
the same. A cell on a shared border belongs to the grid of both
municipalities, so it comes back from both calls, each with only the
addresses of its own municipality (under the default `scope = "muni"`).
Counts add up, so those outputs can be combined by summing `addr_type1`
to `addr_type8` by `id_hex`, but dropping the repeated rows (for example
with
[`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html))
loses one side of the border. Shares and indices shouldn't be averaged
across municipalities. Compute them from the summed counts, or use
[`compute_lumi()`](https://pedreirajr.github.io/cnefetools/dev/reference/compute_lumi.md)
with a vector of codes.

### Border cells at the edge of the region

Under the default `scope = "muni"`, only the addresses of the
municipalities in `code_muni` are read. A cell or polygon on the outer
edge of the requested area, one that also overlaps a municipality you
did not request, therefore describes only the part of it that lies
inside the area: its counts leave out the addresses of the neighbor.
Adding municipalities moves the edge outward but does not remove it.

Under `scope = "geom"`, the municipalities that the output geometries
reach are read too, and every cell or polygon receives all the addresses
inside it, so the edge is complete. On the H3 grid, `code_muni` still
defines the grid, and a message lists the neighboring municipalities
read to complete it. This costs a download and a read of each of them in
full, which is why it is not the default.

### Addresses outside the H3 grid

The grid is built from the 2022 IBGE municipal boundaries, at full
resolution, plus the ring of cells along them, but each address goes to
the cell of its own coordinates. An address geocoded beyond that ring,
for example on the coast or on a small island the boundary doesn't
follow, lands in a cell outside the grid and isn't counted. A warning
reports how many addresses of the municipalities in `code_muni` this
affects, so the counts can be reconciled with their CNEFE totals. Under
`scope = "geom"`, the addresses of the neighboring municipalities that
fall beyond the grid are expected and not reported.

### Municipalities detected from the polygons

When `polygon` is supplied with `scope = "geom"`, the function finds the
municipalities that the polygons intersect, using the 2022 IBGE
municipal boundaries, and reads all of their addresses. A message lists
them, and a `code_muni` passed in this case is ignored.

Polygons from another source rarely follow the IBGE boundaries exactly,
so a polygon along a municipal border can touch a neighboring
municipality by a few meters. That neighbor is then downloaded and read
too. The counts are not affected, since only the points inside the
polygons are counted, but use `scope = "muni"` with `code_muni` whenever
you know which municipalities the polygons belong to, which is faster.

## See also

[`compute_lumi()`](https://pedreirajr.github.io/cnefetools/dev/reference/compute_lumi.md)
for land-use mix indices on the same spatial units.

## Examples

``` r
# \donttest{
# Count addresses per H3 hexagon (resolution 9)
hex_counts <- cnefe_counts(code_muni = 2929057, cache = FALSE)
#> ℹ Step 1/3: Building full H3 grid over the municipal boundaries...
#> ✔ Step 1/3 (H3 grid built) [4s]
#> 
#> ℹ Step 2/3: Ensuring the CNEFE data file...
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2929057_SAO_FELIX_DO_CORIBE.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [38ms]
#> 
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> ✔ Step 2/3 (CNEFE data ready) [12.2s]
#> 
#> ℹ Step 3/3: Counting address species per hexagon...
#> ✔ Step 3/3 (Addresses counted) [365ms]
#> 

# Several municipalities in one call: one grid over their union, with no cell
# repeated along the shared border (Salvador and its neighbor Lauro de Freitas)
region_counts <- cnefe_counts(code_muni = c(2927408, 2919207), cache = FALSE)
#> ℹ Step 1/3: Building full H3 grid over the municipal boundaries...
#> ✔ Step 1/3 (H3 grid built) [1.8s]
#> 
#> ℹ Step 2/3: Ensuring the CNEFE data file...
#> ℹ Municipality 1 of 2: 2927408
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2927408_SALVADOR.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [6.7s]
#> 
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> ℹ Municipality 2 of 2: 2919207
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [427ms]
#> 
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> ✔ Step 2/3 (CNEFE data ready) [22.9s]
#> 
#> ℹ Step 3/3: Counting address species per hexagon...
#> ✔ Step 3/3 (Addresses counted) [2.1s]
#> 

# The grid of Lauro de Freitas, with the cells along its border completed by
# the addresses of the neighboring municipalities
ldf_full <- cnefe_counts(code_muni = 2919207, scope = "geom", cache = FALSE)
#> ℹ Step 1/3: Building full H3 grid over the municipal boundaries...
#> ℹ The H3 grid also reaches 3 municipalities outside `code_muni`, read to
#>   complete the cells along the border:
#>   Camaçari (2905701), Salvador (2927408), Simões Filho (2930709)
#> ℹ Use `scope = "muni"` to read only the municipalities in `code_muni`.
#> ✔ Step 1/3 (H3 grid built) [1.5s]
#> 
#> ℹ Step 2/3: Ensuring the CNEFE data file...
#> ℹ Municipality 1 of 4: 2919207
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [440ms]
#> 
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> ℹ Municipality 2 of 4: 2905701
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2905701_CAMACARI.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [924ms]
#> 
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> ℹ Municipality 3 of 4: 2927408
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2927408_SALVADOR.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [6.7s]
#> 
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> ℹ Municipality 4 of 4: 2930709
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2930709_SIMOES_FILHO.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [240ms]
#> 
#> ℹ Step 2/3: Ensuring the CNEFE data file...

#> ✔ Step 2/3 (CNEFE data ready) [20.3s]
#> 
#> ℹ Step 3/3: Counting address species per hexagon...
#> ✔ Step 3/3 (Addresses counted) [2.9s]
#> 

# Count addresses per user-provided polygon (neighborhoods of Lauro de Freitas-BA)
# Using geobr to download neighborhood boundaries
library(geobr)
nei_ldf <- subset(
  read_neighborhood(year = 2022),
  code_muni == 2919207
)
#> ℹ Using year/date 2022
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpsaIjE9/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
nei_counts <- cnefe_counts(
  code_muni = 2919207,
  polygon = nei_ldf,
  cache = FALSE
)
#> ℹ Step 1/2: Ensuring data and preparing polygon...
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [443ms]
#> 
#> ℹ Step 1/2: Ensuring data and preparing polygon...

#> ✔ Step 1/2 (Data and polygon ready) [10.2s]
#> 
#> ℹ Step 2/2: Counting addresses per polygon...
#> Warning: Polygon coverage: "99.7%" of the CNEFE points of the municipalities in
#> `code_muni` were captured.
#> ! Overlapping polygons: 94 of the 111100 CNEFE points inside the polygons
#>   (0.085%) fall in more than one polygon, so each is counted in every polygon
#>   that contains it, and the counts summed over the polygons exceed the number
#>   of addresses.
#> ℹ 111100 of 111385 points are within the provided polygon.
#> ℹ 285 points from the municipalities in `code_muni` fell outside the polygons
#>   and were not counted. Possible causes are polygons that don't cover the whole
#>   municipality, a coastline the polygons don't follow, or geocoding errors.
#> ✔ Step 2/2 (Addresses counted) [938ms]
#> 

# With scope = "geom", the municipalities the polygons intersect are read
nei_counts_auto <- cnefe_counts(polygon = nei_ldf, scope = "geom", cache = FALSE)
#> ℹ The polygons intersect 4 municipalities:
#>   Camaçari (2905701), Lauro de Freitas (2919207), Salvador (2927408), Simões
#>   Filho (2930709)
#> ℹ Use `scope = "muni"` with `code_muni` to read only the municipalities you
#>   choose.
#> ℹ Step 1/2: Ensuring data and preparing polygon...
#> ℹ Municipality 1 of 4: 2905701
#> ℹ Step 1/2: Ensuring data and preparing polygon...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2905701_CAMACARI.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [934ms]
#> 
#> ℹ Step 1/2: Ensuring data and preparing polygon...

#> ℹ Municipality 2 of 4: 2919207
#> ℹ Step 1/2: Ensuring data and preparing polygon...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [447ms]
#> 
#> ℹ Step 1/2: Ensuring data and preparing polygon...

#> ℹ Municipality 3 of 4: 2927408
#> ℹ Step 1/2: Ensuring data and preparing polygon...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2927408_SALVADOR.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [6.7s]
#> 
#> ℹ Step 1/2: Ensuring data and preparing polygon...

#> ℹ Municipality 4 of 4: 2930709
#> ℹ Step 1/2: Ensuring data and preparing polygon...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2930709_SIMOES_FILHO.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [240ms]
#> 
#> ℹ Step 1/2: Ensuring data and preparing polygon...

#> ✔ Step 1/2 (Data and polygon ready) [18.1s]
#> 
#> ℹ Step 2/2: Counting addresses per polygon...
#> Warning: Overlapping polygons: 94 of the 111188 CNEFE points inside the polygons
#> (0.085%) fall in more than one polygon, so each is counted in every polygon
#> that contains it, and the counts summed over the polygons exceed the number of
#> addresses.
#> ✔ Step 2/2 (Addresses counted) [3.8s]
#> 
# }
```
