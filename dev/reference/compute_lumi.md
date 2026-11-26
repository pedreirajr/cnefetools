# Compute land-use mix indicators on a spatial grid

`compute_lumi()` reads CNEFE records for one or more municipalities,
assigns each address point to spatial units (either H3 hexagonal cells
or user-provided polygons), and computes the residential proportion
(`p_res`) and land-use mix indices, such as the Entropy Index (`ei`),
the Herfindahl-Hirschman Index (`hhi`), the Balance Index (`bal`), the
Index of Concentration at Extremes (`ice`), the adapted HHI (`hhi_adp`),
and the Bidirectional Global-centered Balance Index (`bgbi`), following
the methodology proposed in Pedreira Junior et al. (2025, 2026). The
2026 article introduces the BGBI, and the adapted HHI is documented in
the 2025 preprint.

## Usage

``` r
compute_lumi(
  code_muni = NULL,
  year = 2022,
  polygon_type = lifecycle::deprecated(),
  polygon = NULL,
  crs_output = NULL,
  h3_resolution = 9,
  scope = c("muni", "geom"),
  p_target = NULL,
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
  dropped. With several codes, their addresses are pooled into one
  analysis (see "Several municipalities").

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
  with polygon geometries. When it's supplied, the indices are computed
  for these polygons instead of an H3 grid. Features with the same
  geometry as an earlier feature are dropped, with a message, and the
  first one is kept. Under `scope = "muni"`, a warning reports how many
  addresses of the municipalities in `code_muni` fall outside the
  polygons. When polygons overlap, a warning also reports how many
  addresses fall in more than one of them, since those enter the indices
  of each. If no address falls inside the polygons, the function stops
  with an error.

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
  boundaries. The scope also decides which addresses P comes from (see
  "The citywide baseline P").

- p_target:

  Numeric or `NULL`. A single number strictly between 0 and 1 that
  replaces the observed P, for example a residential share set by a
  land-use plan (`P*` in Pedreira Junior et al., 2026). `NULL` (the
  default) computes P from the data (see "The citywide baseline P").

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

  Character. `"duckdb"` (default) uses DuckDB with the H3 extension,
  reading the cached gzipped CSV directly. `"r"` computes H3 in R using
  h3jsr instead, and needs no DuckDB extension for the indices. The
  municipal boundaries of the H3 grid are still downloaded through
  DuckDB's httpfs extension the first time, on either backend, and if
  that fails the simplified geobr boundaries are used instead.

  `"r"` exists for environments where DuckDB extensions cannot be
  installed, such as some restricted computing clusters. It is **not**
  the lighter option: it materializes the filtered address table in R
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

- When `polygon` is `NULL` (H3 grid)::

  - `id_hex`: H3 cell identifier

  - `p_res`, `ei`, `hhi`, `bal`, `ice`, `hhi_adp`, `bgbi`: land-use mix
    indicators

  - `geometry`: hexagon geometry (CRS 4326)

- When `polygon` is supplied::

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
transitional state rather than a realized land use. Note that
[`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_counts.md)
does **not** apply this exclusion and reports these records as
`addr_type7`.

### The citywide baseline P

Two indices use a citywide residential share P: the `bgbi` index, which
is referenced against it, and the Balance Index (`bal`), which uses it
through r = P / (1 - P). The other indices are computed entirely within
each spatial unit. Two properties of P are worth stating.

First, P is computed from CNEFE address-type counts rather than from
census population, so it describes the distribution of address types and
not the distribution of residents.

Second, by default (`scope = "muni"`) P is computed from every CNEFE
address of the municipalities in `code_muni`, including when `polygon`
is supplied, so it does not adapt to the area the supplied polygons
happen to cover. This is intended, as P describes the context the
addresses sit in, which is the municipality (or the region, when several
are read), and a sub-area of a city is still part of that wider context.

`scope = "geom"` computes P from the addresses inside the output
geometries instead, each counted once where polygons overlap, which
measures mix relative to the area covered rather than relative to the
city. For polygons that cover only part of a municipality, such as a set
of central neighborhoods, P is then the residential share of those
polygons alone. On an H3 grid, P becomes the share of the cells of the
grid, including the addresses of the neighboring municipalities that
fall in the cells along the border.

`p_target` replaces P with a composition chosen by the analyst, such as
a goal set by a land-use plan, a zoning reform or a transit-oriented
development strategy. This is the extension proposed in the conclusions
of Pedreira Junior et al. (2026). With it, `bgbi` measures the distance
to that goal rather than to the observed city, and the same target can
be used across municipalities so that their values become comparable.
`bal` with a target is no longer the original Balance Index of Song et
al. (2013), which is defined against the observed citywide ratio.

### Several municipalities

Pass a vector of codes to cover a region that spans more than one
municipality. The records of all of them are pooled before anything is
computed, the H3 grid is built over the union of their boundaries, and P
is the residential share of the whole region. Each cell appears once,
and a cell on a shared border holds the addresses of both sides.

Because P belongs to the region, `bal` and `bgbi` are measured against
the regional share, not against each municipality's own, and adding a
municipality to `code_muni` changes them in every unit, not only near
it. A call per municipality gives each of them a different P, so indices
from such calls aren't comparable with one another, nor with a regional
call.

### Border cells at the edge of the region

Under the default `scope = "muni"`, only the addresses of the
municipalities in `code_muni` are read. A cell or polygon on the outer
edge of the requested area, one that also overlaps a municipality you
did not request, therefore describes only the part of it that lies
inside the area: its values (counts, `p_res` and indices) leave out the
addresses of the neighbor. Adding municipalities moves the edge outward
but does not remove it, and changes P.

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
affects. With `scope = "muni"` they are still part of P, and with
`scope = "geom"` they are not.

### Municipalities detected from the polygons

When `polygon` is supplied with `scope = "geom"`, the function finds the
municipalities that the polygons intersect, using the 2022 IBGE
municipal boundaries, and reads all of their addresses. A message lists
them, and a `code_muni` passed in this case is ignored.

Polygons from another source rarely follow the IBGE boundaries exactly,
so a polygon along a municipal border can touch a neighboring
municipality by a few meters. That neighbor is then downloaded and read
too. Neither P nor the indices change, since only the addresses inside
the polygons are used. If you know which municipalities the polygons
belong to and want P to describe them, use `scope = "muni"` with
`code_muni`, which is also faster.

## References

Pedreira Junior, J. U.; Louro, T. V.; Assis, L. B. M.; Brito, P. L.;
Bomfim, F. G. (2026). BGBI: A citywide-referenced and bidirectional land
use mix index for planning and policy evaluation. *Land Use Policy*,
169, 108135. https://doi.org/10.1016/j.landusepol.2026.108135

Pedreira Junior, J. U.; Louro, T. V.; Assis, L. B. M.; Brito, P. L.
(2025). Measuring land use mix with address-level census data. *engrXiv*
preprint. https://engrxiv.org/preprint/view/5975 (where the adapted HHI
(`hhi_adp`) is documented)

Massey, D. S. (2001). The prodigal paradigm returns: ecology comes back
to sociology. In A. Booth & A. C. Crouter (Eds.), *Does It Take a
Village? Community Effects on Children, Adolescents, and Families*.
Lawrence Erlbaum.

Song, Y.; Merlin, L.; Rodriguez, D. (2013). Comparing measures of urban
land use mix. *Computers, Environment and Urban Systems*, 42, 1–13.
https://doi.org/10.1016/j.compenvurbsys.2013.08.001

## Examples

``` r
# \donttest{
# Compute land-use mix indices on H3 hexagons
lumi <- compute_lumi(code_muni = 2929057, cache = FALSE)
#> ℹ Processing municipality code 2929057...
#> ℹ Step 1/3: Building the H3 grid and ensuring the CNEFE data...
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2929057_SAO_FELIX_DO_CORIBE.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [39ms]
#> 
#> ℹ Step 1/3: Building the H3 grid and ensuring the CNEFE data...

#> ✔ Step 1/3 (Grid and CNEFE data ready) [3.9s]
#> 
#> ℹ Step 2/3: Counting addresses per H3 cell...
#> ✔ Step 2/3 (Addresses counted) [138ms]
#> 
#> ℹ Step 3/3: Computing LUMI on the grid...
#> ✔ Step 3/3 (Land use mix indices computed) [93ms]
#> 

# Several municipalities in one call: one grid over their union and one P for
# the whole region (Salvador and its neighbor Lauro de Freitas)
lumi_region <- compute_lumi(code_muni = c(2927408, 2919207), cache = FALSE)
#> ℹ Processing municipality code 2927408 and 2919207...
#> ℹ Step 1/3: Building the H3 grid and ensuring the CNEFE data...
#> ℹ Municipality 1 of 2: 2927408
#> ℹ Step 1/3: Building the H3 grid and ensuring the CNEFE data...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2927408_SALVADOR.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [6.5s]
#> 
#> ℹ Step 1/3: Building the H3 grid and ensuring the CNEFE data...

#> ℹ Municipality 2 of 2: 2919207
#> ℹ Step 1/3: Building the H3 grid and ensuring the CNEFE data...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [437ms]
#> 
#> ℹ Step 1/3: Building the H3 grid and ensuring the CNEFE data...

#> ✔ Step 1/3 (Grid and CNEFE data ready) [23s]
#> 
#> ℹ Step 2/3: Counting addresses per H3 cell...
#> ✔ Step 2/3 (Addresses counted) [1.9s]
#> 
#> ℹ Step 3/3: Computing LUMI on the grid...
#> ✔ Step 3/3 (Land use mix indices computed) [118ms]
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
  polygon = nei_ldf,
  cache = FALSE
)
#> ℹ Processing municipality code 2919207...
#> ℹ Step 1/3: Ensuring data and preparing polygon...
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [450ms]
#> 
#> ℹ Step 1/3: Ensuring data and preparing polygon...

#> ✔ Step 1/3 (Data and polygon ready) [1.6s]
#> 
#> ℹ Step 2/3: Counting addresses per polygon...
#> ✔ Step 2/3 (Addresses counted) [894ms]
#> 
#> ℹ Step 3/3: Computing land use mix indices...
#> Warning: Polygon coverage: "99.7%" of the CNEFE points of the municipalities in
#> `code_muni` were captured.
#> ! Overlapping polygons: 91 of the 106975 CNEFE points inside the polygons
#>   (0.085%) fall in more than one polygon, so each enters the indices of every
#>   polygon that contains it.
#> ℹ 106975 of 107244 points are within the provided polygon.
#> ℹ 269 points from the municipalities in `code_muni` fell outside the polygons
#>   and were not counted. Possible causes are polygons that don't cover the whole
#>   municipality, a coastline the polygons don't follow, or geocoding errors.
#> ℹ Under `scope = "muni"`, P covers every address of these municipalities, these
#>   points included. On the other hand, `scope = "geom"` computes P from the
#>   addresses inside the polygons only, and also adds to the polygons the
#>   addresses of neighboring municipalities that fall inside them.
#> ✔ Step 3/3 (Land use mix indices computed) [60ms]
#> 

# With scope = "geom", the municipalities the polygons intersect are read, and
# P comes from the addresses inside the neighborhoods
lumi_poly_geom <- compute_lumi(polygon = nei_ldf, scope = "geom", cache = FALSE)
#> ℹ The polygons intersect 4 municipalities:
#>   Camaçari (2905701), Lauro de Freitas (2919207), Salvador (2927408), Simões
#>   Filho (2930709)
#> ℹ Use `scope = "muni"` with `code_muni` to read only the municipalities you
#>   choose.
#> ℹ Processing municipality code 2905701, 2919207, 2927408, and 2930709...
#> ℹ Step 1/3: Ensuring data and preparing polygon...
#> ℹ Municipality 1 of 4: 2905701
#> ℹ Step 1/3: Ensuring data and preparing polygon...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2905701_CAMACARI.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [932ms]
#> 
#> ℹ Step 1/3: Ensuring data and preparing polygon...

#> ℹ Municipality 2 of 4: 2919207
#> ℹ Step 1/3: Ensuring data and preparing polygon...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [463ms]
#> 
#> ℹ Step 1/3: Ensuring data and preparing polygon...

#> ℹ Municipality 3 of 4: 2927408
#> ℹ Step 1/3: Ensuring data and preparing polygon...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2927408_SALVADOR.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [6.7s]
#> 
#> ℹ Step 1/3: Ensuring data and preparing polygon...

#> ℹ Municipality 4 of 4: 2930709
#> ℹ Step 1/3: Ensuring data and preparing polygon...

#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2930709_SIMOES_FILHO.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [250ms]
#> 
#> ℹ Step 1/3: Ensuring data and preparing polygon...

#> ✔ Step 1/3 (Data and polygon ready) [23.2s]
#> 
#> ℹ Step 2/3: Counting addresses per polygon...
#> ✔ Step 2/3 (Addresses counted) [3.8s]
#> 
#> ℹ Step 3/3: Computing land use mix indices...
#> Warning: Overlapping polygons: 91 of the 107054 CNEFE points inside the polygons
#> (0.085%) fall in more than one polygon, so each enters the indices of every
#> polygon that contains it.
#> ✔ Step 3/3 (Land use mix indices computed) [36ms]
#> 

# BGBI against a target residential share of 70% instead of the observed one
lumi_target <- compute_lumi(code_muni = 2919207, p_target = 0.7, cache = FALSE)
#> ℹ Processing municipality code 2919207...
#> ℹ Step 1/3: Building the H3 grid and ensuring the CNEFE data...
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [443ms]
#> 
#> ℹ Step 1/3: Building the H3 grid and ensuring the CNEFE data...

#> ✔ Step 1/3 (Grid and CNEFE data ready) [10.4s]
#> 
#> ℹ Step 2/3: Counting addresses per H3 cell...
#> ✔ Step 2/3 (Addresses counted) [458ms]
#> 
#> ℹ Step 3/3: Computing LUMI on the grid...
#> ✔ Step 3/3 (Land use mix indices computed) [53ms]
#> 
# }
```
