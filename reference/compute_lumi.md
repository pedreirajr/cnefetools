# Compute land-use mix indicators on a spatial grid

`compute_lumi()` reads CNEFE records for a given municipality, assigns
each address point to spatial units (either H3 hexagonal cells or
user-provided polygons), and computes the residential proportion
(`p_res`) and land-use mix indices, such as the Entropy Index (`ei`),
the Herfindahl-Hirschman Index (`hhi`), the Balance Index (`bal`), the
Index of Concentration at Extremes (`ice`), the adapted HHI (`hhi_adp`),
and the Bidirectional Global-centered Index (`bgbi`), following the
methodology proposed in Pedreira Jr. et al. (2025).

## Usage

``` r
compute_lumi(
  code_muni,
  year = 2022,
  polygon_type = c("hex", "user"),
  polygon = NULL,
  crs_output = NULL,
  h3_resolution = 9,
  verbose = TRUE,
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

  Character. Type of polygon aggregation: `"hex"` (default) uses an H3
  hexagonal grid; `"user"` uses polygons provided via the `polygon`
  parameter.

- polygon:

  An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) object
  with polygon geometries. Required when `polygon_type = "user"`. A
  warning is issued reporting the percentage of CNEFE points covered by
  the polygon area. If no CNEFE points fall within the polygon, an error
  is raised.

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

- backend:

  Character. `"duckdb"` (default) uses DuckDB + H3 extension reading
  directly from the cached ZIP. `"r"` computes H3 in R using h3jsr.

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

## References

Pedreira Jr., J. U.; Louro, T. V.; Assis, L. B. M.; Brito, P. L.
Measuring land use mix with address-level census data (2025). *engrXiv*.
https://engrxiv.org/preprint/view/5975

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
lumi <- compute_lumi(code_muni = 2929057)
#> ℹ Processing municipality code 2929057...
#> ℹ Step 1/3: Ensuring ZIP and inspecting archive...
#> ℹ Using cached file: /home/runner/.cache/R/cnefetools/2929057_SAO_FELIX_DO_CORIBE.zip
#> ℹ Step 1/3: Ensuring ZIP and inspecting archive...
#> ✔ Step 1/3 (CNEFE ZIP ready) [10ms]
#> 
#> ℹ Step 2/3: Counting addresses per H3 cell...
#> ✔ Step 2/3 (Addresses counted) [221ms]
#> 
#> ℹ Step 3/3: Building grid and computing LUMI...
#> ✔ Step 3/3 (Land use mix indices computed) [154ms]
#> 

# Compute land-use mix indices on user-provided polygons (neighborhoods of Lauro de Freitas-BA)
# Using geobr to download neighborhood boundaries
library(geobr)
nei_ldf <- subset(
  read_neighborhood(year = 2022),
  code_muni == 2919207
)
#> Using year/date 2022
lumi_poly <- compute_lumi(
  code_muni = 2919207,
  polygon_type = "user",
  polygon = nei_ldf
)
#> ℹ Processing municipality code 2919207...
#> ℹ Step 1/3: Ensuring data and preparing polygon...
#> ℹ Using cached file: /home/runner/.cache/R/cnefetools/2919207_LAURO_DE_FREITAS.zip
#> ℹ Step 1/3: Ensuring data and preparing polygon...
#> ✔ Step 1/3 (Data and polygon ready) [22ms]
#> 
#> ℹ Step 2/3: Counting addresses per polygon...
#> ℹ Table <user_polygons> dropped
#> ℹ Step 2/3: Counting addresses per polygon...
#> ✔ Table user_polygons successfully imported
#> ℹ Step 2/3: Counting addresses per polygon...
#> ✔ Step 2/3 (Addresses counted) [1.1s]
#> 
#> ℹ Step 3/3: Computing land use mix indices...
#> Warning: Polygon coverage: "99.7%" of CNEFE points captured.
#> ℹ 111103 of 111385 points are within the provided polygon.
#> ℹ 282 points fell outside the polygon and were not counted.
#> ✔ Step 3/3 (Land use mix indices computed) [50ms]
#> 
# }
```
