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

  Character. `"duckdb"` (default) uses DuckDB with H3/spatial
  extensions. `"r"` uses h3jsr and sf in R (slower but no DuckDB
  dependency).

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

## Examples

``` r
# \donttest{
# Count addresses per H3 hexagon (resolution 9)
hex_counts <- cnefe_counts(code_muni = 2929057)
#> ℹ Step 1/3: Ensuring ZIP and inspecting archive...
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2929057_SAO_FELIX_DO_CORIBE.zip
#> ✔ Step 1/3 (CNEFE ZIP ready) [1.3s]
#> 
#> ℹ Step 2/3: Building full H3 grid over municipality boundary...
#> ✔ Step 2/3 (H3 grid built) [5.8s]
#> 
#> ℹ Step 3/3: Counting address species per hexagon...
#> ✔ Step 3/3 (Addresses counted) [805ms]
#> 

# Count addresses per user-provided polygon (neighborhoods of Lauro de Freitas-BA)
# Using geobr to download neighborhood boundaries
library(geobr)
nei_ldf <- subset(
  read_neighborhood(year = 2022),
  code_muni == 2919207
)
#> Using year/date 2022
hex_counts <- cnefe_counts(
  code_muni = 2919207,
  polygon_type = "user",
  polygon = nei_ldf
)
#> ℹ Step 1/2: Ensuring data and preparing polygon...
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2919207_LAURO_DE_FREITAS.zip
#> ✔ Step 1/2 (Data and polygon ready) [1.3s]
#> 
#> ℹ Step 2/2: Counting addresses per polygon...
#> Warning: Polygon coverage: "99.7%" of CNEFE points captured.
#> ℹ 111103 of 111385 points are within the provided polygon.
#> ℹ 282 points fell outside the polygon and were not counted.
#> ✔ Step 2/2 (Addresses counted) [1.2s]
#> 
# }
```
