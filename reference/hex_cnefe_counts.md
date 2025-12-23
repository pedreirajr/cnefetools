# Count CNEFE address species on an H3 grid

`hex_cnefe_counts()` reads CNEFE records for a given municipality,
assigns each address point to an H3 cell, builds the full H3 grid over
the municipal boundary (year fixed at 2024), and returns per-hexagon
counts of `COD_ESPECIE` as `addr_type1` to `addr_type8`.

## Usage

``` r
hex_cnefe_counts(
  code_muni,
  h3_resolution = 9,
  verbose = TRUE,
  backend = c("duckdb", "r")
)
```

## Arguments

- code_muni:

  Integer. Seven-digit IBGE municipality code.

- h3_resolution:

  Integer. H3 grid resolution (default: 9).

- verbose:

  Logical; if `TRUE`, prints messages and timing information.

- backend:

  Character. `"duckdb"` (default) uses DuckDB H3 extension to aggregate
  directly from the cached ZIP. `"r"` uses h3jsr in R.

## Value

An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) object
with CRS 4326 containing:

- `id_hex`: H3 cell identifier

- `addr_type1` ... `addr_type8`: counts per type

- `geometry`: hexagon geometry

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
