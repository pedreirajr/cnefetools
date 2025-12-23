# Compute land-use mix indicators on an H3 grid

`compute_lumi()` reads CNEFE 2022 records for a given municipality,
assigns each address point to an H3 cell, and computes land-use mix
indices per hexagon (EI, HHI, adapted HHI, and BGBI), following the
methodology proposed in Pedreira Jr. et al. (2025).

## Usage

``` r
compute_lumi(
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

  Character. `"duckdb"` (default) uses DuckDB + H3 extension reading
  directly from the cached ZIP. `"r"` computes H3 in R using h3jsr.

## Value

An [`sf::sf`](https://r-spatial.github.io/sf/reference/sf.html) object
with CRS 4326 containing:

- `id_hex`: H3 cell identifier

- `p_res`: share of residential addresses in the hexagon

- `ei`,`bal`, `hhi`, `hhi_adp`, `bgbi`: land-use mix indicators

- `geometry`: hexagon geometry

## References

Pedreira Jr., J. U.; Louro, T. V.; Assis, L. B. M.; Brito, P. L.
Measuring land use mix with address-level census data (2025). *engrXiv*.
https://engrxiv.org/preprint/view/5975
