# Build an H3 grid as an sf object

Internal helper to build an H3 grid either:

- from a vector of H3 cell ids (`id_hex`), or

- from the municipality boundary (`code_muni`) using geobr (year fixed
  at 2024).

## Usage

``` r
build_h3_grid(h3_resolution, id_hex = NULL, code_muni = NULL, boundary = NULL)
```

## Arguments

- h3_resolution:

  Integer. H3 resolution.

- id_hex:

  Character/integer vector of H3 cell ids (optional).

- code_muni:

  Integer. Seven-digit IBGE municipality code (optional).

- boundary:

  An sf polygon for the area of interest (optional).

## Value

An sf object (CRS 4326) with columns `id_hex` and `geometry`.
