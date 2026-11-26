# Delete cached municipality boundaries

`clear_cache_boundaries()` removes the municipality boundaries stored in
the user cache directory. The functions that build an H3 grid from
`code_muni`, or that find the municipalities a layer reaches with
`scope = "geom"`, read the boundaries at full resolution from a file
published with the package and keep each municipality they read in the
cache, so later calls work offline. Each file is small (Salvador takes
about 0.06 MB, and the 39 municipalities of the São Paulo metropolitan
region about 1.6 MB), and the whole country would take about 250 MB.

## Usage

``` r
clear_cache_boundaries(
  code_muni = "all",
  verbose = TRUE,
  cache_dir = NULL,
  year = NULL
)
```

## Arguments

- code_muni:

  Integer or `"all"`. If `"all"` (default), every cached boundary is
  deleted. Otherwise, one or more seven-digit IBGE municipality codes
  whose boundaries are deleted.

- verbose:

  Logical; if `TRUE` (default), reports the number of files deleted and
  the space freed.

- cache_dir:

  Character. Directory to use for cached downloads. If `NULL` (default),
  the `CNEFETOOLS_CACHE_DIR` environment variable is used when it is
  set, otherwise
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) with
  `which = "cache"`. Use this to point large downloads at a secondary
  drive or a shared volume.

- year:

  Integer. Restrict the deletion to one CNEFE edition. `NULL` (default)
  clears every edition, which is the previous behavior.

## Value

Invisibly, the character vector of deleted file paths.

## Examples

``` r
# \donttest{
# Delete every cached boundary
clear_cache_boundaries()
#> ℹ Cache directory does not exist: /home/runner/.cache/R/cnefetools

# Delete only the boundary of Lauro de Freitas-BA
clear_cache_boundaries(2919207)
#> ℹ Cache directory does not exist: /home/runner/.cache/R/cnefetools
# }
```
