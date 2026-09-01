# Delete cached CNEFE data files

`clear_cache_muni()` removes CNEFE data files stored in the user cache
directory by
[`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_counts.md),
[`compute_lumi()`](https://pedreirajr.github.io/cnefetools/dev/reference/compute_lumi.md),
[`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/dev/reference/tracts_to_h3.md),
and related functions.

The cache holds gzipped CSVs (`.csv.gz`). Archives left by versions
before 0.3.0, which cached the ZIP as published by IBGE, are removed as
well.

## Usage

``` r
clear_cache_muni(
  code_muni = "all",
  verbose = TRUE,
  cache_dir = NULL,
  year = NULL
)
```

## Arguments

- code_muni:

  Integer or `"all"`. If `"all"` (default), every cached CNEFE file is
  deleted. If a seven-digit IBGE municipality code is provided, only the
  file for that municipality is deleted.

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
  clears every edition, which is the previous behaviour.

## Value

Invisibly, the character vector of deleted file paths.

## Examples

``` r
# \donttest{
# Delete every cached CNEFE file
clear_cache_muni()
#> ℹ Cache directory does not exist: /home/runner/.cache/R/cnefetools

# Delete only the file for Lauro de Freitas-BA
clear_cache_muni(2919207)
#> ℹ Cache directory does not exist: /home/runner/.cache/R/cnefetools
# }
```
