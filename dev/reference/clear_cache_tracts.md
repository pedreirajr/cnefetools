# Delete cached census tract Parquet files

`clear_cache_tracts()` removes census tract Parquet files stored in the
user cache directory by
[`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/dev/reference/tracts_to_h3.md)
and
[`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/dev/reference/tracts_to_polygon.md).

## Usage

``` r
clear_cache_tracts(uf = "all", verbose = TRUE, cache_dir = NULL, year = NULL)
```

## Arguments

- uf:

  `"all"`, a two-letter UF abbreviation (e.g. `"BA"`), a two-digit
  numeric state code (e.g. `29L`), or a seven-digit IBGE municipality
  code (e.g. `2919207`). If `"all"` (default), all cached Parquet files
  are deleted. Otherwise, only the file for the resolved state is
  deleted.

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
# Delete all cached census tract Parquets
clear_cache_tracts()
#> ℹ Cache directory does not exist: /home/runner/.cache/R/cnefetools

# Delete only the Parquet for Bahia (several equivalent calls)
clear_cache_tracts("BA")
#> ℹ Cache directory does not exist: /home/runner/.cache/R/cnefetools
clear_cache_tracts(29)
#> ℹ Cache directory does not exist: /home/runner/.cache/R/cnefetools
clear_cache_tracts(2919207)  # municipality code → state resolved automatically
#> ℹ Cache directory does not exist: /home/runner/.cache/R/cnefetools
# }
```
