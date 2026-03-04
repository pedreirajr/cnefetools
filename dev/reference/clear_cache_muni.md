# Delete cached CNEFE ZIP files

`clear_cache_muni()` removes CNEFE ZIP files stored in the user cache
directory by
[`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_counts.md),
[`compute_lumi()`](https://pedreirajr.github.io/cnefetools/dev/reference/compute_lumi.md),
[`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/dev/reference/tracts_to_h3.md),
and related functions.

## Usage

``` r
clear_cache_muni(code_muni = "all", verbose = TRUE)
```

## Arguments

- code_muni:

  Integer or `"all"`. If `"all"` (default), all cached CNEFE ZIP files
  are deleted. If a seven-digit IBGE municipality code is provided, only
  the ZIP file for that municipality is deleted.

- verbose:

  Logical; if `TRUE` (default), reports the number of files deleted and
  the space freed.

## Value

Invisibly, the character vector of deleted file paths.

## Examples

``` r
if (FALSE) { # \dontrun{
# Delete all cached CNEFE ZIPs
clear_cache_muni()

# Delete only the ZIP for Lauro de Freitas-BA
clear_cache_muni(2919207)
} # }
```
