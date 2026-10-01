## Submission (v0.3.0)

This is a feature release. The full list of changes is in NEWS.md. The
changes that affect installation are:

* The package now requires R (>= 4.4.0). duckspatial (>= 1.1.0) calls the
  `%||%` operator without importing it, so it only resolves against the base R
  version added in 4.4.0.
* The package now requires geobr (>= 2.0.0), because the data server behind
  geobr 1.x no longer responds.
* The package now requires duckspatial (>= 1.0.0), the first version with
  `ddbs_write_table()`, which replaces the deprecated `ddbs_write_vector()`.

Other changes users will notice:

* The download cache now stores a gzipped CSV instead of the ZIP published by
  IBGE, and it is now split by CNEFE edition. It still lives in
  `tools::R_user_dir("cnefetools", "cache")`. Caches from earlier versions are
  not read, so each municipality is downloaded again once, and
  `clear_cache_muni()` removes the old files. The new `cnefe_export()` writes
  only to a path the user provides.
* The `polygon_type` argument of `cnefe_counts()` and `compute_lumi()` is
  deprecated, since the aggregation mode is now inferred from `polygon`. Code
  that still passes it keeps working and gets a deprecation warning.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Local: Windows 11 x64, R 4.6.0, `devtools::check(remote = TRUE, manual = TRUE)`
* GitHub Actions: macos-latest (R release)
* GitHub Actions: windows-latest (R release)
* GitHub Actions: ubuntu-latest (R release and R oldrel-1)

## Reverse dependencies

There are no reverse dependencies on CRAN.
