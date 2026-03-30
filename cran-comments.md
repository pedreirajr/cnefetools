## Resubmission (v0.2.4)

The package was archived on 2026-03-30 due to a NOTE flagged by the donttest
checker: `\donttest{}` examples in five functions called package functions with
`cache = TRUE` (the default), causing ZIP and Parquet files to be written to
`~/.cache/R/cnefetools` during the check run.

### Fix

Added `cache = FALSE` to all function calls inside `\donttest{}` blocks in:
`read_cnefe()`, `cnefe_counts()`, `compute_lumi()`, `tracts_to_h3()`, and
`tracts_to_polygon()`. Examples now use temporary files and leave no persistent
files on the check machine.

## R CMD check results

0 errors | 0 warnings | 2 notes

* `NOTE: unable to verify current time` — transient CRAN infrastructure
  issue (check server cannot reach time-verification servers); unrelated
  to the package.
* `NOTE: Non-standard file/directory found at top level: 'paper'` — the
  `paper/` directory (R Journal manuscript) lives on disk during local
  checks because it is maintained on a separate git branch. It is listed
  in `.Rbuildignore` and is not included in the built tarball.

## Test environments

- Local: Windows 11 x64, R 4.3.2
- GitHub Actions: ubuntu-24.04 (R release, R oldrel-1)
- GitHub Actions: windows-server-2022 (R release)
- GitHub Actions: macOS 15 Intel (R release)
