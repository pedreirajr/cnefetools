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

0 errors | 0 warnings | 0 notes

## Test environments

- Local: Windows 11 x64, R 4.3.2
- GitHub Actions: ubuntu-24.04 (R release, R oldrel-1)
- GitHub Actions: windows-server-2022 (R release)
- GitHub Actions: macOS 15 Intel (R release)
