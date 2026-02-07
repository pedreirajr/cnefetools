## Resubmission

This is a resubmission addressing feedback from the CRAN reviewer:

- Added IBGE FTP URL to the DESCRIPTION field.
- Replaced `\dontrun{}` with `\donttest{}` in examples that download data,
  and `@examplesIf interactive()` for viewer-only functions.
- Cache directories are no longer created at package load time; directory
  creation is deferred to the point a file is actually downloaded.

Additional fixes included in this version:

- Fixed DuckDB spatial extension installation (core extension was incorrectly
  fetched from the community repository).
- Fixed allocation of `n_resp` and `avg_inc_resp` to private dwellings only.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

- local: Windows 11 x64, R 4.3.2
- GitHub Actions: Ubuntu 24.04 (R release, R oldrel-1)
- GitHub Actions: Windows Server 2022 (R release)
- GitHub Actions: macOS 15 Intel (R release)
