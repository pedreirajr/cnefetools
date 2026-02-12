## Resubmission

This patch release addresses the ERRORs flagged on CRAN check platforms:

- **Segfault on r-devel-linux-x86_64-fedora-clang**: The `tracts_to_h3` test
  loaded the DuckDB spatial extension, which causes a C-level segfault
  (signal 11) on this platform due to a known upstream ABI mismatch between
  clang-compiled duckdb and GCC-built extension binaries
  (duckdb/duckdb-r#1107, still open). This affects all packages that load
  DuckDB extensions on Fedora-clang (e.g. duckspatial shows the same ERROR).
  Since the crash cannot be caught by `tryCatch()`, the test now uses
  `skip_on_cran()`.

- **Missing `arrow` on r-oldrel-macos-x86_64**: This appears to be a
  transient issue — `arrow` currently passes on that platform and other
  packages with `arrow` in Imports (e.g. censobr) show OK. No changes were
  made to the `arrow` dependency; resubmission should resolve this.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

- local: Windows 11 x64, R 4.3.2
- GitHub Actions: Ubuntu 24.04 (R release, R oldrel-1)
- GitHub Actions: Windows Server 2022 (R release)
- GitHub Actions: macOS 15 Intel (R release)
