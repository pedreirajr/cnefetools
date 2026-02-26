## Resubmission (v0.2.2)

This is a resubmission following the package's removal from CRAN on 2026-02-26.

The package was removed because two errors remained in the check results at the
time of the deadline. Both issues are now resolved:

### 1. Segfault on r-devel-linux-x86_64-fedora-clang (fixed in v0.2.1, retained here)

The segfault was caused by loading the DuckDB `spatial` extension via
`LOAD spatial;` (called internally by `duckspatial::ddbs_load()`). On
r-devel-linux-x86_64-fedora-clang, this triggers a C-level SIGSEGV (signal 11)
emitted by the kernel, which terminates the R process entirely and cannot be
caught by R's `tryCatch()`. The root cause is an ABI mismatch between the DuckDB
R package binary (compiled with clang on Fedora) and the spatial extension binary
(compiled with GCC). This is a confirmed upstream bug (duckdb/duckdb-r#1107) that
cannot be resolved within this package.

The `spatial` extension is required for geometric operations (e.g., ST_Within)
in the DuckDB backend of `tracts_to_h3()`. Since the crash is a kernel-level
signal and is not catchable by R code, the only viable mitigation is to prevent
the test from running on CRAN. This was done in v0.2.1 by adding `skip_on_cran()`
as the first call in `test-tracts_to_h3.R`. This fix is retained in v0.2.2.

The upstream issue (duckdb/duckdb-r#1107) remains open and unresolved as of
this writing — a duckdb-r collaborator confirmed the segfault still occurs with
DuckDB v1.3.2. The duckdb-r team has stated they are uncertain how to address
this at the R level. No fix is expected in the short term.

Other functions that also use the spatial extension (`tracts_to_polygon()`,
`cnefe_counts()` with user polygons, `compute_lumi()` with user polygons) are not
affected in CRAN checks because their tests either use the pure-R backend or only
validate function arguments without invoking DuckDB. The package passes
r-devel-linux-x86_64-fedora-clang checks successfully with this configuration.

### 2. Missing `arrow` on r-oldrel-macos-x86_64 (transient, now resolved)

The `arrow` binary was temporarily unavailable on r-oldrel-macos-x86_64 during
the v0.2.1 check window, causing an installation failure. This was a transient
infrastructure issue — `arrow` is currently available on that platform (showing
only a size NOTE of 109.9 Mb, not an ERROR). No code changes were needed.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

- Local: Windows 11 x64, R 4.3.2
- GitHub Actions: ubuntu-24.04 (R release, R oldrel-1)
- GitHub Actions: windows-server-2022 (R release)
- GitHub Actions: macOS 15 Intel (R release)
