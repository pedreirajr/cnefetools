# cnefetools (development version)

## Breaking changes

* cnefetools now requires **R (>= 4.4.0)**, raised from 4.1.0. duckspatial
  (>= 1.1.0) calls the null-coalescing operator `%||%` without importing it, so
  it resolves only against the base R version added in 4.4.0. On older R the
  failure surfaced from geobr as a misleading "a file must have been corrupted
  during download" message. Declaring the requirement turns a confusing runtime
  error into a clear message at install time (#78).

* cnefetools now requires **geobr (>= 2.0.0)**. The data server behind geobr 1.x
  no longer responds: `read_municipality()` on geobr 1.9.1 returns `NULL` for
  every year tested (2020, 2022 and 2024), while geobr 2.0.1 serves them
  normally. Since the failure is a silent `NULL` rather than an error, leaving
  the dependency unpinned meant users on geobr 1.x hit an obscure downstream
  error instead of a clear one (#87). This supersedes the note under #74 below,
  which stated that any geobr version works.

* The H3 grid is now built from the municipality boundary of the **CNEFE data
  year** instead of a hardcoded 2024 boundary, affecting `cnefe_counts()`,
  `compute_lumi()` and `tracts_to_h3()` in `polygon_type = "hex"` mode. IBGE
  geocoded the CNEFE records against the territorial base of their own year, and
  the census tracts of that year nest into the matching municipal mesh, so the
  grid now shares the reference frame of the data aggregated onto it. In
  principle the set of border hexagons can differ from previous releases for
  municipalities whose limits changed between the two bases, though at
  resolution 9 the grids come out identical for Lauro de Freitas-BA (550 cells)
  and Fortaleza-CE (2651 cells), the municipalities used in the package
  articles (#81).

## Bug fixes

* `tracts_to_h3()` and `tracts_to_polygon()` no longer fail with
  `GitHub API error (401): Bad credentials` when an expired or invalid GitHub
  token is present in the environment. The census tract assets live in public
  GitHub releases and need no credential, but `gh` sends whatever token it finds
  in `GITHUB_PAT`, `GITHUB_TOKEN`, `GH_TOKEN` or the git credential store, and
  GitHub then rejects the request instead of serving it anonymously. The
  download now retries once without a token after an authentication failure.
  A valid token is still tried first, so authenticated rate limits are
  preserved, and if the anonymous retry also fails the error names the broken
  credential as a likely cause and points at `gitcreds::gitcreds_delete()`
  (#79).

* `cnefe_counts()`, `compute_lumi()`, and `tracts_to_polygon()` no longer fail
  in the DuckDB backend when the user-supplied `sf` polygon has a geometry
  column not named `"geom"` (e.g. the sf default `"geometry"`). The geometry
  column is now normalized before writing to DuckDB, restoring compatibility
  with duckspatial (>= 1.1.0) (#70).

* cnefetools now works with geobr 2.0.0. geobr 2.0.0 reads boundaries lazily
  through duckspatial, which under a fixed RNG seed (e.g. R CMD check examples)
  could trigger a DuckDB temporary-table name collision
  (`Table dbplyr_<...> already exists`). The RNG state is now isolated around
  the geobr call in the H3-grid path (#74). Note that geobr is nonetheless
  pinned to (>= 2.0.0), for the unrelated reason given above.

# cnefetools 0.2.5

* Fixed an RTREE spatial-index failure introduced by DuckDB 1.5 (which moved
  the `GEOMETRY` type into core, with optional CRS parameters) combined with
  duckspatial 1.0.0 (which writes CRS-parameterised geometry columns). A WKB
  round-trip now strips the CRS parameter to plain `GEOMETRY` before the RTREE
  index is created, in `cnefe_counts()`, `compute_lumi()`, and
  `tracts_to_polygon()` (#68).

* Fixed a temporary ZIP file being deleted before its DuckDB view was
  materialised when `cache = FALSE`, which caused `tracts_to_h3()` and
  `tracts_to_polygon()` to error at the CNEFE point preparation step (#68).

# cnefetools 0.2.4

* Added `cache = FALSE` to `\donttest` examples so they no longer write to the
  user cache directory, resolving a CRAN check NOTE (#66).

# cnefetools 0.2.3

* Fixed missing hexagons at the edges of the H3 grid. `h3jsr::polygon_to_cells()`
  only returns hexagons whose centroid falls inside the municipality boundary, so
  border hexagons that overlap the boundary without their center being inside
  were silently excluded. `build_h3_grid()` now adds those hexagons by checking
  the immediate neighbors of the grid against the municipality boundary (#62).

* `cnefe_counts()` and `compute_lumi()` now expose a `cache` parameter
  (default `TRUE`), consistent with `tracts_to_h3()` and `tracts_to_polygon()`
  (#58).

* New `clear_cache_muni()` function to delete cached CNEFE ZIP files from
  the user cache directory, with optional filtering by municipality code (#59).

* New `clear_cache_tracts()` function to delete cached census tract Parquet
  files, with optional filtering by state (UF) code (#59).

# cnefetools 0.2.2

* Resubmission to CRAN following package removal on 2026-02-26.
* No functional changes. Retains the `skip_on_cran()` fix from v0.2.1 for the
  DuckDB spatial extension segfault on r-devel-linux-x86_64-fedora-clang.

# cnefetools 0.2.1

* Adds `skip_on_cran()` to `test-tracts_to_h3.R` to prevent a segfault on
  r-devel-linux-x86_64-fedora-clang caused by an ABI mismatch between the
  clang-compiled DuckDB binary and GCC-built spatial extension (duckdb/duckdb-r#1107).

# cnefetools 0.2.0

## New functions

- New `tracts_to_polygon()` for dasymetric interpolation from census
  tracts to user-supplied polygons, using CNEFE dwelling points as
  ancillary data.
- New `tracts_variables_ref` reference table mapping cnefetools variable
  names to official IBGE census tract codes.

## Major changes

- `hex_cnefe_counts()` has been renamed to `cnefe_counts()`. The
  function now accepts user-supplied polygons via `polygon_type = "user"`
  in addition to H3 hexagons (`polygon_type = "hex"`, default).
- `compute_lumi()` gains support for user-supplied polygons via the same
  `polygon_type` parameter, and a new land-use mix indicator: ICE (Index
  of Concentration at Extremes).
- `tracts_to_h3()` and `tracts_to_polygon()` gain five new interpolation
  variables related to race: `race_branca`, `race_preta`, `race_parda`,
  `race_amarela` and `race_indigena`.
- `read_cnefe()`, `cnefe_counts()`, `compute_lumi()`, and
  `tracts_to_h3()` gain a `year` argument (default `2022`) to prepare
  for future CNEFE editions.

## Improvements

- All user-facing functions now use the cli package (>= 3.6.0) for
  formatted console messages, replacing plain `message()` calls.
- Improved diagnostic output in `tracts_to_h3()` and
  `tracts_to_polygon()` with two-stage reporting structure.
- Census tract Parquet assets are now downloaded via piggyback from
  GitHub Releases, with on-demand caching.

## Bug fixes

- Removed inconsistencies in spatial join operations in `cnefe_counts()`,
  `compute_lumi()`, and `tracts_to_polygon()` with DuckDB.
- Fixed community extension loading failures in DuckDB.
- Fixed temporary file path bugs in `tracts_to_h3()` and
  `tracts_to_polygon()`.
- Fixed invalid geometry handling when user-supplied polygons.

## Documentation

- Six pre-rendered pkgdown articles: reading CNEFE data, address counts,
  land-use mix indices, dasymetric interpolation, FAQ, and a DuckDB
  performance benchmark.
- The package now features a new, more polished logo with improved visual quality.

# cnefetools 0.1.1

- Adds Balance Index (BAL), `bal` to the `compute_lumi()` function.

# cnefetools 0.1.0

- New: `tracts_to_h3()` for dasymetric interpolation from tract aggregates to CNEFE dwelling points and H3 hexagons using DuckDB (spatial + h3).
- Added UF-level census tract Parquet assets (WKB geometry) hosted on GitHub Releases and downloaded on-demand with caching.

# cnefetools 0.0.6

- Moves DBI and duckdb to Imports so the DuckDB backend works out of the box
- Removes silent fallback to `backend = "r"` when `backend = "duckdb"` is requested
- No changes to outputs; behavior is unchanged aside from dependency handling

# cnefetools 0.0.5

-   Makes unit tests more reproducible and CI-friendly by removing dependencies on network access and local cache state (offline ZIP fixture and mocked downloads)
-   Improves robustness of `read_cnefe(output = "sf")` by handling missing coordinates before converting to `sf`
-   Updates `COD_ESPECIE` documentation and removes tidyselect deprecation warnings in tests and internals

# cnefetools 0.0.4

-   Major speed-up for H3 assignment and hex-level aggregation via DuckDB + H3 extension (SQL), with runtimes dropping from minutes to seconds for large municipalities
-   Adds a configurable backend with backwards compatibility: `backend = "duckdb"` (default) or `backend = "r"` for `hex_cnefe_counts()` and `compute_lumi()`
-   Refactors internals to reuse cached ZIPs and consolidate common helpers, reducing repeated overhead and improving maintainability

# cnefetools 0.0.3

-   More robust downloads with retry logic and increased timeout when needed
-   Avoids leaving partial or corrupted ZIP files in the cache
-   Automatically detects corrupted cached ZIPs and re-downloads before extraction

# cnefetools 0.0.2

-   Add `compute_lumi()` to compute land-use mix indicators (EI, HHI, adapted HHI, BGBI) on H3 grids.
-   Extend README with examples for Fortaleza (code 2304400) and BGBI maps.

# cnefetools 0.0.1

# cnefetools 0.0.0.9000

-   Initial development version. Basic package infrastructure set up.
