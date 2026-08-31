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

## Breaking changes

* `polygon_type` is deprecated in `cnefe_counts()` and `compute_lumi()`. The
  aggregation mode is now inferred from `polygon`: leave it `NULL` for an H3
  grid, or pass an `sf` object for user polygons. Existing code that passes
  `polygon_type` keeps working and emits a deprecation warning. Passing
  `polygon_type = "user"` with no `polygon` remains an error. The previous
  behaviour, where supplying a `polygon` printed three alert lines about
  switching mode automatically, is gone: the inference is now silent (#90).

## New features

* The pure-R backends of `cnefe_counts()` and `compute_lumi()` now push their
  `transmute()` and `filter()` calls down to the Arrow table and collect last,
  instead of calling `as.data.frame()` first. The old order materialised all 34
  columns as an R data frame before keeping three of them. Measured on
  Fortaleza (1.19M rows), peak memory falls from 108.4 MB to 68.3 MB, a 37%
  reduction, at the cost of about 0.2s (#88).

* The download cache now stores a **gzipped CSV** instead of the published ZIP.
  DuckDB decompresses gzip natively, so the community `zipfs` extension is no
  longer loaded on the normal read path, which Referee 1 flagged as a stability
  risk (reported macOS code-signing delays of 4 to 5 seconds per load). Our own
  measurement, in `data-raw/bench_gz_vs_zip.R`, reproduces the referee's
  figures: on Fortaleza a DuckDB read is 2.29x faster from `.csv.gz` than
  through `zipfs`, at the same size on disk (27.6 MB either way). Raw CSV would
  be 4.86x faster but needs 6.7x the disk, so it is not a sensible cache format.
  The conversion is done once, on first download, and is streamed in chunks so
  peak memory does not scale with the file. Caches written by earlier versions
  are still readable, through the `zipfs` route loaded on demand (#93).

* The download cache is now segregated by CNEFE edition, at
  `<cache>/<year>/`. The ZIP names IBGE publishes carry no year, so
  `2919207_LAURO_DE_FREITAS.zip` from a future census would be
  indistinguishable from the 2022 one, and a cached 2022 file could be served
  silently to someone who asked for another edition. `clear_cache_muni()` and
  `clear_cache_tracts()` gain a `year` argument, with `NULL` clearing every
  edition as before. Caches created by earlier versions are ignored and
  re-downloaded once (#81).

* Census tract assets now come from release `sc-assets-v3`, which is
  reproducible from `data-raw/sc_assets_build.R`. The data is unchanged:
  verified against the previous release across all 27 states, 468,097 tracts
  and 3,144,868 values, with every value matching exactly. `sc-assets-v2`
  remains published, so earlier versions of the package keep working (#80
  R1.9).

* New `cnefe_export()` writes a municipality to a persistent, optimised file at
  a location of your choosing, as Parquet (default), CSV or gzipped CSV. The
  package cache is transient by design, lives in a directory the package
  manages and holds the ZIP exactly as IBGE published it, which is the wrong
  shape for an analysis that must still run in a year (#93).

* `read_cnefe()` gains a `file` argument that reads a CNEFE file already on
  disk, skipping the download entirely. It accepts `.zip` as published by IBGE,
  `.csv`, `.csv.gz` and `.parquet`, so data obtained by any means can be read
  without the IBGE server being reachable. `code_muni` and `file` are mutually
  exclusive, and both paths share the same output code, so they return the same
  object for the same data (#93).

* Downloads now recover from an upstream layout change. If the URL held in the
  internal index returns 404, the package scans the published IBGE directory
  listing to reconstruct the correct URL and retries once. The scan reads
  directory pages, never data files: recovering any municipality costs roughly
  170 KB. It handles both a renamed file and a renamed UF directory, and it does
  not mutate the internal index, so a successful scan repairs one call rather
  than changing package state. If the scan finds nothing, the error says so and
  points at the issue tracker (#92).

* Downloads now run a pre-flight availability check that tells two failures
  apart. If the IBGE server cannot be reached, the message points at
  connectivity. If the server answers but the file is missing (HTTP 404), the
  message explains that the upstream directory layout has most likely changed,
  which is a package problem rather than a user problem, and points at the issue
  tracker. The check also aborts before the retry ladder, which previously spent
  300, 600 and 1800 seconds on a URL that could never resolve (#91).

* All functions that download or read cached data gain a `cache_dir` argument:
  `read_cnefe()`, `cnefe_counts()`, `compute_lumi()`, `tracts_to_h3()`,
  `tracts_to_polygon()`, `clear_cache_muni()` and `clear_cache_tracts()`. The
  cache location resolves from the argument first, then the
  `CNEFETOOLS_CACHE_DIR` environment variable, then
  `tools::R_user_dir("cnefetools", "cache")` as before. CNEFE ZIPs reach 901 MB
  uncompressed, and the location was previously fixed to the user's primary
  partition (#89).

## Bug fixes

* `compute_lumi()` no longer returns `NULL` when no hexagon survives
  filtering. Both backends now return a zero-row `sf` carrying the documented
  columns, so downstream code can pipe the result without a `NULL` check (#85).

* `read_cnefe(output = "sf")` now reports, under `verbose`, how many rows were
  dropped for missing coordinates. They were previously discarded silently,
  which hid data quality problems (#85).

* `compute_lumi()` now excludes `COD_ESPECIE == 7` (buildings under
  construction or renovation) in **all** code paths. The DuckDB backend for
  user-supplied polygons was missing that filter, so every index it returned
  (`p_res`, `ei`, `hhi`, `bal`, `ice`, `hhi_adp`, `bgbi`) was computed over a
  denominator that still contained type 7, contradicting both the documented
  behaviour and the other three code paths. Since `backend = "duckdb"` is the
  default, this affected most users of `polygon_type = "user"`. On Lauro de
  Freitas-BA, where type 7 is 3.7% of addresses, the error reached 0.13 in
  `p_res` and 0.32 in `bgbi`. The two backends now agree exactly (#96).

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
