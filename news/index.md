# Changelog

## cnefetools 0.2.0

### New functions

- New
  [`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_polygon.md)
  for dasymetric interpolation from census tracts to user-supplied
  polygons, using CNEFE dwelling points as ancillary data.
- New `tracts_variables_ref` reference table mapping cnefetools variable
  names to official IBGE census tract codes.

### Major changes

- `hex_cnefe_counts()` has been renamed to
  [`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/reference/cnefe_counts.md).
  The function now accepts user-supplied polygons via
  `polygon_type = "user"` in addition to H3 hexagons
  (`polygon_type = "hex"`, default).
- [`compute_lumi()`](https://pedreirajr.github.io/cnefetools/reference/compute_lumi.md)
  gains support for user-supplied polygons via the same `polygon_type`
  parameter, and a new land-use mix indicator: ICE (Index of
  Concentration at Extremes).
- [`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_h3.md)
  and
  [`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_polygon.md)
  gain five new interpolation variables related to race: `race_branca`,
  `race_preta`, `race_parda`, `race_amarela` and `race_indigena`.
- [`read_cnefe()`](https://pedreirajr.github.io/cnefetools/reference/read_cnefe.md),
  [`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/reference/cnefe_counts.md),
  [`compute_lumi()`](https://pedreirajr.github.io/cnefetools/reference/compute_lumi.md),
  and
  [`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_h3.md)
  gain a `year` argument (default `2022`) to prepare for future CNEFE
  editions.

### Improvements

- All user-facing functions now use the cli package (\>= 3.6.0) for
  formatted console messages, replacing plain
  [`message()`](https://rdrr.io/r/base/message.html) calls.
- Improved diagnostic output in
  [`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_h3.md)
  and
  [`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_polygon.md)
  with two-stage reporting structure.
- Census tract Parquet assets are now downloaded via piggyback from
  GitHub Releases, with on-demand caching.

### Bug fixes

- Removed inconsistencies in spatial join operations in
  [`cnefe_counts()`](https://pedreirajr.github.io/cnefetools/reference/cnefe_counts.md),
  [`compute_lumi()`](https://pedreirajr.github.io/cnefetools/reference/compute_lumi.md),
  and
  [`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_polygon.md)
  with DuckDB.
- Fixed community extension loading failures in DuckDB.
- Fixed temporary file path bugs in
  [`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_h3.md)
  and
  [`tracts_to_polygon()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_polygon.md).
- Fixed invalid geometry handling when user-supplied polygons.

### Documentation

- Six pre-rendered pkgdown articles: reading CNEFE data, address counts,
  land-use mix indices, dasymetric interpolation, FAQ, and a DuckDB
  performance benchmark.
- The package now features a new, more polished logo with improved
  visual quality.

## cnefetools 0.1.1

- Adds Balance Index (BAL), `bal` to the
  [`compute_lumi()`](https://pedreirajr.github.io/cnefetools/reference/compute_lumi.md)
  function.

## cnefetools 0.1.0

- New:
  [`tracts_to_h3()`](https://pedreirajr.github.io/cnefetools/reference/tracts_to_h3.md)
  for dasymetric interpolation from tract aggregates to CNEFE dwelling
  points and H3 hexagons using DuckDB (spatial + h3).
- Added UF-level census tract Parquet assets (WKB geometry) hosted on
  GitHub Releases and downloaded on-demand with caching.

## cnefetools 0.0.6

- Moves DBI and duckdb to Imports so the DuckDB backend works out of the
  box
- Removes silent fallback to `backend = "r"` when `backend = "duckdb"`
  is requested
- No changes to outputs; behavior is unchanged aside from dependency
  handling

## cnefetools 0.0.5

- Makes unit tests more reproducible and CI-friendly by removing
  dependencies on network access and local cache state (offline ZIP
  fixture and mocked downloads)
- Improves robustness of `read_cnefe(output = "sf")` by handling missing
  coordinates before converting to `sf`
- Updates `COD_ESPECIE` documentation and removes tidyselect deprecation
  warnings in tests and internals

## cnefetools 0.0.4

- Major speed-up for H3 assignment and hex-level aggregation via
  DuckDB + H3 extension (SQL), with runtimes dropping from minutes to
  seconds for large municipalities
- Adds a configurable backend with backwards compatibility:
  `backend = "duckdb"` (default) or `backend = "r"` for
  `hex_cnefe_counts()` and
  [`compute_lumi()`](https://pedreirajr.github.io/cnefetools/reference/compute_lumi.md)
- Refactors internals to reuse cached ZIPs and consolidate common
  helpers, reducing repeated overhead and improving maintainability

## cnefetools 0.0.3

- More robust downloads with retry logic and increased timeout when
  needed
- Avoids leaving partial or corrupted ZIP files in the cache
- Automatically detects corrupted cached ZIPs and re-downloads before
  extraction

## cnefetools 0.0.2

- Add
  [`compute_lumi()`](https://pedreirajr.github.io/cnefetools/reference/compute_lumi.md)
  to compute land-use mix indicators (EI, HHI, adapted HHI, BGBI) on H3
  grids.
- Extend README with examples for Fortaleza (code 2304400) and BGBI
  maps.

## cnefetools 0.0.1

## cnefetools 0.0.0.9000

- Initial development version. Basic package infrastructure set up.
