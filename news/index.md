# Changelog

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
  [`hex_cnefe_counts()`](https://pedreirajr.github.io/cnefetools/reference/hex_cnefe_counts.md)
  and
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
