## Resubmission (v0.2.5)

### Previous submission (v0.2.4) rejection

v0.2.4 failed the CRAN incoming pre-test (donttest checker, 2026-03-30) with
an ERROR in `cnefe_counts()`:

```
Binder Error: RTree indexes can only be created over GEOMETRY columns.
```

### Root cause

Two packages released between the local check and the CRAN check introduced a
breaking change:

* **DuckDB 1.5.0** (2026-03-09): moved the `GEOMETRY` type from the spatial
  extension into DuckDB core.  The new core `GEOMETRY` type supports optional
  CRS parameters (e.g. `GEOMETRY('OGC:CRS84')`).
* **duckspatial 1.0.0** (2026-03-30): rewritten for DuckDB 1.5; now writes
  geometry columns with embedded CRS metadata (i.e. as a parameterised
  `GEOMETRY` type).

DuckDB's RTREE index only accepts *plain* `GEOMETRY` (no CRS parameter).
The CRS-parameterised column produced by duckspatial 1.0.0 therefore caused
the binder error.

### Fix 1 — RTREE index (DuckDB 1.5 + duckspatial 1.0.0)

Before creating the RTREE index on tables written by `duckspatial::ddbs_write_vector()`,
a WKB round-trip is now performed to strip the CRS parameter and convert the
column to plain `GEOMETRY`:

```sql
ALTER TABLE user_polygons ALTER COLUMN geom SET DATA TYPE GEOMETRY
USING ST_GeomFromWKB(ST_AsWKB(geom));
```

Applied in: `cnefe_counts()`, `tracts_to_polygon()`, and `compute_lumi()`.

### Fix 2 — temporary ZIP deleted before DuckDB view materialisation

The helper `.cnefe_create_points_view_in_duckdb()` creates lazy DuckDB views
that read from a ZIP file.  When `cache = FALSE` (added in v0.2.4), an
`on.exit(unlink(zip_path))` registered inside the helper deleted the ZIP as
soon as the helper returned, before the caller had a chance to materialise the
view into a table (`CREATE TABLE ... AS SELECT * FROM cnefe_pts`).  This
caused `tracts_to_h3()` and `tracts_to_polygon()` to error at the CNEFE point
preparation step.

The cleanup responsibility was moved to each caller: the temp ZIP is now
deleted only after the view has been fully materialised into a DuckDB table.

## R CMD check results

0 errors | 0 warnings | 1 note

* `NOTE: unable to verify current time` — transient CRAN infrastructure
  issue (check server cannot reach time-verification servers); unrelated
  to the package.

## Test environments

- Local: Windows 11 x64, R 4.3.2
- GitHub Actions: ubuntu-24.04 (R release, R oldrel-1)
- GitHub Actions: windows-server-2022 (R release)
- GitHub Actions: macOS 15 Intel (R release)
