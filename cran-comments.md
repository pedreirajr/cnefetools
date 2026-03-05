## Submission (v0.2.3)

This is a new release following the acceptance of v0.2.2 on CRAN.

### Changes in this version

* `cnefe_counts()` and `compute_lumi()` now expose a `cache` parameter
  (default `TRUE`), consistent with `tracts_to_h3()` and `tracts_to_polygon()`.
* New `clear_cache_muni()` and `clear_cache_tracts()` functions to delete
  cached files from the user cache directory.
* Fixed missing hexagons at municipality boundaries in `build_h3_grid()`:
  `h3jsr::polygon_to_cells()` only returns hexagons whose centroid falls
  inside the boundary; border hexagons are now recovered via a k=1 neighbour
  ring filtered by `sf::st_intersects()`.

## R CMD check results

0 errors | 0 warnings | 1 note

* `NOTE: unable to verify current time` — this is a transient CRAN
  infrastructure issue (the check server cannot reach time-verification
  servers) and is unrelated to the package code.

## Test environments

- Local: Windows 11 x64, R 4.3.2
- GitHub Actions: ubuntu-24.04 (R release, R oldrel-1)
- GitHub Actions: windows-server-2022 (R release)
- GitHub Actions: macOS 15 Intel (R release)
