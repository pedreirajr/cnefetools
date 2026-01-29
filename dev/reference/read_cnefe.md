# Read CNEFE data for a given municipality

Downloads and reads the CNEFE CSV file for a given IBGE municipality
code, using the official IBGE FTP structure. The function relies on an
internal index linking municipality codes to the corresponding ZIP URLs.
Data are returned either as an Arrow Table (default) or as an
[sf](https://r-spatial.github.io/sf/reference/st_as_sf.html) object with
SIRGAS 2000 coordinates.

## Usage

``` r
read_cnefe(
  code_muni,
  year = 2022,
  verbose = TRUE,
  cache = TRUE,
  output = c("arrow", "sf")
)
```

## Arguments

- code_muni:

  Integer. Seven-digit IBGE municipality code.

- year:

  Integer. The CNEFE data year. Currently only 2022 is supported.
  Defaults to 2022.

- verbose:

  Logical; if `TRUE`, print informative messages about download,
  extraction, and reading steps.

- cache:

  Logical; if `TRUE`, cache the downloaded ZIP file in a user-level
  cache directory specific to this package. If `FALSE`, a temporary file
  is used and removed after reading.

- output:

  Character. Output format. `"arrow"` (default) returns an arrow::Table,
  whereas `"sf"` returns an
  [sf](https://r-spatial.github.io/sf/reference/st_as_sf.html) point
  object with coordinates built from `LONGITUDE` / `LATITUDE` in CRS
  4674.

## Value

If `output = "arrow"`, an arrow::Table containing all CNEFE records for
the given municipality.

If `output = "sf"`, an
[sf](https://r-spatial.github.io/sf/reference/st_as_sf.html) object with
point geometry in EPSG:4674 (SIRGAS 2000), using the `LONGITUDE` and
`LATITUDE` columns.

## Details

When `output = "arrow"` (default), the function does not perform any
spatial conversion and simply returns the Arrow table. When
`output = "sf"`, the function converts the result to an
[sf](https://r-spatial.github.io/sf/reference/st_as_sf.html) point
object using the `LONGITUDE` and `LATITUDE` columns, with CRS EPSG:4674
(SIRGAS 2000), keeping these columns in the final object
(`remove = FALSE`).

## Caching

When `cache = TRUE` (the default), the downloaded ZIP file is stored in
a user-level cache directory specific to this package, created via
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) with
`which = "cache"`. This avoids re-downloading the same municipality file
across sessions.

When `cache = FALSE`, the ZIP file is stored in a temporary location and
removed when the function exits.
