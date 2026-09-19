# Read CNEFE data for a given municipality

Downloads and reads the CNEFE CSV file for a given IBGE municipality
code, using the official IBGE FTP structure. The function relies on an
internal index linking municipality codes to the corresponding ZIP URLs.
Data are returned either as an Arrow
[Table](https://arrow.apache.org/docs/r/reference/Table-class.html)
(default) or as an
[sf](https://r-spatial.github.io/sf/reference/st_as_sf.html) object with
SIRGAS 2000 coordinates.

## Usage

``` r
read_cnefe(
  code_muni = NULL,
  year = 2022,
  verbose = TRUE,
  cache = TRUE,
  cache_dir = NULL,
  output = c("arrow", "sf"),
  file = NULL
)
```

## Arguments

- code_muni:

  Integer. Seven-digit IBGE municipality code. Omit it when reading a
  local file through `file`.

- year:

  Integer. The CNEFE data year. Currently only 2022 is supported.
  Defaults to 2022.

- verbose:

  Logical; if `TRUE`, print informative messages about download,
  extraction, and reading steps.

- cache:

  Logical; if `TRUE`, keep the downloaded data as a gzipped CSV in the
  cache folder (see `cache_dir`). If `FALSE`, a temporary file is used
  and removed after reading.

- cache_dir:

  Character. Directory to use for cached downloads. If `NULL` (default),
  the `CNEFETOOLS_CACHE_DIR` environment variable is used when it is
  set, otherwise
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) with
  `which = "cache"`. Use this to point large downloads at a secondary
  drive or a shared volume.

- output:

  Character. Output format. `"arrow"` (default) returns an
  [arrow::Table](https://arrow.apache.org/docs/r/reference/Table-class.html),
  whereas `"sf"` returns an
  [sf](https://r-spatial.github.io/sf/reference/st_as_sf.html) point
  object with coordinates built from `LONGITUDE` / `LATITUDE` in CRS
  4674.

- file:

  Character. Path to a CNEFE file already on disk, read instead of
  downloading. Accepts `.zip` as published by IBGE, `.csv`, `.csv.gz`
  and `.parquet`, which is what
  [`cnefe_export()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_export.md)
  writes. Mutually exclusive with `code_muni`, and it makes the function
  independent of the IBGE server.

## Value

If `output = "arrow"`, an
[arrow::Table](https://arrow.apache.org/docs/r/reference/Table-class.html)
containing all CNEFE records for the given municipality.

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

When `cache = TRUE` (the default), the municipality is downloaded once
and kept as a gzipped CSV, so later calls, in this session or any other,
reuse it instead of downloading again. The cache folder is `cache_dir`
when given, otherwise the `CNEFETOOLS_CACHE_DIR` environment variable
when set, otherwise
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) with
`which = "cache"`.

When `cache = FALSE`, the file is stored in a temporary location and
removed when the function exits.

The cache is meant to be cleared (see
[`clear_cache_muni()`](https://pedreirajr.github.io/cnefetools/dev/reference/clear_cache_muni.md)).
For a copy that stays put, use
[`cnefe_export()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_export.md).
The "Cache and exported copies" article on the package website covers
both.

## See also

[`cnefe_export()`](https://pedreirajr.github.io/cnefetools/dev/reference/cnefe_export.md)
to write a municipality to a persistent, optimised file that this
function can read back through `file`.

## Examples

``` r
# \donttest{
# Read CNEFE data as an Arrow table
cnefe <- read_cnefe(code_muni = 2929057, cache = FALSE)
#> ℹ Processing municipality code 2929057
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2929057_SAO_FELIX_DO_CORIBE.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [49ms]
#> 
#> ℹ Reading file1e7f168292b4.csv.gz with arrow
#> ✔ Reading file1e7f168292b4.csv.gz with arrow [22ms]
#> 
#> ✔ Read 9354 records from CNEFE

# Read a local file instead, with no network access. overwrite = TRUE because
# cnefe_export() refuses to clobber an existing export by default.
path <- cnefe_export(2929057, path = tempdir(), cache = FALSE, overwrite = TRUE)
#> ℹ Processing municipality code 2929057
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2929057_SAO_FELIX_DO_CORIBE.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [57ms]
#> 
#> ℹ Reading file1e7f52207b26.csv.gz with arrow
#> ✔ Reading file1e7f52207b26.csv.gz with arrow [20ms]
#> 
#> ✔ Read 9354 records from CNEFE
#> ℹ Writing cnefe_2022_2929057.parquet
#> ✔ Writing cnefe_2022_2929057.parquet [21ms]
#> 
#> ✔ Wrote 9354 records to /tmp/RtmpwinupG/cnefe_2022_2929057.parquet (0.3 MB).
cnefe_local <- read_cnefe(file = path)
#> ℹ Reading cnefe_2022_2929057.parquet as Parquet
#> ✔ Reading cnefe_2022_2929057.parquet as Parquet [12ms]
#> 
#> ✔ Read 9354 records from cnefe_2022_2929057.parquet

# Read as an sf spatial object
cnefe_sf <- read_cnefe(code_muni = 2929057, output = "sf", cache = FALSE)
#> ℹ Processing municipality code 2929057
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2929057_SAO_FELIX_DO_CORIBE.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [57ms]
#> 
#> ℹ Reading file1e7f88e3489.csv.gz with arrow
#> ✔ Reading file1e7f88e3489.csv.gz with arrow [21ms]
#> 
#> ✔ Read 9354 records from CNEFE
#> ℹ Converting to sf object
#> ✔ Converting to sf object [30ms]
#> 
#> ✔ Created <sf> object with 9354 points (CRS: EPSG:4674)
# }
```
