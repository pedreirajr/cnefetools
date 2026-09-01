# Export CNEFE data to a persistent, optimised file

`cnefe_export()` downloads a municipality (or reuses the cache) and
writes it to a location and format of your choosing, so the data no
longer depends on the package cache being present or on the IBGE server
being reachable.

## Usage

``` r
cnefe_export(
  code_muni,
  path,
  format = c("parquet", "csv", "csv.gz"),
  year = 2022,
  overwrite = FALSE,
  cache = TRUE,
  cache_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- code_muni:

  Integer. Seven-digit IBGE municipality code.

- path:

  Character. Directory to write into. Created if missing.

- format:

  Character. `"parquet"` (default), `"csv"` or `"csv.gz"`.

- year:

  Integer. The CNEFE data year. Currently only 2022 is supported.

- overwrite:

  Logical. Whether to replace an existing file. Defaults to `FALSE`,
  which errors instead, since these files are expensive to produce.

- cache:

  Logical. Whether to use the package cache for the download.

- cache_dir:

  Character. Directory to use for cached downloads. If `NULL` (default),
  the `CNEFETOOLS_CACHE_DIR` environment variable is used when it is
  set, otherwise
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) with
  `which = "cache"`.

- verbose:

  Logical. Whether to print progress.

## Value

The path to the written file, invisibly.

## Details

The package cache is designed to be transient: it lives in a directory
the package manages, it holds the ZIP exactly as IBGE published it, and
[`clear_cache_muni()`](https://pedreirajr.github.io/cnefetools/dev/reference/clear_cache_muni.md)
is expected to empty it. That is the wrong shape for a reproducible
analysis that must still run in a year.

This function fills that gap. Point it at a project directory, a shared
volume or an external drive, choose a format, and use the resulting file
directly:

    path <- cnefe_export(2919207, "data/cnefe")
    cnefe <- read_cnefe(file = path)

[`read_cnefe()`](https://pedreirajr.github.io/cnefetools/dev/reference/read_cnefe.md)
accepts any file this function writes, and also the raw ZIP as
distributed by IBGE, so a file obtained by other means can be read
without the download step at all.

Parquet is the default because it is columnar, typed and compressed,
which makes it markedly smaller and faster to read than the published
CSV, and because it is the format Arrow and DuckDB both read natively.

## See also

[`read_cnefe()`](https://pedreirajr.github.io/cnefetools/dev/reference/read_cnefe.md),
which reads the result back through its `file` argument, and
[`clear_cache_muni()`](https://pedreirajr.github.io/cnefetools/dev/reference/clear_cache_muni.md)
for the transient cache.

## Examples

``` r
# \donttest{
# Write a municipality to a project directory as Parquet
path <- cnefe_export(2929057, path = tempdir(), cache = FALSE, overwrite = TRUE)
#> ℹ Processing municipality code 2929057
#> Downloading ZIP (timeout = 300s): https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/29_BA/2929057_SAO_FELIX_DO_CORIBE.zip
#> ℹ Converting the archive to .csv.gz (done once)
#> ✔ Converting the archive to .csv.gz (done once) [51ms]
#> 
#> ℹ Reading file214e62e9393c.csv.gz with arrow
#> ✔ Reading file214e62e9393c.csv.gz with arrow [39ms]
#> 
#> ✔ Read 9354 records from CNEFE
#> ℹ Writing cnefe_2022_2929057.parquet
#> ✔ Writing cnefe_2022_2929057.parquet [20ms]
#> 
#> ✔ Wrote 9354 records to /tmp/RtmpjNSCIC/cnefe_2022_2929057.parquet (0.3 MB).

# Read it back without touching the network
cnefe <- read_cnefe(file = path)
#> ℹ Reading cnefe_2022_2929057.parquet as Parquet
#> ✔ Reading cnefe_2022_2929057.parquet as Parquet [18ms]
#> 
#> ✔ Read 9354 records from cnefe_2022_2929057.parquet
# }
```
