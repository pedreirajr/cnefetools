
# cnefetools

Tools for working with the Brazilian CNEFE 2022 address data from the
Cadastro Nacional de Endereços para Fins Estatísticos (CNEFE).

The initial goal of this package is to provide a simple and efficient
way to download, cache, and read municipality-level CNEFE CSV files into
Arrow tables.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("pedreirajr/cnefetools")
```

## Basic usage

``` r
library(cnefetools)

# Example - read CNEFE data for a municipality (Salvador)
tab_ssa <- read_cnefe(2927408, cache = TRUE)

# Convert to a tibble for inspection
df_ssa <- tab_ssa %>% 
  as.data.frame()

head(df_ssa)
```

## Caching behavior

By default, `cache = TRUE` stores the downloaded ZIP file in a
user-level cache directory specific to this package. The exact location
is handled internally via `tools::R_user_dir("cnefetools", "cache")`.

If you prefer to avoid persistent caching, set:

``` r
tab_ssa <- read_cnefe(2927408, cache = FALSE)
```

In this case, the ZIP file is stored in a temporary location and removed
after reading.

## Roadmap

Planned extensions include:

- Functions to compute land use mix indicators based on CNEFE records.
- Functions to support dasymetric population interpolation using CNEFE
  as ancillary data.
- Additional helpers for working with Arrow tables and spatial
  representations.
