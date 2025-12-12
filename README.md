
<!-- badges: start -->

[![R-CMD-check](https://github.com/pedreirajr/cnefetools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pedreirajr/cnefetools/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)

<!-- badges: end -->

# cnefetools

**cnefetools** provides helper functions to work with the 2022 Brazilian
National Address File for Statistical Purposes (*Cadastro Nacional de
Endereços para Fins Estatísticos*, CNEFE), an address-level dataset
released by the Brazilian Institute of Geography and Statistics
(*Instituto Brasileiro de Geografia e Estatística*, IBGE).  
The initial version focuses on efficiently downloading, caching, and
reading municipality-level CNEFE CSV files into Arrow tables or spatial
objects for downstream analysis.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("pedreirajr/cnefetools")
```

## Basic usage (Arrow output)

``` r
library(cnefetools)

# Example - read CNEFE data for a municipality (Salvador)
tab_ssa <- read_cnefe(2927408, cache = TRUE)

# Convert to a tibble for inspection
df_ssa <- tab_ssa %>% 
  as.data.frame()

head(df_ssa)
```

By default, `read_cnefe()` returns an \[arrow::Table\], which is
convenient for large-scale, columnar workflows.

## Spatial output (sf)

If you prefer a spatial object, set `output = "sf"`. In this case,
`read_cnefe()` returns an `sf` object using the geocoded coordinates
(`LONGITUDE`, `LATITUDE`) in the SIRGAS 2000 reference system
(EPSG:4674).

``` r
library(cnefetools)
library(sf)

tab_ssa_sf <- read_cnefe(2927408, output = "sf", cache = TRUE)

tab_ssa_sf
sf::st_crs(tab_ssa_sf)
```

This is useful when you want to integrate CNEFE addresses directly in
spatial analysis or map production.

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
