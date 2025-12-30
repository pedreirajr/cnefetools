# Script to build the internal index for CNEFE 2022
# This script is intended to be run manually by the package developer.
# It is NOT executed by end users during normal package use.

library(geobr)
library(sf)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(rvest)
library(tidyr)
library(tibble)
library(usethis)

build_cnefe_index_2022 <- function(
    base_url = "https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/",
    verbose = TRUE
) {
  # 1) Download municipalities with geometry from geobr.
  #    Geometry is only used here to get the full municipal list; it will be
  #    dropped afterwards.
  muni_sf <- geobr::read_municipality(
    year         = 2022,
    simplified   = FALSE,
    showProgress = verbose
  )

  # 2) Extract basic municipal metadata without geometry.
  muni_meta <- muni_sf |>
    sf::st_drop_geometry() |>
    dplyr::distinct(code_state, abbrev_state, code_muni, name_muni)

  # 3) Build one FTP URL per state-specific folder in the IBGE structure.
  states <- muni_meta |>
    dplyr::distinct(code_state, abbrev_state) |>
    dplyr::mutate(
      state_folder = sprintf("%02d_%s", code_state, abbrev_state),
      state_url    = paste0(base_url, state_folder, "/")
    )

  # Helper: scrape all .zip file names inside a given state folder.
  scrape_state_zips <- function(state_url) {
    if (verbose) message("Reading index from: ", state_url)

    safe_read_html <- purrr::possibly(rvest::read_html, otherwise = NULL)
    html <- safe_read_html(state_url)

    if (is.null(html)) {
      warning("Failed to access ", state_url)
      return(tibble::tibble(zip_name = character()))
    }

    hrefs <- html |>
      rvest::html_elements("a") |>
      rvest::html_attr("href")

    tibble::tibble(
      zip_name = hrefs[grepl("\\.zip$", hrefs, ignore.case = TRUE)]
    )
  }

  # 4) Iterate over all state URLs and extract municipality codes and zip URLs.
  state_files <- states |>
    dplyr::mutate(zips = purrr::map(state_url, scrape_state_zips)) |>
    tidyr::unnest(zips) |>
    dplyr::mutate(
      code_muni = readr::parse_integer(
        stringr::str_extract(zip_name, "^[0-9]{7}")
      ),
      zip_url   = paste0(state_url, zip_name)
    ) |>
    dplyr::filter(!is.na(code_muni)) |>
    dplyr::select(code_muni, zip_url)

  # Important: we do NOT reorder rows here.
  # The original traversal order of the FTP structure is preserved.
  cnefe_index_2022 <- state_files

  cnefe_index_2022
}

# Build the index object
cnefe_index_2022 <- build_cnefe_index_2022()

# Store it as internal package data (sysdata.rda)
usethis::use_data(cnefe_index_2022, internal = TRUE, overwrite = TRUE)
