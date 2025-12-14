#' Count CNEFE address species on an H3 grid
#'
#' @description
#' `hex_cnefe_counts()` reads CNEFE records for a given municipality, assigns
#' each address point to an H3 cell, builds the full H3 grid over the municipal
#' boundary (year fixed at 2024), and returns per-hexagon counts of `COD_ESPECIE`
#' as `addr_type1` to `addr_type8`.
#'
#' @param code_muni Integer. Seven-digit IBGE municipality code.
#' @param h3_resolution Integer. H3 grid resolution (default: 9).
#' @param verbose Logical; if `TRUE`, prints messages and timing information.
#'
#' @details
#' Mapping between `addr_type*` and `COD_ESPECIE`:
#' - `addr_type1` (1): Private household (Domicílio particular)
#' - `addr_type2` (2): Collective household (Domicílio coletivo)
#' - `addr_type3` (3): Agricultural establishment (Estabelecimento agropecuário)
#' - `addr_type4` (4): Educational establishment (Estabelecimento de ensino)
#' - `addr_type5` (5): Health establishment (Estabelecimento de saúde)
#' - `addr_type6` (6): Establishment for other purposes (Estabelecimento de outras finalidades)
#' - `addr_type7` (7): Building under construction or renovation (Edificação em construção ou reforma)
#' - `addr_type8` (8): Religious establishment (Estabelecimento religioso)
#'
#' @return An [`sf::sf`] object with CRS 4326 containing:
#' - `id_hex`: H3 cell identifier
#' - `addr_type1` ... `addr_type8`: counts per type
#' - `geometry`: hexagon geometry
#'
#' @export
hex_cnefe_counts <- function(code_muni,
                             h3_resolution = 9,
                             verbose       = TRUE) {

  code_muni <- .normalize_code_muni(code_muni)

  timings <- list()

  log_step_time <- function(step_name, t_start) {
    dt <- difftime(Sys.time(), t_start, units = "secs")
    timings[[step_name]] <<- dt
    if (verbose) {
      message(sprintf("%s completed in %.2f s.", step_name, as.numeric(dt)))
    }
  }

  if (verbose) {
    message(sprintf("Processing code %s...", code_muni))
  }

  # ---------------------------------------------------------------------------
  # 1. Read CNEFE as sf + minimal preprocessing
  # ---------------------------------------------------------------------------
  if (verbose) message("Step 1/3: reading and preprocessing CNEFE data...")
  t1 <- Sys.time()

  end_sf <- read_cnefe(
    code_muni = code_muni,
    output    = "sf",
    cache     = TRUE,
    verbose   = verbose
  )

  if (!"COD_ESPECIE" %in% names(end_sf) &&
      "CODIGO_ESPECIE" %in% names(end_sf)) {
    end_sf <- dplyr::rename(end_sf, COD_ESPECIE = CODIGO_ESPECIE)
  }

  if (!"COD_ESPECIE" %in% names(end_sf)) {
    rlang::abort("Column `COD_ESPECIE` not found in CNEFE data for this municipality.")
  }

  end_sf <- end_sf %>%
    dplyr::filter(!is.na(COD_ESPECIE)) %>%
    dplyr::filter(.data$COD_ESPECIE %in% 1L:8L) %>%
    sf::st_transform(4326)

  log_step_time("Step 1/3 (reading and preprocessing)", t1)

  # ---------------------------------------------------------------------------
  # 2. Build full H3 grid for the municipality and assign points
  # ---------------------------------------------------------------------------
  if (verbose) message("Step 2/3: generating full H3 grid and assigning points...")
  t2 <- Sys.time()

  hex_grid <- build_h3_grid(
    h3_resolution = h3_resolution,
    code_muni     = code_muni
  )

  if (nrow(end_sf) > 0L) {
    end_sf$id_hex <- h3jsr::point_to_cell(end_sf, res = h3_resolution, simple = TRUE)
    end_sf <- end_sf %>%
      dplyr::filter(!is.na(id_hex))
  }

  log_step_time("Step 2/3 (H3 grid and assignment)", t2)

  # ---------------------------------------------------------------------------
  # 3. Compute per-hex counts and join to full grid
  # ---------------------------------------------------------------------------
  if (verbose) message("Step 3/3: computing per-hexagon counts...")
  t3 <- Sys.time()

  if (nrow(end_sf) == 0L) {

    out <- hex_grid
    for (k in 1:8) out[[paste0("addr_type", k)]] <- 0L

  } else {

    counts_wide <- end_sf %>%
      sf::st_drop_geometry() %>%
      dplyr::count(id_hex, COD_ESPECIE, name = "n") %>%
      tidyr::pivot_wider(
        names_from   = COD_ESPECIE,
        values_from  = n,
        names_prefix = "addr_type",
        values_fill  = 0
      )

    for (k in 1:8) {
      nm <- paste0("addr_type", k)
      if (!nm %in% names(counts_wide)) counts_wide[[nm]] <- 0L
    }

    counts_wide <- counts_wide %>%
      dplyr::select("id_hex", dplyr::all_of(paste0("addr_type", 1:8))) %>%
      dplyr::mutate(
        dplyr::across(dplyr::starts_with("addr_type"), ~ as.integer(.x))
      )

    out <- hex_grid %>%
      dplyr::left_join(counts_wide, by = "id_hex") %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("addr_type"),
          ~ dplyr::coalesce(as.integer(.x), 0L)
        )
      )
  }

  log_step_time("Step 3/3 (count computation)", t3)

  if (verbose) {
    message("Timing summary (seconds):")
    total_secs <- 0
    for (nm in names(timings)) {
      secs <- as.numeric(timings[[nm]], units = "secs")
      total_secs <- total_secs + secs
      message(sprintf(" - %s: %.2f", nm, secs))
    }
    message(sprintf("Total time: %.2f s.", total_secs))
  }

  out
}
