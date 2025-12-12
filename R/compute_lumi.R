#' Compute land-use mix indicators on an H3 grid
#'
#' @description
#' `compute_lumi()` reads CNEFE 2022 records for a given municipality,
#' assigns each address point to an H3 cell, and computes land-use mix
#' indicators per hexagon (EI, HHI, adapted HHI, and BGBI).
#'
#' @param code_muni Integer. Seven-digit IBGE municipality code.
#' @param h3_resolution Integer. H3 grid resolution (default: 9).
#' @param verbose Logical; if `TRUE`, prints messages and timing information
#'   for each processing step.
#'
#' @return An [`sf::sf`] object with CRS 4326 containing:
#'   - `id_hex`: H3 cell identifier;
#'   - `p_res`: share of residential addresses in the hexagon;
#'   - `ei`, `hhi`, `hhi_adp`, `bgbi`: land-use mix indicators;
#'   - `geometry`: hexagon geometry.
#'
#' @export
compute_lumi <- function(code_muni,
                         h3_resolution = 9,
                         verbose       = TRUE) {

  # ---------------------------------------------------------------------------
  # 0. Setup: internal index and timing helper
  # ---------------------------------------------------------------------------
  code_muni <- .normalize_code_muni(code_muni)

  index <- cnefe_index_2022
  info  <- index[index$code_muni == code_muni, , drop = FALSE]

  if (nrow(info) == 0) {
    rlang::abort(
      sprintf(
        "Municipality code not found in internal CNEFE index: %s",
        code_muni
      )
    )
  }

  # city_name is only used for messages; be robust if name_muni is absent
  if ("name_muni" %in% names(info) && !is.na(info$name_muni[1])) {
    city_name <- info$name_muni[1]
  } else {
    city_name <- as.character(code_muni)
  }

  if (verbose) {
    message(
      sprintf(
        "Processing %s (code %s)...",
        city_name, code_muni
      )
    )
  }

  timings <- list()

  log_step_time <- function(step_name, t_start) {
    dt <- difftime(Sys.time(), t_start, units = "secs")
    timings[[step_name]] <<- dt
    if (verbose) {
      message(
        sprintf(
          "%s completed in %.2f s.",
          step_name,
          as.numeric(dt, units = "secs")
        )
      )
    }
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

  # Ensure COD_ESPECIE exists (rename if needed)
  if (!"COD_ESPECIE" %in% names(end_sf) &&
      "CODIGO_ESPECIE" %in% names(end_sf)) {
    end_sf <- dplyr::rename(end_sf, COD_ESPECIE = CODIGO_ESPECIE)
  }

  if (!"COD_ESPECIE" %in% names(end_sf)) {
    rlang::abort(
      "Column `COD_ESPECIE` not found in CNEFE data for this municipality."
    )
  }

  # Filter: non-missing species and remove "under construction" (7)
  end_sf <- end_sf %>%
    dplyr::filter(!is.na(COD_ESPECIE)) %>%
    dplyr::filter(COD_ESPECIE != 7L)

  if (nrow(end_sf) == 0L) {
    warning(
      sprintf(
        "No valid CNEFE points left for %s after filtering COD_ESPECIE.",
        city_name
      )
    )
    return(NULL)
  }

  # H3 requires WGS84 (EPSG 4326)
  end_sf <- sf::st_transform(end_sf, 4326)

  log_step_time("Step 1/3 (reading and preprocessing)", t1)

  # ---------------------------------------------------------------------------
  # 2. Assign points to H3 cells and build the H3 grid
  # ---------------------------------------------------------------------------
  if (verbose) message("Step 2/3: generating H3 grid and assigning points...")
  t2 <- Sys.time()

  # H3 id for each point
  end_sf$id_hex <- h3jsr::point_to_cell(
    end_sf,
    res    = h3_resolution,
    simple = TRUE
  )

  # Drop points without an id_hex (should be rare)
  end_sf <- end_sf %>%
    dplyr::filter(!is.na(id_hex))

  if (nrow(end_sf) == 0L) {
    warning(
      sprintf(
        "No points could be assigned to H3 cells for %s.",
        city_name
      )
    )
    return(NULL)
  }

  # H3 grid from unique cell IDs
  hex_ids <- unique(end_sf$id_hex)

  hex_grid <- sf::st_sf(
    id_hex   = hex_ids,
    geometry = sf::st_sfc(h3jsr::cell_to_polygon(hex_ids)),
    crs      = 4326
  )

  log_step_time("Step 2/3 (H3 grid and assignment)", t2)

  # ---------------------------------------------------------------------------
  # 3. Compute land-use mix indicators per hexagon
  # ---------------------------------------------------------------------------
  if (verbose) message("Step 3/3: computing land-use mix indicators...")
  t3 <- Sys.time()

  # Counts by land-use type within each hexagon
  landuse_counts <- end_sf %>%
    sf::st_drop_geometry() %>%
    dplyr::count(id_hex, COD_ESPECIE, name = "count")

  landuse_wide <- landuse_counts %>%
    tidyr::pivot_wider(
      names_from   = COD_ESPECIE,
      values_from  = count,
      names_prefix = "COD_ESPECIE",
      values_fill  = 0
    )

  # Ensure residential column exists
  if (!"COD_ESPECIE1" %in% names(landuse_wide)) {
    warning(
      sprintf(
        "No residential establishments (COD_ESPECIE == 1) found in %s.",
        city_name
      )
    )
    landuse_wide$COD_ESPECIE1 <- 0L
  }

  # Global residential share P
  P <- landuse_wide %>%
    dplyr::mutate(
      tot = rowSums(
        dplyr::across(dplyr::starts_with("COD_ESPECIE")),
        na.rm = TRUE
      )
    ) %>%
    dplyr::summarise(
      P = sum(COD_ESPECIE1, na.rm = TRUE) /
        sum(tot, na.rm = TRUE)
    ) %>%
    dplyr::pull(P)

  # BGBI function
  bgbi_fun <- function(p, P) {
    num <- (2 * p - 1) - (2 * P - 1)
    den <- 1 - (2 * p - 1) * (2 * P - 1)
    den[den == 0] <- NA_real_
    ifelse(is.na(num) | is.na(den), NA_real_, num / den)
  }

  # Join counts back to grid and compute indicators
  city_indices <- hex_grid %>%
    dplyr::left_join(landuse_wide, by = "id_hex") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      tot = sum(
        dplyr::c_across(dplyr::starts_with("COD_ESPECIE")),
        na.rm = TRUE
      ),
      p_res  = if (!is.na(tot) && tot > 0) COD_ESPECIE1 / tot else NA_real_,
      q_rest = if (!is.na(p_res)) 1 - p_res else NA_real_,
      k = 2,

      # Conventional indices
      ei = if (!is.na(p_res) && !is.na(q_rest)) {
        -(p_res * log(p_res) + q_rest * log(q_rest)) / log(k)
      } else NA_real_,

      hhi = if (!is.na(p_res) && !is.na(q_rest)) {
        p_res^2 + q_rest^2
      } else NA_real_,

      min_hhi = 1 / k,
      hhi_sc  = if (!is.na(hhi)) {
        (hhi - min_hhi) / (1 - min_hhi)
      } else NA_real_,

      # Proposed indices
      hhi_adp = if (!is.na(p_res) && !is.na(hhi_sc)) {
        sign(p_res - q_rest) * hhi_sc
      } else NA_real_,

      bgbi = if (!is.na(p_res)) bgbi_fun(p_res, P) else NA_real_
    ) %>%
    dplyr::ungroup()

  # Keep only requested columns + geometry
  city_indices <- city_indices %>%
    dplyr::select(
      id_hex,
      p_res,
      ei,
      hhi,
      hhi_adp,
      bgbi,
      geometry
    )

  log_step_time("Step 3/3 (indicator computation)", t3)

  # ---------------------------------------------------------------------------
  # Timing summary
  # ---------------------------------------------------------------------------
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

  city_indices
}
