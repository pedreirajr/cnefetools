#' Build an H3 grid as an sf object
#'
#' @description
#' Internal helper to build an H3 grid either:
#' - from a vector of H3 cell ids (`id_hex`), or
#' - from the municipality boundary (`code_muni`) using geobr (year fixed at 2024).
#'
#' @param h3_resolution Integer. H3 resolution.
#' @param id_hex Character/integer vector of H3 cell ids (optional).
#' @param code_muni Integer. Seven-digit IBGE municipality code (optional).
#' @param boundary An sf polygon for the area of interest (optional).
#'
#' @return An sf object (CRS 4326) with columns `id_hex` and `geometry`.
#'
#' @keywords internal
build_h3_grid <- function(h3_resolution,
                          id_hex    = NULL,
                          code_muni = NULL,
                          boundary  = NULL) {

  h3_resolution <- as.integer(h3_resolution)
  if (length(h3_resolution) != 1L || is.na(h3_resolution) ||
      h3_resolution < 0L || h3_resolution > 15L) {
    rlang::abort("`h3_resolution` must be an integer between 0 and 15.")
  }

  if (!is.null(id_hex)) {

    hex_ids <- unique(stats::na.omit(as.character(id_hex)))

    ok <- h3jsr::is_valid(hex_ids, simple = TRUE)
    hex_ids <- hex_ids[ok]

  } else {

    if (is.null(boundary)) {
      if (is.null(code_muni)) {
        rlang::abort("Provide either `id_hex` or `code_muni`/`boundary`.")
      }
      boundary <- .read_muni_boundary_2024(code_muni)
    }

    boundary <- boundary |>
      sf::st_transform(4326) |>
      sf::st_make_valid()

    geom1 <- sf::st_union(sf::st_geometry(boundary))
    geom1 <- sf::st_make_valid(geom1)

    boundary1 <- sf::st_sf(geometry = sf::st_sfc(geom1, crs = 4326))

    hex_list <- h3jsr::polygon_to_cells(boundary1, res = h3_resolution, simple = TRUE)

    hex_ids <- unlist(hex_list, use.names = FALSE)
    hex_ids <- unique(stats::na.omit(as.character(hex_ids)))

    if (length(hex_ids) == 0L) {
      rlang::abort("No H3 cells were generated for this municipality boundary.")
    }

    ok <- h3jsr::is_valid(hex_ids, simple = TRUE)
    if (any(!ok)) {
      hex_ids <- hex_ids[ok]
    }
  }

  if (length(hex_ids) == 0L) {
    rlang::abort("No valid H3 cells were generated for this input.")
  }

  hex_ids <- sort(hex_ids)

  geom_hex <- h3jsr::cell_to_polygon(hex_ids, simple = TRUE)

  if (inherits(geom_hex, "sfc")) {
    geom_hex <- sf::st_set_crs(geom_hex, 4326)
  } else {
    geom_hex <- sf::st_sfc(geom_hex, crs = 4326)
  }

  sf::st_sf(
    id_hex   = hex_ids,
    geometry = geom_hex
  )
}
