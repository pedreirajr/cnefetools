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
build_h3_grid <- function(
  h3_resolution,
  id_hex = NULL,
  code_muni = NULL,
  boundary = NULL
) {
  h3_resolution <- as.integer(h3_resolution)
  if (
    length(h3_resolution) != 1L ||
      is.na(h3_resolution) ||
      h3_resolution < 0L ||
      h3_resolution > 15L
  ) {
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

    hex_list <- h3jsr::polygon_to_cells(
      boundary1,
      res = h3_resolution,
      simple = TRUE
    )

    hex_ids <- unlist(hex_list, use.names = FALSE)
    hex_ids <- unique(stats::na.omit(as.character(hex_ids)))

    if (length(hex_ids) == 0L) {
      # Fallback for cases where the resolution is so coarse that no cell
      # center falls inside the boundary (e.g., a small municipality at res 1-3).
      # Use the cell containing the boundary centroid instead.
      centroid <- sf::st_centroid(geom1)
      coords   <- sf::st_coordinates(centroid)
      hex_ids  <- as.character(h3jsr::point_to_cell(
        data.frame(lon = coords[1L, 1L], lat = coords[1L, 2L]),
        res    = h3_resolution,
        simple = TRUE
      ))
      hex_ids <- unique(stats::na.omit(hex_ids))
    }

    if (length(hex_ids) == 0L) {
      rlang::abort("No H3 cells were generated for this municipality boundary.")
    }

    # Expand with border hexagons: polygon_to_cells() uses center-based
    # containment, so hexagons whose center falls just outside the boundary
    # are excluded even if they physically overlap the municipality. We add
    # them back by checking the k=1 neighbor ring via st_intersects().
    neighbors <- unique(unlist(
      h3jsr::get_disk(hex_ids, ring_size = 1L, simple = TRUE),
      use.names = FALSE
    ))
    border_candidates <- setdiff(neighbors, hex_ids)

    if (length(border_candidates) > 0L) {
      cand_geom <- h3jsr::cell_to_polygon(border_candidates, simple = TRUE)
      cand_sf   <- sf::st_sf(
        id_hex   = border_candidates,
        geometry = sf::st_sfc(cand_geom, crs = 4326)
      )
      hits    <- lengths(sf::st_intersects(cand_sf, boundary1)) > 0L
      hex_ids <- c(hex_ids, border_candidates[hits])
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
    id_hex = hex_ids,
    geometry = geom_hex
  )
}
