# Fixtures shared by test-multi-muni.R and test-scope.R (#120).
# Moved here from test-multi-muni.R so both files can use them.

# Several municipalities in one call (#120, R2 comment 1).
#
# The H3 grid of a municipality holds every cell whose center falls inside it
# plus the neighbors that intersect its boundary, so a cell on a shared border
# belongs to the grid of BOTH municipalities. Two adjacent synthetic
# municipalities reproduce it offline: A to the west and B to the east of a
# meridian that runs through the center of one H3 cell, the "shared" cell.
# A puts 20 private households in it and B puts 51 other-use establishments, the
# numbers of the referee's example (20 / 71 = 0.282; averaging the two separate
# shares gives 0.5).

multi_muni <- local({
  res <- 9L
  start <- data.frame(lon = -38.5, lat = -12.9)
  shared <- as.character(h3jsr::point_to_cell(start, res = res, simple = TRUE))
  ctr <- sf::st_coordinates(h3jsr::cell_to_point(shared, simple = TRUE))
  lon0 <- ctr[1, 1]
  lat0 <- ctr[1, 2]

  rect <- function(xmin, xmax) {
    sf::st_sf(
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(
          c(xmin, lat0 - 0.01, xmax, lat0 - 0.01, xmax, lat0 + 0.01,
            xmin, lat0 + 0.01, xmin, lat0 - 0.01),
          ncol = 2, byrow = TRUE
        ))),
        crs = 4326
      )
    )
  }

  list(
    res = res,
    shared = shared,
    lon0 = lon0,
    lat0 = lat0,
    code_a = 3000001L,
    code_b = 3000002L,
    boundary_a = rect(lon0 - 0.01, lon0),
    boundary_b = rect(lon0, lon0 + 0.01),
    # Records of A: 20 private households in the shared cell, plus cells that
    # only A reaches.
    records_a = data.frame(
      COD_ESPECIE = c(rep(1L, 20), rep(1L, 10), rep(4L, 3), 7L),
      LONGITUDE = c(rep(lon0 - 1e-5, 20), rep(lon0 - 0.005, 13), lon0 - 0.005),
      LATITUDE = c(rep(lat0, 20), rep(lat0 + 0.003, 13), lat0 + 0.003)
    ),
    # Records of B: 51 other uses in the shared cell, plus cells only B reaches.
    records_b = data.frame(
      COD_ESPECIE = c(rep(6L, 51), rep(2L, 5), rep(1L, 4)),
      LONGITUDE = c(rep(lon0 + 1e-5, 51), rep(lon0 + 0.005, 9)),
      LATITUDE = c(rep(lat0, 51), rep(lat0 - 0.003, 9))
    )
  )
})

skip_unless_multi_deps <- function(backend, polygon = FALSE) {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("h3jsr")
  testthat::skip_if_not_installed("sf")
  if (identical(backend, "duckdb")) {
    skip_unless_duckdb_extensions(if (polygon) "spatial" else "h3")
    if (polygon) testthat::skip_if_not_installed("duckspatial")
  }
}

# Stand-ins for the boundary index and reader (#120, R/boundaries.R): the two
# synthetic municipalities, in one synthetic immediate region, with their
# bounding boxes. `.detect_code_munis()` runs for real on top of them.
mock_boundary_index <- function(year = 2022L) {
  bb <- rbind(
    as.numeric(sf::st_bbox(multi_muni$boundary_a)),
    as.numeric(sf::st_bbox(multi_muni$boundary_b))
  )
  data.frame(
    code_muni = c(multi_muni$code_a, multi_muni$code_b),
    name_muni = c("Municipality A", "Municipality B"),
    code_immediate = 300001L,
    xmin = bb[, 1], ymin = bb[, 2], xmax = bb[, 3], ymax = bb[, 4]
  )
}

# A `.read_muni_boundaries()` that serves the boundaries in `boundaries`, a
# list of sf objects named by code
mock_read_boundaries_from <- function(boundaries) {
  function(code_muni, year = 2022L, cache = TRUE, cache_dir = NULL) {
    codes <- unique(as.integer(code_muni))
    sf::st_sf(
      code_muni = codes,
      geometry = do.call(c, lapply(codes, function(cm) {
        sf::st_geometry(sf::st_transform(boundaries[[as.character(cm)]], 4326))
      }))
    )
  }
}

mock_read_muni_boundaries <- mock_read_boundaries_from(list(
  "3000001" = multi_muni$boundary_a,
  "3000002" = multi_muni$boundary_b
))

# Runs `fn` against the two synthetic municipalities, with the retrieval and the
# boundary read replaced so nothing touches the network. `records_a` and
# `records_b` replace the records of each municipality.
run_multi <- function(fn, code_muni, backend, ...,
                      records_a = multi_muni$records_a,
                      records_b = multi_muni$records_b) {
  files <- list(
    "3000001" = write_cnefe_gz(records_a),
    "3000002" = write_cnefe_gz(records_b)
  )
  on.exit(unlink(unlist(files)), add = TRUE)
  boundaries <- list(
    "3000001" = multi_muni$boundary_a,
    "3000002" = multi_muni$boundary_b
  )

  testthat::with_mocked_bindings(
    fn(code_muni, h3_resolution = multi_muni$res, backend = backend,
       verbose = FALSE, ...),
    .cnefe_ensure_zip = function(code_muni, ...) {
      list(
        zip_path = files[[as.character(code_muni)]],
        cleanup_zip = FALSE,
        url = "synthetic"
      )
    },
    .read_muni_boundaries = mock_read_boundaries_from(boundaries),
    .boundary_index = mock_boundary_index,
    .package = "cnefetools"
  )
}

addr_cols <- paste0("addr_type", 1:8)

# --- tracts_to_h3() and tracts_to_polygon() -----------------------------------
#
# Both municipalities sit in the same state, so one tract Parquet serves both.
# A tract of each municipality covers its own rectangle. Population totals are
# additive and spread over the private and collective dwellings of each tract,
# so the border cell must receive the allocation of both sides, once.

tract_multi <- local({
  lon0 <- multi_muni$lon0
  lat0 <- multi_muni$lat0
  pt <- function(dx, dy, n, cod) {
    data.frame(
      COD_ESPECIE = rep(cod, n),
      LONGITUDE = rep(lon0 + dx, n),
      LATITUDE = rep(lat0 + dy, n)
    )
  }
  list(
    # A: 20 dwellings in the shared cell and 10 elsewhere. B: 5 in the shared
    # cell and 4 elsewhere.
    records_a = rbind(pt(-1e-5, 0, 20, 1L), pt(-0.005, 0.003, 10, 1L)),
    records_b = rbind(pt(1e-5, 0, 5, 1L), pt(0.005, -0.003, 4, 2L)),
    pop_a = 100L,
    pop_b = 200L
  )
})

tract_parquet_multi <- function() {
  rect_wkb <- function(boundary) {
    unclass(sf::st_as_binary(sf::st_geometry(boundary)))[[1]]
  }
  tab <- arrow::arrow_table(
    code_tract = c("300000100000001", "300000200000001"),
    pop_ph = c(tract_multi$pop_a, tract_multi$pop_b),
    pop_ch = c(0L, 0L),
    geom_wkb = arrow::Array$create(
      list(rect_wkb(multi_muni$boundary_a), rect_wkb(multi_muni$boundary_b)),
      type = arrow::binary()
    )
  )
  p <- tempfile(fileext = ".parquet")
  arrow::write_parquet(tab, p)
  p
}

run_tracts <- function(fn, code_muni, ..., quiet = TRUE) {
  files <- list(
    "3000001" = write_cnefe_gz(tract_multi$records_a),
    "3000002" = write_cnefe_gz(tract_multi$records_b)
  )
  parquet <- tract_parquet_multi()
  on.exit(unlink(c(unlist(files), parquet)), add = TRUE)
  boundaries <- list(
    "3000001" = multi_muni$boundary_a,
    "3000002" = multi_muni$boundary_b
  )

  testthat::with_mocked_bindings(
    if (quiet) {
      suppressWarnings(suppressMessages(fn(code_muni, ..., verbose = FALSE)))
    } else {
      fn(code_muni, ..., verbose = FALSE)
    },
    .cnefe_ensure_zip = function(code_muni, ...) {
      list(
        zip_path = files[[as.character(code_muni)]],
        cleanup_zip = FALSE,
        url = "synthetic"
      )
    },
    .sc_ensure_parquet_munis = function(...) parquet,
    .read_muni_boundaries = mock_read_boundaries_from(boundaries),
    .boundary_index = mock_boundary_index,
    .package = "cnefetools"
  )
}

