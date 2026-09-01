# Output correctness for tracts_to_polygon(), which Referee 2 (R2.C8) noted was
# tested only for input validation, unlike tracts_to_h3(). Mirrors the mocked
# setup used there: two tracts, only the first containing CNEFE points, so the
# unallocated total is known exactly rather than merely plausible.

skip_unless_duckdb_ready <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("duckspatial")
  testthat::skip_if_not_installed("sf")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  ok <- function(ext) {
    tryCatch(
      {
        DBI::dbExecute(con, sprintf("LOAD %s;", ext))
        TRUE
      },
      error = function(e) {
        tryCatch(
          {
            DBI::dbExecute(con, sprintf("INSTALL %s; LOAD %s;", ext, ext))
            TRUE
          },
          error = function(e2) FALSE
        )
      }
    )
  }
  if (!ok("spatial")) testthat::skip("DuckDB spatial extension not available.")
}

mock_tracts <- function(con, ...) {
  # Tract 1 covers the CNEFE points, tract 2 has none. pop_ph 100 and 50.
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE VIEW sc_muni AS
    SELECT
      '292740800000001' AS code_tract,
      100::INTEGER AS pop_ph,
      60::INTEGER  AS female,
      ST_GeomFromText('POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))') AS geom
    UNION ALL
    SELECT
      '292740800000002' AS code_tract,
      50::INTEGER  AS pop_ph,
      20::INTEGER  AS female,
      ST_GeomFromText('POLYGON((2 0, 3 0, 3 1, 2 1, 2 0))') AS geom
  "
  )
}

mock_points <- function(con, ...) {
  # Four private dwellings inside tract 1, one point outside every tract.
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE VIEW cnefe_pts AS
    SELECT
      1::BIGINT AS COD_UNICO_ENDERECO,
      1::INTEGER AS COD_ESPECIE,
      0.2::DOUBLE AS lon,
      0.2::DOUBLE AS lat,
      ST_Point(0.2, 0.2) AS geom
    UNION ALL SELECT 2, 1, 0.2, 0.8, ST_Point(0.2, 0.8)
    UNION ALL SELECT 3, 1, 0.8, 0.2, ST_Point(0.8, 0.2)
    UNION ALL SELECT 4, 1, 0.8, 0.8, ST_Point(0.8, 0.8)
    UNION ALL SELECT 5, 1, 10.0, 10.0, ST_Point(10.0, 10.0)
  "
  )
}

one_polygon <- function(crs = 4326) {
  # Covers tract 1 exactly, so it should receive the whole of its pop_ph.
  p <- sf::st_sf(
    zone = "A",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )
  if (crs != 4326) p <- sf::st_transform(p, crs)
  p
}

run_interp <- function(polygon, vars = c("pop_ph", "female"), ...) {
  testthat::with_mocked_bindings(
    cnefetools::tracts_to_polygon(
      code_muni = 2927408,
      polygon = polygon,
      vars = vars,
      verbose = FALSE,
      ...
    ),
    .sc_create_views_in_duckdb = mock_tracts,
    .cnefe_create_points_view_in_duckdb = mock_points,
    .package = "cnefetools"
  )
}


testthat::test_that("tracts_to_polygon() allocates the full tract total to a covering polygon", {
  skip_unless_duckdb_ready()

  out <- run_interp(one_polygon())

  testthat::expect_s3_class(out, "sf")
  testthat::expect_identical(nrow(out), 1L)
  testthat::expect_true(all(c("pop_ph", "female") %in% names(out)))

  # Tract 1 holds pop_ph 100 across four private dwellings, all inside the
  # polygon, so the polygon must receive all 100. Tract 2 has no points, so its
  # 50 stay unallocated and must not leak in.
  testthat::expect_equal(round(out$pop_ph), 100)
  testthat::expect_equal(round(out$female), 60)
})


testthat::test_that("tracts_to_polygon() preserves the caller's columns and CRS", {
  skip_unless_duckdb_ready()

  out <- run_interp(one_polygon())

  testthat::expect_true("zone" %in% names(out))
  testthat::expect_identical(out$zone, "A")
  testthat::expect_identical(sf::st_crs(out)$epsg, 4326L)
  # The internal join column must not surface.
  testthat::expect_false(".poly_row_id" %in% names(out))
})


testthat::test_that("tracts_to_polygon() honours crs_output, and returns the input CRS otherwise", {
  skip_unless_duckdb_ready()

  # Input in 4326, asked for 31983: the output must be reprojected, and the
  # values must not change because reprojection moves geometry, not counts.
  out <- run_interp(one_polygon(), crs_output = 31983)
  testthat::expect_identical(sf::st_crs(out)$epsg, 31983L)
  testthat::expect_equal(round(out$pop_ph), 100)

  # Input in 31983, no crs_output: the output keeps the input CRS.
  out2 <- run_interp(one_polygon(crs = 31983))
  testthat::expect_identical(sf::st_crs(out2)$epsg, 31983L)
  testthat::expect_equal(round(out2$pop_ph), 100)
})


testthat::test_that("tracts_to_polygon() splits a tract between two polygons", {
  skip_unless_duckdb_ready()

  # Two halves of tract 1, two dwellings each, so pop_ph 100 splits 50/50.
  halves <- sf::st_sf(
    zone = c("left", "right"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 0.5, 0, 0.5, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(0.5, 0, 1, 0, 1, 1, 0.5, 1, 0.5, 0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  out <- run_interp(halves)

  testthat::expect_identical(nrow(out), 2L)
  testthat::expect_equal(round(sum(out$pop_ph)), 100)
  testthat::expect_equal(round(out$pop_ph), c(50, 50))
})


testthat::test_that("tracts_to_polygon() emits the two-stage diagnostics", {
  skip_unless_duckdb_ready()

  testthat::expect_message(
    testthat::with_mocked_bindings(
      cnefetools::tracts_to_polygon(
        code_muni = 2927408,
        polygon = one_polygon(),
        vars = "pop_ph",
        verbose = FALSE
      ),
      .sc_create_views_in_duckdb = mock_tracts,
      .cnefe_create_points_view_in_duckdb = mock_points,
      .package = "cnefetools"
    ),
    "Dasymetric interpolation diagnostics",
    fixed = TRUE
  )
})
