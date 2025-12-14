testthat::test_that(
  "hex_cnefe_counts returns full grid with correct addr_type counts (mocked unit test)",
  {

    testthat::skip_if_not_installed("sf")
    testthat::skip_if_not_installed("h3jsr")
    testthat::skip_if_not_installed("dplyr")

    pts_df <- data.frame(
      CODIGO_ESPECIE = c(1L, 2L, 8L, 9L, NA_integer_),
      lon = c(-38.5200, -38.5200, -38.5500, -38.5200, -38.5500),
      lat = c(-3.7300,  -3.7300,  -3.7500,  -3.7300,  -3.7500)
    )

    pts_sf <- sf::st_as_sf(pts_df, coords = c("lon", "lat"), crs = 4326)

    h3_res <- 9L
    id1 <- h3jsr::point_to_cell(pts_sf[1, ], res = h3_res, simple = TRUE)[1]
    id2 <- h3jsr::point_to_cell(pts_sf[3, ], res = h3_res, simple = TRUE)[1]

    id3 <- h3jsr::point_to_cell(
      sf::st_as_sf(
        data.frame(lon = -38.7000, lat = -3.9000),
        coords = c("lon", "lat"),
        crs = 4326
      ),
      res = h3_res,
      simple = TRUE
    )[1]

    grid_ids <- c(as.character(id1), as.character(id2), as.character(id3))

    grid_geom <- h3jsr::cell_to_polygon(grid_ids, simple = TRUE)
    if (inherits(grid_geom, "sfc")) {
      grid_geom <- sf::st_set_crs(grid_geom, 4326)
    } else {
      grid_geom <- sf::st_sfc(grid_geom, crs = 4326)
    }

    hex_grid <- sf::st_sf(id_hex = grid_ids, geometry = grid_geom)

    out <- testthat::with_mocked_bindings(
      {
        cnefetools::hex_cnefe_counts(
          code_muni     = 2929057,
          h3_resolution = h3_res,
          verbose       = FALSE
        )
      },
      read_cnefe    = function(...) pts_sf,
      build_h3_grid = function(...) hex_grid,
      .package      = "cnefetools"
    )

    # CRITICAL: ensure mocks were reverted (this is what was breaking your suite)
    testthat::expect_error(
      cnefetools::read_cnefe("abc"),
      "must be coercible"
    )

    testthat::expect_s3_class(out, "sf")
    testthat::expect_equal(sf::st_crs(out)$epsg, 4326)
    testthat::expect_equal(nrow(out), 3L)

    needed <- c("id_hex", paste0("addr_type", 1:8), "geometry")
    testthat::expect_true(all(needed %in% names(out)))

    out_nog <- sf::st_drop_geometry(out)

    row1 <- dplyr::filter(out_nog, .data$id_hex == as.character(id1))
    row2 <- dplyr::filter(out_nog, .data$id_hex == as.character(id2))
    row3 <- dplyr::filter(out_nog, .data$id_hex == as.character(id3))

    testthat::expect_equal(row1$addr_type1, 1L)
    testthat::expect_equal(row1$addr_type2, 1L)
    testthat::expect_equal(row1$addr_type8, 0L)

    testthat::expect_equal(row2$addr_type8, 1L)
    testthat::expect_equal(row2$addr_type1, 0L)

    testthat::expect_true(all(as.integer(row3[paste0("addr_type", 1:8)]) == 0L))
  }
)


testthat::test_that("hex_cnefe_counts works end-to-end on a small municipality (2929057)", {

  testthat::skip_on_cran()
  testthat::skip_if_offline(host = "ftp.ibge.gov.br")

  testthat::skip_if_not(
    identical(Sys.getenv("CNEFETOOLS_RUN_DOWNLOAD_TESTS"), "true"),
    "Live download tests are disabled by default."
  )

  testthat::skip_if_not_installed("sf")

  out <- cnefetools::hex_cnefe_counts(
    code_muni     = 2929057,
    h3_resolution = 8,
    verbose       = FALSE
  )

  testthat::expect_s3_class(out, "sf")
  testthat::expect_equal(sf::st_crs(out)$epsg, 4326)
  testthat::expect_gt(nrow(out), 0L)

  needed <- c("id_hex", paste0("addr_type", 1:8), "geometry")
  testthat::expect_true(all(needed %in% names(out)))

  out_nog <- sf::st_drop_geometry(out)
  for (k in 1:8) {
    v <- out_nog[[paste0("addr_type", k)]]
    testthat::expect_true(is.numeric(v) || is.integer(v))
    testthat::expect_true(all(v >= 0, na.rm = TRUE))
  }
})
