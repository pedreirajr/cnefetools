testthat::test_that("compute_lumi computes expected p_res on offline fixture (backend r)", {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("h3jsr")
  testthat::skip_if_not_installed("sf")

  code_muni <- 2927408L
  h3_res <- 9L

  tab <- testthat::with_mocked_bindings(
    cnefetools::read_cnefe(
      code_muni,
      verbose = FALSE,
      cache = TRUE,
      output = "arrow"
    ),
    .cnefe_ensure_zip = mock_ensure_zip_fixture,
    .package = "cnefetools"
  )

  df <- as.data.frame(tab) |>
    dplyr::transmute(
      LONGITUDE = suppressWarnings(as.numeric(.data$LONGITUDE)),
      LATITUDE = suppressWarnings(as.numeric(.data$LATITUDE)),
      COD_ESPECIE = suppressWarnings(as.integer(.data$COD_ESPECIE))
    ) |>
    dplyr::filter(
      !is.na(.data$LONGITUDE),
      !is.na(.data$LATITUDE),
      !is.na(.data$COD_ESPECIE),
      .data$COD_ESPECIE %in% 1L:8L,
      .data$COD_ESPECIE != 7L
    )

  ids <- h3jsr::point_to_cell(
    df |>
      dplyr::transmute(
        lon = .data$LONGITUDE,
        lat = .data$LATITUDE
      ),
    res = h3_res,
    simple = TRUE
  )

  expected <- df |>
    dplyr::mutate(id_hex = as.character(ids)) |>
    dplyr::group_by(.data$id_hex) |>
    dplyr::summarise(
      p_res = sum(.data$COD_ESPECIE == 1L) / dplyr::n(),
      .groups = "drop"
    )

  out <- testthat::with_mocked_bindings(
    cnefetools::compute_lumi(
      code_muni,
      h3_resolution = h3_res,
      backend = "r",
      verbose = FALSE
    ),
    .cnefe_ensure_zip = mock_ensure_zip_fixture,
    .package = "cnefetools"
  )

  testthat::expect_s3_class(out, "sf")
  testthat::expect_true(all(
    c("id_hex", "p_res", "ei", "hhi", "hhi_adp", "bgbi") %in% names(out)
  ))

  out_df <- sf::st_drop_geometry(out) |>
    dplyr::select("id_hex", "p_res") |>
    dplyr::inner_join(expected, by = "id_hex", suffix = c("", "_exp"))

  testthat::expect_true(nrow(out_df) > 0L)
  testthat::expect_equal(out_df$p_res, out_df$p_res_exp, tolerance = 1e-12)
})


testthat::test_that("compute_lumi works with user polygon (backend r)", {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("sf")

  code_muni <- 2927408L

  # Read CNEFE data to create a test polygon
  tab <- testthat::with_mocked_bindings(
    cnefetools::read_cnefe(
      code_muni,
      verbose = FALSE,
      cache = TRUE,
      output = "arrow"
    ),
    .cnefe_ensure_zip = mock_ensure_zip_fixture,
    .package = "cnefetools"
  )

  df <- as.data.frame(tab) |>
    dplyr::transmute(
      LONGITUDE = suppressWarnings(as.numeric(.data$LONGITUDE)),
      LATITUDE = suppressWarnings(as.numeric(.data$LATITUDE)),
      COD_ESPECIE = suppressWarnings(as.integer(.data$COD_ESPECIE))
    ) |>
    dplyr::filter(
      !is.na(.data$LONGITUDE),
      !is.na(.data$LATITUDE),
      !is.na(.data$COD_ESPECIE),
      .data$COD_ESPECIE %in% 1L:8L,
      .data$COD_ESPECIE != 7L
    )

  # Create a bounding box polygon that covers all points
  bbox <- sf::st_bbox(
    sf::st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  )
  test_polygon <- sf::st_as_sfc(bbox) |>
    sf::st_sf(id = 1L, geometry = _)

  # Run compute_lumi with user polygon
  out <- testthat::with_mocked_bindings(
    suppressWarnings(
      cnefetools::compute_lumi(
        code_muni,
        polygon_type = "user",
        polygon = test_polygon,
        backend = "r",
        verbose = FALSE
      )
    ),
    .cnefe_ensure_zip = mock_ensure_zip_fixture,
    .package = "cnefetools"
  )

  testthat::expect_s3_class(out, "sf")
  testthat::expect_equal(nrow(out), 1L)

  # Check that LUMI columns exist
  lumi_cols <- c("p_res", "ei", "hhi", "bal", "ice", "hhi_adp", "bgbi")
  for (cc in lumi_cols) {
    testthat::expect_true(cc %in% names(out), info = paste("Missing column:", cc))
  }

  # Check that original polygon columns are preserved
  testthat::expect_true("id" %in% names(out))

  # Check that LUMI indices are within valid ranges
  testthat::expect_true(out$p_res >= 0 && out$p_res <= 1)
  testthat::expect_true(out$ei >= 0 && out$ei <= 1)
  testthat::expect_true(out$hhi >= 0 && out$hhi <= 1)

  # Check CRS matches original polygon (EPSG:4326)
  testthat::expect_equal(sf::st_crs(out)$epsg, 4326L)

  # Verify p_res matches manual computation
  n_res <- sum(df$COD_ESPECIE == 1L)
  n_tot <- nrow(df)
  expected_p_res <- n_res / n_tot
  testthat::expect_equal(out$p_res, expected_p_res, tolerance = 1e-12)
})


testthat::test_that("compute_lumi validates polygon argument", {
  testthat::skip_if_not_installed("sf")

  code_muni <- 2927408L

  # Error when polygon_type = "user" but polygon is NULL
  testthat::expect_error(
    cnefetools::compute_lumi(
      code_muni,
      polygon_type = "user",
      polygon = NULL,
      verbose = FALSE
    ),
    "polygon.*required"
  )

  # Error when polygon is not an sf object
  testthat::expect_error(
    cnefetools::compute_lumi(
      code_muni,
      polygon_type = "user",
      polygon = data.frame(x = 1),
      verbose = FALSE
    ),
    "polygon.*sf"
  )

  # Error when polygon contains non-polygon geometries
  pt_sf <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )
  testthat::expect_error(
    cnefetools::compute_lumi(
      code_muni,
      polygon_type = "user",
      polygon = pt_sf,
      verbose = FALSE
    ),
    "POLYGON|MULTIPOLYGON"
  )
})
