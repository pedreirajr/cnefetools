testthat::test_that("cnefe_counts works offline using ZIP fixture (backend r, polygon_type hex)", {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("h3jsr")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("tidyr")

  code_muni <- 2929057L
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
      .data$COD_ESPECIE %in% 1L:8L
    )

  ids <- suppressMessages(
    h3jsr::point_to_cell(
      df |>
        dplyr::transmute(
          lon = .data$LONGITUDE,
          lat = .data$LATITUDE
        ),
      res = h3_res,
      simple = TRUE
    )
  )

  counts_long <- df |>
    dplyr::mutate(id_hex = as.character(ids)) |>
    dplyr::count(.data$id_hex, .data$COD_ESPECIE, name = "n")

  expected <- counts_long |>
    dplyr::mutate(col = paste0("addr_type", .data$COD_ESPECIE)) |>
    dplyr::select("id_hex", "col", "n") |>
    tidyr::pivot_wider(
      names_from = "col",
      values_from = "n",
      values_fill = 0L
    )

  cols <- paste0("addr_type", 1:8)
  for (cc in cols) {
    if (!cc %in% names(expected)) expected[[cc]] <- 0L
  }
  expected <- expected |>
    dplyr::select("id_hex", dplyr::all_of(cols))

  # grid only for observed ids (no geobr dependency)
  hex_grid <- cnefetools:::build_h3_grid(
    h3_resolution = h3_res,
    id_hex = expected$id_hex
  )

  out <- testthat::with_mocked_bindings(
    cnefetools::cnefe_counts(
      code_muni,
      polygon_type = "hex",
      h3_resolution = h3_res,
      backend = "r",
      verbose = FALSE
    ),
    .cnefe_ensure_zip = mock_ensure_zip_fixture,
    build_h3_grid = function(...) hex_grid,
    .package = "cnefetools"
  )

  testthat::expect_s3_class(out, "sf")

  out_df <- sf::st_drop_geometry(out) |>
    dplyr::select("id_hex", dplyr::all_of(cols)) |>
    dplyr::inner_join(expected, by = "id_hex", suffix = c("", "_exp"))

  testthat::expect_true(nrow(out_df) > 0L)

  for (cc in cols) {
    x <- suppressWarnings(as.integer(out_df[[cc]]))
    y <- suppressWarnings(as.integer(out_df[[paste0(cc, "_exp")]]))
    x[is.na(x)] <- 0L
    y[is.na(y)] <- 0L
    testthat::expect_equal(x, y, tolerance = 0)
  }
})


testthat::test_that("cnefe_counts works with user polygon (backend r, polygon_type user)", {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("tidyr")

  code_muni <- 2929057L

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
      .data$COD_ESPECIE %in% 1L:8L
    )

  # Create a simple bounding box polygon that covers all points
  bbox <- sf::st_bbox(
    sf::st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  )
  test_polygon <- sf::st_as_sfc(bbox) |>
    sf::st_sf(id = 1L, geometry = _)

  # Run cnefe_counts with user polygon
  out <- testthat::with_mocked_bindings(
    suppressWarnings(
      cnefetools::cnefe_counts(
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

  # Check that addr_type columns exist
  cols <- paste0("addr_type", 1:8)
  for (cc in cols) {
    testthat::expect_true(cc %in% names(out))
  }

  # Check that counts are non-negative integers
  for (cc in cols) {
    testthat::expect_true(all(out[[cc]] >= 0L))
  }

  # Check that total counts match expected from raw data
  expected_total <- nrow(df)
  actual_total <- sum(out$addr_type1, out$addr_type2, out$addr_type3,
                      out$addr_type4, out$addr_type5, out$addr_type6,
                      out$addr_type7, out$addr_type8)
  testthat::expect_equal(actual_total, expected_total)

  # Check CRS is EPSG:4326

  testthat::expect_equal(sf::st_crs(out)$epsg, 4326L)
})


testthat::test_that("cnefe_counts validates polygon argument", {
  testthat::skip_if_not_installed("sf")

  code_muni <- 2929057L

  # Error when polygon_type = "user" but polygon is NULL
  testthat::expect_error(
    cnefetools::cnefe_counts(
      code_muni,
      polygon_type = "user",
      polygon = NULL,
      verbose = FALSE
    ),
    "polygon.*required"
  )

  # Error when polygon is not an sf object
  testthat::expect_error(
    cnefetools::cnefe_counts(
      code_muni,
      polygon_type = "user",
      polygon = data.frame(x = 1),
      verbose = FALSE
    ),
    "polygon.*sf"
  )
})
