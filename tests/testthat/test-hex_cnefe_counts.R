testthat::test_that("hex_cnefe_counts works offline using ZIP fixture (backend r)", {
  testthat::skip_if_not_installed("archive")
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("h3jsr")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("tidyr")

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
    cnefetools::hex_cnefe_counts(
      code_muni,
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
