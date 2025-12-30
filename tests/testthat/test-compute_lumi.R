testthat::test_that("compute_lumi computes expected p_res on offline fixture (backend r)", {
  testthat::skip_if_not_installed("archive")
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
