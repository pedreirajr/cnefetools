testthat::test_that("compute_lumi computes expected p_res on a tiny mocked example", {

  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("h3jsr")
  testthat::skip_if_not_installed("dplyr")

  h3_res <- 9L

  pts_df <- data.frame(
    COD_ESPECIE = c(1L, 2L, 1L, 8L, 7L, NA_integer_),  # 7 removed, NA removed
    lon = c(-38.5200, -38.5200, -38.5500, -38.5500, -38.5500, -38.5200),
    lat = c(-3.7300,  -3.7300,  -3.7500,  -3.7500,  -3.7500,  -3.7300)
  )

  # read_cnefe(output="sf") returns CRS 4674; compute_lumi transforms to 4326
  pts_sf <- sf::st_as_sf(pts_df, coords = c("lon", "lat"), crs = 4674)
  pts_for_h3 <- sf::st_transform(pts_sf, 4326)

  id_a <- h3jsr::point_to_cell(pts_for_h3[1, ], res = h3_res, simple = TRUE)[1]
  id_b <- h3jsr::point_to_cell(pts_for_h3[3, ], res = h3_res, simple = TRUE)[1]

  expected <- data.frame(
    id_hex = c(as.character(id_a), as.character(id_b)),
    p_res  = c(0.5, 0.5)
  )

  out <- testthat::with_mocked_bindings(
    {
      cnefetools::compute_lumi(
        code_muni     = 2929057,  # precisa existir no índice real
        h3_resolution = h3_res,
        verbose       = FALSE
      )
    },
    read_cnefe = function(...) pts_sf,
    .package   = "cnefetools"
  )

  # CRITICAL: ensure mocks were reverted
  testthat::expect_error(
    cnefetools::read_cnefe("abc"),
    "must be coercible"
  )

  testthat::expect_s3_class(out, "sf")
  testthat::expect_true(all(
    c("id_hex", "p_res", "ei", "hhi", "hhi_adp", "bgbi", "geometry") %in% names(out)
  ))

  out_df <- sf::st_drop_geometry(out) %>%
    dplyr::select("id_hex", "p_res") %>%
    dplyr::inner_join(expected, by = "id_hex", suffix = c("", "_exp"))

  testthat::expect_equal(nrow(out_df), 2L)
  testthat::expect_equal(out_df$p_res, out_df$p_res_exp, tolerance = 1e-12)
})
