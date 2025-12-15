testthat::test_that("compute_lumi computes expected p_res on a tiny mocked example (backend r)", {

  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("h3jsr")
  testthat::skip_if_not_installed("dplyr")

  h3_res <- 9L

  # --- IMPORTANT ---
  # compute_lumi() sempre chama .cnefe_ensure_zip() e inspeciona o ZIP.
  # Para não baixar nada durante testes, exigimos que o ZIP já exista no cache.
  code_muni <- 2927408L  # Salvador (ajuste se preferir outro que você já tenha cacheado)
  cache_dir <- tools::R_user_dir("cnefetools", "cache")
  cached_zip <- list.files(
    cache_dir,
    pattern = sprintf("^%s_.*\\.zip$", code_muni),
    full.names = TRUE
  )

  testthat::skip_if(
    length(cached_zip) == 0L,
    "Este teste requer o ZIP do município no cache. Rode uma vez: read_cnefe(code_muni, cache = TRUE)."
  )

  # Mock do "arrow": compute_lumi(backend='r') faz as.data.frame(tab) e usa LONGITUDE/LATITUDE
  pts_df <- data.frame(
    COD_ESPECIE = c(1L, 2L, 1L, 8L, 7L, NA_integer_),  # 7 removido, NA removido
    LONGITUDE   = c(-38.5200, -38.5200, -38.5500, -38.5500, -38.5500, -38.5200),
    LATITUDE    = c(-3.7300,  -3.7300,  -3.7500,  -3.7500,  -3.7500,  -3.7300)
  )

  # IDs esperados (mesma chamada que a função usa: data.frame lon/lat -> point_to_cell)
  df_valid <- pts_df %>%
    dplyr::filter(
      !is.na(.data$COD_ESPECIE),
      .data$COD_ESPECIE %in% 1L:8L,
      .data$COD_ESPECIE != 7L,
      !is.na(.data$LONGITUDE),
      !is.na(.data$LATITUDE)
    )

  ids <- h3jsr::point_to_cell(
    df_valid %>% dplyr::transmute(lon = .data$LONGITUDE, lat = .data$LATITUDE),
    res = h3_res,
    simple = TRUE
  )

  expected <- df_valid %>%
    dplyr::mutate(id_hex = as.character(ids)) %>%
    dplyr::group_by(.data$id_hex) %>%
    dplyr::summarise(
      p_res = sum(.data$COD_ESPECIE == 1L) / dplyr::n(),
      .groups = "drop"
    )

  out <- testthat::with_mocked_bindings(
    {
      cnefetools::compute_lumi(
        code_muni     = code_muni,
        h3_resolution = h3_res,
        backend       = "r",
        verbose       = FALSE
      )
    },
    read_cnefe = function(...) pts_df,
    .package   = "cnefetools"
  )

  testthat::expect_s3_class(out, "sf")
  testthat::expect_true(all(
    c("id_hex", "p_res", "ei", "hhi", "hhi_adp", "bgbi", "geometry") %in% names(out)
  ))

  out_df <- sf::st_drop_geometry(out) %>%
    dplyr::select("id_hex", "p_res") %>%
    dplyr::inner_join(expected, by = "id_hex", suffix = c("", "_exp"))

  testthat::expect_equal(nrow(out_df), nrow(expected))
  testthat::expect_equal(out_df$p_res, out_df$p_res_exp, tolerance = 1e-12)
})
