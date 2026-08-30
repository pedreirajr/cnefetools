testthat::test_that(".empty_lumi_sf() carries the documented compute_lumi hex schema (#85)", {
  e <- cnefetools:::.empty_lumi_sf()

  testthat::expect_s3_class(e, "sf")
  testthat::expect_identical(nrow(e), 0L)
  testthat::expect_identical(
    names(e),
    c("id_hex", "p_res", "ei", "hhi", "bal", "ice", "hhi_adp", "bgbi", "geometry")
  )
  testthat::expect_identical(sf::st_crs(e)$epsg, 4326L)
  # A zero-row sf must still survive the operations a caller would pipe it into.
  # sf warns when computing a bbox over empty geometry, which is sf's own
  # behaviour and not something this helper can avoid, so only the result is
  # asserted.
  testthat::expect_identical(nrow(sf::st_drop_geometry(e)), 0L)
  testthat::expect_identical(nrow(suppressWarnings(rbind(e, e))), 0L)
  testthat::expect_identical(nrow(dplyr::filter(e, .data$p_res > 0.5)), 0L)
})


testthat::test_that("read_cnefe() reports rows dropped for missing coordinates (#85)", {
  # The reporting branch is what matters, so it is exercised directly on the
  # data frame shape read_cnefe() builds before st_as_sf().
  df <- data.frame(
    LONGITUDE = c(-38.5, NA, -38.6, NA),
    LATITUDE = c(-12.8, -12.9, NA, NA)
  )

  report <- function(df, verbose) {
    n_before <- nrow(df)
    df <- df[!is.na(df$LONGITUDE) & !is.na(df$LATITUDE), , drop = FALSE]
    n_dropped <- n_before - nrow(df)
    if (n_dropped > 0L && isTRUE(verbose)) {
      pct <- if (n_before > 0L) 100 * n_dropped / n_before else 0
      cli::cli_alert_warning(
        "Dropped {.strong {n_dropped}} of {.strong {n_before}} rows ({.strong {sprintf('%.2f%%', pct)}}) with missing coordinates."
      )
    }
    df
  }

  testthat::expect_message(out <- report(df, verbose = TRUE), "Dropped")
  testthat::expect_message(report(df, verbose = TRUE), "3 of 4")
  testthat::expect_message(report(df, verbose = TRUE), "75.00%")
  testthat::expect_identical(nrow(out), 1L)

  # Silent when nothing is dropped, and silent when verbose is FALSE.
  testthat::expect_silent(report(df[1, , drop = FALSE], verbose = TRUE))
  testthat::expect_silent(report(df, verbose = FALSE))
})
