test_that("read_cnefe() falha para códigos inválidos", {
  expect_error(read_cnefe("abc"), "must be coercible")
  expect_error(read_cnefe(9999999L), "not found in internal CNEFE index")
})

test_that("read_cnefe() baixa dados de um município real (teste opcional online)", {
  # Nunca roda no CRAN
  testthat::skip_on_cran()
  # Pula se não tiver internet
  testthat::skip_if_offline(host = "ftp.ibge.gov.br")
  # E, principalmente, só roda se você explicitamente habilitar:
  testthat::skip_if_not(
    identical(Sys.getenv("CNEFETOOLS_RUN_DOWNLOAD_TESTS"), "true"),
    "Live download tests are disabled by default."
  )

  tab <- read_cnefe(2927408, cache = FALSE, verbose = FALSE)

  # Arrow Table
  testthat::expect_s3_class(tab, "Table")
  testthat::expect_true(all(c("LONGITUDE", "LATITUDE") %in% names(tab)))
})
