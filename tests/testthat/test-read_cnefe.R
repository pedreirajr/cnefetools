testthat::test_that("read_cnefe() falha para códigos inválidos", {
  testthat::expect_error(
    cnefetools::read_cnefe("abc"),
    "must be coercible"
  )

  testthat::expect_error(
    cnefetools::read_cnefe(9999999L),
    "not found in internal CNEFE index"
  )
})

testthat::test_that("read_cnefe() baixa dados de um município real (teste opcional online)", {
  testthat::skip_on_cran()
  testthat::skip_if_offline(host = "ftp.ibge.gov.br")

  testthat::skip_if_not(
    identical(Sys.getenv("CNEFETOOLS_RUN_DOWNLOAD_TESTS"), "true"),
    "Live download tests are disabled by default."
  )

  tab <- cnefetools::read_cnefe(2927408, cache = FALSE, verbose = FALSE)

  testthat::expect_s3_class(tab, "Table")
  testthat::expect_true(all(c("LONGITUDE", "LATITUDE") %in% names(tab)))
})
