test_that(".normalize_code_muni rejects invalid inputs", {
  expect_error(cnefetools:::.normalize_code_muni("abc"), "code_muni")
  expect_error(cnefetools:::.normalize_code_muni(123L),  "code_muni")
})

test_that("read_cnefe rejects codes not in internal index", {
  # 9999999 tem 7 dígitos, mas não existe no índice
  expect_error(
    cnefetools::read_cnefe(9999999L, verbose = FALSE),
    "not found|internal CNEFE index|Municipality code"
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
