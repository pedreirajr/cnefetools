testthat::test_that(".normalize_code_muni validates inputs", {
  testthat::expect_error(
    cnefetools:::.normalize_code_muni(c(2927408, 2919207)),
    "single value"
  )

  testthat::expect_error(
    cnefetools:::.normalize_code_muni("abcdefg"),
    "valid 7-digit IBGE code"
  )

  testthat::expect_error(
    cnefetools:::.normalize_code_muni("123"),
    "valid 7-digit IBGE code"
  )

  testthat::expect_equal(
    cnefetools:::.normalize_code_muni("2927408"),
    2927408L
  )

  testthat::expect_equal(
    cnefetools:::.normalize_code_muni(2927408),
    2927408L
  )
})

testthat::test_that("read_cnefe reads from internal ZIP fixture (offline) as Arrow table", {
  testthat::skip_if_not_installed("archive")
  testthat::skip_if_not_installed("arrow")

  code_muni <- 2927408L

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

  testthat::expect_s3_class(tab, "Table")
  testthat::expect_true(all(
    c("LONGITUDE", "LATITUDE", "COD_ESPECIE") %in% names(tab)
  ))
})

testthat::test_that("read_cnefe reads from internal ZIP fixture (offline) as sf", {
  testthat::skip_if_not_installed("archive")
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("sf")

  code_muni <- 2927408L

  sfobj <- testthat::with_mocked_bindings(
    cnefetools::read_cnefe(
      code_muni,
      verbose = FALSE,
      cache = TRUE,
      output = "sf"
    ),
    .cnefe_ensure_zip = mock_ensure_zip_fixture,
    .package = "cnefetools"
  )

  testthat::expect_s3_class(sfobj, "sf")
  testthat::expect_true(sf::st_is_longlat(sfobj))
  testthat::expect_true("geometry" %in% names(sfobj))
})
