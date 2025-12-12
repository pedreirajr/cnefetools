test_that("read_cnefe() fails for unknown municipality code", {
  fake_code <- 9999999L

  expect_error(
    read_cnefe(fake_code),
    "Municipality code not found in internal CNEFE index"
  )
})

test_that("read_cnefe() requires a single municipality code", {
  expect_error(
    read_cnefe(c(2927408, 3550308)),
    "`code_muni` must be a single value"
  )
})

test_that("read_cnefe() works for a real municipality (offline-safe)", {
  skip_on_cran()
  testthat::skip_if_offline()

  tab <- read_cnefe(2927408, cache = FALSE, verbose = FALSE)

  # Arrow Table S3 class name can be checked this way
  expect_s3_class(tab, "Table")
  expect_gt(tab$num_rows, 0)
})
