# cnefe_dictionary() and cnefe_doc() were the only exported functions with no
# tests at all, at 0% coverage (#80 R2.C8). Both end in utils::browseURL(),
# which would open a spreadsheet or a PDF viewer, so that call is mocked away.

testthat::test_that("cnefe_dictionary() returns the bundled file and opens it", {
  opened <- NULL
  testthat::local_mocked_bindings(
    browseURL = function(url, ...) {
      opened <<- url
      invisible(TRUE)
    },
    .package = "utils"
  )

  path <- cnefe_dictionary()

  testthat::expect_type(path, "character")
  testthat::expect_true(file.exists(path))
  testthat::expect_match(basename(path), "^cnefe_dictionary_2022[.]xls$")
  # The path returned is the one handed to the viewer.
  testthat::expect_identical(opened, path)
})


testthat::test_that("cnefe_doc() returns the bundled note and opens it", {
  opened <- NULL
  testthat::local_mocked_bindings(
    browseURL = function(url, ...) {
      opened <<- url
      invisible(TRUE)
    },
    .package = "utils"
  )

  path <- cnefe_doc()

  testthat::expect_type(path, "character")
  testthat::expect_true(file.exists(path))
  testthat::expect_match(basename(path), "^cnefe_metodologica_2022[.]pdf$")
  testthat::expect_identical(opened, path)
})


testthat::test_that("both return the path invisibly", {
  testthat::local_mocked_bindings(
    browseURL = function(url, ...) invisible(TRUE),
    .package = "utils"
  )

  testthat::expect_invisible(cnefe_dictionary())
  testthat::expect_invisible(cnefe_doc())
})


testthat::test_that("both reject an unsupported edition before touching the disk", {
  testthat::local_mocked_bindings(
    browseURL = function(url, ...) {
      testthat::fail("browseURL() must not be reached for an invalid year")
    },
    .package = "utils"
  )

  testthat::expect_error(cnefe_dictionary(year = 2030), "not available")
  testthat::expect_error(cnefe_doc(year = 2030), "not available")
  testthat::expect_error(cnefe_dictionary(year = c(2022, 2030)), "single value")
})
