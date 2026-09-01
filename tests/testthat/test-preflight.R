# The referee shipped test_url_fallback.R as a starting point but noted it is
# not production-ready, since it mutates internal package data and can trigger
# the 300/600/1800 second retry ladder. These tests mock the HTTP layer instead,
# so nothing touches the network and no retry is ever entered.

fake_resp <- function(status) {
  structure(
    list(status_code = status, url = "http://x", method = "HEAD",
         headers = list(), body = raw()),
    class = "httr2_response"
  )
}

URL <- "https://ftp.ibge.gov.br/a/b/2919207_X.zip"


testthat::test_that(".cnefe_preflight() passes a reachable URL (#91)", {
  testthat::local_mocked_bindings(
    req_perform = function(...) fake_resp(200L),
    .package = "httr2"
  )
  testthat::expect_true(cnefetools:::.cnefe_preflight(URL))
})


testthat::test_that(".cnefe_preflight() reports connectivity separately from 404 (#91)", {
  # Transport-level failure: no server reached at all.
  testthat::local_mocked_bindings(
    req_perform = function(...) rlang::abort("Could not resolve host"),
    .package = "httr2"
  )
  cnd <- tryCatch(cnefetools:::.cnefe_preflight(URL), error = function(e) e)

  testthat::expect_s3_class(cnd, "cnefetools_unreachable")
  msg <- paste(conditionMessage(cnd), collapse = " ")
  testthat::expect_match(msg, "Could not reach the IBGE server")
  testthat::expect_match(msg, "connectivity problem")
  testthat::expect_match(msg, "ftp.ibge.gov.br")
  # It must not blame the package for what is a network problem.
  testthat::expect_false(grepl("directory structure", msg))
})


testthat::test_that(".cnefe_preflight() blames the package on a 404 (#91)", {
  testthat::local_mocked_bindings(
    req_perform = function(...) fake_resp(404L),
    .package = "httr2"
  )
  cnd <- tryCatch(cnefetools:::.cnefe_preflight(URL), error = function(e) e)

  testthat::expect_s3_class(cnd, "cnefetools_not_found")
  msg <- paste(conditionMessage(cnd), collapse = " ")
  testthat::expect_match(msg, "404")
  testthat::expect_match(msg, "directory structure")
  # A 404 on a URL we generated is our problem, so the user gets the tracker.
  testthat::expect_match(msg, "github.com/pedreirajr/cnefetools/issues")
  testthat::expect_false(grepl("connectivity problem", msg))
})


testthat::test_that(".cnefe_preflight() reports other HTTP errors distinctly (#91)", {
  testthat::local_mocked_bindings(
    req_perform = function(...) fake_resp(503L),
    .package = "httr2"
  )
  cnd <- tryCatch(cnefetools:::.cnefe_preflight(URL), error = function(e) e)

  testthat::expect_s3_class(cnd, "cnefetools_http_error")
  testthat::expect_match(paste(conditionMessage(cnd), collapse = " "), "503")
})


testthat::test_that("a failing pre-flight aborts before the retry ladder (#91)", {
  # This is the practical win: the old code spent 300 + 600 + 1800 seconds
  # before giving up on a URL that could never resolve.
  attempts <- 0L
  testthat::local_mocked_bindings(
    req_perform = function(...) {
      attempts <<- attempts + 1L
      fake_resp(404L)
    },
    .package = "httr2"
  )

  testthat::expect_error(
    cnefetools:::.cnefe_download_zip_with_retry(
      url = URL,
      destfile = file.path(tempdir(), "preflight_test.zip"),
      retry_timeouts = c(1L, 1L, 1L),
      verbose = FALSE
    ),
    class = "cnefetools_not_found"
  )

  # Exactly one request: the probe. No download attempt was made.
  testthat::expect_identical(attempts, 1L)
})
