# Offline fixtures. Two formats are kept on purpose (#94):
#
#   .zip     what IBGE publishes, and what a cache written before #93 holds
#   .csv.gz  what the cache holds now, so the primary read path is exercised
#
# `format` defaults to "gz" because that is what users actually hit.

fixture_path <- function(format = c("gz", "zip")) {
  format <- match.arg(format)
  name <- if (format == "gz") "cnefe_fixture_cnefe.csv.gz" else "cnefe_fixture_cnefe.zip"

  p <- testthat::test_path(file.path("../inst/extdata", name))
  if (file.exists(p)) {
    return(p)
  }
  system.file("extdata", name, package = "cnefetools")
}

# Kept for the tests written before the two-format split.
fixture_zip_path <- function() fixture_path("zip")

#' Stand in for .cnefe_ensure_zip() without touching the network
#'
#' Takes `...` rather than naming the internal signature, so a new argument on
#' the real function cannot break every test that mocks it (#94).
mock_ensure_zip_fixture <- function(..., .format = "gz") {
  p <- fixture_path(.format)
  if (!nzchar(p) || !file.exists(p)) {
    rlang::abort(sprintf("Fixture (%s) not found.", .format))
  }
  list(zip_path = p, cleanup_zip = FALSE, url = "fixture")
}

#' The same, forced onto the legacy ZIP path
mock_ensure_zip_fixture_legacy <- function(...) {
  mock_ensure_zip_fixture(.format = "zip")
}
