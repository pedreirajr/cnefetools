fixture_zip_path <- function() {
  p <- testthat::test_path("../inst/extdata/cnefe_fixture_cnefe.zip")
  if (file.exists(p)) {
    return(p)
  }
  system.file("extdata", "cnefe_fixture_cnefe.zip", package = "cnefetools")
}

mock_ensure_zip_fixture <- function(...) {
  zip <- fixture_zip_path()
  if (!nzchar(zip) || !file.exists(zip)) {
    rlang::abort("Fixture ZIP not found.")
  }
  list(zip_path = zip, cleanup_zip = FALSE, url = "fixture")
}
