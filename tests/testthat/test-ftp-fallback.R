# The scanner is exercised entirely offline against a miniature Apache autoindex
# fixture, so no test touches the IBGE server. The real-server behaviour was
# verified by hand while implementing (#92).

autoindex <- function(entries) {
  paste0(
    "<html><head><title>Index of /x</title></head><body><h1>Index of /x</h1><pre>",
    '<a href="?C=N;O=D">Name</a>',
    '<a href="/parent/">Parent Directory</a>',
    paste0(sprintf('<a href="%s">%s</a>', entries, entries), collapse = ""),
    "</pre></body></html>"
  )
}

BASE <- "https://ftp.ibge.gov.br/a/CSV/Municipio/"
KNOWN <- paste0(BASE, "29_BA/2919207_LAURO_DE_FREITAS.zip")

# Serve one canned body per URL, and 404 anything else.
serve <- function(map) {
  function(req, ...) {
    body <- map[[req$url]]
    if (is.null(body)) {
      structure(
        list(status_code = 404L, url = req$url, method = "GET",
             headers = list(), body = raw()),
        class = "httr2_response"
      )
    } else {
      structure(
        list(status_code = 200L, url = req$url, method = "GET",
             headers = list(`content-type` = "text/html"),
             body = charToRaw(body)),
        class = "httr2_response"
      )
    }
  }
}


testthat::test_that(".cnefe_scan_ftp_url() recovers a renamed file in place (#92)", {
  testthat::local_mocked_bindings(
    req_perform = serve(list(
      "https://ftp.ibge.gov.br/a/CSV/Municipio/29_BA/" =
        autoindex(c("2919207_LAURO_DE_FREITAS_V2.zip", "2927408_SALVADOR.zip"))
    )),
    .package = "httr2"
  )

  got <- cnefetools:::.cnefe_scan_ftp_url(2919207, KNOWN, verbose = FALSE)
  testthat::expect_identical(
    got,
    paste0(BASE, "29_BA/2919207_LAURO_DE_FREITAS_V2.zip")
  )
})


testthat::test_that(".cnefe_scan_ftp_url() walks up when the UF directory moved (#92)", {
  testthat::local_mocked_bindings(
    req_perform = serve(list(
      # The old UF directory is gone; the parent lists a renamed one.
      "https://ftp.ibge.gov.br/a/CSV/Municipio/" =
        autoindex(c("29_BAHIA/", "35_SP/", "readme.txt")),
      "https://ftp.ibge.gov.br/a/CSV/Municipio/29_BAHIA/" =
        autoindex(c("2919207_LAURO_DE_FREITAS.zip"))
    )),
    .package = "httr2"
  )

  got <- cnefetools:::.cnefe_scan_ftp_url(2919207, KNOWN, verbose = FALSE)
  testthat::expect_identical(
    got,
    paste0(BASE, "29_BAHIA/2919207_LAURO_DE_FREITAS.zip")
  )
})


testthat::test_that(".cnefe_scan_ftp_url() returns NULL when nothing matches (#92)", {
  testthat::local_mocked_bindings(
    req_perform = serve(list(
      "https://ftp.ibge.gov.br/a/CSV/Municipio/29_BA/" =
        autoindex(c("2927408_SALVADOR.zip")),
      "https://ftp.ibge.gov.br/a/CSV/Municipio/" = autoindex(c("29_BA/"))
    )),
    .package = "httr2"
  )

  testthat::expect_null(
    cnefetools:::.cnefe_scan_ftp_url(2919207, KNOWN, verbose = FALSE)
  )
})


testthat::test_that(".cnefe_scan_ftp_url() ignores absolute and navigation links (#92)", {
  # Apache autoindex pages carry sort links and a parent link that must not be
  # mistaken for content.
  testthat::local_mocked_bindings(
    req_perform = serve(list(
      "https://ftp.ibge.gov.br/a/CSV/Municipio/29_BA/" =
        autoindex(c("2919207_X.zip"))
    )),
    .package = "httr2"
  )

  got <- cnefetools:::.cnefe_scan_ftp_url(2919207, KNOWN, verbose = FALSE)
  testthat::expect_identical(got, paste0(BASE, "29_BA/2919207_X.zip"))
})


testthat::test_that(".cnefe_scan_ftp_url() survives an unreachable server (#92)", {
  testthat::local_mocked_bindings(
    req_perform = function(...) rlang::abort("Could not resolve host"),
    .package = "httr2"
  )

  testthat::expect_null(
    cnefetools:::.cnefe_scan_ftp_url(2919207, KNOWN, verbose = FALSE)
  )
})
