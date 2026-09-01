# Cache entries are segregated per CNEFE edition (#81). The published ZIP names
# carry no year, so 2919207_LAURO_DE_FREITAS.zip from a future census would be
# indistinguishable from the 2022 one and could be served silently in its place.

seed_two_editions <- function(root) {
  unlink(root, recursive = TRUE)
  for (y in c("2022", "2030")) {
    d <- file.path(root, y)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(d, "2919207_LAURO_DE_FREITAS.zip"))
    sc <- file.path(d, "sc_assets")
    dir.create(sc, showWarnings = FALSE)
    file.create(file.path(sc, "sc_29.parquet"))
  }
  root
}


testthat::test_that("cache paths are segregated by edition (#81)", {
  root <- "D:/alt"

  testthat::expect_identical(cnefetools:::.cnefe_cache_dir(root), path.expand(root))
  testthat::expect_identical(
    cnefetools:::.cnefe_cache_dir(root, 2022),
    file.path(path.expand(root), "2022")
  )
  testthat::expect_identical(
    cnefetools:::.sc_cache_dir(root, 2022),
    file.path(path.expand(root), "2022", "sc_assets")
  )
  testthat::expect_identical(
    cnefetools:::.sc_asset_local_path("29", root, 2022),
    file.path(path.expand(root), "2022", "sc_assets", "sc_29.parquet")
  )

  # Two editions cannot land on the same path, which is the whole point.
  testthat::expect_false(identical(
    cnefetools:::.cnefe_cache_dir(root, 2022),
    cnefetools:::.cnefe_cache_dir(root, 2030)
  ))
})


testthat::test_that("clear_cache_muni() can target one edition or all (#81)", {
  root <- seed_two_editions(file.path(tempdir(), "cby_muni"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  deleted <- clear_cache_muni(cache_dir = root, year = 2022, verbose = FALSE)
  testthat::expect_length(deleted, 1L)
  testthat::expect_true(file.exists(file.path(root, "2030", "2919207_LAURO_DE_FREITAS.zip")))
  testthat::expect_false(file.exists(file.path(root, "2022", "2919207_LAURO_DE_FREITAS.zip")))

  seed_two_editions(root)
  testthat::expect_length(clear_cache_muni(cache_dir = root, verbose = FALSE), 2L)
})


testthat::test_that("clear_cache_tracts() can target one edition or all (#81)", {
  root <- seed_two_editions(file.path(tempdir(), "cby_tracts"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  deleted <- clear_cache_tracts(cache_dir = root, year = 2030, verbose = FALSE)
  testthat::expect_length(deleted, 1L)
  testthat::expect_true(file.exists(file.path(root, "2022", "sc_assets", "sc_29.parquet")))

  seed_two_editions(root)
  testthat::expect_length(clear_cache_tracts(cache_dir = root, verbose = FALSE), 2L)
})


testthat::test_that("the cleaners accept an edition this version does not read (#81)", {
  # .validate_year() would reject 2030, but these functions delete directories
  # and must be able to reach a leftover from a newer version.
  testthat::expect_identical(cnefetools:::.cnefe_cache_year(2030), 2030L)
  testthat::expect_null(cnefetools:::.cnefe_cache_year(NULL))
  testthat::expect_error(cnefetools:::.cnefe_cache_year(c(1, 2)), "single value")
  testthat::expect_error(cnefetools:::.cnefe_cache_year("abc"), "coercible to an integer")
})
