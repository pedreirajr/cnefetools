# clear_cache_muni() and the cache format (#93).
#
# #93 changed the download cache from the published ZIP to a gzipped CSV, but
# clear_cache_muni() kept globbing for "\\.zip$". The function became a silent
# no-op: it reported "No cached CNEFE ZIP files found" while every current entry
# sat in the cache undeleted, and returned an empty vector, so nothing looked
# wrong. Users had no working way to clear the cache.
#
# The existing #81 tests did not catch it because they seed .zip fixtures, which
# is exactly the format that still worked.
#
# These tests seed both formats. No network, nothing skipped on CRAN.

seed_cache <- function(root, files) {
  unlink(root, recursive = TRUE)
  d <- file.path(root, "2022")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (f in files) {
    writeLines("x", file.path(d, f))
  }
  # A tract asset, which belongs to clear_cache_tracts() and must survive.
  sc <- file.path(d, "sc_assets")
  dir.create(sc, showWarnings = FALSE)
  writeLines("x", file.path(sc, "sc_29.parquet"))
  root
}


testthat::test_that("clear_cache_muni() deletes the gzipped CSV cache (#93)", {
  root <- seed_cache(
    file.path(tempdir(), "ccgz_current"),
    c("2919207_LAURO_DE_FREITAS.csv.gz", "3550308_SAO_PAULO.csv.gz")
  )
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  deleted <- clear_cache_muni(cache_dir = root, verbose = FALSE)

  testthat::expect_length(deleted, 2L)
  testthat::expect_false(
    file.exists(file.path(root, "2022", "2919207_LAURO_DE_FREITAS.csv.gz"))
  )
  testthat::expect_false(
    file.exists(file.path(root, "2022", "3550308_SAO_PAULO.csv.gz"))
  )
})


testthat::test_that("clear_cache_muni() still deletes pre-0.3.0 ZIP caches", {
  root <- seed_cache(
    file.path(tempdir(), "ccgz_legacy"),
    c("2919207_LAURO_DE_FREITAS.zip")
  )
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  deleted <- clear_cache_muni(cache_dir = root, verbose = FALSE)

  testthat::expect_length(deleted, 1L)
  testthat::expect_false(
    file.exists(file.path(root, "2022", "2919207_LAURO_DE_FREITAS.zip"))
  )
})


testthat::test_that("clear_cache_muni() clears a mixed cache and spares Parquet", {
  root <- seed_cache(
    file.path(tempdir(), "ccgz_mixed"),
    c("2919207_LAURO_DE_FREITAS.csv.gz", "3550308_SAO_PAULO.zip")
  )
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  deleted <- clear_cache_muni(cache_dir = root, verbose = FALSE)

  testthat::expect_length(deleted, 2L)
  # Census tract assets are clear_cache_tracts()'s business, so the recursive
  # walk must not take them with it.
  testthat::expect_true(
    file.exists(file.path(root, "2022", "sc_assets", "sc_29.parquet"))
  )
})


testthat::test_that("clear_cache_muni() targets one municipality by code", {
  root <- seed_cache(
    file.path(tempdir(), "ccgz_one"),
    c("2919207_LAURO_DE_FREITAS.csv.gz", "3550308_SAO_PAULO.csv.gz")
  )
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  deleted <- clear_cache_muni(2919207, cache_dir = root, verbose = FALSE)

  testthat::expect_length(deleted, 1L)
  testthat::expect_false(
    file.exists(file.path(root, "2022", "2919207_LAURO_DE_FREITAS.csv.gz"))
  )
  testthat::expect_true(
    file.exists(file.path(root, "2022", "3550308_SAO_PAULO.csv.gz"))
  )
})


testthat::test_that("clear_cache_muni() reports an empty cache without claiming a format", {
  root <- file.path(tempdir(), "ccgz_empty")
  unlink(root, recursive = TRUE)
  dir.create(file.path(root, "2022"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  testthat::expect_message(
    deleted <- clear_cache_muni(cache_dir = root, verbose = TRUE),
    "No cached CNEFE files found"
  )
  testthat::expect_length(deleted, 0L)
})
