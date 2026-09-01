testthat::test_that(".cnefe_cache_dir() resolves argument, then env var, then default (#89)", {
  alt <- file.path(tempdir(), "cache_dir_test_alt")
  env <- file.path(tempdir(), "cache_dir_test_env")

  withr::local_envvar(c(CNEFETOOLS_CACHE_DIR = ""))
  default <- cnefetools:::.cnefe_cache_dir()
  testthat::expect_match(default, "cnefetools")

  withr::local_envvar(c(CNEFETOOLS_CACHE_DIR = env))
  testthat::expect_identical(cnefetools:::.cnefe_cache_dir(), path.expand(env))

  # The argument outranks the environment variable.
  testthat::expect_identical(cnefetools:::.cnefe_cache_dir(alt), path.expand(alt))
})


testthat::test_that(".cnefe_cache_dir() rejects malformed input (#89)", {
  testthat::expect_error(cnefetools:::.cnefe_cache_dir(c("a", "b")), "single directory path")
  testthat::expect_error(cnefetools:::.cnefe_cache_dir(1L), "single directory path")
  testthat::expect_error(cnefetools:::.cnefe_cache_dir(NA_character_), "single directory path")
  testthat::expect_error(cnefetools:::.cnefe_cache_dir(""), "must not be an empty string")
})


testthat::test_that("the census tract cache derives from the resolved directory (#89)", {
  alt <- file.path(tempdir(), "cache_dir_test_sc")

  testthat::expect_identical(
    cnefetools:::.sc_cache_dir(alt),
    file.path(path.expand(alt), "sc_assets")
  )
  testthat::expect_identical(
    cnefetools:::.sc_asset_local_path("29", alt),
    file.path(path.expand(alt), "sc_assets", "sc_29.parquet")
  )
})


testthat::test_that("every function that caches exposes cache_dir (#89)", {
  # clear_cache_*() matter as much as the readers: a redirected cache that the
  # cleaners cannot see would report success while deleting nothing.
  for (fn in c(
    "read_cnefe", "cnefe_counts", "compute_lumi",
    "tracts_to_h3", "tracts_to_polygon",
    "clear_cache_muni", "clear_cache_tracts"
  )) {
    testthat::expect_true(
      "cache_dir" %in% names(formals(getExportedValue("cnefetools", fn))),
      info = fn
    )
  }
})
