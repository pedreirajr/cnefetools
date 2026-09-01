# lifecycle warns once per session by default, so the verbosity option is forced
# in every test that asserts on the warning.

testthat::test_that(".resolve_polygon_mode() infers the mode without polygon_type (#90)", {
  poly <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  # The new idiom is silent: no deprecation warning either way.
  withr::local_options(lifecycle_verbosity = "warning")
  testthat::expect_silent(
    m <- cnefetools:::.resolve_polygon_mode(NULL, fn = "cnefe_counts")
  )
  testthat::expect_identical(m, "hex")

  testthat::expect_silent(
    m <- cnefetools:::.resolve_polygon_mode(poly, fn = "cnefe_counts")
  )
  testthat::expect_identical(m, "user")
})


testthat::test_that(".resolve_polygon_mode() warns but still honours polygon_type (#90)", {
  withr::local_options(lifecycle_verbosity = "warning")
  poly <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  testthat::expect_warning(
    m <- cnefetools:::.resolve_polygon_mode(poly, "user", fn = "cnefe_counts"),
    "deprecated"
  )
  testthat::expect_identical(m, "user")

  # polygon_type = "hex" alongside a polygon resolves to user, matching the
  # inference the function used to perform with three alert lines.
  testthat::expect_warning(
    m <- cnefetools:::.resolve_polygon_mode(poly, "hex", fn = "cnefe_counts"),
    "deprecated"
  )
  testthat::expect_identical(m, "user")

  testthat::expect_warning(
    m <- cnefetools:::.resolve_polygon_mode(NULL, "hex", fn = "cnefe_counts"),
    "deprecated"
  )
  testthat::expect_identical(m, "hex")
})


testthat::test_that("polygon_type = 'user' without a polygon is still an error (#90)", {
  withr::local_options(lifecycle_verbosity = "quiet")
  testthat::expect_error(
    cnefetools:::.resolve_polygon_mode(NULL, "user", fn = "compute_lumi"),
    "is required when"
  )
})


testthat::test_that("polygon_type is a deprecated formal on both exported functions (#90)", {
  for (fn in c("cnefe_counts", "compute_lumi")) {
    f <- formals(getExportedValue("cnefetools", fn))
    testthat::expect_true("polygon_type" %in% names(f), info = fn)
    # The default is the lifecycle sentinel, not the old character vector.
    testthat::expect_true(
      grepl("deprecated", paste(deparse(f$polygon_type), collapse = "")),
      info = fn
    )
  }
})
