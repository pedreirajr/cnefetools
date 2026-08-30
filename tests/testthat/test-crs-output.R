# crs_output behaviour, not just its validation (#80 R2.C8). The referee noted
# that only the rejection of an invalid CRS was covered, never what the argument
# actually does to the result.
#
# These run on the offline fixture with the pure-R backend, so they need no
# DuckDB extension and therefore no skip_on_cran().

test_poly <- function(crs = 4326) {
  p <- sf::st_sf(
    zone = c("a", "b"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(-38.60, -3.80, -38.55, -3.80, -38.55, -3.70, -38.60, -3.70, -38.60, -3.80),
        ncol = 2, byrow = TRUE
      ))),
      sf::st_polygon(list(matrix(
        c(-38.55, -3.80, -38.50, -3.80, -38.50, -3.70, -38.55, -3.70, -38.55, -3.80),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )
  if (crs != 4326) p <- sf::st_transform(p, crs)
  p
}

run <- function(fn, polygon, ...) {
  testthat::with_mocked_bindings(
    suppressWarnings(
      getExportedValue("cnefetools", fn)(
        2927408L,
        polygon = polygon,
        backend = "r",
        verbose = FALSE,
        ...
      )
    ),
    .cnefe_ensure_zip = mock_ensure_zip_fixture,
    .package = "cnefetools"
  )
}


testthat::test_that("crs_output reprojects the result without changing the counts", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("arrow")

  for (fn in c("cnefe_counts", "compute_lumi")) {
    base <- run(fn, test_poly())
    proj <- run(fn, test_poly(), crs_output = 31983)

    testthat::expect_identical(sf::st_crs(base)$epsg, 4326L, info = fn)
    testthat::expect_identical(sf::st_crs(proj)$epsg, 31983L, info = fn)
    testthat::expect_identical(nrow(base), nrow(proj), info = fn)

    # Reprojection moves geometry, never values.
    value_col <- if (fn == "cnefe_counts") "addr_type1" else "p_res"
    testthat::expect_equal(
      sf::st_drop_geometry(base)[[value_col]],
      sf::st_drop_geometry(proj)[[value_col]],
      info = fn
    )
  }
})


testthat::test_that("without crs_output the input CRS is preserved", {
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("arrow")

  # A projected input must come back projected, not silently in 4326.
  out <- run("cnefe_counts", test_poly(crs = 31983))
  testthat::expect_identical(sf::st_crs(out)$epsg, 31983L)
})


testthat::test_that("an invalid crs_output is rejected before any work is done", {
  testthat::skip_if_not_installed("sf")

  for (fn in c("cnefe_counts", "compute_lumi")) {
    testthat::expect_error(
      getExportedValue("cnefetools", fn)(
        2927408L,
        polygon = test_poly(),
        crs_output = "not-a-crs",
        verbose = FALSE
      ),
      "not a valid CRS",
      info = fn
    )
  }
})
