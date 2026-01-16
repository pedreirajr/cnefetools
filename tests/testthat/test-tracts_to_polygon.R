testthat::test_that("tracts_to_polygon validates polygon argument", {
  testthat::skip_if_not_installed("sf")

  code_muni <- 2929057L

  # Error when polygon is NULL
  testthat::expect_error(
    cnefetools::tracts_to_polygon(
      code_muni,
      polygon = NULL,
      verbose = FALSE
    ),
    "polygon.*required"
  )

  # Error when polygon is not an sf object
  testthat::expect_error(
    cnefetools::tracts_to_polygon(
      code_muni,
      polygon = data.frame(x = 1),
      verbose = FALSE
    ),
    "polygon.*sf"
  )

  # Error when polygon has invalid geometry type
  point_sf <- sf::st_as_sf(
    data.frame(x = -38.5, y = -12.9),
    coords = c("x", "y"),
    crs = 4326
  )
  testthat::expect_error(
    cnefetools::tracts_to_polygon(
      code_muni,
      polygon = point_sf,
      verbose = FALSE
    ),
    "POLYGON.*MULTIPOLYGON"
  )
})


testthat::test_that("tracts_to_polygon validates vars argument", {
  testthat::skip_if_not_installed("sf")

  code_muni <- 2929057L

  # Create a simple test polygon
  test_polygon <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-38.6, -13.0),
      c(-38.4, -13.0),
      c(-38.4, -12.8),
      c(-38.6, -12.8),
      c(-38.6, -13.0)
    ))),
    crs = 4326
  ) |>
    sf::st_sf(id = 1L, geometry = _)

  # Error when vars is empty
  testthat::expect_error(
    cnefetools::tracts_to_polygon(
      code_muni,
      polygon = test_polygon,
      vars = character(0),
      verbose = FALSE
    ),
    "vars.*must contain"
  )

  # Error when vars contains unknown variable
  testthat::expect_error(
    cnefetools::tracts_to_polygon(
      code_muni,
      polygon = test_polygon,
      vars = c("n_inhab_p", "invalid_var"),
      verbose = FALSE
    ),
    "Unknown.*vars"
  )
})


testthat::test_that("tracts_to_polygon validates crs_output argument", {
  testthat::skip_if_not_installed("sf")

  code_muni <- 2929057L

  # Create a simple test polygon
  test_polygon <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-38.6, -13.0),
      c(-38.4, -13.0),
      c(-38.4, -12.8),
      c(-38.6, -12.8),
      c(-38.6, -13.0)
    ))),
    crs = 4326
  ) |>
    sf::st_sf(id = 1L, geometry = _)

  # Error when crs_output is invalid
  testthat::expect_error(
    cnefetools::tracts_to_polygon(
      code_muni,
      polygon = test_polygon,
      crs_output = "invalid_crs",
      verbose = FALSE
    ),
    "crs_output.*not.*valid"
  )
})


testthat::test_that("tracts_to_polygon validates code_muni argument", {
  testthat::skip_if_not_installed("sf")

  # Create a simple test polygon
  test_polygon <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-38.6, -13.0),
      c(-38.4, -13.0),
      c(-38.4, -12.8),
      c(-38.6, -12.8),
      c(-38.6, -13.0)
    ))),
    crs = 4326
  ) |>
    sf::st_sf(id = 1L, geometry = _)

  # Error when code_muni is invalid (not 7 digits)
  testthat::expect_error(
    cnefetools::tracts_to_polygon(
      123,
      polygon = test_polygon,
      verbose = FALSE
    ),
    "code_muni.*7-digit"
  )

  # Error when code_muni has multiple values
  testthat::expect_error(
    cnefetools::tracts_to_polygon(
      c(2929057L, 2927408L),
      polygon = test_polygon,
      verbose = FALSE
    ),
    "code_muni.*single"
  )
})
