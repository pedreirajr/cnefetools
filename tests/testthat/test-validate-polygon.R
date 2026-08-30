make_poly <- function(n = 1L) {
  g <- sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
      ncol = 2, byrow = TRUE
    ))),
    crs = 4326
  )
  out <- sf::st_sf(id = 1L, geometry = g)
  if (n == 0L) out[0, ] else out
}


testthat::test_that(".validate_polygon_arg() accepts a valid polygon", {
  p <- make_poly()
  testthat::expect_identical(cnefetools:::.validate_polygon_arg(p), p)
})


testthat::test_that(".validate_polygon_arg() rejects a zero-feature sf (#71)", {
  testthat::expect_error(
    cnefetools:::.validate_polygon_arg(make_poly(0L)),
    "at least one feature"
  )
})


testthat::test_that(".validate_polygon_arg() phrases the required message per caller", {
  testthat::expect_error(
    cnefetools:::.validate_polygon_arg(NULL),
    "`polygon` is required\\."
  )
  # The condition is passed as cli markup and must be expanded, not pasted in.
  cnd <- tryCatch(
    cnefetools:::.validate_polygon_arg(
      NULL,
      required_when = "{.arg polygon_type} is {.val user}"
    ),
    error = function(e) e
  )
  msg <- conditionMessage(cnd)
  testthat::expect_match(msg, "is required when")
  testthat::expect_false(grepl("{.arg", msg, fixed = TRUE))
})


testthat::test_that(".validate_polygon_arg() rejects non-sf and wrong geometry types", {
  testthat::expect_error(
    cnefetools:::.validate_polygon_arg(data.frame(a = 1)),
    "must be an <sf> object"
  )

  pts <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )
  testthat::expect_error(
    cnefetools:::.validate_polygon_arg(pts),
    "POLYGON or MULTIPOLYGON"
  )
})


testthat::test_that(".validate_polygon_arg() validates crs_output when given", {
  p <- make_poly()
  testthat::expect_error(
    cnefetools:::.validate_polygon_arg(p, crs_output = "not-a-crs"),
    "not a valid CRS"
  )
  testthat::expect_silent(cnefetools:::.validate_polygon_arg(p, crs_output = 31983))
})


testthat::test_that("the zero-feature guard reaches all three exported callers (#71)", {
  empty <- make_poly(0L)

  testthat::expect_error(
    cnefe_counts(2919207, polygon_type = "user", polygon = empty, verbose = FALSE),
    "at least one feature"
  )
  testthat::expect_error(
    compute_lumi(2919207, polygon_type = "user", polygon = empty, verbose = FALSE),
    "at least one feature"
  )
  testthat::expect_error(
    tracts_to_polygon(2919207, polygon = empty, vars = "pop_ph", verbose = FALSE),
    "at least one feature"
  )
})
