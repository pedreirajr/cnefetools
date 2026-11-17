# p_target, p_scope, optional code_muni in polygon mode, and the report of
# addresses that fall outside the H3 grid (#120, R2 comment 1).
#
# The synthetic municipalities A and B, the runners and the geobr stand-ins are
# in helper-multi-muni.R.

both <- c(multi_muni$code_a, multi_muni$code_b)

# Residential share of all records of A and B, type 7 excluded: 34 private
# households among 93 addresses.
P_both <- 34 / 93

# A small square around the interior points of A (10 private households, 3
# schools and 1 building under construction), far from the border with B.
poly_in_a <- local({
  x <- multi_muni$lon0 - 0.005
  y <- multi_muni$lat0 + 0.003
  d <- 0.001
  sf::st_sf(
    zone = "inside A",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(x - d, y - d, x + d, y - d, x + d, y + d, x - d, y + d, x - d, y - d),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )
})

# A large square that covers both municipalities.
poly_region <- local({
  x <- multi_muni$lon0
  y <- multi_muni$lat0
  d <- 0.02
  sf::st_sf(
    zone = "region",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(x - d, y - d, x + d, y - d, x + d, y + d, x - d, y + d, x - d, y - d),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )
})

# A with 3 private households geocoded 0.04 degrees west of its boundary, well
# beyond the ring of border cells, so their H3 cell is not in the grid.
records_a_outside <- rbind(
  multi_muni$records_a,
  data.frame(
    COD_ESPECIE = rep(1L, 3),
    LONGITUDE = rep(multi_muni$lon0 - 0.05, 3),
    LATITUDE = rep(multi_muni$lat0, 3)
  )
)

lumi_cols <- c("p_res", "ei", "hhi", "bal", "ice", "hhi_adp", "bgbi")

# --- Validation ----------------------------------------------------------------

testthat::test_that("p_target must be one number strictly between 0 and 1 (#120)", {
  f <- cnefetools:::.validate_p_target
  testthat::expect_null(f(NULL))
  testthat::expect_equal(f(0.7), 0.7)
  testthat::expect_equal(f(1L / 3L), 1 / 3)
  for (bad in list(0, 1, -0.1, 1.2, NA_real_, "0.5", c(0.3, 0.4), numeric(0))) {
    testthat::expect_error(f(bad), "strictly between 0 and 1")
  }
})

testthat::test_that("p_scope takes muni or poly and cannot be combined with p_target (#120)", {
  f <- cnefetools:::.resolve_p_scope
  testthat::expect_equal(f(c("muni", "poly"), FALSE, NULL), "muni")
  testthat::expect_equal(f("poly", TRUE, NULL), "poly")
  # the default is not an explicit choice, so p_target alone is fine
  testthat::expect_equal(f(c("muni", "poly"), FALSE, 0.5), "muni")
  testthat::expect_error(f("muni", TRUE, 0.5), "cannot be used together")
  testthat::expect_error(f("all", TRUE, NULL))

  testthat::expect_error(
    cnefetools::compute_lumi(2927408, p_target = 0.5, p_scope = "poly"),
    "cannot be used together"
  )
  testthat::expect_error(
    cnefetools::compute_lumi(2927408, p_target = 2),
    "strictly between 0 and 1"
  )
})

testthat::test_that("code_muni is still required for the H3 grid (#120)", {
  testthat::expect_error(cnefetools::cnefe_counts(), "required for the H3 grid")
  testthat::expect_error(cnefetools::compute_lumi(), "required for the H3 grid")
  testthat::expect_error(
    cnefetools::compute_lumi(polygon = NULL),
    "required for the H3 grid"
  )
})

testthat::test_that("a polygon passed by position into code_muni gets a clear error (#120)", {
  testthat::skip_if_not_installed("sf")
  testthat::expect_error(cnefetools::cnefe_counts(poly_region), "by name")
  testthat::expect_error(cnefetools::compute_lumi(poly_region), "by name")
  testthat::expect_error(cnefetools::tracts_to_polygon(poly_region), "by name")
})

# --- p_target ------------------------------------------------------------------

for (backend in c("r", "duckdb")) {
  local({
    backend <- backend

    testthat::test_that(
      paste0("p_target equal to the observed P reproduces the default output (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend)
        default <- run_multi(cnefetools::compute_lumi, both, backend)
        target <- run_multi(cnefetools::compute_lumi, both, backend, p_target = P_both)
        testthat::expect_equal(target, default)
      }
    )

    testthat::test_that(
      paste0("p_target changes bal and bgbi only (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend)
        default <- sf::st_drop_geometry(run_multi(cnefetools::compute_lumi, both, backend))
        half <- sf::st_drop_geometry(
          run_multi(cnefetools::compute_lumi, both, backend, p_target = 0.5)
        )
        for (cc in c("p_res", "ei", "hhi", "ice", "hhi_adp")) {
          testthat::expect_equal(half[[cc]], default[[cc]])
        }
        testthat::expect_equal(half$bgbi, cnefetools:::.bgbi_fun(half$p_res, 0.5))
        testthat::expect_equal(half$bal, cnefetools:::.bal_fun(half$p_res, 0.5))
        testthat::expect_false(isTRUE(all.equal(half$bgbi, default$bgbi)))
      }
    )

    testthat::test_that(
      paste0("p_target and p_scope in polygon mode (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend, polygon = TRUE)
        p_in <- 10 / 13

        muni <- suppressWarnings(
          run_multi(cnefetools::compute_lumi, both, backend, polygon = poly_in_a)
        )
        testthat::expect_equal(muni$p_res, p_in)
        testthat::expect_equal(muni$bgbi, cnefetools:::.bgbi_fun(p_in, P_both))

        # p_target equal to the municipal P gives the same output
        target <- suppressWarnings(
          run_multi(cnefetools::compute_lumi, both, backend,
                    polygon = poly_in_a, p_target = P_both)
        )
        testthat::expect_equal(target, muni)

        # p_scope = "poly": P is the share inside the polygon, so a polygon on
        # its own is perfectly balanced against itself
        poly <- suppressWarnings(
          run_multi(cnefetools::compute_lumi, both, backend,
                    polygon = poly_in_a, p_scope = "poly")
        )
        testthat::expect_equal(poly$bgbi, 0)
        testthat::expect_equal(poly$bal, 1)
        testthat::expect_equal(poly$p_res, p_in)
      }
    )

    testthat::test_that(
      paste0("p_scope = 'poly' counts a point once where polygons overlap (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend, polygon = TRUE)
        two <- rbind(poly_in_a, poly_in_a)
        out <- suppressWarnings(
          run_multi(cnefetools::compute_lumi, both, backend,
                    polygon = two, p_scope = "poly")
        )
        # P over the distinct points is still 10 / 13, not a doubled count
        testthat::expect_equal(out$bgbi, c(0, 0))
      }
    )
  })
}

# --- Addresses outside the H3 grid ----------------------------------------------

for (backend in c("r", "duckdb")) {
  local({
    backend <- backend

    testthat::test_that(
      paste0("cnefe_counts() reports addresses outside the grid (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend)
        n_records <- nrow(multi_muni$records_a) + nrow(multi_muni$records_b)

        out <- NULL
        testthat::expect_warning(
          out <- run_multi(cnefetools::cnefe_counts, both, backend,
                           records_a = records_a_outside),
          "3.*of.*97.*outside the grid"
        )
        testthat::expect_equal(
          sum(as.matrix(sf::st_drop_geometry(out)[addr_cols])),
          n_records
        )

        # no address outside, no warning
        testthat::expect_no_warning(
          run_multi(cnefetools::cnefe_counts, both, backend)
        )
      }
    )

    testthat::test_that(
      paste0("compute_lumi() keeps outside addresses in P unless p_scope = 'poly' (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend)
        shared_p <- 20 / 71

        muni <- NULL
        testthat::expect_warning(
          muni <- run_multi(cnefetools::compute_lumi, both, backend,
                            records_a = records_a_outside),
          "still part of P"
        )
        row <- muni[muni$id_hex == multi_muni$shared, ]
        testthat::expect_equal(row$bgbi, cnefetools:::.bgbi_fun(shared_p, 37 / 96))

        poly <- NULL
        testthat::expect_warning(
          poly <- run_multi(cnefetools::compute_lumi, both, backend,
                            records_a = records_a_outside, p_scope = "poly"),
          "grid only"
        )
        # P over the cells of the grid is the P of the data without them
        default <- run_multi(cnefetools::compute_lumi, both, backend)
        testthat::expect_equal(poly, default)
      }
    )
  })
}

testthat::test_that("tracts_to_h3() reports what was allocated to cells outside the grid (#120)", {
  skip_unless_multi_deps("duckdb")
  testthat::skip_if_not_installed("duckspatial")
  skip_unless_duckdb_extensions("h3")

  real_grid <- cnefetools:::build_h3_grid
  grid_without_shared <- function(h3_resolution, ...) {
    g <- real_grid(h3_resolution, ...)
    g[g$id_hex != multi_muni$shared, ]
  }

  msgs <- character(0)
  out <- testthat::with_mocked_bindings(
    {
      msgs <- testthat::capture_messages(
        res <- run_tracts(cnefetools::tracts_to_h3, both, vars = "pop_ph",
                          h3_resolution = multi_muni$res, quiet = FALSE)
      )
      res
    },
    build_h3_grid = grid_without_shared,
    .package = "cnefetools"
  )

  # The shared cell held 100 * 20 / 30 from A and all 200 of B
  lost <- tract_multi$pop_a * 20 / 30 + tract_multi$pop_b
  testthat::expect_equal(
    sum(out$pop_ph),
    tract_multi$pop_a + tract_multi$pop_b - lost
  )
  all_msgs <- paste(msgs, collapse = "\n")
  testthat::expect_match(all_msgs, "outside the grid: 25 of 39")
  testthat::expect_match(all_msgs, "pop_ph.*outside the grid: 267 of 300")
})

# --- code_muni detected from the polygons ----------------------------------------

testthat::test_that(".detect_code_munis() returns the municipalities the polygons touch (#120)", {
  testthat::skip_if_not_installed("sf")
  detect <- function(poly) {
    testthat::with_mocked_bindings(
      cnefetools:::.detect_code_munis(poly),
      .read_state_boundaries = mock_state_boundaries,
      .read_muni_boundaries_of_states = mock_muni_boundaries_of_states,
      .package = "cnefetools"
    )
  }

  testthat::expect_equal(detect(poly_region)$code_muni, both)
  testthat::expect_equal(detect(poly_in_a)$code_muni, multi_muni$code_a)
  testthat::expect_equal(detect(poly_in_a)$name_muni, "Municipality A")

  # the CRS of the input does not matter
  testthat::expect_equal(
    detect(sf::st_transform(poly_region, 31984))$code_muni,
    both
  )

  far <- sf::st_set_geometry(poly_in_a, sf::st_geometry(poly_in_a) + c(10, 10))
  sf::st_crs(far) <- 4326
  testthat::expect_error(detect(far), "do not intersect any Brazilian state")
})

testthat::test_that("the detection message truncates long lists (#120)", {
  detected <- data.frame(
    code_muni = 3000001L:3000007L,
    name_muni = paste("Town", LETTERS[1:7])
  )
  msg <- paste(
    testthat::capture_messages(cnefetools:::.inform_detected_munis(detected)),
    collapse = ""
  )
  testthat::expect_match(msg, "7 municipalities")
  testthat::expect_match(msg, "Town E \\(3000005\\)")
  testthat::expect_match(msg, "and 2 more")
  testthat::expect_no_match(msg, "Town F")
  testthat::expect_match(msg, "code_muni")
})

for (backend in c("r", "duckdb")) {
  local({
    backend <- backend

    testthat::test_that(
      paste0("cnefe_counts() without code_muni matches the explicit call and does not warn (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend, polygon = TRUE)

        auto <- NULL
        testthat::expect_no_warning(
          testthat::expect_message(
            auto <- run_multi(cnefetools::cnefe_counts, NULL, backend, polygon = poly_region),
            "intersect 2 municipalities"
          )
        )
        explicit <- suppressMessages(
          run_multi(cnefetools::cnefe_counts, both, backend, polygon = poly_region)
        )
        testthat::expect_equal(auto, explicit)

        # a polygon inside A: only A is read, and nothing is said about the
        # points of A outside it
        testthat::expect_no_warning(
          auto_a <- suppressMessages(
            run_multi(cnefetools::cnefe_counts, NULL, backend, polygon = poly_in_a)
          )
        )
        testthat::expect_equal(auto_a$addr_type1, 10L)

        # with code_muni explicit, the points left outside are reported
        testthat::expect_warning(
          run_multi(cnefetools::cnefe_counts, both, backend, polygon = poly_in_a),
          "from the municipalities in `code_muni` fell outside"
        )
      }
    )

    testthat::test_that(
      paste0("compute_lumi() without code_muni warns only when P covers the detected municipalities (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend, polygon = TRUE)
        P_a <- 30 / 33

        muni <- NULL
        testthat::expect_warning(
          muni <- suppressMessages(
            run_multi(cnefetools::compute_lumi, NULL, backend, polygon = poly_in_a)
          ),
          "P was computed from all .*33.* addresses of the 1 municipality"
        )
        testthat::expect_equal(muni$bgbi, cnefetools:::.bgbi_fun(10 / 13, P_a))

        testthat::expect_no_warning(
          poly <- suppressMessages(
            run_multi(cnefetools::compute_lumi, NULL, backend,
                      polygon = poly_in_a, p_scope = "poly")
          )
        )
        testthat::expect_equal(poly$bgbi, 0)

        testthat::expect_no_warning(
          target <- suppressMessages(
            run_multi(cnefetools::compute_lumi, NULL, backend,
                      polygon = poly_in_a, p_target = 0.5)
          )
        )
        testthat::expect_equal(target$bgbi, cnefetools:::.bgbi_fun(10 / 13, 0.5))

        # explicit code_muni: the coverage warning, with the note about P
        testthat::expect_warning(
          run_multi(cnefetools::compute_lumi, both, backend, polygon = poly_in_a),
          "still part of P"
        )
      }
    )
  })
}

testthat::test_that("tracts_to_polygon() without code_muni matches the explicit call (#120)", {
  skip_unless_multi_deps("duckdb", polygon = TRUE)

  auto <- run_tracts(cnefetools::tracts_to_polygon, NULL,
                     polygon = poly_region, vars = "pop_ph")
  explicit <- run_tracts(cnefetools::tracts_to_polygon, both,
                         polygon = poly_region, vars = "pop_ph")
  testthat::expect_equal(auto, explicit)
  testthat::expect_equal(auto$pop_ph, tract_multi$pop_a + tract_multi$pop_b)

  # with the detected set, the coverage line is not about code_muni
  msgs <- testthat::capture_messages(
    run_tracts(cnefetools::tracts_to_polygon, NULL, polygon = poly_region,
               vars = "pop_ph", quiet = FALSE)
  )
  all_msgs <- paste(msgs, collapse = "\n")
  testthat::expect_match(all_msgs, "intersect 2 municipalities")
  testthat::expect_no_match(all_msgs, "Polygon coverage")
})

# --- Formatting and errors added after the real-data run ----------------------------

testthat::test_that("tiny shares outside the grid do not print as 0.000% (#120)", {
  f <- cnefetools:::.fmt_small_pct
  testthat::expect_equal(f(4 / 1390897 * 100), "<0.001%")
  testthat::expect_equal(f(0.0013), "0.0013%")
  testthat::expect_equal(f(0.4234), "0.42%")
  testthat::expect_equal(f(11.43), "11.4%")
  testthat::expect_equal(f(0), "0%")
  testthat::expect_equal(f(NA_real_), "0%")

  testthat::expect_warning(
    cnefetools:::.warn_outside_grid(4L, 1390897L),
    "4.*of.*1390897.*<0.001%"
  )
  testthat::expect_no_warning(cnefetools:::.warn_outside_grid(0L, 100L))
})

testthat::test_that("tracts_to_h3() explains that code_muni is required (#120)", {
  testthat::expect_error(cnefetools::tracts_to_h3(), "required for the H3 grid")
  testthat::expect_error(cnefetools::tracts_to_h3(NULL), "tracts_to_polygon")
  testthat::skip_if_not_installed("sf")
  testthat::expect_error(
    cnefetools::tracts_to_h3(poly_region),
    "polygon = <your sf object>"
  )
})
