# scope, p_target, optional code_muni with scope = "geom", and the report of
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

# A slightly larger square around the same interior points of A, so it holds
# the same 14 records as poly_in_a and overlaps it.
poly_in_a_wide <- local({
  x <- multi_muni$lon0 - 0.005
  y <- multi_muni$lat0 + 0.003
  d <- 0.0015
  sf::st_sf(
    zone = "inside A, wider",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(x - d, y - d, x + d, y - d, x + d, y + d, x - d, y + d, x - d, y - d),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )
})

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
  testthat::expect_error(
    cnefetools::compute_lumi(2927408, p_target = 2),
    "strictly between 0 and 1"
  )
})

testthat::test_that("scope takes muni or geom and decides when code_muni is needed (#120)", {
  f <- cnefetools:::.resolve_scope
  testthat::expect_equal(f(c("muni", "geom"), TRUE, "hex"), list(scope = "muni", detect = FALSE))
  testthat::expect_equal(f("geom", TRUE, "hex"), list(scope = "geom", detect = FALSE))
  testthat::expect_equal(f("muni", TRUE, "user"), list(scope = "muni", detect = FALSE))
  testthat::expect_equal(f("geom", FALSE, "user"), list(scope = "geom", detect = TRUE))
  testthat::expect_error(f("poly", TRUE, "user"))

  # polygons with scope = "muni" need the municipalities
  testthat::expect_error(f("muni", FALSE, "user"), "required when")
  # with scope = "geom" a code_muni that was passed is ignored, and said so
  testthat::expect_message(
    out <- f("geom", TRUE, "user"),
    "ignored"
  )
  testthat::expect_equal(out, list(scope = "geom", detect = TRUE))
})

testthat::test_that("code_muni is still required for the H3 grid (#120)", {
  testthat::expect_error(cnefetools::cnefe_counts(), "required for the H3 grid")
  testthat::expect_error(cnefetools::compute_lumi(), "required for the H3 grid")
  testthat::expect_error(
    cnefetools::compute_lumi(polygon = NULL, scope = "geom"),
    "required for the H3 grid"
  )
})

testthat::test_that("polygons without code_muni need scope = 'geom' (#120)", {
  testthat::skip_if_not_installed("sf")
  for (fn in list(cnefetools::cnefe_counts, cnefetools::compute_lumi)) {
    testthat::expect_error(fn(polygon = poly_region), "scope = \"geom\"")
  }
  testthat::expect_error(
    cnefetools::tracts_to_polygon(polygon = poly_region),
    "scope = \"geom\""
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
      paste0("scope and p_target in polygon mode (", backend, ") (#120)"),
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

        # scope = "geom": P is the share inside the polygon, so a polygon that
        # covers only part of a municipality is balanced against itself
        geom <- suppressMessages(
          run_multi(cnefetools::compute_lumi, NULL, backend,
                    polygon = poly_in_a, scope = "geom")
        )
        testthat::expect_equal(geom$bgbi, 0)
        testthat::expect_equal(geom$bal, 1)
        testthat::expect_equal(geom$p_res, p_in)

        # p_target combines with scope = "geom"
        geom_target <- suppressMessages(
          run_multi(cnefetools::compute_lumi, NULL, backend,
                    polygon = poly_in_a, scope = "geom", p_target = 0.5)
        )
        testthat::expect_equal(geom_target$bgbi, cnefetools:::.bgbi_fun(p_in, 0.5))
      }
    )

    testthat::test_that(
      paste0("scope = 'geom' counts a point once where polygons overlap (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend, polygon = TRUE)
        two <- rbind(poly_in_a, poly_in_a_wide)
        out <- suppressWarnings(suppressMessages(
          run_multi(cnefetools::compute_lumi, NULL, backend,
                    polygon = two, scope = "geom")
        ))
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
      paste0("compute_lumi() keeps outside addresses in P unless scope = 'geom' (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend)
        shared_p <- 20 / 71

        muni <- NULL
        testthat::expect_warning(
          muni <- run_multi(cnefetools::compute_lumi, both, backend,
                            records_a = records_a_outside),
          "P covers every address"
        )
        row <- muni[muni$id_hex == multi_muni$shared, ]
        testthat::expect_equal(row$bgbi, cnefetools:::.bgbi_fun(shared_p, 37 / 96))

        geom <- NULL
        testthat::expect_warning(
          geom <- run_multi(cnefetools::compute_lumi, both, backend,
                            records_a = records_a_outside, scope = "geom"),
          "grid only"
        )
        # P over the cells of the grid is the P of the data without them. A and
        # B are both in code_muni, so the grid reaches no other municipality.
        default <- run_multi(cnefetools::compute_lumi, both, backend)
        testthat::expect_equal(geom, default)
      }
    )
  })
}

# --- scope = "geom" on the H3 grid -----------------------------------------------
#
# With code_muni = A, the grid of A reaches B through the ring of border cells,
# so B is read too. The cells of A's grid must then hold what a call for A and B
# together puts in them.

grid_a <- function() {
  testthat::with_mocked_bindings(
    cnefetools:::build_h3_grid(multi_muni$res, code_muni = multi_muni$code_a),
    .read_muni_boundaries = mock_read_muni_boundaries,
    .package = "cnefetools"
  )
}

for (backend in c("r", "duckdb")) {
  local({
    backend <- backend

    testthat::test_that(
      paste0("cnefe_counts(scope = 'geom') completes the border cells (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend)
        ids_a <- grid_a()$id_hex

        geom <- NULL
        testthat::expect_message(
          geom <- run_multi(cnefetools::cnefe_counts, multi_muni$code_a, backend,
                            scope = "geom"),
          "also reaches 1 municipality"
        )
        both_out <- run_multi(cnefetools::cnefe_counts, both, backend)
        both_out <- both_out[both_out$id_hex %in% ids_a, ]

        testthat::expect_equal(geom$id_hex, both_out$id_hex)
        testthat::expect_equal(
          sf::st_drop_geometry(geom)[addr_cols],
          sf::st_drop_geometry(both_out)[addr_cols],
          ignore_attr = TRUE
        )

        shared <- geom[geom$id_hex == multi_muni$shared, ]
        testthat::expect_equal(shared$addr_type1, 20L)
        testthat::expect_equal(shared$addr_type6, 51L)

        # scope = "muni" keeps only A's side
        muni <- run_multi(cnefetools::cnefe_counts, multi_muni$code_a, backend)
        testthat::expect_equal(muni$addr_type6[muni$id_hex == multi_muni$shared], 0L)
      }
    )

    testthat::test_that(
      paste0("compute_lumi(scope = 'geom') takes P from the cells of the grid (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend)
        counts <- suppressMessages(
          run_multi(cnefetools::cnefe_counts, multi_muni$code_a, backend, scope = "geom")
        )
        m <- as.matrix(sf::st_drop_geometry(counts)[addr_cols])
        P_grid <- sum(m[, 1]) / sum(m[, -7])

        geom <- suppressMessages(
          run_multi(cnefetools::compute_lumi, multi_muni$code_a, backend, scope = "geom")
        )
        ok <- !is.na(geom$p_res)
        testthat::expect_equal(geom$bgbi[ok], cnefetools:::.bgbi_fun(geom$p_res[ok], P_grid))
        testthat::expect_equal(
          geom$p_res[geom$id_hex == multi_muni$shared],
          20 / 71
        )

        # p_target combines with scope = "geom"
        target <- suppressMessages(
          run_multi(cnefetools::compute_lumi, multi_muni$code_a, backend,
                    scope = "geom", p_target = 0.5)
        )
        testthat::expect_equal(target$bgbi[ok], cnefetools:::.bgbi_fun(geom$p_res[ok], 0.5))
      }
    )

    testthat::test_that(
      paste0("the neighbors read with scope = 'geom' do not trigger the outside-grid warning (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend)
        # B has 9 points beyond the grid of A, which must not be reported
        testthat::expect_no_warning(
          suppressMessages(
            run_multi(cnefetools::cnefe_counts, multi_muni$code_a, backend, scope = "geom")
          )
        )
        # the points of A outside its grid still are
        testthat::expect_warning(
          suppressMessages(
            run_multi(cnefetools::cnefe_counts, multi_muni$code_a, backend,
                      scope = "geom", records_a = records_a_outside)
          ),
          "3.*of.*37.*outside the grid"
        )
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

testthat::test_that("tracts_to_h3(scope = 'geom') matches a call for both municipalities on A's grid (#120)", {
  skip_unless_multi_deps("duckdb")
  testthat::skip_if_not_installed("duckspatial")
  skip_unless_duckdb_extensions("h3")
  ids_a <- grid_a()$id_hex

  geom <- run_tracts(cnefetools::tracts_to_h3, multi_muni$code_a, vars = "pop_ph",
                     h3_resolution = multi_muni$res, scope = "geom")
  both_out <- run_tracts(cnefetools::tracts_to_h3, both, vars = "pop_ph",
                         h3_resolution = multi_muni$res)
  both_out <- both_out[both_out$id_hex %in% ids_a, ]

  testthat::expect_equal(geom$id_hex, both_out$id_hex)
  testthat::expect_equal(geom$pop_ph, both_out$pop_ph)
  # the shared cell gets 100 * 20 / 30 from A and all 200 of B, whose only
  # private dwellings are there
  testthat::expect_equal(
    geom$pop_ph[geom$id_hex == multi_muni$shared],
    tract_multi$pop_a * 20 / 30 + tract_multi$pop_b
  )

  muni <- run_tracts(cnefetools::tracts_to_h3, multi_muni$code_a, vars = "pop_ph",
                     h3_resolution = multi_muni$res)
  testthat::expect_equal(
    muni$pop_ph[muni$id_hex == multi_muni$shared],
    tract_multi$pop_a * 20 / 30
  )
})

# --- Municipalities detected from the polygons (scope = "geom") -------------------

testthat::test_that(".detect_code_munis() returns the municipalities the polygons touch (#120)", {
  testthat::skip_if_not_installed("sf")
  detect <- function(poly) {
    testthat::with_mocked_bindings(
      cnefetools:::.detect_code_munis(poly),
      .read_muni_boundaries = mock_read_muni_boundaries,
      .boundary_index = mock_boundary_index,
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
  testthat::expect_error(detect(far), "do not intersect any municipality")
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
  testthat::expect_match(msg, "scope")
})

for (backend in c("r", "duckdb")) {
  local({
    backend <- backend

    testthat::test_that(
      paste0("cnefe_counts(scope = 'geom') matches the explicit call and does not warn (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend, polygon = TRUE)

        auto <- NULL
        testthat::expect_no_warning(
          testthat::expect_message(
            auto <- run_multi(cnefetools::cnefe_counts, NULL, backend,
                              polygon = poly_region, scope = "geom"),
            "intersect 2 municipalities"
          )
        )
        explicit <- suppressMessages(
          run_multi(cnefetools::cnefe_counts, both, backend, polygon = poly_region)
        )
        testthat::expect_equal(auto, explicit)

        # code_muni passed with scope = "geom" is ignored
        ignored <- NULL
        testthat::expect_message(
          ignored <- suppressWarnings(
            run_multi(cnefetools::cnefe_counts, multi_muni$code_a, backend,
                      polygon = poly_region, scope = "geom")
          ),
          "ignored"
        )
        testthat::expect_equal(ignored, auto)

        # a polygon inside A: only A is read, and nothing is said about the
        # points of A outside it
        testthat::expect_no_warning(
          auto_a <- suppressMessages(
            run_multi(cnefetools::cnefe_counts, NULL, backend,
                      polygon = poly_in_a, scope = "geom")
          )
        )
        testthat::expect_equal(auto_a$addr_type1, 10L)

        # with scope = "muni", the points left outside are reported
        testthat::expect_warning(
          run_multi(cnefetools::cnefe_counts, both, backend, polygon = poly_in_a),
          "from the municipalities in `code_muni` fell outside"
        )
      }
    )

    testthat::test_that(
      paste0("compute_lumi() warns about P only with scope = 'muni' (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend, polygon = TRUE)

        testthat::expect_no_warning(
          geom <- suppressMessages(
            run_multi(cnefetools::compute_lumi, NULL, backend,
                      polygon = poly_in_a, scope = "geom")
          )
        )
        testthat::expect_equal(geom$bgbi, 0)

        testthat::expect_warning(
          run_multi(cnefetools::compute_lumi, both, backend, polygon = poly_in_a),
          "P covers every address"
        )
      }
    )
  })
}

testthat::test_that("tracts_to_polygon(scope = 'geom') matches the explicit call (#120)", {
  skip_unless_multi_deps("duckdb", polygon = TRUE)

  auto <- run_tracts(cnefetools::tracts_to_polygon, NULL,
                     polygon = poly_region, vars = "pop_ph", scope = "geom")
  explicit <- run_tracts(cnefetools::tracts_to_polygon, both,
                         polygon = poly_region, vars = "pop_ph")
  testthat::expect_equal(auto, explicit)
  testthat::expect_equal(auto$pop_ph, tract_multi$pop_a + tract_multi$pop_b)

  # with the detected set, the coverage line is not about code_muni
  msgs <- testthat::capture_messages(
    run_tracts(cnefetools::tracts_to_polygon, NULL, polygon = poly_region,
               vars = "pop_ph", scope = "geom", quiet = FALSE)
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


# --- Repeated and overlapping polygons ------------------------------------------

testthat::test_that(".drop_repeated_polygons() keeps the first of each geometry (#120)", {
  testthat::skip_if_not_installed("sf")
  three <- rbind(poly_in_a, poly_region, poly_in_a)
  three$zone <- c("first", "region", "copy")
  out <- NULL
  testthat::expect_message(
    out <- cnefetools:::.drop_repeated_polygons(three),
    "1 of the 3 features.*2 features are used"
  )
  testthat::expect_equal(out$zone, c("first", "region"))
  # nothing repeated, nothing said
  testthat::expect_no_message(cnefetools:::.drop_repeated_polygons(poly_region))
})

for (backend in c("r", "duckdb")) {
  local({
    backend <- backend

    testthat::test_that(
      paste0("repeated polygons are removed before counting (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend, polygon = TRUE)
        two <- rbind(poly_in_a, poly_in_a)
        two$zone <- c("first", "copy")
        out <- NULL
        w <- NULL
        testthat::expect_message(
          w <- testthat::capture_warnings(
            out <- run_multi(cnefetools::cnefe_counts, both, backend, polygon = two)
          ),
          "1 of the 2 features"
        )
        # the copy is not reported as an overlap
        testthat::expect_false(any(grepl("Overlapping", w)))
        testthat::expect_equal(out$zone, "first")
        testthat::expect_equal(sum(as.matrix(sf::st_drop_geometry(out)[addr_cols])), 14)

        lumi <- suppressWarnings(suppressMessages(
          run_multi(cnefetools::compute_lumi, both, backend, polygon = two)
        ))
        testthat::expect_equal(nrow(lumi), 1L)
      }
    )

    testthat::test_that(
      paste0("overlapping polygons warn with the points counted twice (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend, polygon = TRUE)
        two <- rbind(poly_in_a, poly_in_a_wide)
        # all 14 records of the interior of A fall in both squares
        testthat::expect_warning(
          suppressMessages(
            run_multi(cnefetools::cnefe_counts, NULL, backend,
                      polygon = two, scope = "geom")
          ),
          "Overlapping polygons.*14.*of the.*14.*100.0%"
        )
        # compute_lumi() leaves type 7 out, so 13 points
        testthat::expect_warning(
          suppressMessages(
            run_multi(cnefetools::compute_lumi, NULL, backend,
                      polygon = two, scope = "geom")
          ),
          "Overlapping polygons.*13.*of the.*13"
        )
        # scope = "muni": the overlap opens the coverage warning
        w <- testthat::capture_warnings(suppressMessages(
          run_multi(cnefetools::cnefe_counts, both, backend, polygon = two)
        ))
        testthat::expect_length(w, 1L)
        testthat::expect_match(w, "Polygon coverage.*Overlapping polygons.*14.*of the.*14")

        # polygons that do not overlap: no overlap warning
        testthat::expect_no_warning(
          suppressMessages(
            run_multi(cnefetools::cnefe_counts, NULL, backend,
                      polygon = poly_in_a, scope = "geom")
          )
        )
      }
    )
  })
}

testthat::test_that("tracts_to_polygon() warns about dwellings in two polygons (#120)", {
  skip_unless_multi_deps("duckdb", polygon = TRUE)
  # two halves of the region that share a band over A's interior dwellings
  band <- function(xmin, xmax) {
    sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(
      c(xmin, multi_muni$lat0 - 0.02, xmax, multi_muni$lat0 - 0.02,
        xmax, multi_muni$lat0 + 0.02, xmin, multi_muni$lat0 + 0.02,
        xmin, multi_muni$lat0 - 0.02),
      ncol = 2, byrow = TRUE
    ))), crs = 4326))
  }
  lon0 <- multi_muni$lon0
  two <- rbind(band(lon0 - 0.02, lon0 - 0.004), band(lon0 - 0.006, lon0 + 0.02))
  m <- testthat::capture_messages(
    run_tracts(cnefetools::tracts_to_polygon, both, polygon = two,
               vars = "pop_ph", quiet = FALSE) |> suppressWarnings()
  )
  m <- cli::ansi_strip(m)
  # the 10 interior dwellings of A are in both bands. The total inside is not
  # checked, since the synthetic address codes repeat across A and B.
  at <- grep("Overlapping polygons: 10 of", m)
  testthat::expect_length(at, 1L)
  # right under the title of the diagnostics, before stage 1
  testthat::expect_lt(at, grep("Stage 1", m))
  testthat::expect_gt(at, grep("Dasymetric interpolation diagnostics", m))
})
