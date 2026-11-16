# Several municipalities in one call (#120, R2 comment 1).
#
# The H3 grid of a municipality holds every cell whose centre falls inside it
# plus the neighbours that intersect its boundary, so a cell on a shared border
# belongs to the grid of BOTH municipalities. Two adjacent synthetic
# municipalities reproduce it offline: A to the west and B to the east of a
# meridian that runs through the centre of one H3 cell, the "shared" cell.
# A puts 20 private households in it and B puts 51 other-use establishments, the
# numbers of the referee's example (20 / 71 = 0.282; averaging the two separate
# shares gives 0.5).

multi_muni <- local({
  res <- 9L
  start <- data.frame(lon = -38.5, lat = -12.9)
  shared <- as.character(h3jsr::point_to_cell(start, res = res, simple = TRUE))
  ctr <- sf::st_coordinates(h3jsr::cell_to_point(shared, simple = TRUE))
  lon0 <- ctr[1, 1]
  lat0 <- ctr[1, 2]

  rect <- function(xmin, xmax) {
    sf::st_sf(
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(
          c(xmin, lat0 - 0.01, xmax, lat0 - 0.01, xmax, lat0 + 0.01,
            xmin, lat0 + 0.01, xmin, lat0 - 0.01),
          ncol = 2, byrow = TRUE
        ))),
        crs = 4326
      )
    )
  }

  list(
    res = res,
    shared = shared,
    lon0 = lon0,
    lat0 = lat0,
    code_a = 3000001L,
    code_b = 3000002L,
    boundary_a = rect(lon0 - 0.01, lon0),
    boundary_b = rect(lon0, lon0 + 0.01),
    # Records of A: 20 private households in the shared cell, plus cells that
    # only A reaches.
    records_a = data.frame(
      COD_ESPECIE = c(rep(1L, 20), rep(1L, 10), rep(4L, 3), 7L),
      LONGITUDE = c(rep(lon0 - 1e-5, 20), rep(lon0 - 0.005, 13), lon0 - 0.005),
      LATITUDE = c(rep(lat0, 20), rep(lat0 + 0.003, 13), lat0 + 0.003)
    ),
    # Records of B: 51 other uses in the shared cell, plus cells only B reaches.
    records_b = data.frame(
      COD_ESPECIE = c(rep(6L, 51), rep(2L, 5), rep(1L, 4)),
      LONGITUDE = c(rep(lon0 + 1e-5, 51), rep(lon0 + 0.005, 9)),
      LATITUDE = c(rep(lat0, 51), rep(lat0 - 0.003, 9))
    )
  )
})

skip_unless_multi_deps <- function(backend, polygon = FALSE) {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("h3jsr")
  testthat::skip_if_not_installed("sf")
  if (identical(backend, "duckdb")) {
    skip_unless_duckdb_extensions(if (polygon) "spatial" else "h3")
    if (polygon) testthat::skip_if_not_installed("duckspatial")
  }
}

# Runs `fn` against the two synthetic municipalities, with the retrieval and the
# boundary read replaced so nothing touches the network.
run_multi <- function(fn, code_muni, backend, ...) {
  files <- list(
    "3000001" = write_cnefe_gz(multi_muni$records_a),
    "3000002" = write_cnefe_gz(multi_muni$records_b)
  )
  on.exit(unlink(unlist(files)), add = TRUE)
  boundaries <- list(
    "3000001" = multi_muni$boundary_a,
    "3000002" = multi_muni$boundary_b
  )

  testthat::with_mocked_bindings(
    fn(code_muni, h3_resolution = multi_muni$res, backend = backend,
       verbose = FALSE, ...),
    .cnefe_ensure_zip = function(code_muni, ...) {
      list(
        zip_path = files[[as.character(code_muni)]],
        cleanup_zip = FALSE,
        url = "synthetic"
      )
    },
    .read_muni_boundary = function(code_muni, year = 2022L) {
      boundaries[[as.character(code_muni)]]
    },
    .package = "cnefetools"
  )
}

addr_cols <- paste0("addr_type", 1:8)

# --- Input handling -----------------------------------------------------------

testthat::test_that(".normalize_code_munis() accepts vectors and drops repeats (#120)", {
  testthat::expect_equal(
    cnefetools:::.normalize_code_munis(c(2927408, "2919207", 2927408)),
    c(2927408L, 2919207L)
  )
  testthat::expect_equal(cnefetools:::.normalize_code_munis(2927408), 2927408L)
  testthat::expect_error(cnefetools:::.normalize_code_munis(c(2927408, 123)))
  testthat::expect_error(cnefetools:::.normalize_code_munis(integer(0)))
})

testthat::test_that(".combine_poly_joins() adds counts and coverage totals (#120)", {
  a <- list(
    counts = dplyr::tibble(.poly_row_id = c(1L, 2L), n_res = c(3L, 1L), n_tot = c(4L, 2L)),
    total_points = 10L, points_matched = 6L, points_outside = 4L,
    total_n_res = 5L, total_n_tot = 10L
  )
  b <- list(
    counts = dplyr::tibble(.poly_row_id = c(2L, 3L), n_res = c(2L, 1L), n_tot = c(5L, 1L)),
    total_points = 8L, points_matched = 6L, points_outside = 2L,
    total_n_res = 4L, total_n_tot = 8L
  )

  out <- cnefetools:::.combine_poly_joins(list(a, b))
  out$counts <- out$counts[order(out$counts$.poly_row_id), ]

  testthat::expect_equal(out$counts$n_res, c(3L, 3L, 1L))
  testthat::expect_equal(out$counts$n_tot, c(4L, 7L, 1L))
  testthat::expect_equal(out$total_points, 18)
  testthat::expect_equal(out$points_matched, 12)
  testthat::expect_equal(out$total_n_res, 9)
  testthat::expect_identical(cnefetools:::.combine_poly_joins(list(a)), a)
})

# --- The two grids share the border cell -------------------------------------

testthat::test_that("separate municipal grids both contain the shared border cell (#120)", {
  testthat::skip_if_not_installed("h3jsr")
  testthat::skip_if_not_installed("sf")

  grid_of <- function(code) {
    testthat::with_mocked_bindings(
      cnefetools:::build_h3_grid(multi_muni$res, code_muni = code),
      .read_muni_boundary = function(code_muni, year = 2022L) {
        if (code_muni == multi_muni$code_a) multi_muni$boundary_a else multi_muni$boundary_b
      },
      .package = "cnefetools"
    )
  }

  ga <- grid_of(multi_muni$code_a)
  gb <- grid_of(multi_muni$code_b)
  gab <- grid_of(c(multi_muni$code_a, multi_muni$code_b))

  testthat::expect_true(multi_muni$shared %in% ga$id_hex)
  testthat::expect_true(multi_muni$shared %in% gb$id_hex)
  testthat::expect_gt(length(intersect(ga$id_hex, gb$id_hex)), 1L)

  # Over the union every cell is listed once, and none is lost.
  testthat::expect_equal(anyDuplicated(gab$id_hex), 0L)
  testthat::expect_setequal(gab$id_hex, union(ga$id_hex, gb$id_hex))
})

# --- cnefe_counts() ------------------------------------------------------------

for (backend in c("r", "duckdb")) {
  local({
    backend <- backend

    testthat::test_that(
      paste0("cnefe_counts() over two municipalities keeps both contributions (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend)
        both <- c(multi_muni$code_a, multi_muni$code_b)

        a <- run_multi(cnefetools::cnefe_counts, multi_muni$code_a, backend)
        b <- run_multi(cnefetools::cnefe_counts, multi_muni$code_b, backend)
        ab <- run_multi(cnefetools::cnefe_counts, both, backend)

        # Called one by one, the border cell shows up in both outputs, each with
        # its own side only.
        sa <- sf::st_drop_geometry(a)[a$id_hex == multi_muni$shared, ]
        sb <- sf::st_drop_geometry(b)[b$id_hex == multi_muni$shared, ]
        testthat::expect_equal(sa$addr_type1, 20L)
        testthat::expect_equal(sa$addr_type6, 0L)
        testthat::expect_equal(sb$addr_type1, 0L)
        testthat::expect_equal(sb$addr_type6, 51L)

        # Called together, it is one row carrying the sum.
        testthat::expect_equal(anyDuplicated(ab$id_hex), 0L)
        sab <- sf::st_drop_geometry(ab)[ab$id_hex == multi_muni$shared, ]
        testthat::expect_equal(sab$addr_type1, 20L)
        testthat::expect_equal(sab$addr_type6, 51L)

        # Nothing is lost or invented: the total is the number of records, and
        # each cell equals the sum of the two separate calls.
        n_records <- nrow(multi_muni$records_a) + nrow(multi_muni$records_b)
        testthat::expect_equal(sum(as.matrix(sf::st_drop_geometry(ab)[addr_cols])), n_records)

        summed <- dplyr::bind_rows(sf::st_drop_geometry(a), sf::st_drop_geometry(b)) |>
          dplyr::group_by(.data$id_hex) |>
          dplyr::summarise(dplyr::across(dplyr::all_of(addr_cols), sum), .groups = "drop") |>
          dplyr::arrange(.data$id_hex)
        together <- sf::st_drop_geometry(ab) |>
          dplyr::arrange(.data$id_hex)
        testthat::expect_equal(together$id_hex, summed$id_hex)
        for (cc in addr_cols) {
          testthat::expect_equal(together[[cc]], summed[[cc]])
        }
      }
    )

    testthat::test_that(
      paste0("compute_lumi() over two municipalities recomputes shares and P from the pooled records (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend)
        both <- c(multi_muni$code_a, multi_muni$code_b)

        a <- run_multi(cnefetools::compute_lumi, multi_muni$code_a, backend)
        b <- run_multi(cnefetools::compute_lumi, multi_muni$code_b, backend)
        ab <- run_multi(cnefetools::compute_lumi, both, backend)

        testthat::expect_equal(anyDuplicated(ab$id_hex), 0L)

        # Averaging the two separate shares of the border cell is wrong.
        pa <- a$p_res[a$id_hex == multi_muni$shared]
        pb <- b$p_res[b$id_hex == multi_muni$shared]
        testthat::expect_equal(mean(c(pa, pb)), 0.5)

        # The pooled share is the referee's 20 / 71.
        p_shared <- ab$p_res[ab$id_hex == multi_muni$shared]
        testthat::expect_equal(p_shared, 20 / 71)

        # P is the residential share of all records of the region, type 7
        # excluded, and Balance and BGBI are defined against it.
        recs <- rbind(multi_muni$records_a, multi_muni$records_b)
        recs <- recs[recs$COD_ESPECIE != 7L, ]
        P <- mean(recs$COD_ESPECIE == 1L)
        row <- ab[ab$id_hex == multi_muni$shared, ]
        testthat::expect_equal(row$bal, cnefetools:::.bal_fun(20 / 71, P))
        testthat::expect_equal(row$bgbi, cnefetools:::.bgbi_fun(20 / 71, P))

        # A municipal P would give another value, so the regional one matters.
        P_a <- mean(multi_muni$records_a$COD_ESPECIE[multi_muni$records_a$COD_ESPECIE != 7L] == 1L)
        testthat::expect_false(isTRUE(all.equal(
          row$bgbi, cnefetools:::.bgbi_fun(20 / 71, P_a)
        )))
      }
    )

    testthat::test_that(
      paste0("a polygon across the border counts both sides without a coverage warning (", backend, ") (#120)"),
      {
        skip_unless_multi_deps(backend, polygon = TRUE)
        both <- c(multi_muni$code_a, multi_muni$code_b)
        poly <- sf::st_sf(
          zone = "region",
          geometry = sf::st_sfc(
            sf::st_polygon(list(matrix(
              c(multi_muni$lon0 - 0.02, multi_muni$lat0 - 0.02,
                multi_muni$lon0 + 0.02, multi_muni$lat0 - 0.02,
                multi_muni$lon0 + 0.02, multi_muni$lat0 + 0.02,
                multi_muni$lon0 - 0.02, multi_muni$lat0 + 0.02,
                multi_muni$lon0 - 0.02, multi_muni$lat0 - 0.02),
              ncol = 2, byrow = TRUE
            ))),
            crs = 4326
          )
        )

        out <- NULL
        testthat::expect_no_warning(
          out <- run_multi(cnefetools::cnefe_counts, both, backend, polygon = poly)
        )

        d <- sf::st_drop_geometry(out)
        testthat::expect_equal(d$addr_type1, 20L + 10L + 4L)
        testthat::expect_equal(d$addr_type6, 51L)
        testthat::expect_equal(d$addr_type7, 1L)

        lum <- suppressMessages(
          run_multi(cnefetools::compute_lumi, both, backend, polygon = poly)
        )
        recs <- rbind(multi_muni$records_a, multi_muni$records_b)
        recs <- recs[recs$COD_ESPECIE != 7L, ]
        testthat::expect_equal(lum$p_res, mean(recs$COD_ESPECIE == 1L))
      }
    )
  })
}

testthat::test_that("one municipality in a vector gives the same result as the scalar call (#120)", {
  skip_unless_multi_deps("r")

  once <- run_multi(cnefetools::cnefe_counts, multi_muni$code_a, "r")
  again <- run_multi(cnefetools::cnefe_counts, c(multi_muni$code_a, multi_muni$code_a), "r")
  testthat::expect_equal(once, again)
})

# --- tracts_to_h3() and tracts_to_polygon() -----------------------------------
#
# Both municipalities sit in the same state, so one tract Parquet serves both.
# A tract of each municipality covers its own rectangle. Population totals are
# additive and spread over the private and collective dwellings of each tract,
# so the border cell must receive the allocation of both sides, once.

tract_multi <- local({
  lon0 <- multi_muni$lon0
  lat0 <- multi_muni$lat0
  pt <- function(dx, dy, n, cod) {
    data.frame(
      COD_ESPECIE = rep(cod, n),
      LONGITUDE = rep(lon0 + dx, n),
      LATITUDE = rep(lat0 + dy, n)
    )
  }
  list(
    # A: 20 dwellings in the shared cell and 10 elsewhere. B: 5 in the shared
    # cell and 4 elsewhere.
    records_a = rbind(pt(-1e-5, 0, 20, 1L), pt(-0.005, 0.003, 10, 1L)),
    records_b = rbind(pt(1e-5, 0, 5, 1L), pt(0.005, -0.003, 4, 2L)),
    pop_a = 100L,
    pop_b = 200L
  )
})

tract_parquet_multi <- function() {
  rect_wkb <- function(boundary) {
    unclass(sf::st_as_binary(sf::st_geometry(boundary)))[[1]]
  }
  tab <- arrow::arrow_table(
    code_tract = c("300000100000001", "300000200000001"),
    pop_ph = c(tract_multi$pop_a, tract_multi$pop_b),
    pop_ch = c(0L, 0L),
    geom_wkb = arrow::Array$create(
      list(rect_wkb(multi_muni$boundary_a), rect_wkb(multi_muni$boundary_b)),
      type = arrow::binary()
    )
  )
  p <- tempfile(fileext = ".parquet")
  arrow::write_parquet(tab, p)
  p
}

run_tracts <- function(fn, code_muni, ...) {
  files <- list(
    "3000001" = write_cnefe_gz(tract_multi$records_a),
    "3000002" = write_cnefe_gz(tract_multi$records_b)
  )
  parquet <- tract_parquet_multi()
  on.exit(unlink(c(unlist(files), parquet)), add = TRUE)
  boundaries <- list(
    "3000001" = multi_muni$boundary_a,
    "3000002" = multi_muni$boundary_b
  )

  testthat::with_mocked_bindings(
    suppressWarnings(suppressMessages(fn(code_muni, ..., verbose = FALSE))),
    .cnefe_ensure_zip = function(code_muni, ...) {
      list(
        zip_path = files[[as.character(code_muni)]],
        cleanup_zip = FALSE,
        url = "synthetic"
      )
    },
    .sc_ensure_parquet_uf = function(...) parquet,
    .read_muni_boundary = function(code_muni, year = 2022L) {
      boundaries[[as.character(code_muni)]]
    },
    .package = "cnefetools"
  )
}

testthat::test_that("tracts_to_h3() over two municipalities adds both allocations to the border cell (#120)", {
  skip_unless_multi_deps("duckdb")
  testthat::skip_if_not_installed("duckspatial")
  skip_unless_duckdb_extensions("h3")
  both <- c(multi_muni$code_a, multi_muni$code_b)

  a <- run_tracts(cnefetools::tracts_to_h3, multi_muni$code_a,
                  vars = "pop_ph", h3_resolution = multi_muni$res)
  b <- run_tracts(cnefetools::tracts_to_h3, multi_muni$code_b,
                  vars = "pop_ph", h3_resolution = multi_muni$res)
  ab <- run_tracts(cnefetools::tracts_to_h3, both,
                   vars = "pop_ph", h3_resolution = multi_muni$res)

  testthat::expect_equal(anyDuplicated(ab$id_hex), 0L)

  # Each tract total is conserved, and the sum is the sum of the two parts.
  testthat::expect_equal(sum(ab$pop_ph), tract_multi$pop_a + tract_multi$pop_b)

  # The border cell: 20 of the 30 private dwellings of tract A, and all 5 of
  # tract B (its other 4 dwellings are collective, which pop_ph does not use).
  expected <- tract_multi$pop_a * 20 / 30 + tract_multi$pop_b
  testthat::expect_equal(ab$pop_ph[ab$id_hex == multi_muni$shared], expected)

  # Called one by one, each output holds only its own side of that cell.
  testthat::expect_equal(
    a$pop_ph[a$id_hex == multi_muni$shared], tract_multi$pop_a * 20 / 30
  )
  testthat::expect_equal(
    b$pop_ph[b$id_hex == multi_muni$shared], tract_multi$pop_b
  )

  summed <- dplyr::bind_rows(sf::st_drop_geometry(a), sf::st_drop_geometry(b)) |>
    dplyr::group_by(.data$id_hex) |>
    dplyr::summarise(pop_ph = sum(.data$pop_ph), .groups = "drop") |>
    dplyr::arrange(.data$id_hex)
  together <- sf::st_drop_geometry(ab) |> dplyr::arrange(.data$id_hex)
  testthat::expect_equal(together$id_hex, summed$id_hex)
  testthat::expect_equal(together$pop_ph, summed$pop_ph)
})

testthat::test_that("tracts_to_polygon() over two municipalities adds both allocations (#120)", {
  skip_unless_multi_deps("duckdb", polygon = TRUE)
  both <- c(multi_muni$code_a, multi_muni$code_b)
  poly <- sf::st_sf(
    zone = "region",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(multi_muni$lon0 - 0.02, multi_muni$lat0 - 0.02,
          multi_muni$lon0 + 0.02, multi_muni$lat0 - 0.02,
          multi_muni$lon0 + 0.02, multi_muni$lat0 + 0.02,
          multi_muni$lon0 - 0.02, multi_muni$lat0 + 0.02,
          multi_muni$lon0 - 0.02, multi_muni$lat0 - 0.02),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )

  ab <- run_tracts(cnefetools::tracts_to_polygon, both,
                   polygon = poly, vars = "pop_ph")
  a <- run_tracts(cnefetools::tracts_to_polygon, multi_muni$code_a,
                  polygon = poly, vars = "pop_ph")
  b <- run_tracts(cnefetools::tracts_to_polygon, multi_muni$code_b,
                  polygon = poly, vars = "pop_ph")

  testthat::expect_equal(ab$pop_ph, tract_multi$pop_a + tract_multi$pop_b)
  testthat::expect_equal(ab$pop_ph, a$pop_ph + b$pop_ph)
})
