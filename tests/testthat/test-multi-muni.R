# Several municipalities in one call (#120, R2 comment 1). The synthetic
# municipalities and the runners are in helper-multi-muni.R.

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

testthat::test_that(".normalize_code_munis() takes numbers, strings and a mix (#120)", {
  f <- cnefetools:::.normalize_code_munis
  want <- c(2927408L, 2919207L)

  testthat::expect_equal(f(c(2927408, 2919207)), want)
  testthat::expect_equal(f(c("2927408", "2919207")), want)
  testthat::expect_equal(f(c(2927408L, 2919207L)), want)
  testthat::expect_equal(f(list(2927408, "2919207")), want)
  testthat::expect_equal(f(c(" 2927408 ", "2919207")), want)
  testthat::expect_equal(f("2927408"), 2927408L)

  # one bad element rejects the whole call instead of being silently dropped
  testthat::expect_error(f(c("2927408", "abc")), "7-digit")
  testthat::expect_error(f(c(2927408, NA)), "7-digit")
  testthat::expect_error(f(c("2927408", "292740")), "7-digit")
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
      .read_muni_boundaries = mock_read_muni_boundaries,
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

# --- tracts_to_h3() and tracts_to_polygon()
#
# Fixtures in helper-multi-muni.R.

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
