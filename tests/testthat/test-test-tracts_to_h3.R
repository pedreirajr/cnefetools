testthat::test_that("tracts_to_h3 returns an sf object with requested variables", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("duckspatial")
  testthat::skip_if_not_installed("h3jsr")

  # Optional: skip if DuckDB cannot load/install required extensions
  con_check <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con_check, shutdown = TRUE), add = TRUE)

  ok_zipfs <- tryCatch(
    {
      DBI::dbExecute(con_check, "LOAD zipfs;")
      TRUE
    },
    error = function(e) {
      tryCatch(
        {
          DBI::dbExecute(con_check, "INSTALL zipfs; LOAD zipfs;")
          TRUE
        },
        error = function(e2) FALSE
      )
    }
  )

  ok_h3 <- tryCatch(
    {
      DBI::dbExecute(con_check, "LOAD h3;")
      TRUE
    },
    error = function(e) {
      tryCatch(
        {
          DBI::dbExecute(con_check, "INSTALL h3; LOAD h3;")
          TRUE
        },
        error = function(e2) FALSE
      )
    }
  )

  if (!ok_zipfs) {
    testthat::skip("DuckDB zipfs extension not available.")
  }
  if (!ok_h3) {
    testthat::skip("DuckDB h3 extension not available.")
  }

  res <- NULL

  testthat::with_mocked_bindings(
    {
      testthat::expect_message(
        res <- cnefetools::tracts_to_h3(
          code_muni = 2927408,
          h3_resolution = 9,
          vars = c("n_inhab_p", "avg_inc_resp", "n_resp", "female", "age_70m"),
          cache = TRUE,
          verbose = FALSE
        ),
        "Dasymetric interpolation diagnostics",
        fixed = TRUE
      )

      testthat::expect_s3_class(res, "sf")

      needed <- c(
        "id_hex",
        "n_inhab_p",
        "avg_inc_resp",
        "n_resp",
        "female",
        "age_70m"
      )
      testthat::expect_true(all(needed %in% names(res)))

      timing <- attr(res, "timing", exact = TRUE)
      testthat::expect_true(is.list(timing) || is.null(timing))

      # Minimal sanity checks on returned values
      testthat::expect_true(is.numeric(res$n_inhab_p))
      testthat::expect_gt(sum(res$n_inhab_p, na.rm = TRUE), 0)

      # With our mocked data: tract1 allocates 100; tract2 is unallocated
      testthat::expect_equal(round(sum(res$n_inhab_p, na.rm = TRUE)), 100)

      # avg_inc_resp should be present and numeric (mean over eligible points)
      testthat::expect_true(
        is.numeric(res$avg_inc_resp) || all(is.na(res$avg_inc_resp))
      )
    },
    .sc_create_views_in_duckdb = function(con, code_muni, cache, verbose) {
      # Two tracts. Only the first has CNEFE points in the mocked CNEFE view.
      DBI::dbExecute(
        con,
        "
        CREATE OR REPLACE VIEW sc_muni AS
        SELECT
          '292740800000001' AS code_tract,
          100::INTEGER AS n_inhab_p,
          60::INTEGER  AS female,
          5::INTEGER   AS age_70m,
          40::INTEGER  AS n_resp,
          2000.0::DOUBLE AS avg_inc_resp,
          ST_GeomFromText('POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))') AS geom
        UNION ALL
        SELECT
          '292740800000002' AS code_tract,
          50::INTEGER  AS n_inhab_p,
          20::INTEGER  AS female,
          2::INTEGER   AS age_70m,
          10::INTEGER  AS n_resp,
          3000.0::DOUBLE AS avg_inc_resp,
          ST_GeomFromText('POLYGON((2 0, 3 0, 3 1, 2 1, 2 0))') AS geom
      "
      )
    },
    .cnefe_create_points_view_in_duckdb = function(
      con,
      code_muni,
      index,
      cache,
      verbose
    ) {
      # 4 private points inside tract 1; 1 point outside any tract (unmatched)
      DBI::dbExecute(
        con,
        "
        CREATE OR REPLACE VIEW cnefe_pts AS
        SELECT
          1::BIGINT AS COD_UNICO_ENDERECO,
          1::INTEGER AS COD_ESPECIE,
          0.2::DOUBLE AS lon,
          0.2::DOUBLE AS lat,
          ST_Point(0.2, 0.2) AS geom
        UNION ALL
        SELECT 2, 1, 0.2, 0.8, ST_Point(0.2, 0.8)
        UNION ALL
        SELECT 3, 1, 0.8, 0.2, ST_Point(0.8, 0.2)
        UNION ALL
        SELECT 4, 1, 0.8, 0.8, ST_Point(0.8, 0.8)
        UNION ALL
        SELECT 5, 1, 10.0, 10.0, ST_Point(10.0, 10.0)
      "
      )
    }
  )
})
