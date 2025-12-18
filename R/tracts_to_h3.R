#' Convert census tract aggregates to an H3 grid using CNEFE points (DuckDB backend)
#'
#' @description
#' `tracts_to_h3()` performs a two-step dasymetric interpolation:
#' 1) census tract totals are allocated to CNEFE dwelling points inside each tract; then
#' 2) allocated values are aggregated to an H3 grid at a user-defined resolution.
#'
#' The function uses DuckDB with the spatial and H3 extensions for the heavy work.
#'
#' @param code_muni Integer. Seven-digit IBGE municipality code.
#' @param h3_resolution Integer. H3 resolution (0 to 15). Default is 9.
#' @param vars Character vector. Names of tract-level variables to interpolate.
#'   Supported variables:
#'   - `n_inhab_p`: population in private households (Domicílios particulares).
#'   - `n_inhab_c`: population in collective households (Domicílios coletivos).
#'   - `n_inhab`: total population (as provided in the tract aggregates).
#'   - `male`: total male population.
#'   - `female`: total female population.
#'   - `age_0_4`, `age_5_9`, `age_10_14`, `age_15_19`, `age_20_24`, `age_25_29`,
#'     `age_30_39`, `age_40_49`, `age_50_59`, `age_60_69`, `age_70m`: population by age group.
#'   - `n_resp`: number of household heads in the census tract (Pessoas responsáveis por domicílios).
#'   - `avg_inc_resp`: average income of the household head in the census tract.
#'
#'   Allocation rules:
#'   - `n_inhab_p` is allocated only to private dwellings.
#'   - `n_inhab_c` is allocated only to collective dwellings.
#'   - All other sum-like variables are allocated to private dwellings when the tract has any;
#'     if the tract has zero private dwellings but has collective dwellings, they are allocated to collective.
#'   - `avg_inc_resp` is assigned (not split) to each eligible dwelling point using the same eligibility rule.
#'
#' @param cache Logical. Whether to use the existing package cache for assets and CNEFE zips.
#' @param verbose Logical. Whether to print step messages and timing.
#'
#' @return An `sf` object (CRS 4326) with an H3 grid and the requested interpolated variables.
#'   Attributes:
#'   - `attr(x, "timing")`: named numeric vector with step timings (seconds).
#'
#' @export
tracts_to_h3 <- function(code_muni,
                         h3_resolution = 9L,
                         vars = c("n_inhab_p", "n_inhab_c"),
                         cache = TRUE,
                         verbose = TRUE) {

  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || is.na(x)) y else x

  t0_total <- Sys.time()

  code_muni <- as.integer(code_muni)
  h3_resolution <- as.integer(h3_resolution)

  if (length(code_muni) != 1L || is.na(code_muni)) {
    rlang::abort("`code_muni` must be a single integer (7-digit IBGE municipality code).")
  }
  if (length(h3_resolution) != 1L || is.na(h3_resolution) ||
      h3_resolution < 0L || h3_resolution > 15L) {
    rlang::abort("`h3_resolution` must be an integer between 0 and 15.")
  }

  vars <- unique(as.character(vars))
  if (length(vars) == 0L) {
    rlang::abort("`vars` must contain at least one variable name.")
  }

  allowed_vars <- c(
    "code_type",
    "n_inhab", "male", "female",
    "age_0_4", "age_5_9", "age_10_14", "age_15_19",
    "age_20_24", "age_25_29", "age_30_39", "age_40_49",
    "age_50_59", "age_60_69", "age_70m",
    "n_resp",
    "avg_inc_resp",
    "n_inhab_p", "n_inhab_c"
  )
  bad <- setdiff(vars, allowed_vars)
  if (length(bad) > 0L) {
    rlang::abort(paste0(
      "Unknown `vars`: ", paste(bad, collapse = ", "),
      ". Supported: ", paste(allowed_vars, collapse = ", "), "."
    ))
  }

  pop_private <- "n_inhab_p"
  pop_collect <- "n_inhab_c"

  if (verbose) message("Processing code ", code_muni, "...")

  # ---- Step 1/6: connect + load extensions ----
  t1 <- Sys.time()
  if (verbose) message("Step 1/6: connecting to DuckDB and loading extensions...")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Always suppress DuckDB stdout noise for extension loading
  invisible(capture.output({
    duckspatial::ddbs_install(con)
    duckspatial::ddbs_load(con)
    DBI::dbExecute(con, "INSTALL zipfs; LOAD zipfs;")
    DBI::dbExecute(con, "INSTALL h3; LOAD h3;")
  }))

  t1_end <- Sys.time()
  time_step1 <- as.numeric(difftime(t1_end, t1, units = "secs"))
  if (verbose) message(sprintf("Step 1/6 (DuckDB ready) completed in %.2f s.", time_step1))

  # ---- Step 2/6: tracts (UF parquet) ----
  t2 <- Sys.time()
  if (verbose) message("Step 2/6: preparing census tracts in DuckDB...")

  # Creates view `sc_muni` with columns:
  # - code_tract (character, 15 digits)
  # - requested aggregate variables
  # - geom (DuckDB GEOMETRY)
  .sc_create_views_in_duckdb(con, code_muni = code_muni, cache = cache, verbose = FALSE)

  DBI::dbExecute(con, "CREATE OR REPLACE TABLE sc_muni_tbl AS SELECT * FROM sc_muni;")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS sc_muni_geom_idx ON sc_muni_tbl USING RTREE (geom);")

  t2_end <- Sys.time()
  time_step2 <- as.numeric(difftime(t2_end, t2, units = "secs"))
  if (verbose) message(sprintf("Step 2/6 (tracts ready) completed in %.2f s.", time_step2))

  # ---- Step 3/6: CNEFE points ----
  t3 <- Sys.time()
  if (verbose) message("Step 3/6: preparing CNEFE points in DuckDB...")

  # Creates view `cnefe_pts` with at least: COD_ESPECIE, lon, lat, geom
  .cnefe_create_points_view_in_duckdb(con, code_muni = code_muni, cache = cache, verbose = FALSE)
  DBI::dbExecute(con, "CREATE OR REPLACE TABLE cnefe_pts_tbl AS SELECT * FROM cnefe_pts;")

  t3_end <- Sys.time()
  time_step3 <- as.numeric(difftime(t3_end, t3, units = "secs"))
  if (verbose) message(sprintf("Step 3/6 (CNEFE points ready) completed in %.2f s.", time_step3))

  # ---- Step 4/6: spatial join + allocation prep ----
  t4 <- Sys.time()
  if (verbose) message("Step 4/6: spatial join (points to tracts) and allocation prep...")

  n_pts_coords <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) AS n
    FROM cnefe_pts_tbl
    WHERE lon IS NOT NULL AND lat IS NOT NULL;
  ")$n[1]

  # Spatial match using FROM + WHERE ST_Within
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE cnefe_sc AS
    SELECT
      p.*,
      s.code_tract
    FROM cnefe_pts_tbl p, sc_muni_tbl s
    WHERE p.lon IS NOT NULL AND p.lat IS NOT NULL
      AND ST_Within(p.geom, s.geom);
  ")

  n_matched <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cnefe_sc;")$n[1]
  n_unmatched <- max(n_pts_coords - n_matched, 0)

  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE dom_counts AS
    SELECT
      code_tract,
      SUM(CASE WHEN COD_ESPECIE = 1 THEN 1 ELSE 0 END) AS n_dom_p,
      SUM(CASE WHEN COD_ESPECIE = 2 THEN 1 ELSE 0 END) AS n_dom_c
    FROM cnefe_sc
    GROUP BY 1;
  ")

  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE sc_muni_w_dom AS
    SELECT
      s.*,
      COALESCE(d.n_dom_p, 0) AS n_dom_p,
      COALESCE(d.n_dom_c, 0) AS n_dom_c
    FROM sc_muni_tbl s
    LEFT JOIN dom_counts d
      USING (code_tract);
  ")

  elig_count_sql <- "(CASE WHEN s.n_dom_p > 0 THEN s.n_dom_p WHEN s.n_dom_p = 0 AND s.n_dom_c > 0 THEN s.n_dom_c ELSE 0 END)"
  elig_type_sql  <- "(CASE WHEN s.n_dom_p > 0 THEN 1 WHEN s.n_dom_p = 0 AND s.n_dom_c > 0 THEN 2 ELSE NULL END)"

  alloc_cols_sql <- character(0)

  for (v in vars) {
    if (v == "avg_inc_resp") {
      alloc_cols_sql <- c(
        alloc_cols_sql,
        paste0(
          "CASE
             WHEN s.code_tract IS NULL THEN NULL
             WHEN s.avg_inc_resp IS NULL THEN NULL
             WHEN ", elig_count_sql, " = 0 THEN NULL
             WHEN p.COD_ESPECIE = ", elig_type_sql, " THEN CAST(s.avg_inc_resp AS DOUBLE)
             ELSE NULL
           END AS avg_inc_resp_pt"
        )
      )
    } else if (v == pop_private) {
      alloc_cols_sql <- c(
        alloc_cols_sql,
        "
        CASE
          WHEN s.code_tract IS NULL THEN NULL
          WHEN s.n_inhab_p IS NULL THEN NULL
          WHEN p.COD_ESPECIE = 1 AND s.n_dom_p > 0 THEN CAST(s.n_inhab_p AS DOUBLE) / s.n_dom_p
          ELSE NULL
        END AS n_inhab_p_pt
        "
      )
    } else if (v == pop_collect) {
      alloc_cols_sql <- c(
        alloc_cols_sql,
        "
        CASE
          WHEN s.code_tract IS NULL THEN NULL
          WHEN s.n_inhab_c IS NULL THEN NULL
          WHEN p.COD_ESPECIE = 2 AND s.n_dom_c > 0 THEN CAST(s.n_inhab_c AS DOUBLE) / s.n_dom_c
          ELSE NULL
        END AS n_inhab_c_pt
        "
      )
    } else {
      alloc_cols_sql <- c(
        alloc_cols_sql,
        paste0(
          "CASE
             WHEN s.code_tract IS NULL THEN NULL
             WHEN s.", v, " IS NULL THEN NULL
             WHEN ", elig_count_sql, " = 0 THEN NULL
             WHEN p.COD_ESPECIE = ", elig_type_sql, " THEN CAST(s.", v, " AS DOUBLE) / ", elig_count_sql, "
             ELSE NULL
           END AS ", v, "_pt"
        )
      )
    }
  }

  DBI::dbExecute(con, paste0("
    CREATE OR REPLACE VIEW cnefe_alloc AS
    SELECT
      p.*,
      s.n_dom_p,
      s.n_dom_c,
      ", paste(alloc_cols_sql, collapse = ",\n      "), "
    FROM cnefe_sc p
    LEFT JOIN sc_muni_w_dom s
      USING (code_tract);
  "))

  t4_end <- Sys.time()
  time_step4 <- as.numeric(difftime(t4_end, t4, units = "secs"))
  if (verbose) message(sprintf("Step 4/6 (join and allocation prep) completed in %.2f s.", time_step4))

  # ---- Step 5/6: aggregate to H3 ----
  t5 <- Sys.time()
  if (verbose) message("Step 5/6: aggregating allocated values to H3 cells...")

  DBI::dbExecute(con, sprintf("
    CREATE OR REPLACE VIEW cnefe_alloc_hex AS
    SELECT
      *,
      h3_latlng_to_cell_string(lat, lon, %d) AS id_hex
    FROM cnefe_alloc
    WHERE lon IS NOT NULL AND lat IS NOT NULL;
  ", h3_resolution))

  agg_cols_sql <- character(0)
  for (v in vars) {
    if (v == "avg_inc_resp") {
      agg_cols_sql <- c(agg_cols_sql, "AVG(avg_inc_resp_pt) AS avg_inc_resp")
    } else if (v == pop_private) {
      agg_cols_sql <- c(agg_cols_sql, "SUM(COALESCE(n_inhab_p_pt, 0)) AS n_inhab_p")
    } else if (v == pop_collect) {
      agg_cols_sql <- c(agg_cols_sql, "SUM(COALESCE(n_inhab_c_pt, 0)) AS n_inhab_c")
    } else {
      agg_cols_sql <- c(agg_cols_sql, paste0("SUM(COALESCE(", v, "_pt, 0)) AS ", v))
    }
  }

  DBI::dbExecute(con, paste0("
    CREATE OR REPLACE TABLE hex_vals AS
    SELECT
      id_hex,
      ", paste(agg_cols_sql, collapse = ",\n      "), "
    FROM cnefe_alloc_hex
    WHERE id_hex IS NOT NULL
    GROUP BY 1;
  "))

  hex_vals <- DBI::dbGetQuery(con, "SELECT * FROM hex_vals;")

  t5_end <- Sys.time()
  time_step5 <- as.numeric(difftime(t5_end, t5, units = "secs"))
  if (verbose) message(sprintf("Step 5/6 (hex aggregation) completed in %.2f s.", time_step5))

  # ---- Step 6/6: build H3 grid + join ----
  t6 <- Sys.time()
  if (verbose) message("Step 6/6: building H3 grid and joining results...")

  # Expects an existing helper in your package that returns an sf grid with column `id_hex`
  hex_grid <- build_h3_grid(h3_resolution = h3_resolution, code_muni = code_muni)

  out <- dplyr::left_join(hex_grid, hex_vals, by = "id_hex")

  for (v in vars) {
    if (v == "avg_inc_resp") next
    if (v %in% names(out)) out[[v]] <- dplyr::coalesce(out[[v]], 0)
  }

  t6_end <- Sys.time()
  time_step6 <- as.numeric(difftime(t6_end, t6, units = "secs"))
  if (verbose) message(sprintf("Step 6/6 (sf output) completed in %.2f s.", time_step6))

  # ---- Diagnostics (only for requested vars) ----
  diag_lines <- character(0)
  tracts_na_parts <- character(0)
  tracts_noelig_parts <- character(0)

  for (v in vars) {
    if (v == "avg_inc_resp") next

    if (v == pop_private) {
      total <- DBI::dbGetQuery(con, "SELECT SUM(n_inhab_p) AS total FROM sc_muni_w_dom;")$total[1]
      alloc <- DBI::dbGetQuery(con, "SELECT SUM(COALESCE(n_inhab_p_pt,0)) AS a FROM cnefe_alloc;")$a[1]
      total <- ifelse(is.na(total), 0, total)
      alloc <- ifelse(is.na(alloc), 0, alloc)
      unalloc <- max(total - alloc, 0)
      pct <- if (total > 0) 100 * unalloc / total else NA_real_

      if (unalloc > 0 && total > 0) {
        diag_lines <- c(diag_lines, sprintf(
          "- Unallocated total for population from private households (n_inhab_p) = %.0f (%.2f%% of total).",
          unalloc, pct
        ))
      }

      n_na <- DBI::dbGetQuery(con, "SELECT SUM(CASE WHEN n_inhab_p IS NULL THEN 1 ELSE 0 END) AS n FROM sc_muni_w_dom;")$n[1]
      n_noelig <- DBI::dbGetQuery(con, "
        SELECT SUM(CASE WHEN n_inhab_p IS NOT NULL AND n_inhab_p <> 0 AND n_dom_p = 0 THEN 1 ELSE 0 END) AS n
        FROM sc_muni_w_dom;
      ")$n[1]

      if (!is.na(n_na) && n_na > 0) tracts_na_parts <- c(tracts_na_parts, sprintf("n_inhab_p NA in %d", n_na))
      if (!is.na(n_noelig) && n_noelig > 0) tracts_noelig_parts <- c(tracts_noelig_parts, sprintf("n_inhab_p in %d tracts", n_noelig))

    } else if (v == pop_collect) {
      total <- DBI::dbGetQuery(con, "SELECT SUM(n_inhab_c) AS total FROM sc_muni_w_dom;")$total[1]
      alloc <- DBI::dbGetQuery(con, "SELECT SUM(COALESCE(n_inhab_c_pt,0)) AS a FROM cnefe_alloc;")$a[1]
      total <- ifelse(is.na(total), 0, total)
      alloc <- ifelse(is.na(alloc), 0, alloc)
      unalloc <- max(total - alloc, 0)
      pct <- if (total > 0) 100 * unalloc / total else NA_real_

      if (unalloc > 0 && total > 0) {
        diag_lines <- c(diag_lines, sprintf(
          "- Unallocated total for population from collective households (n_inhab_c) = %.0f (%.2f%% of total).",
          unalloc, pct
        ))
      }

      n_na <- DBI::dbGetQuery(con, "SELECT SUM(CASE WHEN n_inhab_c IS NULL THEN 1 ELSE 0 END) AS n FROM sc_muni_w_dom;")$n[1]
      n_noelig <- DBI::dbGetQuery(con, "
        SELECT SUM(CASE WHEN n_inhab_c IS NOT NULL AND n_inhab_c <> 0 AND n_dom_c = 0 THEN 1 ELSE 0 END) AS n
        FROM sc_muni_w_dom;
      ")$n[1]

      if (!is.na(n_na) && n_na > 0) tracts_na_parts <- c(tracts_na_parts, sprintf("n_inhab_c NA in %d", n_na))
      if (!is.na(n_noelig) && n_noelig > 0) tracts_noelig_parts <- c(tracts_noelig_parts, sprintf("n_inhab_c in %d tracts", n_noelig))

    } else {
      total <- DBI::dbGetQuery(con, sprintf("SELECT SUM(%s) AS total FROM sc_muni_w_dom;", v))$total[1]
      alloc <- DBI::dbGetQuery(con, sprintf("SELECT SUM(COALESCE(%s_pt,0)) AS a FROM cnefe_alloc;", v))$a[1]
      total <- ifelse(is.na(total), 0, total)
      alloc <- ifelse(is.na(alloc), 0, alloc)
      unalloc <- max(total - alloc, 0)
      pct <- if (total > 0) 100 * unalloc / total else NA_real_

      if (unalloc > 0 && total > 0) {
        diag_lines <- c(diag_lines, sprintf(
          "- Unallocated total for %s = %.0f (%.2f%% of total).",
          v, unalloc, pct
        ))
      }

      n_na <- DBI::dbGetQuery(con, sprintf(
        "SELECT SUM(CASE WHEN %s IS NULL THEN 1 ELSE 0 END) AS n FROM sc_muni_w_dom;", v
      ))$n[1]

      n_noelig <- DBI::dbGetQuery(con, sprintf("
        SELECT SUM(
          CASE
            WHEN %s IS NOT NULL AND %s <> 0
             AND (CASE WHEN n_dom_p > 0 THEN n_dom_p WHEN n_dom_p = 0 AND n_dom_c > 0 THEN n_dom_c ELSE 0 END) = 0
            THEN 1 ELSE 0
          END
        ) AS n
        FROM sc_muni_w_dom;
      ", v, v))$n[1]

      if (!is.na(n_na) && n_na > 0) tracts_na_parts <- c(tracts_na_parts, sprintf("%s NA in %d", v, n_na))
      if (!is.na(n_noelig) && n_noelig > 0) tracts_noelig_parts <- c(tracts_noelig_parts, sprintf("%s in %d tracts", v, n_noelig))
    }
  }

  if ("avg_inc_resp" %in% vars) {
    n_assigned <- DBI::dbGetQuery(con, "
      SELECT SUM(CASE WHEN avg_inc_resp_pt IS NOT NULL THEN 1 ELSE 0 END) AS n
      FROM cnefe_alloc;
    ")$n[1]

    n_eligible <- DBI::dbGetQuery(con, "
      SELECT SUM(
        CASE
          WHEN (CASE WHEN n_dom_p > 0 THEN n_dom_p WHEN n_dom_p = 0 AND n_dom_c > 0 THEN n_dom_c ELSE 0 END) > 0
          THEN (CASE WHEN n_dom_p > 0 THEN n_dom_p WHEN n_dom_p = 0 AND n_dom_c > 0 THEN n_dom_c ELSE 0 END)
          ELSE 0
        END
      ) AS n
      FROM sc_muni_w_dom;
    ")$n[1]

    n_income_na <- DBI::dbGetQuery(con, "
      SELECT SUM(CASE WHEN avg_inc_resp IS NULL THEN 1 ELSE 0 END) AS n
      FROM sc_muni_w_dom;
    ")$n[1]

    diag_lines <- c(diag_lines, sprintf(
      "- avg_inc_resp assigned to %d of %d eligible points.",
      as.integer(n_assigned %||% 0), as.integer(n_eligible %||% 0)
    ))
    if (!is.na(n_income_na) && n_income_na > 0) {
      diag_lines <- c(diag_lines, sprintf("- avg_inc_resp NA in %d tracts.", n_income_na))
    }
  }

  if (!is.na(n_unmatched) && n_unmatched > 0) {
    diag_lines <- c(diag_lines, sprintf("- Unmatched CNEFE points (no tract) = %d.", n_unmatched))
  }

  if (length(tracts_na_parts) > 0) {
    diag_lines <- c(diag_lines, paste0("- Tracts with NA totals: ", paste(tracts_na_parts, collapse = "; "), "."))
  }
  if (length(tracts_noelig_parts) > 0) {
    diag_lines <- c(diag_lines, paste0("- Tracts with no eligible dwellings: ", paste(tracts_noelig_parts, collapse = "; "), "."))
  }

  if (length(diag_lines) > 0) {
    warning(
      paste0("Dasymetric interpolation diagnostics:\n", paste(diag_lines, collapse = "\n")),
      call. = FALSE
    )
  }

  timing <- c(
    step_1_duckdb_ready = time_step1,
    step_2_tracts_ready = time_step2,
    step_3_cnefe_ready  = time_step3,
    step_4_join_alloc   = time_step4,
    step_5_hex_agg      = time_step5,
    step_6_sf_output    = time_step6,
    total               = as.numeric(difftime(Sys.time(), t0_total, units = "secs"))
  )
  attr(out, "timing") <- timing

  if (verbose) {
    message("Timing summary (seconds):")
    message(sprintf(" - Step 1/6 (DuckDB ready): %.2f", timing["step_1_duckdb_ready"]))
    message(sprintf(" - Step 2/6 (tracts ready): %.2f", timing["step_2_tracts_ready"]))
    message(sprintf(" - Step 3/6 (CNEFE points ready): %.2f", timing["step_3_cnefe_ready"]))
    message(sprintf(" - Step 4/6 (join and allocation prep): %.2f", timing["step_4_join_alloc"]))
    message(sprintf(" - Step 5/6 (hex aggregation): %.2f", timing["step_5_hex_agg"]))
    message(sprintf(" - Step 6/6 (sf output): %.2f", timing["step_6_sf_output"]))
    message(sprintf("Total time: %.2f s.", timing["total"]))
  }

  out
}
