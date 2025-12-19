#' Convert census tract aggregates to an H3 grid using CNEFE points
#'
#' @description
#' `tracts_to_h3()` performs a dasymetric interpolation, considering the following steps:
#' 1) census tract totals are allocated to CNEFE dwelling points inside each tract;
#' 2) allocated values are aggregated to an H3 grid at a user-defined resolution.
#'
#' The function uses DuckDB with the spatial and H3 extensions for the heavy work.
#'
#' @param code_muni Integer. Seven-digit IBGE municipality code.
#' @param h3_resolution Integer. H3 resolution (0 to 15). Defaults to resolution 9.
#' @param vars Character vector. Names of tract-level variables to interpolate.
#'   Supported variables:
#'   - `n_inhab_p`: population in private households (*Domicílios particulares*).
#'   - `n_inhab_c`: population in collective households (*Domicílios coletivos*).
#'   - `n_inhab`: total population (as provided in the tract aggregates).
#'   - `male`: total male population.
#'   - `female`: total female population.
#'   - `age_0_4`, `age_5_9`, `age_10_14`, `age_15_19`, `age_20_24`, `age_25_29`,
#'     `age_30_39`, `age_40_49`, `age_50_59`, `age_60_69`, `age_70m`: population by age group.
#'   - `n_resp`: number of household heads in the census tract (*Pessoas responsáveis por domicílios*).
#'   - `avg_inc_resp`: average income of the household heads.
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

  # normalize inputs ----------------------------------------------------------
  if (exists(".normalize_code_muni", mode = "function")) {
    code_muni <- .normalize_code_muni(code_muni)
  } else {
    code_muni <- as.integer(code_muni)
  }

  h3_resolution <- as.integer(h3_resolution)
  vars <- unique(as.character(vars))

  if (length(vars) == 0) {
    stop("`vars` must contain at least one variable name.")
  }

  allowed_vars <- c(
    "n_inhab_p", "n_inhab_c",
    "male", "female",
    "age_0_4", "age_5_9", "age_10_14", "age_15_19", "age_20_24", "age_25_29",
    "age_30_39", "age_40_49", "age_50_59", "age_60_69", "age_70m",
    "n_resp",
    "avg_inc_resp"
  )

  bad_vars <- setdiff(vars, allowed_vars)
  if (length(bad_vars) > 0) {
    stop(
      "Unknown `vars`: ", paste(bad_vars, collapse = ", "),
      ". See `?tracts_to_h3` for available variables."
    )
  }

  # helpers -------------------------------------------------------------------
  .t0 <- function() Sys.time()
  .td <- function(t_start, t_end) as.numeric(difftime(t_end, t_start, units = "secs"))

  .duckdb_quiet_execute <- function(con, sql) {
    invisible(utils::capture.output(
      suppressMessages(DBI::dbExecute(con, sql)),
      type = "output"
    ))
  }

  .duckdb_load_ext <- function(con, ext) {
    ok <- tryCatch({
      .duckdb_quiet_execute(con, sprintf("LOAD %s;", ext))
      TRUE
    }, error = function(e) FALSE)

    if (!ok) {
      .duckdb_quiet_execute(con, sprintf("INSTALL %s;", ext))
      .duckdb_quiet_execute(con, sprintf("LOAD %s;", ext))
    }
    invisible(TRUE)
  }

  .fmt_pct <- function(x) sprintf("%.2f%%", x)

  # timing container ----------------------------------------------------------
  step_times <- numeric(0)

  if (verbose) message(sprintf("Processing code %s...", code_muni))

  # Step 1/6 ------------------------------------------------------------------
  if (verbose) message("Step 1/6: connecting to DuckDB and loading extensions...")
  t1 <- .t0()

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  duckspatial::ddbs_load(con)

  # always keep DuckDB extension load messages silent
  .duckdb_load_ext(con, "zipfs")
  .duckdb_load_ext(con, "h3")

  t1e <- .t0()
  step_times["DuckDB ready"] <- .td(t1, t1e)
  if (verbose) message(sprintf("Step 1/6 (DuckDB ready) completed in %.2f s.", step_times["DuckDB ready"]))

  # Step 2/6 ------------------------------------------------------------------
  if (verbose) message("Step 2/6: preparing census tracts in DuckDB...")
  t2 <- .t0()

  .sc_create_views_in_duckdb(con, code_muni = code_muni, cache = cache, verbose = verbose)

  .duckdb_quiet_execute(con, "CREATE OR REPLACE TABLE sc_muni_tbl AS SELECT * FROM sc_muni;")
  .duckdb_quiet_execute(con, "CREATE INDEX IF NOT EXISTS sc_muni_geom_idx ON sc_muni_tbl USING RTREE (geom);")

  t2e <- .t0()
  step_times["tracts ready"] <- .td(t2, t2e)
  if (verbose) message(sprintf("Step 2/6 (tracts ready) completed in %.2f s.", step_times["tracts ready"]))

  # Step 3/6 ------------------------------------------------------------------
  if (verbose) message("Step 3/6: preparing CNEFE points in DuckDB...")
  t3 <- .t0()

  .cnefe_create_points_view_in_duckdb(con, code_muni = code_muni, cache = cache, verbose = verbose)

  .duckdb_quiet_execute(con, "
    CREATE OR REPLACE TABLE cnefe_pts_tbl AS
    SELECT *
    FROM cnefe_pts
    WHERE COD_ESPECIE IN (1, 2)
      AND lon IS NOT NULL
      AND lat IS NOT NULL
      AND geom IS NOT NULL;
  ")

  total_pts <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cnefe_pts_tbl;")$n[1]

  t3e <- .t0()
  step_times["CNEFE points ready"] <- .td(t3, t3e)
  if (verbose) message(sprintf("Step 3/6 (CNEFE points ready) completed in %.2f s.", step_times["CNEFE points ready"]))

  # Step 4/6 ------------------------------------------------------------------
  if (verbose) message("Step 4/6: spatial join (points to tracts) and allocation prep...")
  t4 <- .t0()

  # Matched points only (spatial join without LEFT JOIN)
  # IMPORTANT: bring ONLY code_tract from tracts (fixes the `s.` parser bug
  # and avoids carrying unrequested columns).
  .duckdb_quiet_execute(con, "
    CREATE OR REPLACE TABLE cnefe_sc AS
    SELECT
      p.*,
      s.code_tract
    FROM cnefe_pts_tbl p,
         sc_muni_tbl s
    WHERE ST_Within(p.geom, s.geom);
  ")

  matched_pts <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cnefe_sc;")$n[1]
  unmatched_pts <- max(total_pts - matched_pts, 0)

  # Denominators by tract (counts of dwellings of each type)
  .duckdb_quiet_execute(con, "
    CREATE OR REPLACE VIEW dom_counts AS
    SELECT
      code_tract,
      SUM(CASE WHEN COD_ESPECIE = 1 THEN 1 ELSE 0 END) AS n_dom_p,
      SUM(CASE WHEN COD_ESPECIE = 2 THEN 1 ELSE 0 END) AS n_dom_c
    FROM cnefe_sc
    GROUP BY 1;
  ")

  .duckdb_quiet_execute(con, "
    CREATE OR REPLACE VIEW sc_muni_w_dom AS
    SELECT
      s.*,
      COALESCE(d.n_dom_p, 0) AS n_dom_p,
      COALESCE(d.n_dom_c, 0) AS n_dom_c
    FROM sc_muni_tbl s
    LEFT JOIN dom_counts d
      USING (code_tract);
  ")

  # Allocation view:
  # - totals: per-point = total / eligible_count
  # - avg_inc_resp: assigned to each eligible point, aggregated later as mean
  alloc_exprs <- character(0)

  for (v in vars) {
    if (v == "avg_inc_resp") {
      alloc_exprs <- c(alloc_exprs, "
        CASE
          WHEN (CASE
                  WHEN s.n_dom_p > 0 THEN (p.COD_ESPECIE = 1)
                  WHEN s.n_dom_c > 0 THEN (p.COD_ESPECIE = 2)
                  ELSE FALSE
                END)
           AND s.avg_inc_resp IS NOT NULL
          THEN CAST(s.avg_inc_resp AS DOUBLE)
          ELSE NULL
        END AS avg_inc_resp_pt
      ")
    } else if (v == "n_inhab_p") {
      alloc_exprs <- c(alloc_exprs, "
        CASE
          WHEN p.COD_ESPECIE = 1
           AND s.n_inhab_p IS NOT NULL
           AND s.n_dom_p > 0
          THEN CAST(s.n_inhab_p AS DOUBLE) / s.n_dom_p
          ELSE NULL
        END AS n_inhab_p_pt
      ")
    } else if (v == "n_inhab_c") {
      alloc_exprs <- c(alloc_exprs, "
        CASE
          WHEN p.COD_ESPECIE = 2
           AND s.n_inhab_c IS NOT NULL
           AND s.n_dom_c > 0
          THEN CAST(s.n_inhab_c AS DOUBLE) / s.n_dom_c
          ELSE NULL
        END AS n_inhab_c_pt
      ")
    } else {
      alloc_exprs <- c(alloc_exprs, sprintf("
        CASE
          WHEN (CASE
                  WHEN s.n_dom_p > 0 THEN (p.COD_ESPECIE = 1)
                  WHEN s.n_dom_c > 0 THEN (p.COD_ESPECIE = 2)
                  ELSE FALSE
                END)
           AND s.%s IS NOT NULL
           AND (CASE
                  WHEN s.n_dom_p > 0 THEN s.n_dom_p
                  WHEN s.n_dom_c > 0 THEN s.n_dom_c
                  ELSE 0
                END) > 0
          THEN CAST(s.%s AS DOUBLE) /
               (CASE
                  WHEN s.n_dom_p > 0 THEN s.n_dom_p
                  WHEN s.n_dom_c > 0 THEN s.n_dom_c
                  ELSE 0
                END)
          ELSE NULL
        END AS %s_pt
      ", v, v, v))
    }
  }

  alloc_sql <- paste(alloc_exprs, collapse = ",\n")

  .duckdb_quiet_execute(con, sprintf("
    CREATE OR REPLACE VIEW cnefe_alloc AS
    SELECT
      p.*,
      s.n_dom_p,
      s.n_dom_c,
      h3_latlng_to_cell_string(p.lat, p.lon, %d) AS id_hex,
      %s
    FROM cnefe_sc p
    JOIN sc_muni_w_dom s
      USING (code_tract)
    WHERE p.lon IS NOT NULL AND p.lat IS NOT NULL;
  ", h3_resolution, alloc_sql))

  t4e <- .t0()
  step_times["join and allocation prep"] <- .td(t4, t4e)
  if (verbose) message(sprintf("Step 4/6 (join and allocation prep) completed in %.2f s.", step_times["join and allocation prep"]))

  # Step 5/6 ------------------------------------------------------------------
  if (verbose) message("Step 5/6: aggregating allocated values to H3 cells...")
  t5 <- .t0()

  agg_exprs <- character(0)
  for (v in vars) {
    if (v == "avg_inc_resp") {
      agg_exprs <- c(agg_exprs, "AVG(avg_inc_resp_pt) AS avg_inc_resp")
    } else {
      agg_exprs <- c(agg_exprs, sprintf("SUM(%s_pt) AS %s", v, v))
    }
  }

  .duckdb_quiet_execute(con, sprintf("
    CREATE OR REPLACE VIEW hex_vals AS
    SELECT
      id_hex,
      %s
    FROM cnefe_alloc
    WHERE id_hex IS NOT NULL
    GROUP BY 1;
  ", paste(agg_exprs, collapse = ",\n      ")))

  t5e <- .t0()
  step_times["hex aggregation"] <- .td(t5, t5e)
  if (verbose) message(sprintf("Step 5/6 (hex aggregation) completed in %.2f s.", step_times["hex aggregation"]))

  # Step 6/6 ------------------------------------------------------------------
  if (verbose) message("Step 6/6: building H3 grid and joining results...")
  t6 <- .t0()

  hex_df <- DBI::dbGetQuery(con, "SELECT * FROM hex_vals;")

  hex_sf <- h3jsr::cell_to_polygon(hex_df$id_hex, simple = FALSE) %>%
    dplyr::rename(id_hex = "h3_address")

  out <- hex_sf %>%
    dplyr::left_join(hex_df, by = "id_hex") %>%
    sf::st_as_sf() %>%
    dplyr::select(id_hex, dplyr::all_of(vars), geometry)

  sf::st_crs(out) <- 4326

  t6e <- .t0()
  step_times["sf output"] <- .td(t6, t6e)
  if (verbose) message(sprintf("Step 6/6 (sf output) completed in %.2f s.", step_times["sf output"]))

  # attributes ----------------------------------------------------------------
  attr(out, "timing") <- as.list(step_times)

  if (verbose) {
    total_time <- sum(step_times)
    message("Timing summary (seconds):")
    for (nm in names(step_times)) {
      message(sprintf(" - %s: %.2f", nm, step_times[[nm]]))
    }
    message(sprintf("Total time: %.2f s.", total_time))
  }

  # diagnostics and warning ----------------------------------------------------
  warn_lines <- character(0)

  totals_vars <- setdiff(vars, "avg_inc_resp")

  for (v in totals_vars) {
    total_v <- DBI::dbGetQuery(con, sprintf(
      "SELECT SUM(%s) AS total FROM sc_muni_tbl WHERE %s IS NOT NULL;", v, v
    ))$total[1]
    alloc_v <- DBI::dbGetQuery(con, sprintf(
      "SELECT SUM(%s_pt) AS alloc FROM cnefe_alloc WHERE %s_pt IS NOT NULL;", v, v
    ))$alloc[1]

    total_v <- if (is.null(total_v) || is.na(total_v)) 0 else as.numeric(total_v)
    alloc_v <- if (is.null(alloc_v)  || is.na(alloc_v)) 0 else as.numeric(alloc_v)

    unalloc <- max(total_v - alloc_v, 0)
    if (total_v > 0 && unalloc > 0) {
      pct <- 100 * unalloc / total_v

      label <- v
      if (v == "n_inhab_p") label <- "population from private households (n_inhab_p)"
      if (v == "n_inhab_c") label <- "population from collective households (n_inhab_c)"

      warn_lines <- c(
        warn_lines,
        sprintf("- Unallocated total for %s = %.0f (%s of total).", label, unalloc, .fmt_pct(pct))
      )
    }
  }

  if ("avg_inc_resp" %in% vars) {
    eligible_avg <- DBI::dbGetQuery(con, "
      SELECT COUNT(*) AS n
      FROM cnefe_sc p
      JOIN sc_muni_w_dom s USING (code_tract)
      WHERE
        (CASE
           WHEN s.n_dom_p > 0 THEN (p.COD_ESPECIE = 1)
           WHEN s.n_dom_c > 0 THEN (p.COD_ESPECIE = 2)
           ELSE FALSE
         END);
    ")$n[1]

    assigned_avg <- DBI::dbGetQuery(con, "
      SELECT COUNT(*) AS n
      FROM cnefe_alloc
      WHERE avg_inc_resp_pt IS NOT NULL;
    ")$n[1]

    warn_lines <- c(
      warn_lines,
      sprintf("- avg_inc_resp assigned to %d of %d eligible points.", assigned_avg, eligible_avg)
    )

    na_avg_tracts <- DBI::dbGetQuery(con, "
      SELECT COUNT(*) AS n
      FROM sc_muni_tbl
      WHERE avg_inc_resp IS NULL;
    ")$n[1]

    if (na_avg_tracts > 0) {
      warn_lines <- c(warn_lines, sprintf("- avg_inc_resp NA in %d tracts.", na_avg_tracts))
    }
  }

  if (unmatched_pts > 0) {
    warn_lines <- c(warn_lines, sprintf("- Unmatched CNEFE points (no tract) = %d.", unmatched_pts))
  }

  na_totals <- character(0)
  for (v in totals_vars) {
    n_na <- DBI::dbGetQuery(con, sprintf(
      "SELECT COUNT(*) AS n FROM sc_muni_tbl WHERE %s IS NULL;", v
    ))$n[1]
    if (n_na > 0) {
      na_totals <- c(na_totals, sprintf("%s NA in %d", v, n_na))
    }
  }
  if (length(na_totals) > 0) {
    warn_lines <- c(warn_lines, paste0("- Tracts with NA totals: ", paste(na_totals, collapse = "; "), "."))
  }

  no_elig <- character(0)
  for (v in totals_vars) {
    if (v == "n_inhab_p") {
      n0 <- DBI::dbGetQuery(con, "
        SELECT COUNT(*) AS n
        FROM sc_muni_w_dom
        WHERE n_inhab_p IS NOT NULL AND n_inhab_p > 0 AND n_dom_p = 0;
      ")$n[1]
    } else if (v == "n_inhab_c") {
      n0 <- DBI::dbGetQuery(con, "
        SELECT COUNT(*) AS n
        FROM sc_muni_w_dom
        WHERE n_inhab_c IS NOT NULL AND n_inhab_c > 0 AND n_dom_c = 0;
      ")$n[1]
    } else {
      n0 <- DBI::dbGetQuery(con, sprintf("
        SELECT COUNT(*) AS n
        FROM sc_muni_w_dom
        WHERE %s IS NOT NULL AND %s > 0
          AND (CASE
                 WHEN n_dom_p > 0 THEN n_dom_p
                 WHEN n_dom_c > 0 THEN n_dom_c
                 ELSE 0
               END) = 0;
      ", v, v))$n[1]
    }

    if (n0 > 0) {
      no_elig <- c(no_elig, sprintf("%s in %d tracts", v, n0))
    }
  }
  if (length(no_elig) > 0) {
    warn_lines <- c(warn_lines, paste0("- Tracts with no eligible dwellings: ", paste(no_elig, collapse = "; "), "."))
  }

  if (length(warn_lines) > 0) {
    warning(
      paste(c("Dasymetric interpolation diagnostics:", warn_lines), collapse = "\n"),
      call. = FALSE
    )
  }

  out
}
