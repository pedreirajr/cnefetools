#' Convert census tract aggregates to an H3 grid using CNEFE points
#'
#' @description
#' `tracts_to_h3()` performs a dasymetric interpolation with the following steps:
#' 1) census tract totals are allocated to CNEFE dwelling points inside each tract;
#' 2) allocated values are aggregated to an H3 grid at a user-defined resolution.
#'
#' The function uses DuckDB with the spatial and H3 extensions for the heavy work.
#'
#' @param code_muni Integer. Seven-digit IBGE municipality code.
#' @param year Integer. The CNEFE data year. Currently only 2022 is supported.
#'   Defaults to 2022.
#' @param h3_resolution Integer. H3 resolution (0 to 15). Defaults to 9.
#' @param vars Character vector. Names of tract-level variables to interpolate.
#'   Supported variables:
#'   - `pop_ph`: population in private households (*Domicílios particulares*).
#'   - `pop_ch`: population in collective households (*Domicílios coletivos*).
#'   - `male`: total male population.
#'   - `female`: total female population.
#'   - `age_0_4`, `age_5_9`, `age_10_14`, `age_15_19`, `age_20_24`, `age_25_29`,
#'     `age_30_39`, `age_40_49`, `age_50_59`, `age_60_69`, `age_70m`: population by age group.
#'   - `race_branca`, `race_preta`, `race_amarela`, `race_parda`, `race_indigena`:
#'     population by race/color (*cor ou raça*).
#'   - `n_resp`: number of household heads (*Pessoas responsáveis por domicílios*).
#'   - `avg_inc_resp`: average income of the household heads.
#'
#'   For a reference table mapping these variable names to the official IBGE
#'   census tract codes and descriptions, see [tracts_variables_ref].
#'
#'   Allocation rules:
#'   - `pop_ph` is allocated only to private dwellings.
#'   - `pop_ch` is allocated only to collective dwellings.
#'   - All other sum-like variables are allocated to private dwellings when the tract has any;
#'     if the tract has zero private dwellings but has collective dwellings, they are allocated to collective.
#'   - `avg_inc_resp` is assigned (not split) to each eligible dwelling point using the same eligibility rule.
#'
#' @param cache Logical. Whether to use the existing package cache for assets and CNEFE zips.
#' @param verbose Logical. Whether to print step messages and timing.
#'
#' @return An `sf` object (CRS 4326) with an H3 grid and the requested interpolated variables.
#'
#' @examples
#' \donttest{
#' # Interpolate population to H3 hexagons
#' hex_pop <- tracts_to_h3(
#'   code_muni = 2929057,
#'   vars = c("pop_ph", "pop_ch")
#' )
#' }
#'
#' @export
tracts_to_h3 <- function(
  code_muni,
  year = 2022,
  h3_resolution = 9,
  vars = c("pop_ph", "pop_ch"),
  cache = TRUE,
  verbose = TRUE
) {
  # normalize inputs ----------------------------------------------------------
  if (exists(".normalize_code_muni", mode = "function")) {
    code_muni <- .normalize_code_muni(code_muni)
  } else {
    code_muni <- as.integer(code_muni)
  }

  year <- .validate_year(year)

  # Get the appropriate index for the requested year
  cnefe_index <- .get_cnefe_index(year)

  h3_resolution <- as.integer(h3_resolution)
  vars <- unique(as.character(vars))

  if (length(vars) == 0) {
    cli::cli_abort("`vars` must contain at least one variable name.")
  }

  allowed_vars <- c(
    "pop_ph",
    "pop_ch",
    "male",
    "female",
    "age_0_4",
    "age_5_9",
    "age_10_14",
    "age_15_19",
    "age_20_24",
    "age_25_29",
    "age_30_39",
    "age_40_49",
    "age_50_59",
    "age_60_69",
    "age_70m",
    "race_branca",
    "race_preta",
    "race_amarela",
    "race_parda",
    "race_indigena",
    "n_resp",
    "avg_inc_resp"
  )

  bad_vars <- setdiff(vars, allowed_vars)
  if (length(bad_vars) > 0) {
    cli::cli_abort(
      "Unknown `vars`: {bad_vars}. See `?tracts_to_h3` for available variables."
    )
  }

  # helpers -------------------------------------------------------------------

  .duckdb_quiet_execute <- function(con, sql) {
    invisible(utils::capture.output(
      utils::capture.output(
        DBI::dbExecute(con, sql),
        type = "message"
      ),
      type = "output"
    ))
  }

  .duckdb_load_ext <- function(con, ext) {
    utils::capture.output(
      utils::capture.output({
        ok <- tryCatch(
          {
            .duckdb_quiet_execute(con, sprintf("LOAD %s;", ext))
            TRUE
          },
          error = function(e) FALSE
        )

        if (!ok) {
          .duckdb_quiet_execute(con, sprintf("INSTALL %s FROM community;", ext))
          .duckdb_quiet_execute(con, sprintf("LOAD %s;", ext))
        }
      }, type = "message"),
      type = "output"
    )

    invisible(TRUE)
  }

  .fmt_pct <- function(x) sprintf("%.2f%%", x)

  # timing container ----------------------------------------------------------
  if (verbose) {
    cli::cli_alert_info("Processing code {code_muni}")
  }


  # Step 1/6 ------------------------------------------------------------------

  if (verbose) {
    cli::cli_progress_step("Step 1/6: connecting to DuckDB and loading extensions...",
                           msg_done = "Step 1/6 (DuckDB ready)")

  }

  con <- NULL
  utils::capture.output(
    utils::capture.output({
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:",
                            config = list(
                              'enable_progress_bar' = FALSE,
                              'enable_print_progress' = FALSE,
                              'print_progress_bar' = FALSE
                            ))

      duckspatial::ddbs_load(con)
      .duckdb_load_ext(con, "zipfs")
      .duckdb_load_ext(con, "h3")
    }, type = "message"),
    type = "output"
  )

  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  if (verbose) {
    cli::cli_progress_done("Step 1/6: connecting to DuckDB and loading extensions...")
  }

  # Step 2/6 ------------------------------------------------------------------

  if (verbose) {
    cli::cli_progress_step("Step 2/6: preparing census tracts in DuckDB...",
                           msg_done = "Step 2/6 (Tracts ready)")

  }

  .sc_create_views_in_duckdb(
    con,
    code_muni = code_muni,
    cache = cache,
    verbose = verbose
  )

  .duckdb_quiet_execute(
    con,
    "CREATE OR REPLACE TABLE sc_muni_tbl AS SELECT * FROM sc_muni;"
  )
  .duckdb_quiet_execute(
    con,
    "CREATE INDEX IF NOT EXISTS sc_muni_geom_idx ON sc_muni_tbl USING RTREE (geom);"
  )

  if (verbose) {
    cli::cli_progress_done("Step 2/6: preparing census tracts in DuckDB...")
  }

  # Step 3/6 ------------------------------------------------------------------

  if (verbose) {
    cli::cli_progress_step("Step 3/6: preparing CNEFE points in DuckDB...",
                           msg_done = "Step 3/6 (CNEFE points ready)")

  }

  invisible(utils::capture.output({
    invisible(utils::capture.output({
      .cnefe_create_points_view_in_duckdb(
        con,
        code_muni = code_muni,
        index = cnefe_index,
        cache = cache,
        verbose = verbose
      )

      .duckdb_quiet_execute(
        con,
        "
      CREATE OR REPLACE TABLE cnefe_pts_tbl AS
      SELECT *
      FROM cnefe_pts
      WHERE COD_ESPECIE IN (1, 2)
        AND lon IS NOT NULL
        AND lat IS NOT NULL
        AND geom IS NOT NULL;
    "
      )

      total_pts <- DBI::dbGetQuery(
        con,
        "SELECT COUNT(*) AS n FROM cnefe_pts_tbl;"
      )$n[1]
    }, type = "message"))
  }, type = "output"))

  if (verbose) {
    cli::cli_progress_done("Step 3/6: preparing CNEFE points in DuckDB...")
  }

  # Step 4/6 ------------------------------------------------------------------

  if (verbose) {
    cli::cli_progress_step("Step 4/6: spatial join (points to tracts) and allocation prep...",
                           msg_done = "Step 4/6 (Join and allocation)")

  }

  # Matched points only (spatial join without LEFT JOIN)
  # IMPORTANT: bring ONLY code_tract from tracts (fixes the `s.` parser bug
  # and avoids carrying unrequested columns).
  .duckdb_quiet_execute(
    con,
    "
    CREATE OR REPLACE TABLE cnefe_sc AS
    SELECT
      p.*,
      s.code_tract
    FROM cnefe_pts_tbl p,
         sc_muni_tbl s
    WHERE ST_Within(p.geom, s.geom);
  "
  )

  matched_pts <- suppressMessages(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cnefe_sc;")$n[1]
  )
  unmatched_pts <- max(total_pts - matched_pts, 0)

  # Denominators by tract (counts of dwellings of each type)
  .duckdb_quiet_execute(
    con,
    "
    CREATE OR REPLACE VIEW dom_counts AS
    SELECT
      code_tract,
      SUM(CASE WHEN COD_ESPECIE = 1 THEN 1 ELSE 0 END) AS n_dom_p,
      SUM(CASE WHEN COD_ESPECIE = 2 THEN 1 ELSE 0 END) AS n_dom_c
    FROM cnefe_sc
    GROUP BY 1;
  "
  )

  .duckdb_quiet_execute(
    con,
    "
    CREATE OR REPLACE VIEW sc_muni_w_dom AS
    SELECT
      s.*,
      COALESCE(d.n_dom_p, 0) AS n_dom_p,
      COALESCE(d.n_dom_c, 0) AS n_dom_c
    FROM sc_muni_tbl s
    LEFT JOIN dom_counts d
      USING (code_tract);
  "
  )

  # Allocation view:
  # - totals: per-point = total / eligible_count
  # - avg_inc_resp: assigned to each eligible point, aggregated later as mean
  alloc_exprs <- character(0)

  for (v in vars) {
    if (v == "avg_inc_resp") {
      alloc_exprs <- c(
        alloc_exprs,
        "
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
      "
      )
    } else if (v == "pop_ph") {
      alloc_exprs <- c(
        alloc_exprs,
        "
        CASE
          WHEN p.COD_ESPECIE = 1
           AND s.pop_ph IS NOT NULL
           AND s.n_dom_p > 0
          THEN CAST(s.pop_ph AS DOUBLE) / s.n_dom_p
          ELSE NULL
        END AS pop_ph_pt
      "
      )
    } else if (v == "pop_ch") {
      alloc_exprs <- c(
        alloc_exprs,
        "
        CASE
          WHEN p.COD_ESPECIE = 2
           AND s.pop_ch IS NOT NULL
           AND s.n_dom_c > 0
          THEN CAST(s.pop_ch AS DOUBLE) / s.n_dom_c
          ELSE NULL
        END AS pop_ch_pt
      "
      )
    } else {
      alloc_exprs <- c(
        alloc_exprs,
        sprintf(
          "
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
      ",
          v,
          v,
          v
        )
      )
    }
  }

  alloc_sql <- paste(alloc_exprs, collapse = ",\n")

  .duckdb_quiet_execute(
    con,
    sprintf(
      "
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
  ",
      h3_resolution,
      alloc_sql
    )
  )

  if (verbose) {
    cli::cli_progress_done("Step 4/6: spatial join (points to tracts) and allocation prep...")
  }

  # Step 5/6 ------------------------------------------------------------------

  if (verbose) {
    cli::cli_progress_step("Step 5/6: aggregating allocated values to H3 cells...",
                           msg_done = "Step 5/6 (Hex aggregation)")

  }

  agg_exprs <- character(0)
  for (v in vars) {
    if (v == "avg_inc_resp") {
      agg_exprs <- c(agg_exprs, "AVG(avg_inc_resp_pt) AS avg_inc_resp")
    } else {
      agg_exprs <- c(agg_exprs, sprintf("SUM(%s_pt) AS %s", v, v))
    }
  }

  .duckdb_quiet_execute(
    con,
    sprintf(
      "
    CREATE OR REPLACE VIEW hex_vals AS
    SELECT
      id_hex,
      %s
    FROM cnefe_alloc
    WHERE id_hex IS NOT NULL
    GROUP BY 1;
  ",
      paste(agg_exprs, collapse = ",\n      ")
    )
  )

  if (verbose) {
    cli::cli_progress_done("Step 5/6: aggregating allocated values to H3 cells...")
  }

  # Step 6/6 ------------------------------------------------------------------

  if (verbose) {
    cli::cli_progress_step("Step 6/6: building H3 grid and joining results...",
                           msg_done = "Step 6/6 (sf output)")

  }

  # capture.output para capturar TUDO (output E messages)
  invisible(utils::capture.output({
    invisible(utils::capture.output({
      hex_df <- DBI::dbGetQuery(con, "SELECT * FROM hex_vals;")

      hex_sf <- h3jsr::cell_to_polygon(hex_df$id_hex, simple = FALSE) |>
        dplyr::rename(id_hex = "h3_address")

      out <- hex_sf |>
        dplyr::left_join(hex_df, by = "id_hex") |>
        sf::st_as_sf() |>
        dplyr::select(id_hex, dplyr::all_of(vars), geometry)
    }, type = "message"))
  }, type = "output"))

  sf::st_crs(out) <- 4326

  if (verbose) {
    cli::cli_progress_done("Step 6/6: building H3 grid and joining results...")
    cli::cli_progress_done()
  }
  # diagnostics and warning ----------------------------------------------------
  warn_lines <- character(0)

  n_tracts <- suppressMessages(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM sc_muni_tbl;")$n[1]
  )

  totals_vars <- setdiff(vars, "avg_inc_resp")

  for (v in totals_vars) {
    total_v <- suppressMessages(
      DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT SUM(%s) AS total FROM sc_muni_tbl WHERE %s IS NOT NULL;",
        v,
        v
      )
    )$total[1]
    )

    alloc_v <- suppressMessages(
      DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT SUM(%s_pt) AS alloc FROM cnefe_alloc WHERE %s_pt IS NOT NULL;",
        v,
        v
      )
    )$alloc[1]
    )

    total_v <- if (is.null(total_v) || is.na(total_v)) {
      0
    } else {
      as.numeric(total_v)
    }
    alloc_v <- if (is.null(alloc_v) || is.na(alloc_v)) {
      0
    } else {
      as.numeric(alloc_v)
    }

    # Use threshold >= 0.5 to avoid floating point precision issues
    unalloc <- max(total_v - alloc_v, 0)
    unalloc <- if (unalloc < 0.5) 0 else round(unalloc)
    pct <- if (total_v > 0) 100 * unalloc / total_v else 0

    label <- switch(v,
                    "pop_ph" = "population from private households",
                    "pop_ch" = "population from collective households",
                    v  # default: use variable name
    )

    # Always show all requested variables for consistency
    warn_lines <- c(
      warn_lines,
      cli::format_inline(
        "Unallocated total for {label} ({.field {v}}): {.strong {sprintf('%.0f', unalloc)}} of {.strong {sprintf('%.0f', total_v)}} ({.strong {sprintf('%.2f%%', pct)}})"
      )
    )
  }

  if ("avg_inc_resp" %in% vars) {
    eligible_avg <- suppressMessages(
      DBI::dbGetQuery(
      con,
      "
    SELECT COUNT(*) AS n
    FROM cnefe_sc p
    JOIN sc_muni_w_dom s USING (code_tract)
    WHERE
      (CASE
         WHEN s.n_dom_p > 0 THEN (p.COD_ESPECIE = 1)
         WHEN s.n_dom_c > 0 THEN (p.COD_ESPECIE = 2)
         ELSE FALSE
       END);
  "
    )$n[1]
    )

    assigned_avg <- suppressMessages(
      DBI::dbGetQuery(
      con,
      "
    SELECT COUNT(*) AS n
    FROM cnefe_alloc
    WHERE avg_inc_resp_pt IS NOT NULL;
  "
    )$n[1]
    )

    assigned_pct <- if (eligible_avg > 0) 100 * assigned_avg / eligible_avg else 0
    warn_lines <- c(
      warn_lines,
      cli::format_inline(
        "{.field avg_inc_resp} assigned to {.strong {assigned_avg}} of {.strong {eligible_avg}} eligible points ({.strong {sprintf('%.2f%%', assigned_pct)}} of total points)"
      )
    )

    na_avg_tracts <- suppressMessages(
      DBI::dbGetQuery(
      con,
      "
    SELECT COUNT(*) AS n
    FROM sc_muni_tbl
    WHERE avg_inc_resp IS NULL;
  "
    )$n[1]
    )

    if (na_avg_tracts > 0) {
      na_avg_pct <- if (n_tracts > 0) 100 * na_avg_tracts / n_tracts else 0
      warn_lines <- c(
        warn_lines,
        cli::format_inline(
          "{.field avg_inc_resp} is {.strong NA} in {.strong {na_avg_tracts}} of {.strong {n_tracts}} tracts ({.strong {sprintf('%.2f%%', na_avg_pct)}} of total tracts)"
        )
      )
    }
  }

  if (unmatched_pts > 0) {
    unmatched_pct <- if (total_pts > 0) 100 * unmatched_pts / total_pts else 0
    warn_lines <- c(
      warn_lines,
      cli::format_inline(
        "Unmatched CNEFE points (no tract): {.strong {unmatched_pts}} of {.strong {total_pts}} points ({.strong {sprintf('%.2f%%', unmatched_pct)}} of total points)"
      )
    )
  }

  na_totals <- character(0)
  for (v in totals_vars) {
    n_na <- suppressMessages(
      DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT COUNT(*) AS n FROM sc_muni_tbl WHERE %s IS NULL;",
        v
      )
    )$n[1]
    )

    if (n_na > 0) {
      na_pct <- if (n_tracts > 0) 100 * n_na / n_tracts else 0
      na_totals <- c(
        na_totals,
        cli::format_inline("{.field {v}} in {.strong {n_na}} of {.strong {n_tracts}} tracts ({.strong {sprintf('%.2f%%', na_pct)}} of total tracts)")
      )
    }
  }

  if (length(na_totals) > 0) {
    warn_lines <- c(
      warn_lines,
      cli::format_inline(
        "Tracts with {.strong NA} totals: {paste(na_totals, collapse = '; ')}"
      )
    )
  }

  no_elig <- character(0)
  for (v in totals_vars) {
    if (v == "pop_ph") {
      n0 <- suppressMessages(
        DBI::dbGetQuery(
        con,
        "
      SELECT COUNT(*) AS n
      FROM sc_muni_w_dom
      WHERE pop_ph IS NOT NULL AND pop_ph > 0 AND n_dom_p = 0;
    "
      )$n[1]
      )

    } else if (v == "pop_ch") {
      n0 <- suppressMessages(
        DBI::dbGetQuery(
        con,
        "
      SELECT COUNT(*) AS n
      FROM sc_muni_w_dom
      WHERE pop_ch IS NOT NULL AND pop_ch > 0 AND n_dom_c = 0;
    "
      )$n[1]
      )

    } else {
      n0 <- suppressMessages(
        DBI::dbGetQuery(
        con,
        sprintf(
          "
      SELECT COUNT(*) AS n
      FROM sc_muni_w_dom
      WHERE %s IS NOT NULL AND %s > 0
        AND (CASE
               WHEN n_dom_p > 0 THEN n_dom_p
               WHEN n_dom_c > 0 THEN n_dom_c
               ELSE 0
             END) = 0;
    ",
          v,
          v
        )
      )$n[1]
      )
    }

    if (n0 > 0) {
      n0_pct <- if (n_tracts > 0) 100 * n0 / n_tracts else 0
      no_elig <- c(
        no_elig,
        cli::format_inline("{.field {v}} in {.strong {n0}} of {.strong {n_tracts}} tracts ({.strong {sprintf('%.2f%%', n0_pct)}} of total tracts)")
      )
    }
  }

  if (length(no_elig) > 0) {
    warn_lines <- c(
      warn_lines,
      cli::format_inline(
        "Tracts with no eligible dwellings: {paste(no_elig, collapse = '; ')}"
      )
    )
  }

  # --- emit diagnostics ---
  cli::cli_h2("Dasymetric interpolation diagnostics")

  # Stage 1
  cli::cli_h3("Stage 1: Tracts \u2192 CNEFE points")
  if (length(warn_lines) > 0) {
    cli::cli_bullets(
      stats::setNames(warn_lines, rep("!", length(warn_lines)))
    )
  } else {
    cli::cli_alert_success("All tract values fully allocated to CNEFE points.")
  }

  # Stage 2
  cli::cli_h3("Stage 2: CNEFE points \u2192 H3 hexagons")
  pts_with_hex <- suppressMessages(
    DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM cnefe_alloc WHERE id_hex IS NOT NULL;"
    )$n[1]
  )
  total_alloc_pts <- suppressMessages(
    DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM cnefe_alloc;"
    )$n[1]
  )

  pts_pct <- if (total_alloc_pts > 0) 100 * pts_with_hex / total_alloc_pts else 0

  stage2_lines <- c(
    cli::format_inline(
      "CNEFE points mapped to H3 cells: {.strong {pts_with_hex}} of {.strong {total_alloc_pts}} allocated points ({.strong {sprintf('%.2f%%', pts_pct)}})"
    )
  )
  cli::cli_bullets(
    stats::setNames(stage2_lines, rep("i", length(stage2_lines)))
  )

  return(out)

}
