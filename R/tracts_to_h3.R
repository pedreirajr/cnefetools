#' Convert census tract aggregates to an H3 grid using CNEFE points
#'
#' @description
#' `tracts_to_h3()` performs a dasymetric interpolation with the following steps:
#' 1) census tract totals are allocated to CNEFE dwelling points inside each tract;
#' 2) allocated values are aggregated to an H3 grid at a user-defined resolution.
#'
#' The function uses DuckDB with the spatial and H3 extensions for the heavy work.
#'
#' Unlike [cnefe_counts()] and [compute_lumi()], this function does not expose a
#' `backend` argument and relies on DuckDB exclusively. The dominant cost here is
#' a spatial overlay between the full CNEFE point set of the municipality and the
#' census tract polygons, which is a different workload from the tabular
#' aggregation those other functions perform. Running that overlay in R would
#' take prohibitively long in medium and large municipalities, so a pure-R
#' fallback would offer users a path that does not finish rather than a slower
#' one.
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
#'   - `n_resp` is allocated only to private dwellings (same rule as `pop_ph`).
#'   - Demographic variables (`male`, `female`, `age_*`, `race_*`) are allocated
#'     to private dwellings when the tract has any; if the tract has zero private
#'     dwellings but has collective dwellings, they are allocated to collective.
#'   - `avg_inc_resp` is assigned (not split) to each private dwelling point;
#'     tracts with no private dwellings receive no allocation.
#'
#' @param cache Logical. Whether to use the existing package cache for assets and CNEFE zips.
#' @param cache_dir Character. Directory to use for cached downloads. If `NULL`
#'   (default), the `CNEFETOOLS_CACHE_DIR` environment variable is used when it
#'   is set, otherwise [tools::R_user_dir()] with `which = "cache"`. Use this to
#'   point large downloads at a secondary drive or a shared volume.
#' @param verbose Logical. Whether to print step messages and timing.
#'
#' @return An `sf` object (CRS 4326) with an H3 grid and the requested interpolated variables.
#'
#' @examples
#' \donttest{
#' # Interpolate population to H3 hexagons
#' hex_pop <- tracts_to_h3(
#'   code_muni = 2929057,
#'   vars = c("pop_ph", "pop_ch"),
#'   cache = FALSE
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
  cache_dir = NULL,
  verbose = TRUE
) {
  # normalize inputs ----------------------------------------------------------
  code_muni <- .normalize_code_muni(code_muni)

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
    invisible(.duckdb_quiet(DBI::dbExecute(con, sql)))
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

  con <- .duckdb_connect(
    extensions = c("zipfs", "h3"),
    spatial = TRUE,
    reason = "to run the dasymetric interpolation in `tracts_to_h3()`.",
    verbose = verbose
  )

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
    cache_dir = cache_dir,
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

  # Create lazy views (cnefe_raw, cnefe_pts) that read from the ZIP file.
  # The ZIP must remain on disk until the views are materialised into a table.
  zip_info_cnefe <- suppressMessages(
    .cnefe_create_points_view_in_duckdb(
      con,
      code_muni = code_muni,
      index = cnefe_index,
      cache = cache,
      cache_dir = cache_dir,
      verbose = verbose
    )
  )

  # Materialise the lazy view into a table (reads ZIP data into DuckDB memory).
  .duckdb_quiet({
    {
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
    }
  })

  # ZIP data is now fully in DuckDB — safe to delete the temp file.
  if (is.list(zip_info_cnefe) && isTRUE(zip_info_cnefe$cleanup_zip)) {
    on.exit(unlink(zip_info_cnefe$zip_path), add = TRUE)
  }

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
  alloc_sql <- .build_alloc_sql(vars)

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

  # Build the full municipality grid first, then LEFT_JOIN allocated values.
  # This ensures hexagons with no eligible dwelling points are retained (with
  # count variables coalesced to 0 and avg_inc_resp left as NA).
  hex_grid <- build_h3_grid(
    h3_resolution = h3_resolution,
    code_muni     = code_muni,
    year          = year
  )

  .duckdb_quiet({
    {
      hex_df <- DBI::dbGetQuery(con, "SELECT * FROM hex_vals;")

      out <- hex_grid |>
        dplyr::left_join(hex_df, by = "id_hex") |>
        sf::st_as_sf() |>
        dplyr::select(id_hex, dplyr::all_of(vars), geometry)

      # Coalesce count variables to 0 for empty hexagons; avg_inc_resp stays NA
      count_vars <- setdiff(vars, "avg_inc_resp")
      for (v in count_vars) {
        out[[v]] <- dplyr::coalesce(out[[v]], 0)
      }
    }
  })

  sf::st_crs(out) <- 4326

  if (verbose) {
    cli::cli_progress_done("Step 6/6: building H3 grid and joining results...")
    cli::cli_progress_done()
  }
  # diagnostics and warning ----------------------------------------------------
  warn_lines <- .build_interp_diagnostics(con, vars, unmatched_pts, total_pts)

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

  .report_interp_diagnostics(warn_lines, stage2_lines, "H3 hexagons")

  return(out)

}
