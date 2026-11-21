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
#' @param code_muni Numeric or character. One or more seven-digit IBGE
#'   municipality codes, for example `2927408` or `c(2927408, 2919207)`.
#'   Repeated codes are dropped. With several codes, they're interpolated
#'   together (see "Several municipalities"). Always required, since the grid is built from the
#'   boundaries of these municipalities.
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
#' @param scope Character. Which CNEFE addresses (and census tracts) the
#'   function works with. `"muni"` (the default, and the faster option) uses
#'   every address and tract of the municipalities in `code_muni`. `"geom"` uses
#'   the addresses that fall inside the H3 cells, whichever municipality they
#'   belong to, so the municipalities the grid reaches are read as well. The H3
#'   grid holds the cells whose center falls inside the boundaries of
#'   `code_muni`, plus the neighboring cells that intersect those boundaries.
#'   See the section "Border cells at the edge of the region".
#' @param cache Logical. If `TRUE` (default), the downloaded files are kept in
#'   the user cache directory and reused in later calls. These are the CNEFE
#'   file, the census tracts and the boundary of each municipality read. Only
#'   the tracts of those municipalities are downloaded, not the whole state. If
#'   `FALSE`, temporary files are used and deleted after the call. See
#'   [clear_cache_muni()], [clear_cache_tracts()] and [clear_cache_boundaries()]
#'   to delete the cached files.
#' @param cache_dir Character. Directory to use for cached downloads. If `NULL`
#'   (default), the `CNEFETOOLS_CACHE_DIR` environment variable is used when it
#'   is set, otherwise [tools::R_user_dir()] with `which = "cache"`. Use this to
#'   point large downloads at a secondary drive or a shared volume.
#' @param verbose Logical. Whether to print step messages and timing.
#'
#' @return An `sf` object (CRS 4326) with an H3 grid and the requested interpolated variables.
#'
#' @details
#' ## Several municipalities
#'
#' Pass a vector of codes to interpolate a region that spans more than one
#' municipality in a single call. The tracts and the CNEFE points of all of them
#' go through one allocation, each point is matched only to the tracts of its own
#' municipality, and the grid is built over the union of the boundaries, so every
#' cell appears once.
#'
#' Calling the function once per municipality and binding the results isn't the
#' same. A cell on a shared border belongs to the grid of both municipalities,
#' so it comes back from both calls, each with only its own side (under the
#' default `scope = "muni"`), and dropping the repeated rows loses one side.
#' Totals (every variable except `avg_inc_resp`) add up, so separate outputs
#' can be combined by summing them by `id_hex`. `avg_inc_resp` can't, because
#' it's an average over the allocated points, and two averages don't give the
#' average of the pooled points. Use a vector of codes instead.
#'
#' ## Border cells at the edge of the region
#'
#' Under the default `scope = "muni"`, only the addresses and tracts of the
#' municipalities in `code_muni` are read. A cell on the outer edge of the
#' requested area, one that also overlaps a municipality you did not request,
#' therefore describes only the part of it that lies inside the area: its
#' values (totals and `avg_inc_resp`) leave out the addresses of the neighbor.
#' Adding municipalities moves the edge outward but does not remove it.
#'
#' Under `scope = "geom"`, the municipalities that the grid reaches are read
#' too, and every cell receives the allocation of all the dwellings inside it,
#' so the edge is complete. A message lists the neighboring municipalities
#' read. The CNEFE file of each neighbor is read whole, since the files come
#' one per municipality, but only the neighbor's tracts with at least one
#' dwelling inside the grid enter the allocation. Those tracts enter with all
#' their dwellings, so the result matches a call that lists the neighbors in
#' `code_muni`, restricted to the cells of this grid. It's still slower than
#' `"muni"`, which is why it isn't the default.
#'
#' ## Addresses outside the H3 grid
#'
#' The grid is built from the 2022 IBGE municipal boundaries, at full
#' resolution, plus the ring of cells along them, but each dwelling point goes
#' to the cell of its own coordinates. A point geocoded beyond that ring, for
#' example on the coast or on a small island the boundary doesn't follow, lands
#' in a cell outside the grid, and what was allocated to it isn't in the
#' output. The diagnostics report how many
#' allocated points of the municipalities in `code_muni` this affects and how
#' much of each total they carried, so the output can be reconciled with the
#' tract totals. Under `scope = "geom"`, the points of the neighboring
#' municipalities that fall beyond the grid are expected and not reported.
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
  scope = c("muni", "geom"),
  cache = TRUE,
  cache_dir = NULL,
  verbose = TRUE
) {
  # normalize inputs ----------------------------------------------------------
  # code_muni stays required here: an H3 grid needs municipal boundaries, and
  # the polygon counterpart is the function where it is optional (#120)
  if (missing(code_muni) || is.null(code_muni)) {
    cli::cli_abort(c(
      "{.arg code_muni} is required for the H3 grid.",
      "i" = "Pass one or more 7-digit IBGE codes. To interpolate onto your own polygons without listing the municipalities, use {.fn tracts_to_polygon}, where {.arg code_muni} is optional."
    ))
  }
  if (inherits(code_muni, "sf") || inherits(code_muni, "sfc")) {
    cli::cli_abort(c(
      "{.arg code_muni} received an {.cls sf} object.",
      "i" = "To interpolate onto your own polygons, use {.code tracts_to_polygon(polygon = <your sf object>)}."
    ))
  }
  code_muni <- .normalize_code_munis(code_muni)
  scope <- .resolve_scope(scope, TRUE, "hex")$scope

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
      extensions = "h3",
    spatial = TRUE,
    reason = "to run the dasymetric interpolation in `tracts_to_h3()`.",
    verbose = verbose
  )

  if (verbose) {
    cli::cli_progress_done("Step 1/6: connecting to DuckDB and loading extensions...")
  }

  # The grid is built before the tracts and points are read because, with
  # scope = "geom", it decides which municipalities are read: those in
  # code_muni plus the ones the grid reaches, so the cells along the border
  # receive the allocation of both sides (#120).
  grid_set <- .h3_grid_and_read_set(code_muni, scope, h3_resolution, year = year,
                                    cache = cache, cache_dir = cache_dir)
  hex_grid <- grid_set$hex_grid
  read_munis <- grid_set$read_munis

  # Step 2/6 ------------------------------------------------------------------

  if (verbose) {
    cli::cli_progress_step("Step 2/6: preparing census tracts in DuckDB...",
                           msg_done = "Step 2/6 (Tracts ready)")

  }

  .sc_create_views_in_duckdb(
    con,
    code_muni = read_munis,
    cache = cache,
    cache_dir = cache_dir,
    year = year,
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
  # The ZIP must remain on disk until the views are materialized into a table.
  suppressMessages(
    .cnefe_create_points_view_in_duckdb(
      con,
      code_muni = read_munis,
      index = cnefe_index,
      cache = cache,
      cache_dir = cache_dir,
      year = year,
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

    }
  })

  # scope = "geom": keep only the tracts of the neighboring municipalities that
  # hold a dwelling inside a cell of the grid (#120). The CNEFE file of each
  # neighbor is read whole, since the files come by municipality, but the
  # spatial join then runs against these few tracts instead of all of theirs.
  # A tract that is kept enters whole, with all its dwellings, so the share of
  # each dwelling in the tract total is the same as in a call that lists the
  # neighbors in code_muni.
  own_list <- paste(as.integer(code_muni), collapse = ", ")
  has_neighbors <- length(read_munis) > length(code_muni)
  if (has_neighbors) {
    .sc_keep_tracts_touching_grid(con, hex_grid$id_hex, h3_resolution, own_list)
  }

  # Points of code_muni only: the dwellings of a neighbor that lie outside the
  # tracts kept are expected to find no tract and are not reported as unmatched
  total_pts <- suppressMessages(DBI::dbGetQuery(
    con,
    sprintf("SELECT COUNT(*) AS n FROM cnefe_pts_tbl WHERE code_muni IN (%s);", own_list)
  )$n[1])

  # A temporary CNEFE file was registered for deletion on exit by
  # .cnefe_create_points_view_in_duckdb(), before anything here could fail.

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

  # With several municipalities a point is only matched to the tracts of its own
  # municipality, the same pairing a call per municipality would make. The
  # filter runs after the join, not inside it: an equality predicate on two
  # columns of the joined tables takes the spatial join out of its optimized
  # operator and turns it into a nested loop, which does not finish on a
  # metropolitan region.
  if (length(read_munis) > 1L) {
    .duckdb_quiet_execute(
      con,
      "
      DELETE FROM cnefe_sc
      WHERE substr(code_tract, 1, 7) <> CAST(code_muni AS VARCHAR);
    "
    )
  }

  matched_pts <- suppressMessages(DBI::dbGetQuery(
    con,
    sprintf("SELECT COUNT(*) AS n FROM cnefe_sc WHERE code_muni IN (%s);", own_list)
  )$n[1])
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
      COUNT(*) AS n_pts_hex,
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
    cli::cli_progress_step("Step 6/6: joining results to the H3 grid...",
                           msg_done = "Step 6/6 (sf output)")

  }

  # LEFT_JOIN the allocated values onto the full grid built earlier. This
  # ensures hexagons with no eligible dwelling points are retained (with count
  # variables coalesced to 0 and avg_inc_resp left as NA).
  .duckdb_quiet({
    {
      hex_df <- DBI::dbGetQuery(con, "SELECT * FROM hex_vals;")

      # Points of code_muni whose cell is not in the grid are dropped by the
      # join below, together with what was allocated to them, so they are
      # reported. The neighbors read under scope = "geom" fall outside the
      # grid by construction and are left out of that report (#120).
      total_vars <- setdiff(vars, "avg_inc_resp")
      own_cols <- c(
        "id_hex",
        "COUNT(*) AS n_pts_hex",
        sprintf("SUM(%s_pt) AS %s", total_vars, total_vars)
      )
      own_df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT %s FROM cnefe_alloc WHERE id_hex IS NOT NULL AND code_muni IN (%s) GROUP BY 1;",
          paste(own_cols, collapse = ", "),
          paste(as.integer(code_muni), collapse = ", ")
        )
      )
      outside_df <- own_df[!own_df$id_hex %in% hex_grid$id_hex, , drop = FALSE]

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
    cli::cli_progress_done("Step 6/6: joining results to the H3 grid...")
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

  # Allocated points of code_muni whose cell falls outside the grid, and what
  # they carried
  stage2_warn_lines <- character(0)
  pts_outside <- sum(outside_df$n_pts_hex)
  pts_own <- sum(own_df$n_pts_hex)
  if (pts_outside > 0) {
    out_pct <- if (pts_own > 0) 100 * pts_outside / pts_own else 0
    # Formatted first: cli reads "{.name" as a style, not as a function call
    out_pct_txt <- .fmt_small_pct(out_pct)
    stage2_warn_lines <- cli::format_inline(
      "Allocated points in H3 cells outside the grid: {.strong {pts_outside}} of {.strong {pts_own}} ({.strong {out_pct_txt}}), not included in the output"
    )
    for (v in setdiff(vars, "avg_inc_resp")) {
      lost_v <- sum(outside_df[[v]], na.rm = TRUE)
      total_v <- sum(own_df[[v]], na.rm = TRUE)
      lost_pct <- if (total_v > 0) 100 * lost_v / total_v else 0
      lost_pct_txt <- .fmt_small_pct(lost_pct)
      stage2_warn_lines <- c(
        stage2_warn_lines,
        cli::format_inline(
          "{.field {v}} allocated to cells outside the grid: {.strong {sprintf('%.0f', lost_v)}} of {.strong {sprintf('%.0f', total_v)}} ({.strong {lost_pct_txt}})"
        )
      )
    }
  }

  .report_interp_diagnostics(
    warn_lines, stage2_lines, "H3 hexagons",
    stage2_warn_lines = stage2_warn_lines
  )

  return(out)

}
