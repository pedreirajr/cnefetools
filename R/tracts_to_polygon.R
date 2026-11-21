#' Convert census tract aggregates to user-provided polygons using CNEFE points
#'
#' @description
#' `tracts_to_polygon()` performs a dasymetric interpolation with the following steps:
#' 1) census tract totals are allocated to CNEFE dwelling points inside each tract;
#' 2) allocated values are aggregated to user-provided polygons (neighborhoods,
#'    administrative divisions, custom areas, etc.).
#'
#' The function uses DuckDB with spatial extensions for the heavy work.
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
#'   together, in a single allocation (see "Several municipalities").
#'
#'   Required under `scope = "muni"` (the default) and ignored under
#'   `scope = "geom"`, which finds the municipalities that the polygons
#'   intersect (see "Municipalities detected from the polygons").
#' @param polygon An [`sf::sf`] object with polygon geometries (POLYGON or
#'   MULTIPOLYGON), in any CRS. Features with the same geometry as an earlier
#'   feature are dropped, with a message, and the first one is kept. Under
#'   `scope = "muni"`, the diagnostics report the share of the allocated points
#'   of the municipalities in `code_muni` that falls inside the polygons. When
#'   polygons overlap, the diagnostics open with how many dwellings fall in more
#'   than one of them, since their allocated values are added to each.
#' @param year Integer. The CNEFE data year. Currently only 2022 is supported.
#'   Defaults to 2022.
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
#' @param crs_output The CRS for the output object. Default is `NULL`, which uses
#'   the original CRS of the `polygon` argument. Can be an EPSG code (e.g., 4326,
#'   31983) or any CRS object accepted by [sf::st_transform()].
#' @param scope Character. Which CNEFE addresses (and census tracts) the
#'   function works with. `"muni"` (the default, and the faster option) uses
#'   every address and tract of the municipalities in `code_muni`. `"geom"` uses
#'   the addresses that fall inside the polygons in `polygon`, whichever
#'   municipality they belong to, so the municipalities those polygons reach are
#'   read as well. See the section "Polygons at the edge of the region".
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
#' @return An `sf` object with the user-provided polygons and the requested
#'   interpolated variables. The output CRS matches the original `polygon` CRS
#'   (or `crs_output` if specified).
#'
#' @details
#' ## Several municipalities
#'
#' Pass a vector of codes to interpolate a region that spans more than one
#' municipality in a single call. The tracts and the CNEFE points of all of them
#' go through one allocation, each point is matched only to the tracts of its own
#' municipality, and the result has one row per polygon, whichever municipality
#' the polygon crosses.
#'
#' Calling the function once per municipality and binding the results isn't the
#' same. A polygon that crosses a municipal border gets, in each call, only the
#' part of the allocation that comes from the municipality of that call.
#' Totals (every variable except `avg_inc_resp`) add up, so separate outputs
#' can be combined by summing them by polygon. `avg_inc_resp` can't, because
#' it's an average over the allocated points, and two averages don't give the
#' average of the pooled points. Use a vector of codes instead.
#'
#' ## Polygons at the edge of the region
#'
#' Under the default `scope = "muni"`, only the addresses and tracts of the
#' municipalities in `code_muni` are used. A polygon that extends beyond them
#' receives only the part of the allocation that comes from the municipalities
#' requested, so its totals and `avg_inc_resp` describe a fraction of it. Use
#' `scope = "geom"` for a complete result, which reads every municipality the
#' polygons reach.
#'
#' ## Municipalities detected from the polygons
#'
#' With `scope = "geom"`, the function finds the municipalities that the
#' polygons intersect, using the 2022 IBGE municipal boundaries, and uses all
#' of their tracts and addresses, even when they belong to different states. A
#' message lists them, and a `code_muni` passed in this case is ignored.
#'
#' Polygons from another source rarely follow the IBGE boundaries exactly, so
#' a polygon along a municipal border can touch a neighboring municipality by
#' a few meters. That neighbor is then downloaded and read too. The values are
#' not affected, since only the points inside the polygons are aggregated, but
#' use `scope = "muni"` with `code_muni` whenever you know which municipalities
#' the polygons belong to, which is faster.
#'
#' @examples
#' \donttest{
#' # Interpolate population to user-provided polygons (neighborhoods of Lauro de Freitas-BA)
#' # Using geobr to download neighborhood boundaries
#' library(geobr)
#' nei_ldf <- subset(
#'   read_neighborhood(year = 2022),
#'   code_muni == 2919207
#' )
#' poly_pop <- tracts_to_polygon(
#'   code_muni = 2919207,
#'   polygon = nei_ldf,
#'   vars = c("pop_ph", "pop_ch"),
#'   cache = FALSE
#' )
#'
#' # With scope = "geom", the municipalities the polygons intersect are used
#' poly_pop_geom <- tracts_to_polygon(polygon = nei_ldf, scope = "geom", cache = FALSE)
#' }
#'
#' @export
tracts_to_polygon <- function(
  code_muni = NULL,
  polygon,
  year = 2022,

  vars = c("pop_ph", "pop_ch"),
  crs_output = NULL,
  scope = c("muni", "geom"),
  cache = TRUE,
  cache_dir = NULL,
  verbose = TRUE
) {
  # normalize inputs ----------------------------------------------------------
  code_muni_given <- .check_code_muni_arg(code_muni, "user")
  scope <- .resolve_scope(scope, code_muni_given, "user")
  year <- .validate_year(year)

  # Get the appropriate index for the requested year

cnefe_index <- .get_cnefe_index(year)

  vars <- unique(as.character(vars))

  if (length(vars) == 0) {
    cli::cli_abort("{.arg vars} must contain at least one variable name.")
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
    cli::cli_abort(c(
      "Unknown {.arg vars}: {.val {bad_vars}}.",
      "i" = "See {.code ?tracts_to_polygon} for available variables."
    ))
  }

  # validate polygon ----------------------------------------------------------
  .validate_polygon_arg(polygon, crs_output = crs_output)
  # Repeated features would receive the same dwellings again (#120)
  polygon <- .drop_repeated_polygons(polygon)

  # scope = "geom": use the municipalities the polygons intersect (#120)
  detected <- NULL
  if (scope$detect) {
    detected <- .detect_code_munis(polygon, year = year, cache = cache, cache_dir = cache_dir)
    .inform_detected_munis(detected)
    code_muni <- detected$code_muni
  }
  code_muni <- .normalize_code_munis(code_muni)

  # helpers -------------------------------------------------------------------

  .duckdb_quiet_execute <- function(con, sql) {
    invisible(.duckdb_quiet(DBI::dbExecute(con, sql)))
  }

  .fmt_pct <- function(x) sprintf("%.2f%%", x)

  # timing container ----------------------------------------------------------
  if (verbose) {
    cli::cli_alert_info("Processing municipality code {.val {code_muni}}...")
  }

  # Step 1/6: CRS alignment ---------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 1/6: aligning CRS...",
                           msg_done = "Step 1/6 (CRS alignment)")

  }

  # Store original CRS for output transformation
  original_crs <- sf::st_crs(polygon)

  # Determine output CRS: use crs_output if provided, otherwise use original
  if (is.null(crs_output)) {
    output_crs <- original_crs
  } else {
    output_crs <- sf::st_crs(crs_output)
  }

  if (verbose) {
    crs_input_label <- if (!is.na(original_crs$epsg)) {
      paste0("EPSG:", original_crs$epsg)
    } else if (!is.null(original_crs$input)) {
      original_crs$input
    } else {
      "unknown"
    }
    crs_output_label <- if (!is.na(output_crs$epsg)) {
      paste0("EPSG:", output_crs$epsg)
    } else if (!is.null(output_crs$input)) {
      output_crs$input
    } else {
      "unknown"
    }
    cli::cli_alert_info("Input CRS: {.val {crs_input_label}} | Output CRS: {.val {crs_output_label}}")
  }

  # Fix invalid geometries before any spatial operation
  polygon <- sf::st_make_valid(polygon)

  # Transform polygon to WGS84 internally for spatial join with CNEFE points
  polygon_4326 <- sf::st_transform(polygon, 4326)

  # Add row ID for joining
  polygon_4326 <- polygon_4326 |>
    dplyr::mutate(.poly_row_id = dplyr::row_number())

  if (verbose) {
    cli::cli_progress_done("Step 1/6: connecting to DuckDB and loading extensions...")
  }

  # Step 2/6: Connect to DuckDB -----------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 2/6: connecting to DuckDB and loading extensions...",
                           msg_done = "Step 2/6 (DuckDB ready)")

  }

  con <- .duckdb_connect(
      extensions = character(0),
    spatial = TRUE,
    reason = "to run the dasymetric interpolation in `tracts_to_polygon()`.",
    verbose = verbose
  )

  if (verbose) {
    cli::cli_progress_done("Step 2/6: connecting to DuckDB and loading extensions...")
  }

  # Step 3/6: Prepare census tracts -------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 3/6: preparing census tracts in DuckDB...",
                           msg_done = "Step 3/6 (Tracts ready)")

  }

  .sc_create_views_in_duckdb(
    con,
    code_muni = code_muni,
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

  # Check polygon coverage against census tracts union
  tracts_union_wkt <- DBI::dbGetQuery(
    con,
    "SELECT ST_AsText(ST_Union_Agg(geom)) AS wkt FROM sc_muni_tbl;"
  )$wkt[1]

  if (!is.null(tracts_union_wkt) && !is.na(tracts_union_wkt)) {
    tracts_union <- sf::st_as_sfc(tracts_union_wkt, crs = 4326)
    tracts_union <- sf::st_make_valid(tracts_union)

    polygon_union <- sf::st_union(polygon_4326)
    polygon_union <- sf::st_make_valid(polygon_union)

    polygon_inside <- tryCatch(
      sf::st_intersection(polygon_union, tracts_union),
      error = function(e) polygon_union
    )
    polygon_inside <- sf::st_make_valid(polygon_inside)

    # Use a projected CRS for accurate area calculation (UTM based on centroid)
    centroid <- sf::st_centroid(polygon_union)
    centroid_coords <- sf::st_coordinates(centroid)
    utm_zone <- floor((centroid_coords[1] + 180) / 6) + 1
    utm_crs <- if (centroid_coords[2] >= 0) {
      sf::st_crs(paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs"))
    } else {
      sf::st_crs(paste0("+proj=utm +zone=", utm_zone, " +south +datum=WGS84 +units=m +no_defs"))
    }

    polygon_union_proj <- sf::st_transform(polygon_union, utm_crs)
    polygon_inside_proj <- sf::st_transform(polygon_inside, utm_crs)

    total_area <- as.numeric(sf::st_area(polygon_union_proj))
    inside_area <- as.numeric(sf::st_area(polygon_inside_proj))
    outside_area <- total_area - inside_area
    outside_pct <- (outside_area / total_area) * 100

  }

  if (verbose) {
    cli::cli_progress_done("Step 3/6: preparing census tracts in DuckDB...")
  }

  # Step 4/6: Prepare CNEFE points --------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 4/6: preparing CNEFE points in DuckDB...",
                           msg_done = "Step 4/6 (CNEFE points ready)")

  }

  # Create lazy views (cnefe_raw, cnefe_pts) that read from the ZIP file.
  .cnefe_create_points_view_in_duckdb(
    con,
    code_muni = code_muni,
    index = cnefe_index,
    cache = cache,
    cache_dir = cache_dir,
    year = year,
    verbose = verbose
  )

  # Materialise the lazy view (reads ZIP data into DuckDB memory).
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

  total_cnefe_pts <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM cnefe_pts_tbl;"
  )$n[1]

  # A temporary CNEFE file was registered for deletion on exit by
  # .cnefe_create_points_view_in_duckdb(), before anything here could fail.

  if (verbose) {
    cli::cli_progress_done("Step 4/6: preparing CNEFE points in DuckDB...")
  }

  # Step 5/6: Spatial join and allocation -------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 5/6: spatial join (points to tracts) and allocation...",
                           msg_done = "Step 5/6 (Join and allocation)")

  }

  # Matched points only (spatial join without LEFT JOIN)
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
  if (length(code_muni) > 1L) {
    .duckdb_quiet_execute(
      con,
      "
      DELETE FROM cnefe_sc
      WHERE substr(code_tract, 1, 7) <> CAST(code_muni AS VARCHAR);
    "
    )
  }

  matched_pts <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cnefe_sc;")$n[1]
  unmatched_pts <- max(total_cnefe_pts - matched_pts, 0)

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

  # Allocation expressions (reused from tracts_to_h3)
  alloc_sql <- .build_alloc_sql(vars)

  # Create allocated points table with geometry for spatial join
  .duckdb_quiet_execute(
    con,
    sprintf(
      "
    CREATE OR REPLACE TABLE cnefe_alloc AS
    SELECT
      p.*,
      s.n_dom_p,
      s.n_dom_c,
      ST_Point(p.lon, p.lat) AS pt_geom,
      %s
    FROM cnefe_sc p
    JOIN sc_muni_w_dom s
      USING (code_tract)
    WHERE p.lon IS NOT NULL AND p.lat IS NOT NULL;
  ",
      alloc_sql
    )
  )

  # Check if any points were allocated
  total_alloc_pts <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM cnefe_alloc;"
  )$n[1]

  if (total_alloc_pts == 0) {
    cli::cli_abort(c(
      "No CNEFE coordinates were captured for interpolation.",
      "i" = "This may indicate that:",
      "*" = "The municipality code {.val {code_muni}} is incorrect, or",
      "*" = "No dwelling points exist in the CNEFE data for this municipality."
    ))
  }

  # Register user polygons in DuckDB using duckspatial (quiet)
  invisible(
    .duckdb_quiet(
      duckspatial::ddbs_write_table(
        conn = con,
        # Normalize geometry column to "geom"; duckspatial preserves the
        # input sf geometry name, but the SQL below hardcodes "geom".
        data = sf::st_set_geometry(polygon_4326[, ".poly_row_id"], "geom"),
        name = "user_polygons",
        overwrite = TRUE
      )
    )
  )

  # duckspatial 1.0.0 (DuckDB 1.5+) writes GEOMETRY with embedded CRS metadata,
  # which RTREE does not accept. Strip CRS via WKB round-trip first.
  .duckdb_quiet_execute(
    con,
    "ALTER TABLE user_polygons ALTER COLUMN geom SET DATA TYPE GEOMETRY
     USING ST_GeomFromWKB(ST_AsWKB(geom));"
  )

  # Create spatial index on user polygons
  .duckdb_quiet_execute(
    con,
    "CREATE INDEX IF NOT EXISTS user_poly_geom_idx ON user_polygons USING RTREE (geom);"
  )

  # Spatial join between allocated points and user polygons in DuckDB
  .duckdb_quiet_execute(
    con,
    "
    CREATE OR REPLACE TABLE cnefe_poly_joined AS
    SELECT
      a.*,
      u.\".poly_row_id\" AS poly_row_id
    FROM cnefe_alloc a
    LEFT JOIN user_polygons u
      ON ST_Within(a.pt_geom, u.geom);
  "
  )

  # Count unique points inside/outside polygons

  # When polygons overlap, a single CNEFE point can match multiple polygons.
 # Use COUNT(DISTINCT) to count each point only once.
  coverage_stats <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(DISTINCT COD_UNICO_ENDERECO) AS total,
      COUNT(DISTINCT CASE WHEN poly_row_id IS NOT NULL THEN COD_UNICO_ENDERECO END) AS inside
    FROM cnefe_poly_joined;
  "
  )

  # Dwellings inside two or more polygons, for the overlap warning (#120)
  points_multi <- DBI::dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM (
      SELECT COD_UNICO_ENDERECO
      FROM cnefe_poly_joined
      WHERE poly_row_id IS NOT NULL
      GROUP BY COD_UNICO_ENDERECO
      HAVING COUNT(DISTINCT poly_row_id) > 1
    );
  "
  )$n[1]

  points_inside <- coverage_stats$inside[1]
  points_outside <- coverage_stats$total[1] - points_inside

  if (points_inside == 0) {
    .abort_no_points_in_polygons(code_muni, detected)
  }

  # Opens the diagnostics below (#120)
  overlap_line <- .polygon_overlap_line(
    points_multi,
    points_inside,
    "the values allocated to each are added to every polygon that contains it, and the totals summed over the polygons exceed those of the tracts.",
    what = "CNEFE dwellings"
  )

  # Coverage stats saved for Stage 2 diagnostics
  coverage_pct <- (points_inside / total_alloc_pts) * 100

  if (verbose) {
    cli::cli_progress_done("Step 5/6: spatial join (points to tracts) and allocation...")
  }

  # Step 6/6: Aggregate to polygons -------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 6/6: aggregating allocated values to polygons...",
                           msg_done = "Step 6/6 (Polygon aggregation)")

  }

  # Build SQL aggregation expressions
  agg_sql_exprs <- character(0)
  for (v in vars) {
    pt_col <- paste0(v, "_pt")
    if (v == "avg_inc_resp") {
      agg_sql_exprs <- c(agg_sql_exprs, sprintf("AVG(%s) AS %s", pt_col, v))
    } else {
      agg_sql_exprs <- c(agg_sql_exprs, sprintf("SUM(%s) AS %s", pt_col, v))
    }
  }

  # Aggregate in DuckDB
  agg_sql <- sprintf(
    "
    SELECT
      poly_row_id AS \".poly_row_id\",
      %s
    FROM cnefe_poly_joined
    WHERE poly_row_id IS NOT NULL
    GROUP BY poly_row_id;
  ",
    paste(agg_sql_exprs, collapse = ",\n      ")
  )

  poly_vals <- .duckdb_quiet(DBI::dbGetQuery(con, agg_sql))

  # Join back to polygon
  out <- polygon_4326 |>
    dplyr::left_join(poly_vals, by = ".poly_row_id") |>
    dplyr::select(-".poly_row_id")

  # Fill NAs with 0 for sum variables, keep NA for avg_inc_resp
  for (v in vars) {
    if (v != "avg_inc_resp") {
      out[[v]] <- dplyr::coalesce(out[[v]], 0)
    }
  }

  # Transform to output CRS
  out <- sf::st_transform(out, output_crs)

  if (verbose) {
    cli::cli_progress_done("Step 6/6: aggregating allocated values to polygons...")
    #force to close the last progress
    cli::cli_progress_done()
  }


  # diagnostics and warning ----------------------------------------------------
  warn_lines <- .build_interp_diagnostics(con, vars, unmatched_pts, total_cnefe_pts)

  total_polygons <- nrow(polygon_4326)
  polygons_empty <- sum(!polygon_4326$.poly_row_id %in% poly_vals$.poly_row_id)

  # With scope = "geom" the detected municipalities are read whole, so the
  # share left outside the polygons says nothing about the call (#120).
  stage2_lines <- character(0)
  if (is.null(detected)) {
    stage2_lines <- c(
      stage2_lines,
      cli::format_inline(
        "Polygon coverage: {.strong {points_inside}} of {.strong {total_alloc_pts}} allocated points of the municipalities in {.arg code_muni} captured ({.strong {sprintf('%.2f%%', coverage_pct)}})"
      )
    )
  } else {
    stage2_lines <- c(
      stage2_lines,
      cli::format_inline(
        "Allocated points inside the polygons: {.strong {points_inside}}, from the {.strong {nrow(detected)}} municipalities the polygons intersect"
      )
    )
  }
  if (polygons_empty > 0) {
    polygons_empty_pct <- 100 * polygons_empty / total_polygons
    stage2_lines <- c(
      stage2_lines,
      cli::format_inline(
        "Polygons with no CNEFE points: {.strong {polygons_empty}} of {.strong {total_polygons}} total polygons ({.strong {sprintf('%.2f%%', polygons_empty_pct)}})"
      )
    )
  }

  .report_interp_diagnostics(warn_lines, stage2_lines, "Polygons",
                             head_lines = overlap_line)

  return(out)
}


