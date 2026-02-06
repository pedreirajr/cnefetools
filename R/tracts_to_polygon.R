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
#' @param code_muni Integer. Seven-digit IBGE municipality code.
#' @param polygon An [`sf::sf`] object with polygon geometries (POLYGON or
#'   MULTIPOLYGON). The function will automatically align CRS and issue a warning
#'   reporting the percentage of the polygon area that falls outside the municipality.
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
#' @param cache Logical. Whether to use the existing package cache for assets and CNEFE zips.
#' @param verbose Logical. Whether to print step messages and timing.
#'
#' @return An `sf` object with the user-provided polygons and the requested
#'   interpolated variables. The output CRS matches the original `polygon` CRS
#'   (or `crs_output` if specified).
#'
#' @examples
#' \donttest{
#' # Interpolate population to user-provided polygons (neighborhoods of Lauro de Freitas-BA)
#' nei_ldf <- subset(
#'   geobr::read_neighborhood(year = 2022),
#'   code_muni == 2919207
#' )
#' poly_pop <- tracts_to_polygon(
#'   code_muni = 2919207,
#'   polygon = nei_ldf,
#'   vars = c("pop_ph", "pop_ch")
#' )
#' }
#'
#' @export
tracts_to_polygon <- function(
  code_muni,
  polygon,
  year = 2022,

  vars = c("pop_ph", "pop_ch"),
  crs_output = NULL,
  cache = TRUE,
  verbose = TRUE
) {
  # normalize inputs ----------------------------------------------------------
  code_muni <- .normalize_code_muni(code_muni)
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
  if (is.null(polygon)) {
    cli::cli_abort(c(
      "{.arg polygon} is required.",
      "i" = "Provide an {.cls sf} object with polygon geometries."
    ))
  }

  if (!inherits(polygon, "sf")) {
    cli::cli_abort(c(
      "{.arg polygon} must be an {.cls sf} object.",
      "i" = "Received: {.cls {class(polygon)[1]}}"
    ))
  }

  geom_types <- unique(sf::st_geometry_type(polygon))
  valid_types <- c("POLYGON", "MULTIPOLYGON")
  if (!all(geom_types %in% valid_types)) {
    cli::cli_abort(c(
      "{.arg polygon} must contain only POLYGON or MULTIPOLYGON geometries.",
      "i" = "Found: {.val {as.character(geom_types)}}"
    ))
  }

  # Validate crs_output if provided
  if (!is.null(crs_output)) {
    test_crs <- tryCatch(
      suppressWarnings(sf::st_crs(crs_output)),
      error = function(e) NULL
    )
    if (is.null(test_crs) || is.na(test_crs$wkt)) {
      cli::cli_abort(c(
        "{.arg crs_output} is not a valid CRS.",
        "i" = "Value received: {.val {crs_output}}",
        "i" = "Use a valid EPSG code (e.g., 4674, 31983) or a CRS object."
      ))
    }
  }

  # helpers -------------------------------------------------------------------

  .duckdb_quiet_execute <- function(con, sql) {
    invisible(utils::capture.output(
      suppressMessages(DBI::dbExecute(con, sql)),
      type = "output"
    ))
  }

  .duckdb_load_ext <- function(con, ext) {
    ok <- tryCatch(
      {
        .duckdb_quiet_execute(con, sprintf("LOAD %s;", ext))
        TRUE
      },
      error = function(e) FALSE
    )

    if (!ok) {
      # zipfs and h3 are community extensions
      .duckdb_quiet_execute(con, sprintf("INSTALL %s FROM community;", ext))
      .duckdb_quiet_execute(con, sprintf("LOAD %s;", ext))
    }
    invisible(TRUE)
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

  #silent please
  con <- NULL
  utils::capture.output(
    utils::capture.output({
      con <- DBI::dbConnect(
        duckdb::duckdb(),
        dbdir = ":memory:",
        config = list(
          'enable_progress_bar' = FALSE,
          'enable_print_progress' = FALSE,
          'print_progress_bar' = FALSE
        )
      )

      tryCatch(
        duckspatial::ddbs_load(con),
        error = function(e) {
          duckspatial::ddbs_install(con)
          duckspatial::ddbs_load(con)
        }
      )
      .duckdb_load_ext(con, "zipfs")
    }, type = "message"),
    type = "output"
  )

  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

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

  total_cnefe_pts <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM cnefe_pts_tbl;"
  )$n[1]

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
  alloc_exprs <- character(0)

  for (v in vars) {
    if (v == "avg_inc_resp") {
      alloc_exprs <- c(
        alloc_exprs,
        "
        CASE
          WHEN p.COD_ESPECIE = 1
           AND s.avg_inc_resp IS NOT NULL
           AND s.n_dom_p > 0
          THEN CAST(s.avg_inc_resp AS DOUBLE)
          ELSE NULL
        END AS avg_inc_resp_pt
      "
      )
    } else if (v == "n_resp") {
      alloc_exprs <- c(
        alloc_exprs,
        "
        CASE
          WHEN p.COD_ESPECIE = 1
           AND s.n_resp IS NOT NULL
           AND s.n_dom_p > 0
          THEN CAST(s.n_resp AS DOUBLE) / s.n_dom_p
          ELSE NULL
        END AS n_resp_pt
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
    utils::capture.output(
      suppressMessages(
        duckspatial::ddbs_write_vector(
          conn = con,
          data = polygon_4326[, ".poly_row_id"],
          name = "user_polygons",
          overwrite = TRUE
        )
      ),
      type = "output"
    )
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

  points_inside <- coverage_stats$inside[1]
  points_outside <- coverage_stats$total[1] - points_inside

  if (points_inside == 0) {
    cli::cli_abort(c(
      "No CNEFE coordinates were captured within the provided polygon.",
      "i" = "This may indicate that:",
      "*" = "The municipality code {.val {code_muni}} does not correspond to the polygon's municipality, or",
      "*" = "The polygon is not located within municipality {.val {code_muni}}."
    ))
  }

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

  poly_vals <- NULL
  utils::capture.output(
    utils::capture.output(
      poly_vals <- DBI::dbGetQuery(con, agg_sql),
      type = "message"
    ),
    type = "output"
  )

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
  warn_lines <- character(0)

  n_tracts <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM sc_muni_tbl;")$n[1]

  totals_vars <- setdiff(vars, "avg_inc_resp")

  for (v in totals_vars) {
    total_v <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT SUM(%s) AS total FROM sc_muni_tbl WHERE %s IS NOT NULL;",
        v,
        v
      )
    )$total[1]
    alloc_v <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT SUM(%s_pt) AS alloc FROM cnefe_alloc WHERE %s_pt IS NOT NULL;",
        v,
        v
      )
    )$alloc[1]

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
    eligible_avg <- DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM cnefe_sc p
      JOIN sc_muni_w_dom s USING (code_tract)
      WHERE p.COD_ESPECIE = 1 AND s.n_dom_p > 0;
    "
    )$n[1]

    assigned_avg <- DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM cnefe_alloc
      WHERE avg_inc_resp_pt IS NOT NULL;
    "
    )$n[1]

    assigned_pct <- if (eligible_avg > 0) 100 * assigned_avg / eligible_avg else 0
    warn_lines <- c(
      warn_lines,
      cli::format_inline(
        "{.field avg_inc_resp} assigned to {.strong {assigned_avg}} of {.strong {eligible_avg}} eligible points ({.strong {sprintf('%.2f%%', assigned_pct)}} of total points)"
      )
    )

    na_avg_tracts <- DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM sc_muni_tbl
      WHERE avg_inc_resp IS NULL;
    "
    )$n[1]

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
    unmatched_pct <- if (total_cnefe_pts > 0) 100 * unmatched_pts / total_cnefe_pts else 0
    warn_lines <- c(
      warn_lines,
      cli::format_inline(
        "Unmatched CNEFE points (no tract): {.strong {unmatched_pts}} of {.strong {total_cnefe_pts}} points ({.strong {sprintf('%.2f%%', unmatched_pct)}} of total points)"
      )
    )
  }

  na_totals <- character(0)
  for (v in totals_vars) {
    n_na <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT COUNT(*) AS n FROM sc_muni_tbl WHERE %s IS NULL;",
        v
      )
    )$n[1]
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
        "Tracts with {.strong NA} totals: {paste(na_totals, collapse = '; ')}."
      )
    )
  }

  no_elig <- character(0)
  for (v in totals_vars) {
    if (v %in% c("pop_ph", "n_resp")) {
      n0 <- DBI::dbGetQuery(
        con,
        sprintf(
          "
      SELECT COUNT(*) AS n
      FROM sc_muni_w_dom
      WHERE %s IS NOT NULL AND %s > 0 AND n_dom_p = 0;
    ",
          v,
          v
        )
      )$n[1]
    } else if (v == "pop_ch") {
      n0 <- DBI::dbGetQuery(
        con,
        "
      SELECT COUNT(*) AS n
      FROM sc_muni_w_dom
      WHERE pop_ch IS NOT NULL AND pop_ch > 0 AND n_dom_c = 0;
    "
      )$n[1]
    } else {
      n0 <- DBI::dbGetQuery(
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
  cli::cli_h3("Stage 2: CNEFE points \u2192 Polygons")
  total_polygons <- nrow(polygon_4326)
  polygons_with_values <- nrow(poly_vals)
  polygons_empty <- total_polygons - polygons_with_values

  stage2_lines <- character(0)
  stage2_lines <- c(
    stage2_lines,
    cli::format_inline(
      "Polygon coverage: {.strong {points_inside}} of {.strong {total_alloc_pts}} allocated points captured ({.strong {sprintf('%.2f%%', coverage_pct)}})"
    )
  )
  if (polygons_empty > 0) {
    polygons_empty_pct <- 100 * polygons_empty / total_polygons
    stage2_lines <- c(
      stage2_lines,
      cli::format_inline(
        "Polygons with no CNEFE points: {.strong {polygons_empty}} of {.strong {total_polygons}} total polygons ({.strong {sprintf('%.2f%%', polygons_empty_pct)}})"
      )
    )
  }
  cli::cli_bullets(
    stats::setNames(stage2_lines, rep("i", length(stage2_lines)))
  )

  return(out)
}


