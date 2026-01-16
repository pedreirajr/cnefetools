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
#'   - `n_inhab_p`: population in private households (*Domicílios particulares*).
#'   - `n_inhab_c`: population in collective households (*Domicílios coletivos*).
#'   - `male`: total male population.
#'   - `female`: total female population.
#'   - `age_0_4`, `age_5_9`, `age_10_14`, `age_15_19`, `age_20_24`, `age_25_29`,
#'     `age_30_39`, `age_40_49`, `age_50_59`, `age_60_69`, `age_70m`: population by age group.
#'   - `n_resp`: number of household heads (*Pessoas responsáveis por domicílios*).
#'   - `avg_inc_resp`: average income of the household heads.
#'
#'   Allocation rules:
#'   - `n_inhab_p` is allocated only to private dwellings.
#'   - `n_inhab_c` is allocated only to collective dwellings.
#'   - All other sum-like variables are allocated to private dwellings when the tract has any;
#'     if the tract has zero private dwellings but has collective dwellings, they are allocated to collective.
#'   - `avg_inc_resp` is assigned (not split) to each eligible dwelling point using the same eligibility rule.
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
#'   Attributes:
#'   - `attr(x, "timing")`: named numeric vector with step timings (seconds).
#'
#' @export
tracts_to_polygon <- function(
  code_muni,
  polygon,
  year = 2022,

  vars = c("n_inhab_p", "n_inhab_c"),
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
    "n_inhab_p",
    "n_inhab_c",
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
  .t0 <- function() Sys.time()
  .td <- function(t_start, t_end) {
    as.numeric(difftime(t_end, t_start, units = "secs"))
  }

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
      .duckdb_quiet_execute(con, sprintf("INSTALL %s;", ext))
      .duckdb_quiet_execute(con, sprintf("LOAD %s;", ext))
    }
    invisible(TRUE)
  }

  .fmt_pct <- function(x) sprintf("%.2f%%", x)

  # timing container ----------------------------------------------------------
  step_times <- numeric(0)

  if (verbose) {
    cli::cli_alert_info("Processing municipality code {.val {code_muni}}...")
  }

  # Step 1/6: CRS alignment ---------------------------------------------------
  if (verbose) {
    cli::cli_alert("Step 1/6: aligning CRS...")
  }
  t1 <- .t0()

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

  # Transform polygon to WGS84 internally for spatial join with CNEFE points
  polygon_4326 <- sf::st_transform(polygon, 4326)
  polygon_4326 <- sf::st_make_valid(polygon_4326)

  # Add row ID for joining
  polygon_4326 <- polygon_4326 |>
    dplyr::mutate(.poly_row_id = dplyr::row_number())

  t1e <- .t0()
  step_times["CRS alignment"] <- .td(t1, t1e)
  if (verbose) {
    cli::cli_alert_success("Step 1/6 (CRS alignment) completed in {.val {sprintf('%.2f', step_times['CRS alignment'])}} s.")
  }

  # Step 2/6: Connect to DuckDB -----------------------------------------------
  if (verbose) {
    cli::cli_alert("Step 2/6: connecting to DuckDB and loading extensions...")
  }
  t2 <- .t0()

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  duckspatial::ddbs_load(con)

  # always keep DuckDB extension load messages silent
  .duckdb_load_ext(con, "zipfs")

  t2e <- .t0()
  step_times["DuckDB ready"] <- .td(t2, t2e)
  if (verbose) {
    cli::cli_alert_success("Step 2/6 (DuckDB ready) completed in {.val {sprintf('%.2f', step_times['DuckDB ready'])}} s.")
  }

  # Step 3/6: Prepare census tracts -------------------------------------------
  if (verbose) {
    cli::cli_alert("Step 3/6: preparing census tracts in DuckDB...")
  }
  t3 <- .t0()

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

    if (outside_pct > 0.01) {
      outside_pct_fmt <- .fmt_pct(outside_pct)
      cli::cli_warn(c(
        "{.val {outside_pct_fmt}} of polygon area falls outside census tracts of municipality {.val {code_muni}}.",
        "i" = "Only CNEFE points within census tracts will be used for interpolation."
      ))
    }
  }

  t3e <- .t0()
  step_times["tracts ready"] <- .td(t3, t3e)
  if (verbose) {
    cli::cli_alert_success("Step 3/6 (tracts ready) completed in {.val {sprintf('%.2f', step_times['tracts ready'])}} s.")
  }

  # Step 4/6: Prepare CNEFE points --------------------------------------------
  if (verbose) {
    cli::cli_alert("Step 4/6: preparing CNEFE points in DuckDB...")
  }
  t4 <- .t0()

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

  t4e <- .t0()
  step_times["CNEFE points ready"] <- .td(t4, t4e)
  if (verbose) {
    cli::cli_alert_success("Step 4/6 (CNEFE points ready) completed in {.val {sprintf('%.2f', step_times['CNEFE points ready'])}} s.")
  }

  # Step 5/6: Spatial join and allocation -------------------------------------
  if (verbose) {
    cli::cli_alert("Step 5/6: spatial join (points to tracts) and allocation...")
  }
  t5 <- .t0()

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
    } else if (v == "n_inhab_p") {
      alloc_exprs <- c(
        alloc_exprs,
        "
        CASE
          WHEN p.COD_ESPECIE = 1
           AND s.n_inhab_p IS NOT NULL
           AND s.n_dom_p > 0
          THEN CAST(s.n_inhab_p AS DOUBLE) / s.n_dom_p
          ELSE NULL
        END AS n_inhab_p_pt
      "
      )
    } else if (v == "n_inhab_c") {
      alloc_exprs <- c(
        alloc_exprs,
        "
        CASE
          WHEN p.COD_ESPECIE = 2
           AND s.n_inhab_c IS NOT NULL
           AND s.n_dom_c > 0
          THEN CAST(s.n_inhab_c AS DOUBLE) / s.n_dom_c
          ELSE NULL
        END AS n_inhab_c_pt
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

  # Register user polygons in DuckDB using duckspatial
  duckspatial::ddbs_write_vector(
    conn = con,
    data = polygon_4326[, ".poly_row_id"],
    name = "user_polygons",
    overwrite = TRUE
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

  # Count points inside/outside polygons
  coverage_stats <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total,
      COUNT(poly_row_id) AS inside,
      COUNT(*) - COUNT(poly_row_id) AS outside
    FROM cnefe_poly_joined;
  "
  )

  points_inside <- coverage_stats$inside[1]
  points_outside <- coverage_stats$outside[1]

  if (points_inside == 0) {
    cli::cli_abort(c(
      "No CNEFE coordinates were captured within the provided polygon.",
      "i" = "This may indicate that:",
      "*" = "The municipality code {.val {code_muni}} does not correspond to the polygon's municipality, or",
      "*" = "The polygon is not located within municipality {.val {code_muni}}."
    ))
  }

  # Report coverage
  coverage_pct <- (points_inside / total_alloc_pts) * 100
  if (points_outside > 0) {
    # Use 2 decimal places to avoid showing 100% when some points are outside
    coverage_pct_fmt <- sprintf("%.2f", coverage_pct)
    cli::cli_warn(c(
      "Polygon coverage: {.val {coverage_pct_fmt}%} of CNEFE dwelling points captured.",
      "i" = "{.val {points_inside}} of {.val {total_alloc_pts}} points are within the provided polygon.",
      "i" = "{.val {points_outside}} points fell outside the polygon and were not counted."
    ))
  } else if (verbose) {
    cli::cli_alert_success("All {.val {total_alloc_pts}} CNEFE dwelling points were captured within the provided polygon.")
  }

  t5e <- .t0()
  step_times["join and allocation"] <- .td(t5, t5e)
  if (verbose) {
    cli::cli_alert_success("Step 5/6 (join and allocation) completed in {.val {sprintf('%.2f', step_times['join and allocation'])}} s.")
  }

  # Step 6/6: Aggregate to polygons -------------------------------------------
  if (verbose) {
    cli::cli_alert("Step 6/6: aggregating allocated values to polygons...")
  }
  t6 <- .t0()

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

  poly_vals <- DBI::dbGetQuery(con, agg_sql)

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

  t6e <- .t0()
  step_times["polygon aggregation"] <- .td(t6, t6e)
  if (verbose) {
    cli::cli_alert_success("Step 6/6 (polygon aggregation) completed in {.val {sprintf('%.2f', step_times['polygon aggregation'])}} s.")
  }

  # attributes ----------------------------------------------------------------
  attr(out, "timing") <- as.list(step_times)

  if (verbose) {
    total_time <- sum(step_times)
    cli::cli_h3("Timing summary (seconds)")
    for (nm in names(step_times)) {
      cli::cli_alert_info("{nm}: {.val {sprintf('%.2f', step_times[[nm]])}}")
    }
    cli::cli_alert_success("Total time: {.val {sprintf('%.2f', total_time)}} s.")
  }

  # diagnostics and warning ----------------------------------------------------
  warn_lines <- character(0)

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

    unalloc <- max(total_v - alloc_v, 0)
    if (total_v > 0 && unalloc > 0) {
      pct <- 100 * unalloc / total_v

      label <- v
      if (v == "n_inhab_p") {
        label <- "population from private households (n_inhab_p)"
      }
      if (v == "n_inhab_c") {
        label <- "population from collective households (n_inhab_c)"
      }

      warn_lines <- c(
        warn_lines,
        sprintf(
          "- Unallocated total for %s = %.0f (%s of total).",
          label,
          unalloc,
          .fmt_pct(pct)
        )
      )
    }
  }

  if ("avg_inc_resp" %in% vars) {
    eligible_avg <- DBI::dbGetQuery(
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

    assigned_avg <- DBI::dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM cnefe_alloc
      WHERE avg_inc_resp_pt IS NOT NULL;
    "
    )$n[1]

    warn_lines <- c(
      warn_lines,
      sprintf(
        "- avg_inc_resp assigned to %d of %d eligible points.",
        assigned_avg,
        eligible_avg
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
      warn_lines <- c(
        warn_lines,
        sprintf("- avg_inc_resp NA in %d tracts.", na_avg_tracts)
      )
    }
  }

  if (unmatched_pts > 0) {
    warn_lines <- c(
      warn_lines,
      sprintf("- Unmatched CNEFE points (no tract) = %d.", unmatched_pts)
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
      na_totals <- c(na_totals, sprintf("%s NA in %d", v, n_na))
    }
  }
  if (length(na_totals) > 0) {
    warn_lines <- c(
      warn_lines,
      paste0(
        "- Tracts with NA totals: ",
        paste(na_totals, collapse = "; "),
        "."
      )
    )
  }

  no_elig <- character(0)
  for (v in totals_vars) {
    if (v == "n_inhab_p") {
      n0 <- DBI::dbGetQuery(
        con,
        "
        SELECT COUNT(*) AS n
        FROM sc_muni_w_dom
        WHERE n_inhab_p IS NOT NULL AND n_inhab_p > 0 AND n_dom_p = 0;
      "
      )$n[1]
    } else if (v == "n_inhab_c") {
      n0 <- DBI::dbGetQuery(
        con,
        "
        SELECT COUNT(*) AS n
        FROM sc_muni_w_dom
        WHERE n_inhab_c IS NOT NULL AND n_inhab_c > 0 AND n_dom_c = 0;
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
      no_elig <- c(no_elig, sprintf("%s in %d tracts", v, n0))
    }
  }
  if (length(no_elig) > 0) {
    warn_lines <- c(
      warn_lines,
      paste0(
        "- Tracts with no eligible dwellings: ",
        paste(no_elig, collapse = "; "),
        "."
      )
    )
  }

  if (length(warn_lines) > 0) {
    warning(
      paste(
        c("Dasymetric interpolation diagnostics:", warn_lines),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  out
}
