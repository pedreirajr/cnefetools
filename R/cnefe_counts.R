#' Count CNEFE address species on a spatial grid
#'
#' @description
#' `cnefe_counts()` reads CNEFE records for a given municipality, assigns
#' each address point to spatial units (either H3 hexagonal cells or user-provided
#' polygons), and returns per-unit counts of `COD_ESPECIE` as `addr_type1` to
#' `addr_type8`.
#'
#' @param code_muni Integer. Seven-digit IBGE municipality code.
#' @param year Integer. The CNEFE data year. Currently only 2022 is supported.
#'   Defaults to 2022.
#' @param polygon_type Character. Type of polygon aggregation: `"hex"` (default)
#'   uses an H3 hexagonal grid; `"user"` uses polygons provided via the `polygon`
#'   parameter.
#' @param polygon An [`sf::sf`] object with polygon geometries. Required when
#'   `polygon_type = "user"`. A warning is issued reporting the percentage of
#'   CNEFE points covered by the polygon area. If no CNEFE points fall within
#'   the polygon, an error is raised.
#' @param crs_output The CRS for the output object. Only used when
#'   `polygon_type = "user"`. Default is `NULL`, which uses the original CRS of
#'   the `polygon` argument. Can be an EPSG code (e.g., 4326, 31983) or any CRS
#'   object accepted by [sf::st_transform()].
#' @param h3_resolution Integer. H3 grid resolution (default: 9). Only used when
#'   `polygon_type = "hex"`.
#' @param verbose Logical; if `TRUE`, prints messages and timing information.
#' @param backend Character. `"duckdb"` (default) uses DuckDB with H3/spatial
#'   extensions. `"r"` uses h3jsr and sf in R (slower but no DuckDB dependency).
#'
#' @return An [`sf::sf`] object containing:
#' - `id_hex` (when `polygon_type = "hex"`): H3 cell identifier
#' - Original columns from `polygon` (when `polygon_type = "user"`)
#' - `addr_type1` ... `addr_type8`: counts per address type
#' - `geometry`: polygon geometry
#'
#' When `polygon_type = "user"`, the output CRS matches the original `polygon` CRS
#' (or `crs_output` if specified).
#'
#' @details
#' The counts in the columns `addr_type1` to `addr_type8` correspond to:
#' - `addr_type1`: Private household (Domicílio particular)
#' - `addr_type2`: Collective household (Domicílio coletivo)
#' - `addr_type3`: Agricultural establishment (Estabelecimento agropecuário)
#' - `addr_type4`: Educational establishment (Estabelecimento de ensino)
#' - `addr_type5`: Health establishment (Estabelecimento de saúde)
#' - `addr_type6`: Establishment for other purposes (Estabelecimento de outras finalidades)
#' - `addr_type7`: Building under construction or renovation (Edificação em construção ou reforma)
#' - `addr_type8`: Religious establishment (Estabelecimento religioso)
#'
#' @export
cnefe_counts <- function(
  code_muni,
  year = 2022L,
  polygon_type = c("hex", "user"),
  polygon = NULL,

  crs_output = NULL,
  h3_resolution = 9,
  verbose = TRUE,
  backend = c("duckdb", "r")
) {
  polygon_type <- match.arg(polygon_type)
  backend <- match.arg(backend)
  code_muni <- .normalize_code_muni(code_muni)
  year <- .validate_year(year)

  # If polygon is provided but polygon_type is "hex" (default), switch to "user" with warning
  if (!is.null(polygon) && polygon_type == "hex") {
    cli::cli_alert_warning(
      "{.arg polygon} was provided but {.arg polygon_type} was not set to {.val user}."
    )
    cli::cli_alert_info("Setting {.arg polygon_type} to {.val user} automatically.")
    cli::cli_alert_info("To use H3 hexagonal grid instead, set {.code polygon = NULL}.")
    polygon_type <- "user"
  }

  # Validate polygon argument
  if (polygon_type == "user") {
    if (is.null(polygon)) {
      cli::cli_abort(c(
        "{.arg polygon} is required when {.arg polygon_type} is {.val user}.",
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
          "i" = "Use a valid EPSG code (e.
g., 4674, 31983) or a CRS object."
        ))
      }
    }
  }

  # Get the appropriate index for the requested year
  cnefe_index <- .get_cnefe_index(year)

  timings <- list()

  log_step_time <- function(step_name, t_start) {
    dt <- difftime(Sys.time(), t_start, units = "secs")
    timings[[step_name]] <<- dt
    if (verbose) {
      message(sprintf("%s completed in %.2f s.", step_name, as.numeric(dt)))
    }
  }

  if (verbose) {
    message(sprintf("Processing code %s...", code_muni))
  }

  # ---------------------------------------------------------------------------
  # Branch: H3 grid vs user-provided polygon
  # ---------------------------------------------------------------------------
  if (polygon_type == "hex") {
    out <- .cnefe_counts_h3(
      code_muni = code_muni,
      year = year,
      h3_resolution = h3_resolution,
      backend = backend,
      cnefe_index = cnefe_index,
      verbose = verbose,
      timings = timings,
      log_step_time = log_step_time
    )
  } else {
    out <- .cnefe_counts_user_poly(
      code_muni = code_muni,
      year = year,
      polygon = polygon,
      crs_output = crs_output,
      backend = backend,
      cnefe_index = cnefe_index,
      verbose = verbose,
      timings = timings,
      log_step_time = log_step_time
    )
  }

  if (verbose) {
    message("Timing summary (seconds):")
    total_secs <- 0
    for (nm in names(timings)) {
      secs <- as.numeric(timings[[nm]], units = "secs")
      total_secs <- total_secs + secs
      message(sprintf(" - %s: %.2f", nm, secs))
    }
    message(sprintf("Total time: %.2f s.", total_secs))
  }

  out
}


# -----------------------------------------------------------------------------
# Internal: H3 grid aggregation (original behavior)
# -----------------------------------------------------------------------------
.cnefe_counts_h3 <- function(
  code_muni,
  year,
  h3_resolution,
  backend,
  cnefe_index,
  verbose,
  timings,
  log_step_time
) {
  # ---------------------------------------------------------------------------
  # Step 1/3: Ensure ZIP exists in cache and find CSV inside
  # ---------------------------------------------------------------------------
  if (verbose) {
    message("Step 1/3: ensuring ZIP and inspecting archive...")
  }

  t1 <- Sys.time()

  zip_info <- .cnefe_ensure_zip(
    code_muni = code_muni,
    index = cnefe_index,
    cache = TRUE,
    verbose = verbose,
    retry_timeouts = c(300L, 600L, 1800L)
  )
  zip_path <- zip_info$zip_path

  csv_inside <- .cnefe_first_csv_in_zip(zip_path)

  log_step_time("Step 1/3 (ZIP ready)", t1)

  # ---------------------------------------------------------------------------
  # Step 2/3: Build full H3 grid over municipality boundary
  # ---------------------------------------------------------------------------
  if (verbose) {
    message("Step 2/3: building full H3 grid over municipality boundary...")
  }
  t2 <- Sys.time()

  hex_grid <- build_h3_grid(
    h3_resolution = h3_resolution,
    code_muni = code_muni
  )

  log_step_time("Step 2/3 (H3 grid)", t2)

  # ---------------------------------------------------------------------------
  # Step 3/3: Count address species per hexagon
  # ---------------------------------------------------------------------------
  if (verbose) {
    message("Step 3/3: counting address species per hexagon...")
  }
  t3 <- Sys.time()

  counts_long <- NULL

  if (identical(backend, "duckdb")) {
    rlang::check_installed(
      "DBI",
      reason = "to use backend = 'duckdb' in `cnefe_counts()`."
    )
    rlang::check_installed(
      "duckdb",
      reason = "to use backend = 'duckdb' in `cnefe_counts()`."
    )

    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

    .duckdb_ensure_extension(con, "zipfs", verbose = verbose)
    .duckdb_ensure_extension(con, "h3", verbose = verbose)

    zip_norm <- normalizePath(zip_path, winslash = "/", mustWork = TRUE)
    uri <- sprintf("zip://%s/%s", zip_norm, csv_inside)
    uri_sql <- gsub("'", "''", uri)

    sql <- sprintf(
      "
      WITH src AS (
        SELECT
          CAST(LONGITUDE AS DOUBLE) AS lon,
          CAST(LATITUDE  AS DOUBLE) AS lat,
          try_cast(COD_ESPECIE AS INTEGER) AS cod
        FROM read_csv_auto('%s', delim=';', header=true, strict_mode=false)
      )
      SELECT
        lower(hex(CAST(h3_latlng_to_cell(lat, lon, %d) AS UBIGINT))) AS id_hex,
        cod AS COD_ESPECIE,
        COUNT(*)::BIGINT AS n
      FROM src
      WHERE
        lon IS NOT NULL AND lat IS NOT NULL
        AND cod BETWEEN 1 AND 8
      GROUP BY 1, 2;
    ",
      uri_sql,
      as.integer(h3_resolution)
    )

    counts_long <- DBI::dbGetQuery(con, sql) |>
      dplyr::as_tibble() |>
      dplyr::mutate(
        id_hex = as.character(.data$id_hex),
        COD_ESPECIE = as.integer(.data$COD_ESPECIE),
        n = as.integer(.data$n)
      )
  } else {
    # Backend "r" (slower): read Arrow, compute H3 in R
    tab <- read_cnefe(
      code_muni = code_muni,
      year = year,
      output = "arrow",
      cache = TRUE,
      verbose = verbose
    )

    df <- as.data.frame(tab) |>
      dplyr::transmute(
        LONGITUDE = as.numeric(.data$LONGITUDE),
        LATITUDE = as.numeric(.data$LATITUDE),
        COD_ESPECIE = as.integer(.data$COD_ESPECIE)
      ) |>
      dplyr::filter(
        !is.na(.data$LONGITUDE),
        !is.na(.data$LATITUDE),
        !is.na(.data$COD_ESPECIE),
        .data$COD_ESPECIE %in% 1L:8L
      )

    if (nrow(df) > 0L) {
      coords <- df |>
        dplyr::transmute(lon = .data$LONGITUDE, lat = .data$LATITUDE)

      id_hex <- suppressMessages(
        h3jsr::point_to_cell(coords, res = h3_resolution, simple = TRUE)
      )

      counts_long <- df |>
        dplyr::mutate(id_hex = as.character(id_hex)) |>
        dplyr::filter(!is.na(.data$id_hex)) |>
        dplyr::count(.data$id_hex, .data$COD_ESPECIE, name = "n") |>
        dplyr::mutate(
          COD_ESPECIE = as.integer(.data$COD_ESPECIE),
          n = as.integer(.data$n)
        )
    } else {
      counts_long <- dplyr::tibble(
        id_hex = character(0),
        COD_ESPECIE = integer(0),
        n = integer(0)
      )
    }
  }

  # Wide with addr_type1..addr_type8 and robust typing
  if (nrow(counts_long) == 0L) {
    out <- hex_grid
    for (k in 1:8) {
      out[[paste0("addr_type", k)]] <- 0L
    }
  } else {
    counts_wide <- counts_long |>
      tidyr::pivot_wider(
        id_cols = "id_hex",
        names_from = "COD_ESPECIE",
        values_from = "n",
        names_prefix = "addr_type",
        values_fill = list(n = 0L)
      )

    for (k in 1:8) {
      nm <- paste0("addr_type", k)
      if (!nm %in% names(counts_wide)) counts_wide[[nm]] <- 0L
    }

    counts_wide <- counts_wide |>
      dplyr::select("id_hex", dplyr::all_of(paste0("addr_type", 1:8))) |>
      dplyr::mutate(
        dplyr::across(dplyr::starts_with("addr_type"), ~ as.integer(.x))
      )

    out <- hex_grid |>
      dplyr::left_join(counts_wide, by = "id_hex") |>
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("addr_type"),
          ~ dplyr::coalesce(as.integer(.x), 0L)
        )
      )
  }

  # Final safety: force integer and non-negative
  out <- out |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("addr_type"),
        ~ pmax(as.integer(.x), 0L)
      )
    )

  log_step_time("Step 3/3 (count computation)", t3)

  out
}


# -----------------------------------------------------------------------------
# Internal: User-provided polygon aggregation
# -----------------------------------------------------------------------------
.cnefe_counts_user_poly <- function(
  code_muni,
  year,
  polygon,
  crs_output,
  backend,
  cnefe_index,
  verbose,
  timings,
  log_step_time
) {
  # ---------------------------------------------------------------------------
  # Step 1/4: Ensure ZIP exists in cache
  # ---------------------------------------------------------------------------
  if (verbose) {
    message("Step 1/4: ensuring ZIP and inspecting archive...")
  }
  t1 <- Sys.time()

  zip_info <- .cnefe_ensure_zip(
    code_muni = code_muni,
    index = cnefe_index,
    cache = TRUE,
    verbose = verbose,
    retry_timeouts = c(300L, 600L, 1800L)
  )
  zip_path <- zip_info$zip_path
  csv_inside <- .cnefe_first_csv_in_zip(zip_path)

  log_step_time("Step 1/4 (ZIP ready)", t1)

  # ---------------------------------------------------------------------------
  # Step 2/4: Store original CRS and prepare polygon for spatial join
  # ---------------------------------------------------------------------------
  if (verbose) {
    message("Step 2/4: preparing polygon for spatial join...")
  }
  t2 <- Sys.time()

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
    message(sprintf("  Input CRS: %s | Output CRS: %s", crs_input_label, crs_output_label))
  }

  # Transform polygon to WGS84 internally for spatial join with CNEFE points
  polygon_4326 <- sf::st_transform(polygon, 4326)
  polygon_4326 <- sf::st_make_valid(polygon_4326)

  # Add row ID for joining
  polygon_4326 <- polygon_4326 |>
    dplyr::mutate(.poly_row_id = dplyr::row_number())

  log_step_time("Step 2/4 (CRS alignment)", t2)

  # ---------------------------------------------------------------------------
  # Step 3/4: Read CNEFE points and perform spatial join
  # ---------------------------------------------------------------------------
  if (verbose) {
    message("Step 3/4: reading CNEFE points and performing spatial join...")
  }
  t3 <- Sys.time()

  if (identical(backend, "duckdb")) {
    rlang::check_installed(
      "DBI",
      reason = "to use backend = 'duckdb' in `cnefe_counts()`."
    )
    rlang::check_installed(
      "duckdb",
      reason = "to use backend = 'duckdb' in `cnefe_counts()`."
    )

    join_result <- .cnefe_counts_user_poly_duckdb(
      zip_path = zip_path,
      csv_inside = csv_inside,
      polygon = polygon_4326,
      verbose = verbose
    )
  } else {
    # Backend "r": read via Arrow, spatial join via sf
    join_result <- .cnefe_counts_user_poly_r(
      code_muni = code_muni,
      year = year,
      polygon = polygon_4326,
      verbose = verbose
    )
  }

  log_step_time("Step 3/4 (spatial join)", t3)

  # ---------------------------------------------------------------------------
  # Step 4/4: Report coverage, pivot to wide, and join back to polygon
  # ---------------------------------------------------------------------------
  if (verbose) {
    message("Step 4/4: aggregating counts per polygon...")
  }
  t4 <- Sys.time()

  # Extract coverage statistics
  total_points <- join_result$total_points
  points_matched <- join_result$points_matched
  points_outside <- join_result$points_outside
  counts_long <- join_result$counts

  # Check for zero coverage
  if (points_matched == 0L) {
    cli::cli_abort(c(
      "No CNEFE coordinates were captured within the provided polygon.",
      "i" = "This may indicate that:",
      "*" = "The municipality code {.val {code_muni}} does not correspond to the polygon's municipality, or",
      "*" = "The polygon is not located within municipality {.val {code_muni}}."
    ))
  }

  # Calculate coverage percentage
  coverage_pct <- (points_matched / total_points) * 100

  # Issue warning about coverage
  if (points_outside > 0L) {
    cli::cli_warn(c(
      "Polygon coverage: {.val {sprintf('%.1f', coverage_pct)}%} of CNEFE points captured.",
      "i" = "{.val {points_matched}} of {.val {total_points}} points are within the provided polygon.",
      "i" = "{.val {points_outside}} points fell outside the polygon and were not counted."
    ))
  } else {
    cli::cli_inform(c(
      "v" = "All {.val {total_points}} CNEFE points were captured within the provided polygon."
    ))
  }

  # Pivot to wide format
  if (nrow(counts_long) == 0L || all(is.na(counts_long$.poly_row_id))) {
    # No points in any polygon
    counts_wide <- dplyr::tibble(
      .poly_row_id = integer(0)
    )
    for (k in 1:8) {
      counts_wide[[paste0("addr_type", k)]] <- integer(0)
    }
  } else {
    counts_wide <- counts_long |>
      dplyr::filter(!is.na(.data$.poly_row_id)) |>
      tidyr::pivot_wider(
        id_cols = ".poly_row_id",
        names_from = "COD_ESPECIE",
        values_from = "n",
        names_prefix = "addr_type",
        values_fill = list(n = 0L)
      )
  }

  # Ensure all addr_type columns exist
  for (k in 1:8) {
    nm <- paste0("addr_type", k)
    if (!nm %in% names(counts_wide)) counts_wide[[nm]] <- 0L
  }

  counts_wide <- counts_wide |>
    dplyr::select(".poly_row_id", dplyr::all_of(paste0("addr_type", 1:8))) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("addr_type"), ~ as.integer(.x))
    )

  # Join back to polygon (using the 4326 version with row IDs)
  out <- polygon_4326 |>
    dplyr::left_join(counts_wide, by = ".poly_row_id") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("addr_type"),
        ~ dplyr::coalesce(as.integer(.x), 0L)
      )
    ) |>
    dplyr::select(-".poly_row_id")

  # Final safety: force integer and non-negative
  out <- out |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("addr_type"),
        ~ pmax(as.integer(.x), 0L)
      )
    )

  # Transform to output CRS
  out <- sf::st_transform(out, output_crs)

  log_step_time("Step 4/4 (aggregation)", t4)

  out
}


# -----------------------------------------------------------------------------
# Internal: DuckDB backend for user polygon aggregation
# -----------------------------------------------------------------------------
.cnefe_counts_user_poly_duckdb <- function(
  zip_path,
  csv_inside,
  polygon,
  verbose
) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  .duckdb_ensure_extension(con, "zipfs", verbose = verbose)
  .duckdb_ensure_extension(con, "spatial", verbose = verbose)

  zip_norm <- normalizePath(zip_path, winslash = "/", mustWork = TRUE)
  uri <- sprintf("zip://%s/%s", zip_norm, csv_inside)
  uri_sql <- gsub("'", "''", uri)

  # Read all CNEFE points
  sql_points <- sprintf(
    "
    SELECT
      CAST(LONGITUDE AS DOUBLE) AS lon,
      CAST(LATITUDE  AS DOUBLE) AS lat,
      try_cast(COD_ESPECIE AS INTEGER) AS COD_ESPECIE
    FROM read_csv_auto('%s', delim=';', header=true, strict_mode=false)
    WHERE
      LONGITUDE IS NOT NULL AND LATITUDE IS NOT NULL
      AND try_cast(COD_ESPECIE AS INTEGER) BETWEEN 1 AND 8;
  ",
    uri_sql
  )

  cnefe_df <- DBI::dbGetQuery(con, sql_points)
  total_points <- nrow(cnefe_df)

  if (total_points == 0L) {
    return(list(
      counts = dplyr::tibble(
        .poly_row_id = integer(0),
        COD_ESPECIE = integer(0),
        n = integer(0)
      ),
      total_points = 0L,
      points_matched = 0L,
      points_outside = 0L
    ))
  }

  # Convert to sf and perform spatial join in R
  cnefe_pts <- sf::st_as_sf(
    cnefe_df,
    coords = c("lon", "lat"),
    crs = 4326
  )

  # Spatial join - use st_intersects to handle edge cases better
  joined <- sf::st_join(cnefe_pts, polygon[, ".poly_row_id"], join = sf::st_within)

  # Count unique points that matched at least one polygon
  points_matched <- sum(!is.na(joined$.poly_row_id))

  # For counting unique points matched (not duplicated by overlapping polygons)
  unique_matched <- length(unique(which(!is.na(joined$.poly_row_id))))

  # Use original point count to determine points outside
  # A point is "outside" if it didn't match any polygon
  pts_with_match <- !is.na(joined$.poly_row_id)

  # Get row indices of original points that got at least one match
  # Since st_join preserves order and can duplicate rows for overlapping polygons,
  # we need to track which original points got matched

  # Add point ID before join to track properly
  cnefe_pts$.pt_id <- seq_len(nrow(cnefe_pts))
  joined_with_id <- sf::st_join(cnefe_pts, polygon[, ".poly_row_id"], join = sf::st_within)

  unique_pts_matched <- length(unique(joined_with_id$.pt_id[!is.na(joined_with_id$.poly_row_id)]))
  points_outside <- total_points - unique_pts_matched

  # Count by polygon and species (using joined without pt_id for cleaner output)
  counts_long <- joined |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(.data$.poly_row_id)) |>
    dplyr::count(.data$.poly_row_id, .data$COD_ESPECIE, name = "n") |>
    dplyr::mutate(
      COD_ESPECIE = as.integer(.data$COD_ESPECIE),
      n = as.integer(.data$n)
    )

  list(
    counts = counts_long,
    total_points = total_points,
    points_matched = unique_pts_matched,
    points_outside = points_outside
  )
}


# -----------------------------------------------------------------------------
# Internal: R backend for user polygon aggregation
# -----------------------------------------------------------------------------
.cnefe_counts_user_poly_r <- function(
  code_muni,
  year,
  polygon,
  verbose
) {
  # Read CNEFE data via Arrow
  tab <- read_cnefe(
    code_muni = code_muni,
    year = year,
    output = "arrow",
    cache = TRUE,
    verbose = verbose
  )

  df <- as.data.frame(tab) |>
    dplyr::transmute(
      LONGITUDE = as.numeric(.data$LONGITUDE),
      LATITUDE = as.numeric(.data$LATITUDE),
      COD_ESPECIE = as.integer(.data$COD_ESPECIE)
    ) |>
    dplyr::filter(
      !is.na(.data$LONGITUDE),
      !is.na(.data$LATITUDE),
      !is.na(.data$COD_ESPECIE),
      .data$COD_ESPECIE %in% 1L:8L
    )

  total_points <- nrow(df)

  if (total_points == 0L) {
    return(list(
      counts = dplyr::tibble(
        .poly_row_id = integer(0),
        COD_ESPECIE = integer(0),
        n = integer(0)
      ),
      total_points = 0L,
      points_matched = 0L,
      points_outside = 0L
    ))
  }

  # Convert to sf points and add point ID
  cnefe_pts <- sf::st_as_sf(
    df,
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4326
  )
  cnefe_pts$.pt_id <- seq_len(nrow(cnefe_pts))

  # Spatial join
  joined <- sf::st_join(cnefe_pts, polygon[, ".poly_row_id"], join = sf::st_within)

  # Count unique points that matched at least one polygon
  unique_pts_matched <- length(unique(joined$.pt_id[!is.na(joined$.poly_row_id)]))
  points_outside <- total_points - unique_pts_matched

  # Count by polygon and species
  counts_long <- joined |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(.data$.poly_row_id)) |>
    dplyr::count(.data$.poly_row_id, .data$COD_ESPECIE, name = "n") |>
    dplyr::mutate(
      COD_ESPECIE = as.integer(.data$COD_ESPECIE),
      n = as.integer(.data$n)
    )

  list(
    counts = counts_long,
    total_points = total_points,
    points_matched = unique_pts_matched,
    points_outside = points_outside
  )
}


# -----------------------------------------------------------------------------
# Internal: Helper to ensure DuckDB extension is loaded
# -----------------------------------------------------------------------------
.duckdb_ensure_extension <- function(
  con,
  ext,
  repo = "community",
  verbose = TRUE
) {
  info <- tryCatch(
    DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT installed, loaded FROM duckdb_extensions() WHERE extension_name = '%s';",
        ext
      )
    ),
    error = function(e) NULL
  )

  if (!is.null(info) && nrow(info) == 1) {
    if (isTRUE(info$loaded[[1]])) {
      if (verbose) {
        message("DuckDB: extension '", ext, "' already loaded.")
      }
      return(invisible(TRUE))
    }
    if (isTRUE(info$installed[[1]])) {
      if (verbose) {
        message("DuckDB: loading extension '", ext, "'...")
      }
      DBI::dbExecute(con, sprintf("LOAD %s;", ext))
      return(invisible(TRUE))
    }
  }

  ok_load <- tryCatch(
    {
      if (verbose) {
        message("DuckDB: trying to LOAD extension '", ext, "'...")
      }
      DBI::dbExecute(con, sprintf("LOAD %s;", ext))
      TRUE
    },
    error = function(e) FALSE
  )

  if (ok_load) {
    return(invisible(TRUE))
  }

  if (verbose) {
    message("DuckDB: installing extension '", ext, "' from ", repo, "...")
  }
  DBI::dbExecute(con, sprintf("INSTALL %s FROM %s;", ext, repo))
  if (verbose) {
    message("DuckDB: loading extension '", ext, "'...")
  }
  DBI::dbExecute(con, sprintf("LOAD %s;", ext))

  invisible(TRUE)
}
