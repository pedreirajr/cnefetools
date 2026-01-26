#' Compute land-use mix indicators on a spatial grid
#'
#' @description
#' `compute_lumi()` reads CNEFE records for a given municipality,
#' assigns each address point to spatial units (either H3 hexagonal cells or
#' user-provided polygons), and computes the residential proportion (`p_res`) and land-use mix
#' indices, such as the Entropy Index (`ei`), the Herfindahl-Hirschman Index (`hhi`),
#' the Balance Index (`bal`), the Index of Concentration at Extremes (`ice`), the adapted HHI (`hhi_adp`),
#' and the Bidirectional Global-centered Index (`bgbi`), following the methodology proposed in
#' Pedreira Jr. et al. (2025).
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
#' @param backend Character. `"duckdb"` (default) uses DuckDB + H3 extension
#'   reading directly from the cached ZIP. `"r"` computes H3 in R using h3jsr.
#'
#' @return An [`sf::sf`] object containing:
#' \describe{
#'   \item{When `polygon_type = "hex"`:}{
#'     \itemize{
#'       \item `id_hex`: H3 cell identifier
#'       \item `p_res`, `ei`, `hhi`, `bal`, `ice`, `hhi_adp`, `bgbi`: land-use
#'         mix indicators
#'       \item `geometry`: hexagon geometry (CRS 4326)
#'     }
#'   }
#'   \item{When `polygon_type = "user"`:}{
#'     \itemize{
#'       \item Original columns from `polygon`
#'       \item `p_res`, `ei`, `hhi`, `bal`, `ice`, `hhi_adp`, `bgbi`: land-use
#'         mix indicators
#'       \item `geometry`: polygon geometry (in the original or `crs_output` CRS)
#'     }
#'   }
#' }
#'
#' @references
#' Pedreira Jr., J. U.; Louro, T. V.; Assis, L. B. M.; Brito, P. L.
#' Measuring land use mix with address-level census data (2025).
#' *engrXiv*. https://engrxiv.org/preprint/view/5975
#'
#' Booth, A.; Crouter, A. C. (Eds.). (2001).
#' *Does It Take a Village? Community Effects on Children, Adolescents, and Families*.
#' Psychology Press.
#'
#' Song, Y.; Merlin, L.; Rodriguez, D. (2013).
#' Comparing measures of urban land use mix.
#' *Computers, Environment and Urban Systems*, 42, 1--13.
#' https://doi.org/10.1016/j.compenvurbsys.2013.08.001
#'
#' @export
compute_lumi <- function(
  code_muni,
  year = 2022,
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

  # If polygon is provided but polygon_type is "hex" (default), switch to "user"
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
          "i" = "Use a valid EPSG code (e.g., 4674, 31983) or a CRS object."
        ))
      }
    }
  }

  # Get the appropriate index for the requested year
  cnefe_index <- .get_cnefe_index(year)

  # Name (optional)
  info <- cnefe_index[
    cnefe_index$code_muni == code_muni,
    ,
    drop = FALSE
  ]
  city_name <- if (
    nrow(info) > 0 && "name_muni" %in% names(info) && !is.na(info$name_muni[1])
  ) {
    info$name_muni[1]
  } else {
    as.character(code_muni)
  }

  if (verbose) {
    cli::cli_alert_info("Processing municipality code {.val {code_muni}}...")
  }

  # We will return sf objects
  rlang::check_installed(
    "sf",
    reason = "to return an sf grid in `compute_lumi()`."
  )

  # Branch: H3 grid vs user-provided polygon
  if (polygon_type == "hex") {
    out <- .compute_lumi_h3(
      code_muni = code_muni,
      year = year,
      h3_resolution = h3_resolution,
      backend = backend,
      cnefe_index = cnefe_index,
      verbose = verbose
    )
  } else {
    out <- .compute_lumi_user_poly(
      code_muni = code_muni,
      year = year,
      polygon = polygon,
      crs_output = crs_output,
      backend = backend,
      cnefe_index = cnefe_index,
      verbose = verbose
    )
  }

  out
}


# -----------------------------------------------------------------------------
# Internal: Shared LUMI index computation helpers
# -----------------------------------------------------------------------------

# Balance index (BAL)
.bal_fun <- function(p, P) {
  num <- abs(p - (P / (1 - P)) * (1 - p))
  den <- p + (P / (1 - P)) * (1 - p)
  ifelse(is.na(num) | is.na(den), NA_real_, 1 - num / den)
}

# BGBI function
.bgbi_fun <- function(p, P) {
  num <- (2 * p - 1) - (2 * P - 1)
  den <- 1 - (2 * p - 1) * (2 * P - 1)
  den[den == 0] <- NA_real_
  ifelse(is.na(num) | is.na(den), NA_real_, num / den)
}

# Numerically safe entropy term: treat 0*log(0) as 0
.safe_plogp <- function(x) {
  ifelse(is.na(x) | x <= 0, 0, x * log(x))
}

#' Compute LUMI indices from n_res and n_tot columns
#'
#' Adds p_res, ei, hhi, bal, ice, hhi_adp, bgbi columns to a data frame.
#'
#' @param df A data frame with `n_res` and `n_tot` columns.
#' @param P Global residential proportion for the municipality.
#' @return The data frame with LUMI columns added.
#' @keywords internal
#' @noRd
.compute_lumi_indices <- function(df, P) {
  df |>
    dplyr::mutate(
      p_res = dplyr::if_else(
        .data$n_tot > 0,
        .data$n_res / .data$n_tot,
        NA_real_
      ),
      q_rest = dplyr::if_else(!is.na(.data$p_res), 1 - .data$p_res, NA_real_),

      # EI (k=2)
      ei = dplyr::if_else(
        !is.na(.data$p_res),
        -(.safe_plogp(.data$p_res) + .safe_plogp(.data$q_rest)) / log(2),
        NA_real_
      ),

      # HHI (2 categories)
      hhi = dplyr::if_else(
        !is.na(.data$p_res),
        (.data$p_res^2 + .data$q_rest^2),
        NA_real_
      ),

      # BAL
      bal = dplyr::if_else(
        !is.na(.data$p_res),
        .bal_fun(.data$p_res, P),
        NA_real_
      ),

      # scaled HHI (min=0.5 for k=2)
      hhi_sc = dplyr::if_else(
        !is.na(.data$hhi),
        (.data$hhi - 0.5) / (1 - 0.5),
        NA_real_
      ),

      # ICE (Index of Concentration at Extremes)
      ice = dplyr::if_else(
        !is.na(.data$p_res),
        .data$p_res - .data$q_rest,
        NA_real_
      ),

      hhi_adp = dplyr::if_else(
        !is.na(.data$p_res) & !is.na(.data$hhi_sc),
        sign(.data$p_res - .data$q_rest) * .data$hhi_sc,
        NA_real_
      ),

      bgbi = dplyr::if_else(
        !is.na(.data$p_res),
        .bgbi_fun(.data$p_res, P),
        NA_real_
      )
    )
}


# -----------------------------------------------------------------------------
# Internal: H3 grid aggregation (original behavior)
# -----------------------------------------------------------------------------
.compute_lumi_h3 <- function(
  code_muni,
  year,
  h3_resolution,
  backend,
  cnefe_index,
  verbose
) {
  # ---------------------------------------------------------------------------
  # Step 1/3: Ensure ZIP and find CSV inside
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 1/3: ensuring ZIP and inspecting archive...",
                           msg_done = "Step 1/3 (ZIP ready)")
  }

  zip_info <- .cnefe_ensure_zip(
    code_muni = code_muni,
    index = cnefe_index,
    cache = TRUE,
    verbose = verbose,
    retry_timeouts = c(300L, 600L, 1800L)
  )
  zip_path <- zip_info$zip_path

  csv_inside <- .cnefe_first_csv_in_zip(zip_path)

  if (verbose) {
    cli::cli_progress_done("Step 1/3: ensuring ZIP and inspecting archive...")
  }

  # ---------------------------------------------------------------------------
  # Step 2/3: Aggregate counts per hex (n_res, n_tot)
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 2/3: aggregating CNEFE counts per H3 cell...",
                           msg_done = "Step 2/3 (counts per hex)")
  }

  counts_hex <- NULL

  if (identical(backend, "duckdb")) {
    rlang::check_installed(
      "DBI",
      reason = "to use backend = 'duckdb' in `compute_lumi()`."
    )
    rlang::check_installed(
      "duckdb",
      reason = "to use backend = 'duckdb' in `compute_lumi()`."
    )

    con <- NULL
    utils::capture.output(
      utils::capture.output({
        con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:",
                              config = list(
                                'enable_progress_bar' = FALSE,
                                'enable_print_progress' = FALSE,
                                'print_progress_bar' = FALSE
                              ))

        .duckdb_ensure_extension(con, "zipfs", verbose = verbose)
        .duckdb_ensure_extension(con, "h3", verbose = verbose)

        zip_norm <- normalizePath(zip_path, winslash = "/", mustWork = TRUE)
        uri <- sprintf("zip://%s/%s", zip_norm, csv_inside)
        uri_sql <- gsub("'", "''", uri)

        # only keep the columns needed; exclude COD_ESPECIE == 7
        sql <- sprintf(
          "
        WITH src AS (
          SELECT
            CAST(LONGITUDE AS DOUBLE) AS lon,
            CAST(LATITUDE  AS DOUBLE) AS lat,
            try_cast(COD_ESPECIE AS INTEGER) AS cod
          FROM read_csv_auto('%s', delim=';', header=true, strict_mode=false)
        ),
        filtered AS (
          SELECT
            lower(hex(CAST(h3_latlng_to_cell(lat, lon, %d) AS UBIGINT))) AS id_hex,
            cod
          FROM src
          WHERE
            lon IS NOT NULL AND lat IS NOT NULL
            AND cod BETWEEN 1 AND 8
            AND cod != 7
        )
        SELECT
          id_hex,
          SUM(CASE WHEN cod = 1 THEN 1 ELSE 0 END)::BIGINT AS n_res,
          COUNT(*)::BIGINT AS n_tot
        FROM filtered
        GROUP BY 1;
      ",
          uri_sql,
          as.integer(h3_resolution)
        )

        counts_hex <- DBI::dbGetQuery(con, sql) |>
          dplyr::as_tibble() |>
          dplyr::mutate(
            id_hex = as.character(.data$id_hex),
            n_res = as.integer(.data$n_res),
            n_tot = as.integer(.data$n_tot)
          )
      }, type = "message"),
      type = "output"
    )

    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  } else {
    # backend "r"
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
        .data$COD_ESPECIE %in% 1L:8L,
        .data$COD_ESPECIE != 7L
      )

    if (nrow(df) == 0L) {
      if (verbose) {
        message(
          "No valid CNEFE points after filtering (COD_ESPECIE 1:8 excluding 7). Returning NULL."
        )
      }
      return(NULL)
    }

    coords <- df |>
      dplyr::transmute(lon = .data$LONGITUDE, lat = .data$LATITUDE)

    id_hex <- suppressMessages(h3jsr::point_to_cell(
      coords,
      res = h3_resolution,
      simple = TRUE
    ))

    counts_hex <- df |>
      dplyr::mutate(id_hex = as.character(id_hex)) |>
      dplyr::filter(!is.na(.data$id_hex)) |>
      dplyr::group_by(.data$id_hex) |>
      dplyr::summarise(
        n_res = sum(.data$COD_ESPECIE == 1L, na.rm = TRUE),
        n_tot = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        n_res = as.integer(.data$n_res),
        n_tot = as.integer(.data$n_tot)
      )
  }

  if (verbose) {
    cli::cli_progress_done("Step 2/3: aggregating CNEFE counts per H3 cell...")
  }

  if (is.null(counts_hex) || nrow(counts_hex) == 0L) {
    if (verbose) {
      message("No hexagons found after aggregation. Returning NULL.")
    }
    return(NULL)
  }

  # ---------------------------------------------------------------------------
  # Step 3/3: Build H3 grid from ids + compute indices
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 3/3: building grid and computing LUMI indices...",
                           msg_done = "Step 3/3 (indices)")
  }

  hex_grid <- build_h3_grid(
    h3_resolution = h3_resolution,
    id_hex = counts_hex$id_hex
  )

  # Global city residential proportion P (exclude COD_ESPECIE == 7 already)
  P <- sum(counts_hex$n_res, na.rm = TRUE) / sum(counts_hex$n_tot, na.rm = TRUE)

  out <- hex_grid |>
    dplyr::left_join(counts_hex, by = "id_hex") |>
    dplyr::mutate(
      n_res = dplyr::coalesce(as.integer(.data$n_res), 0L),
      n_tot = dplyr::coalesce(as.integer(.data$n_tot), 0L)
    ) |>
    .compute_lumi_indices(P) |>
    dplyr::select(
      "id_hex",
      "p_res",
      "ei",
      "hhi",
      "bal",
      "ice",
      "hhi_adp",
      "bgbi",
      "geometry"
    )

  if (verbose) {
    cli::cli_progress_done("Step 3/3: building grid and computing LUMI indices...")
  }

  out
}


# -----------------------------------------------------------------------------
# Internal: User-provided polygon aggregation
# -----------------------------------------------------------------------------
.compute_lumi_user_poly <- function(
  code_muni,
  year,
  polygon,
  crs_output,
  backend,
  cnefe_index,
  verbose
) {
  # ---------------------------------------------------------------------------
  # Step 1/4: Ensure ZIP exists in cache
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 1/4: ensuring ZIP and inspecting archive...",
                           msg_done = "Step 1/4 (ZIP ready)")
  }


  zip_info <- .cnefe_ensure_zip(
    code_muni = code_muni,
    index = cnefe_index,
    cache = TRUE,
    verbose = verbose,
    retry_timeouts = c(300L, 600L, 1800L)
  )
  zip_path <- zip_info$zip_path
  csv_inside <- .cnefe_first_csv_in_zip(zip_path)

  if (verbose) {
    cli::cli_progress_done("Step 1/4: ensuring ZIP and inspecting archive...")
  }

  # ---------------------------------------------------------------------------
  # Step 2/4: Store original CRS and prepare polygon for spatial join
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 2/4: preparing polygon for spatial join...",
                           msg_done = "Step 2/4 (CRS alignment)")
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
    message(sprintf("  Input CRS: %s | Output CRS: %s", crs_input_label, crs_output_label))
  }

  # Transform polygon to WGS84 internally for spatial join with CNEFE points
  polygon_4326 <- sf::st_transform(polygon, 4326)
  polygon_4326 <- sf::st_make_valid(polygon_4326)

  # Add row ID for joining
  polygon_4326 <- polygon_4326 |>
    dplyr::mutate(.poly_row_id = dplyr::row_number())

  if (verbose) {
    cli::cli_progress_done("Step 2/4: preparing polygon for spatial join...")
  }

  # ---------------------------------------------------------------------------
  # Step 3/4: Read CNEFE points and perform spatial join
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 3/4: reading CNEFE points and performing spatial join...",
                           msg_done = "Step 3/4 (spatial join)")
  }

  if (identical(backend, "duckdb")) {
    rlang::check_installed(
      "DBI",
      reason = "to use backend = 'duckdb' in `compute_lumi()`."
    )
    rlang::check_installed(
      "duckdb",
      reason = "to use backend = 'duckdb' in `compute_lumi()`."
    )

    join_result <- .compute_lumi_user_poly_duckdb(
      zip_path = zip_path,
      csv_inside = csv_inside,
      polygon = polygon_4326,
      verbose = verbose
    )
  } else {
    join_result <- .compute_lumi_user_poly_r(
      code_muni = code_muni,
      year = year,
      polygon = polygon_4326,
      verbose = verbose
    )
  }

  if (verbose) {
    cli::cli_progress_done("Step 3/4: reading CNEFE points and performing spatial join...")
  }

  # ---------------------------------------------------------------------------
  # Step 4/4: Report coverage, compute LUMI indices, and join back to polygon
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 4/4: computing LUMI indices per polygon...",
                           msg_done = "Step 4/4 (LUMI indices)")
  }

  # Extract coverage statistics
  total_points <- join_result$total_points
  points_matched <- join_result$points_matched
  points_outside <- join_result$points_outside
  counts <- join_result$counts
  total_n_res <- join_result$total_n_res
  total_n_tot <- join_result$total_n_tot

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

  # Global P from all municipality CNEFE points (not just those in polygons)
  P <- total_n_res / total_n_tot

  # Join counts to polygon and compute LUMI indices
  if (nrow(counts) == 0L || all(is.na(counts$.poly_row_id))) {
    # No matched points: add empty LUMI columns
    out <- polygon_4326 |>
      dplyr::mutate(
        n_res = 0L,
        n_tot = 0L
      ) |>
      .compute_lumi_indices(P) |>
      dplyr::select(
        -"n_res", -"n_tot", -"q_rest", -"hhi_sc",
        -".poly_row_id"
      )
  } else {
    counts_per_poly <- counts |>
      dplyr::filter(!is.na(.data$.poly_row_id))

    out <- polygon_4326 |>
      dplyr::left_join(counts_per_poly, by = ".poly_row_id") |>
      dplyr::mutate(
        n_res = dplyr::coalesce(as.integer(.data$n_res), 0L),
        n_tot = dplyr::coalesce(as.integer(.data$n_tot), 0L)
      ) |>
      .compute_lumi_indices(P) |>
      dplyr::select(
        -"n_res", -"n_tot", -"q_rest", -"hhi_sc",
        -".poly_row_id"
      )
  }

  # Transform to output CRS
  out <- sf::st_transform(out, output_crs)

  if (verbose) {
    cli::cli_progress_done("Step 4/4: computing LUMI indices per polygon...")
  }

  out
}


# -----------------------------------------------------------------------------
# Internal: DuckDB backend for user polygon spatial join (compute_lumi)
# -----------------------------------------------------------------------------
.compute_lumi_user_poly_duckdb <- function(
  zip_path,
  csv_inside,
  polygon,
  verbose
) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:",
                        config = list(
                          'enable_progress_bar' = FALSE,
                          'enable_print_progress' = FALSE,
                          'print_progress_bar' = FALSE
                        ))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  .duckdb_ensure_extension(con, "zipfs", verbose = verbose)
  .duckdb_ensure_extension(con, "spatial", verbose = verbose)

  zip_norm <- normalizePath(zip_path, winslash = "/", mustWork = TRUE)
  uri <- sprintf("zip://%s/%s", zip_norm, csv_inside)
  uri_sql <- gsub("'", "''", uri)

  # Read all CNEFE points, excluding COD_ESPECIE == 7, keeping 1-6 and 8
  sql_points <- sprintf(
    "
    SELECT
      CAST(LONGITUDE AS DOUBLE) AS lon,
      CAST(LATITUDE  AS DOUBLE) AS lat,
      try_cast(COD_ESPECIE AS INTEGER) AS COD_ESPECIE
    FROM read_csv_auto('%s', delim=';', header=true, strict_mode=false)
    WHERE
      LONGITUDE IS NOT NULL AND LATITUDE IS NOT NULL
      AND try_cast(COD_ESPECIE AS INTEGER) BETWEEN 1 AND 8
      AND try_cast(COD_ESPECIE AS INTEGER) != 7;
  ",
    uri_sql
  )

  cnefe_df <- DBI::dbGetQuery(con, sql_points)
  total_points <- nrow(cnefe_df)

  if (total_points == 0L) {
    return(list(
      counts = dplyr::tibble(
        .poly_row_id = integer(0),
        n_res = integer(0),
        n_tot = integer(0)
      ),
      total_points = 0L,
      points_matched = 0L,
      points_outside = 0L,
      total_n_res = 0L,
      total_n_tot = 0L
    ))
  }

  # Global totals from all municipality points
  total_n_res <- sum(cnefe_df$COD_ESPECIE == 1L, na.rm = TRUE)
  total_n_tot <- nrow(cnefe_df)

  # Convert to sf and perform spatial join
  cnefe_pts <- sf::st_as_sf(
    cnefe_df,
    coords = c("lon", "lat"),
    crs = 4326
  )
  cnefe_pts$.pt_id <- seq_len(nrow(cnefe_pts))

  joined <- sf::st_join(cnefe_pts, polygon[, ".poly_row_id"], join = sf::st_within)

  # Count unique points matched
  unique_pts_matched <- length(unique(joined$.pt_id[!is.na(joined$.poly_row_id)]))
  points_outside <- total_points - unique_pts_matched

  # Aggregate n_res and n_tot per polygon
  counts <- joined |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(.data$.poly_row_id)) |>
    dplyr::group_by(.data$.poly_row_id) |>
    dplyr::summarise(
      n_res = as.integer(sum(.data$COD_ESPECIE == 1L, na.rm = TRUE)),
      n_tot = as.integer(dplyr::n()),
      .groups = "drop"
    )

  list(
    counts = counts,
    total_points = total_points,
    points_matched = unique_pts_matched,
    points_outside = points_outside,
    total_n_res = as.integer(total_n_res),
    total_n_tot = as.integer(total_n_tot)
  )
}


# -----------------------------------------------------------------------------
# Internal: R backend for user polygon spatial join (compute_lumi)
# -----------------------------------------------------------------------------
.compute_lumi_user_poly_r <- function(
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
      .data$COD_ESPECIE %in% 1L:8L,
      .data$COD_ESPECIE != 7L
    )

  total_points <- nrow(df)

  if (total_points == 0L) {
    return(list(
      counts = dplyr::tibble(
        .poly_row_id = integer(0),
        n_res = integer(0),
        n_tot = integer(0)
      ),
      total_points = 0L,
      points_matched = 0L,
      points_outside = 0L,
      total_n_res = 0L,
      total_n_tot = 0L
    ))
  }

  # Global totals from all municipality points
  total_n_res <- as.integer(sum(df$COD_ESPECIE == 1L, na.rm = TRUE))
  total_n_tot <- as.integer(nrow(df))

  # Convert to sf points and add point ID
  cnefe_pts <- sf::st_as_sf(
    df,
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4326
  )
  cnefe_pts$.pt_id <- seq_len(nrow(cnefe_pts))

  # Spatial join
  joined <- sf::st_join(cnefe_pts, polygon[, ".poly_row_id"], join = sf::st_within)

  # Count unique points matched
  unique_pts_matched <- length(unique(joined$.pt_id[!is.na(joined$.poly_row_id)]))
  points_outside <- total_points - unique_pts_matched

  # Aggregate n_res and n_tot per polygon
  counts <- joined |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(.data$.poly_row_id)) |>
    dplyr::group_by(.data$.poly_row_id) |>
    dplyr::summarise(
      n_res = as.integer(sum(.data$COD_ESPECIE == 1L, na.rm = TRUE)),
      n_tot = as.integer(dplyr::n()),
      .groups = "drop"
    )

  list(
    counts = counts,
    total_points = total_points,
    points_matched = unique_pts_matched,
    points_outside = points_outside,
    total_n_res = total_n_res,
    total_n_tot = total_n_tot
  )
}
