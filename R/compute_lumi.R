#' Compute land-use mix indicators on an H3 grid
#'
#' @description
#' `compute_lumi()` reads CNEFE records for a given municipality,
#' assigns each address point to an H3 cell, and computes land-use mix
#' indices per hexagon (EI, HHI, adapted HHI, and BGBI), following the
#' methodology proposed in Pedreira Jr. et al. (2025).
#'
#' @param code_muni Integer. Seven-digit IBGE municipality code.
#' @param year Integer. The CNEFE data year. Currently only 2022 is supported.
#'   Defaults to 2022.
#' @param h3_resolution Integer. H3 grid resolution (default: 9).
#' @param verbose Logical; if `TRUE`, prints messages and timing information.
#' @param backend Character. `"duckdb"` (default) uses DuckDB + H3 extension
#'   reading directly from the cached ZIP. `"r"` computes H3 in R using h3jsr.
#'
#' @return An [`sf::sf`] object with CRS 4326 containing:
#'   - `id_hex`: H3 cell identifier
#'   - `p_res`: share of residential addresses in the hexagon
#'   - `ei`,`bal`, `hhi`, `hhi_adp`, `bgbi`: land-use mix indicators
#'   - `geometry`: hexagon geometry
#'
#' @references
#' Pedreira Jr., J. U.; Louro, T. V.; Assis, L. B. M.; Brito, P. L.
#' Measuring land use mix with address-level census data (2025).
#' *engrXiv*. https://engrxiv.org/preprint/view/5975
#'
#' @export
compute_lumi <- function(
  code_muni,
  year = 2022,
  h3_resolution = 9,
  verbose = TRUE,
  backend = c("duckdb", "r")
) {
  backend <- match.arg(backend)
  code_muni <- .normalize_code_muni(code_muni)
  year <- .validate_year(year)

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
    message(sprintf("Processing %s (code %s)...", city_name, code_muni))
  }

  timings <- list()
  log_step_time <- function(step_name, t_start) {
    dt <- difftime(Sys.time(), t_start, units = "secs")
    timings[[step_name]] <<- dt
    if (verbose) {
      message(sprintf("%s completed in %.2f s.", step_name, as.numeric(dt)))
    }
  }

  # We will return sf hexagons
  rlang::check_installed(
    "sf",
    reason = "to return an sf grid in `compute_lumi()`."
  )

  # ---------------------------------------------------------------------------
  # Step 1/3: Ensure ZIP and find CSV inside
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
  # Step 2/3: Aggregate counts per hex (n_res, n_tot)
  # ---------------------------------------------------------------------------
  if (verbose) {
    message("Step 2/3: aggregating CNEFE counts per H3 cell...")
  }
  t2 <- Sys.time()

  # local helper (same spirit as in hex_cnefe_counts)
  duckdb_ensure_extension <- function(
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

  counts_hex <- NULL

  if (identical(backend, "duckdb")) {
    # DBI/duckdb are now Imports, so we no longer perform silent fallbacks.
    rlang::check_installed(
      "DBI",
      reason = "to use backend = 'duckdb' in `compute_lumi()`."
    )
    rlang::check_installed(
      "duckdb",
      reason = "to use backend = 'duckdb' in `compute_lumi()`."
    )
  }

  if (identical(backend, "duckdb")) {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

    duckdb_ensure_extension(con, "zipfs", verbose = verbose)
    duckdb_ensure_extension(con, "h3", verbose = verbose)

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

  log_step_time("Step 2/3 (counts per hex)", t2)

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
    message("Step 3/3: building grid and computing LUMI indices...")
  }
  t3 <- Sys.time()

  hex_grid <- build_h3_grid(
    h3_resolution = h3_resolution,
    id_hex = counts_hex$id_hex
  )

  # Global city residential proportion P (exclude COD_ESPECIE == 7 already)
  P <- sum(counts_hex$n_res, na.rm = TRUE) / sum(counts_hex$n_tot, na.rm = TRUE)

  # Balance index (BAL)
  bal_fun <- function(p, P) {
    num <- abs(p - (P / (1 - P)) * (1 - p))
    den <- p + (P / (1 - P)) * (1 - p)
    ifelse(is.na(num) | is.na(den), NA_real_, 1 - num / den)
  }

  # BGBI function
  bgbi_fun <- function(p, P) {
    num <- (2 * p - 1) - (2 * P - 1)
    den <- 1 - (2 * p - 1) * (2 * P - 1)
    den[den == 0] <- NA_real_
    ifelse(is.na(num) | is.na(den), NA_real_, num / den)
  }

  # numerically safe entropy term: treat 0*log(0) as 0
  safe_plogp <- function(x) {
    ifelse(is.na(x) | x <= 0, 0, x * log(x))
  }

  out <- hex_grid |>
    dplyr::left_join(counts_hex, by = "id_hex") |>
    dplyr::mutate(
      n_res = dplyr::coalesce(as.integer(.data$n_res), 0L),
      n_tot = dplyr::coalesce(as.integer(.data$n_tot), 0L),

      p_res = dplyr::if_else(
        .data$n_tot > 0,
        .data$n_res / .data$n_tot,
        NA_real_
      ),
      q_rest = dplyr::if_else(!is.na(.data$p_res), 1 - .data$p_res, NA_real_),

      # EI (k=2)
      ei = dplyr::if_else(
        !is.na(.data$p_res),
        -(safe_plogp(.data$p_res) + safe_plogp(.data$q_rest)) / log(2),
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
        bal_fun(.data$p_res, P),
        NA_real_
      ),

      # scaled HHI (min=0.5 for k=2)
      hhi_sc = dplyr::if_else(
        !is.na(.data$hhi),
        (.data$hhi - 0.5) / (1 - 0.5),
        NA_real_
      ),

      hhi_adp = dplyr::if_else(
        !is.na(.data$p_res) & !is.na(.data$hhi_sc),
        sign(.data$p_res - .data$q_rest) * .data$hhi_sc,
        NA_real_
      ),

      bgbi = dplyr::if_else(
        !is.na(.data$p_res),
        bgbi_fun(.data$p_res, P),
        NA_real_
      )
    ) |>
    dplyr::select(
      id_hex,
      p_res,
      ei,
      hhi,
      bal,
      hhi_adp,
      bgbi,
      geometry
    )

  log_step_time("Step 3/3 (indices)", t3)

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
