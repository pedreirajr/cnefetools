#' Count CNEFE address species on an H3 grid
#'
#' @description
#' `hex_cnefe_counts()` reads CNEFE records for a given municipality, assigns
#' each address point to an H3 cell, builds the full H3 grid over the municipal
#' boundary (year fixed at 2024), and returns per-hexagon counts of `COD_ESPECIE`
#' as `addr_type1` to `addr_type8`.
#'
#' @param code_muni Integer. Seven-digit IBGE municipality code.
#' @param h3_resolution Integer. H3 grid resolution (default: 9).
#' @param verbose Logical; if `TRUE`, prints messages and timing information.
#' @param backend Character. `"duckdb"` (default) uses DuckDB H3 extension to
#'   aggregate directly from the cached ZIP. `"r"` uses h3jsr in R.
#'
#' @return An [`sf::sf`] object with CRS 4326 containing:
#' - `id_hex`: H3 cell identifier
#' - `addr_type1` ... `addr_type8`: counts per type
#' - `geometry`: hexagon geometry
#'
#' @export
hex_cnefe_counts <- function(code_muni,
                             h3_resolution = 9,
                             verbose       = TRUE,
                             backend       = c("duckdb", "r")) {

  backend   <- match.arg(backend)
  code_muni <- .normalize_code_muni(code_muni)

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
  # Step 1/3: Ensure ZIP exists in cache and find CSV inside
  # ---------------------------------------------------------------------------
  if (verbose) message("Step 1/3: ensuring ZIP and inspecting archive...")
  t1 <- Sys.time()

  zip_info <- .cnefe_ensure_zip(
    code_muni     = code_muni,
    index         = cnefe_index_2022,
    cache         = TRUE,
    verbose       = verbose,
    base_timeout  = 300L,
    timeouts      = c(300L, 600L, 1800L)
  )
  zip_path <- zip_info$zip_path

  arch_info  <- archive::archive(zip_path)
  csv_inside <- .cnefe_first_csv_in_archive(arch_info)

  log_step_time("Step 1/3 (ZIP ready)", t1)

  # ---------------------------------------------------------------------------
  # Step 2/3: Build full H3 grid over municipality boundary
  # ---------------------------------------------------------------------------
  if (verbose) message("Step 2/3: building full H3 grid over municipality boundary...")
  t2 <- Sys.time()

  hex_grid <- build_h3_grid(
    h3_resolution = h3_resolution,
    code_muni     = code_muni
  )

  log_step_time("Step 2/3 (H3 grid)", t2)

  # ---------------------------------------------------------------------------
  # Step 3/3: Count address species per hexagon
  # ---------------------------------------------------------------------------
  if (verbose) message("Step 3/3: counting address species per hexagon...")
  t3 <- Sys.time()

  counts_long <- NULL

  # Helper: ensure DuckDB extension without reinstalling every time
  duckdb_ensure_extension <- function(con, ext, repo = "community", verbose = TRUE) {
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
        if (verbose) message("DuckDB: extension '", ext, "' already loaded.")
        return(invisible(TRUE))
      }
      if (isTRUE(info$installed[[1]])) {
        if (verbose) message("DuckDB: loading extension '", ext, "'...")
        DBI::dbExecute(con, sprintf("LOAD %s;", ext))
        return(invisible(TRUE))
      }
    }

    ok_load <- tryCatch({
      if (verbose) message("DuckDB: trying to LOAD extension '", ext, "'...")
      DBI::dbExecute(con, sprintf("LOAD %s;", ext))
      TRUE
    }, error = function(e) FALSE)

    if (ok_load) return(invisible(TRUE))

    if (verbose) message("DuckDB: installing extension '", ext, "' from ", repo, "...")
    DBI::dbExecute(con, sprintf("INSTALL %s FROM %s;", ext, repo))
    if (verbose) message("DuckDB: loading extension '", ext, "'...")
    DBI::dbExecute(con, sprintf("LOAD %s;", ext))

    invisible(TRUE)
  }

  if (identical(backend, "duckdb")) {

    # DuckDB is optional: fallback to R if not installed
    ok_duck <- TRUE
    ok_duck <- ok_duck && rlang::is_installed("DBI")
    ok_duck <- ok_duck && rlang::is_installed("duckdb")

    if (!ok_duck) {
      if (verbose) {
        message("DuckDB backend requested but DBI/duckdb not installed. Falling back to backend = 'r'.")
      }
      backend <- "r"
    }
  }

  if (identical(backend, "duckdb")) {

    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

    duckdb_ensure_extension(con, "zipfs", verbose = verbose)
    duckdb_ensure_extension(con, "h3",    verbose = verbose)

    zip_norm <- normalizePath(zip_path, winslash = "/", mustWork = TRUE)
    uri      <- sprintf("zip://%s/%s", zip_norm, csv_inside)
    uri_sql  <- gsub("'", "''", uri)

    sql <- sprintf("
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
    ", uri_sql, as.integer(h3_resolution))

    counts_long <- DBI::dbGetQuery(con, sql) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        id_hex      = as.character(.data$id_hex),
        COD_ESPECIE = as.integer(.data$COD_ESPECIE),
        n           = as.integer(.data$n)
      )

  } else {

    # Backend "r" (slower): read Arrow, compute H3 in R
    tab <- read_cnefe(
      code_muni = code_muni,
      output    = "arrow",
      cache     = TRUE,
      verbose   = verbose
    )

    df <- as.data.frame(tab) %>%
      dplyr::transmute(
        LONGITUDE   = as.numeric(.data$LONGITUDE),
        LATITUDE    = as.numeric(.data$LATITUDE),
        COD_ESPECIE = as.integer(.data$COD_ESPECIE)
      ) %>%
      dplyr::filter(
        !is.na(.data$LONGITUDE),
        !is.na(.data$LATITUDE),
        !is.na(.data$COD_ESPECIE),
        .data$COD_ESPECIE %in% 1L:8L
      )

    if (nrow(df) > 0L) {
      coords <- df %>%
        dplyr::transmute(lon = .data$LONGITUDE, lat = .data$LATITUDE)

      id_hex <- suppressMessages(
        h3jsr::point_to_cell(coords, res = h3_resolution, simple = TRUE)
      )

      counts_long <- df %>%
        dplyr::mutate(id_hex = as.character(id_hex)) %>%
        dplyr::filter(!is.na(.data$id_hex)) %>%
        dplyr::count(.data$id_hex, .data$COD_ESPECIE, name = "n") %>%
        dplyr::mutate(
          COD_ESPECIE = as.integer(.data$COD_ESPECIE),
          n           = as.integer(.data$n)
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
    for (k in 1:8) out[[paste0("addr_type", k)]] <- 0L

  } else {

    counts_wide <- counts_long %>%
      tidyr::pivot_wider(
        names_from   = .data$COD_ESPECIE,
        values_from  = .data$n,
        names_prefix = "addr_type",
        values_fill  = 0
      )

    for (k in 1:8) {
      nm <- paste0("addr_type", k)
      if (!nm %in% names(counts_wide)) counts_wide[[nm]] <- 0L
    }

    counts_wide <- counts_wide %>%
      dplyr::select("id_hex", dplyr::all_of(paste0("addr_type", 1:8))) %>%
      dplyr::mutate(
        dplyr::across(dplyr::starts_with("addr_type"), ~ as.integer(.x))
      )

    out <- hex_grid %>%
      dplyr::left_join(counts_wide, by = "id_hex") %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("addr_type"),
          ~ dplyr::coalesce(as.integer(.x), 0L)
        )
      )
  }

  # Final safety: force integer and non-negative
  out <- out %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("addr_type"),
        ~ pmax(as.integer(.x), 0L)
      )
    )

  log_step_time("Step 3/3 (count computation)", t3)

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
