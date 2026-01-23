# Internal helper functions for cnefetools (not exported)

## Theme: Year and index selection

#' Get the CNEFE index for a given year
#'
#' @param year Integer. The CNEFE data year.
#' @return A data.frame with municipality codes and ZIP URLs.
#' @keywords internal
#' @noRd
.get_cnefe_index <- function(year) {
  year <- as.integer(year)

  # TODO: When new CNEFE versions become available (2030+), add cases here:
  # if (year == 2030L) return(cnefe_index_2030)
  # if (year == 2040L) return(cnefe_index_2040)

  if (year == 2022) {
    return(cnefe_index_2022)
  }

  cli::cli_abort(c(
    "CNEFE data for year {.val {year}} is not available.",
    "i" = "Currently supported years: {.val {2022}}."
    # TODO: Update this message when new years are added
  ))
}

#' Validate and normalize the year argument
#'
#' @param year Integer. The year to validate.
#' @return Integer. The validated year.
#' @keywords internal
#' @noRd
.validate_year <- function(year) {

  if (length(year) != 1L) {
    cli::cli_abort("{.arg year} must be a single value.")
  }

  year <- as.integer(year)

  if (is.na(year)) {
    cli::cli_abort("{.arg year} must be a valid integer.")
  }

  # TODO: Update valid_years when new CNEFE versions become available (e.g., 2030, 2040)
  valid_years <- c(2022)

  if (!year %in% valid_years) {
    cli::cli_abort(c(
      "CNEFE data for year {.val {year}} is not available.",
      "i" = "Currently supported years: {.val {valid_years}}."
    ))
  }

  year
}


## Theme: Input validation

#' @keywords internal
#' @noRd
.normalize_code_muni <- function(code_muni) {
  # 1. Length validation
  if (length(code_muni) != 1L) {
    cli::cli_abort("{.arg code_muni} must be a single value.")
  }

  # 2. Safe conversion and initial cleaning
  # Direct coercion to string simplifies pattern validation (regex)
  code_str <- trimws(as.character(code_muni))

  # 3. Pattern validation (7 numeric digits)
  # IBGE uses 7-digit codes; we check this before converting to integer
  if (!grepl("^\\d{7}$", code_str)) {
    cli::cli_abort(c(
      "{.arg code_muni} must be coercible to a valid 7-digit IBGE code.",
      "i" = "Value received: {.val {code_muni}}",
      "i" = "Example: {.val 2927408} (Salvador)"
    ))
  }

  # 4. Final conversion
  code_int <- as.integer(code_str)

  # 5. Final integrity check (prevents unexpected NAs)
  if (is.na(code_int)) {
    cli::cli_abort("Failed to convert {.arg code_muni} to an integer.")
  }

  code_int
}


## Theme: Cache management

#' @keywords internal
#' @noRd
.cnefe_cache_dir <- function() {
  cache_dir <- tools::R_user_dir("cnefetools", which = "cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  cache_dir
}


# Theme: Download and file handling

#' @keywords internal
#' @noRd
.cnefe_ensure_zip <- function(
  code_muni,
  index,
  cache = TRUE,
  verbose = TRUE,
  retry_timeouts = c(300L, 600L, 1800L)
) {
  info <- index[index$code_muni == code_muni, , drop = FALSE]
  if (nrow(info) == 0) {
    rlang::abort(
      sprintf(
        "Municipality code not found in internal CNEFE index: %s",
        code_muni
      )
    )
  }

  url <- info$zip_url[1]
  if (is.na(url) || !nzchar(url)) {
    rlang::abort(
      sprintf(
        "Missing `zip_url` in internal index for municipality: %s",
        code_muni
      )
    )
  }

  ext <- tools::file_ext(url)
  if (!nzchar(ext)) {
    ext <- "zip"
  }

  if (isTRUE(cache)) {
    cache_dir <- .cnefe_cache_dir()
    zip_path <- file.path(cache_dir, basename(url))
    cleanup_zip <- FALSE
  } else {
    zip_path <- tempfile(fileext = paste0(".", ext))
    cleanup_zip <- TRUE
  }

  # If cached file exists, validate it; if invalid, delete and re-download
  if (isTRUE(cache) && file.exists(zip_path)) {
    valid <- tryCatch(
      {
        info <- utils::unzip(zip_path, list = TRUE)

        any(grepl("\\.csv$", info$Name, ignore.case = TRUE))
      },
      error = function(e) FALSE
    )

    if (!valid) {
      if (verbose) {
        message("Cached ZIP appears corrupted. Deleting it...")
      }
      unlink(zip_path)
    }
  }

  # Download if needed
  if (!file.exists(zip_path)) {
    .cnefe_download_zip_with_retry(
      url = url,
      destfile = zip_path,
      verbose = verbose,
      retry_timeouts = retry_timeouts
    )
  } else if (verbose) {
    cli::cli_alert_info("Using cached file: {zip_path}")
  }

  list(
    zip_path = zip_path,
    cleanup_zip = cleanup_zip,
    url = url
  )
}

#' @keywords internal
#' @noRd
.cnefe_download_zip_with_retry <- function(
    url,
    destfile,
    retry_timeouts = c(300L, 600L, 1800L),
    verbose = TRUE
) {
  # argument checks
  checkmate::assert_string(url, min.chars = 1)
  checkmate::assert_path_for_output(destfile, overwrite = TRUE)
  checkmate::assert_logical(verbose, len = 1)

  if (!grepl("^https?://", url)) {
    rlang::abort(
      "`url` must be an HTTP or HTTPS URL."
    )
  }

  retry_timeouts <- unique(as.integer(retry_timeouts))
  retry_timeouts <- retry_timeouts[!is.na(retry_timeouts) & retry_timeouts > 0L]

  if (length(retry_timeouts) == 0L) {
    rlang::abort(
      "`retry_timeouts` must contain at least one positive value."
    )
  }

  fs::dir_create(fs::path_dir(destfile))

  last_err <- NULL

  for (t in retry_timeouts) {
    tmp <- tempfile(fileext = ".zip")

    if (isTRUE(verbose)) {
      message(
        "Downloading ZIP (timeout = ",
        t,
        "s): ",
        url
      )
    }

    res <- tryCatch(
      {
        req <- httr2::request(url) |>
          httr2::req_timeout(t)

        httr2::req_perform(req, path = tmp)

        if (!fs::file_exists(tmp) || fs::file_size(tmp) == 0) {
          rlang::abort("Downloaded file is empty.")
        }

        # ZIP integrity check
        utils::unzip(tmp, list = TRUE)

        if (fs::file_exists(destfile)) {
          fs::file_delete(destfile)
        }

        ok_copy <- file.copy(tmp, destfile, overwrite = TRUE)

        if (!isTRUE(ok_copy)) {
          rlang::abort("Failed to copy downloaded ZIP to destination.")
        }

        list(ok = TRUE, err = NULL)
      },
      error = function(e) {
        if (inherits(e, "interrupt")) rlang::interrupt()
        list(ok = FALSE, err = e)
      },
      finally = {
        if (fs::file_exists(tmp)) fs::file_delete(tmp)
      }
    )

    if (isTRUE(res$ok)) {
      return(invisible(destfile))
    }

    last_err <- res$err

    if (isTRUE(verbose) && !is.null(last_err)) {
      message(
        "Download attempt failed: ",
        conditionMessage(last_err)
      )
    }
  }

  rlang::abort(
    "Failed to download ZIP after multiple attempts.",
    parent = last_err
  )
}

#' @keywords internal
#' @noRd
.cnefe_first_csv_in_zip <- function(zip_path) {

  checkmate::assert_file_exists(zip_path)

  info <- utils::unzip(zip_path, list = TRUE)

  csv <- info$Name[
    grepl("\\.csv$", info$Name, ignore.case = TRUE)
  ]

  if (length(csv) == 0L) {
    rlang::abort("No .csv file found inside CNEFE ZIP.")
  }

  if (length(csv) > 1L) {
    rlang::abort(
      "Multiple CSV files found inside CNEFE ZIP. This is unexpected."
    )
  }

  csv[[1L]]
}


# Theme: Spatial boundaries (geobr)

#' @keywords internal
#' @noRd
.read_muni_boundary_2024 <- function(code_muni) {
  # 1. Dependency check with specific reason
  rlang::check_installed(
    "geobr",
    reason = "to read municipality boundaries (needed to build the H3 grid)."
  )

  # 2. Input normalization
  code_muni <- .normalize_code_muni(code_muni)

  # 3. Argument construction
  args <- list(
    code_muni = code_muni,
    year = 2024L,
    simplified = TRUE,
    showProgress = FALSE,
    cache = TRUE
  )

  # Conditionally add arguments based on installed geobr version
  # This handles API changes in geobr without breaking older versions
  geobr_args <- names(formals(geobr::read_municipality))
  if ("keep_areas_operacionais" %in% geobr_args) {
    args$keep_areas_operacionais <- FALSE
  }

  # 4. Safe execution with error handling
  muni <- tryCatch(
    {
      suppressMessages(
        suppressWarnings(
          rlang::exec(geobr::read_municipality, !!!args)
        )
      )
    },
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Could not read municipality boundary via {.pkg geobr} for 2024.",
          "i" = "Municipality code: {.val {code_muni}}"
        ),
        parent = cnd
      )
    }
  )

  # 5. Output Validation
  # Ensure we actually got a valid sf object back
  if (!inherits(muni, "sf") || nrow(muni) == 0L) {
    cli::cli_abort(c(
      "{.pkg geobr} returned an empty or invalid object.",
      "i" = "Try updating {.pkg geobr} with {.code remotes::install_github('ipeaGIT/geobr')}."
    ))
  }

  muni
}

## Theme: Census tract (SC) Parquet assets (GitHub Release)

#' @keywords internal
#' @noRd
.sc_assets_tag <- function() {
  # Advanced users can override via options() without changing the API
  getOption("cnefetools.sc_assets_tag", "sc-assets-v1")
}


#' @keywords internal
#' @noRd
.sc_asset_filename <- function(uf) {
  uf <- as.character(uf)
  uf <- trimws(uf)
  if (nchar(uf) == 1L) {
    uf <- paste0("0", uf)
  }
  if (!grepl("^[0-9]{2}$", uf)) {
    rlang::abort("`uf` must be a two-digit string like '29'.")
  }
  sprintf("sc_%s.parquet", uf)
}

#' @keywords internal
#' @noRd
.uf_from_code_muni <- function(code_muni) {
  code_muni <- .normalize_code_muni(code_muni)
  substr(sprintf("%07d", code_muni), 1L, 2L)
}

#' @keywords internal
#' @noRd
.sc_cache_dir <- function() {
  dir <- file.path(.cnefe_cache_dir(), "sc_assets")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

#' @keywords internal
#' @noRd
.sc_asset_local_path <- function(uf) {
  file.path(.sc_cache_dir(), .sc_asset_filename(uf))
}


#' @keywords internal
#' @noRd
.validate_sc_parquet <- function(path) {
  # Cheap validation: open Parquet metadata and check required fields
  tryCatch(
    {
      reader <- arrow::ParquetFileReader$create(path)
      schema <- reader$GetSchema()
      fields <- schema$names
      all(c("code_tract", "geom_wkb") %in% fields)
    },
    error = function(e) {
      FALSE
    }
  )
}


#' @keywords internal
#' @noRd
.sc_ensure_parquet_uf <- function(
    uf,
    cache = TRUE,
    verbose = TRUE,
    retry_timeouts = c(300L, 600L, 1800L)  # Ignorado, mantido para compatibilidade
) {

  uf <- as.character(uf)

  uf <- trimws(uf)

  if (nchar(uf) == 1L) {
    uf <- paste0("0", uf)
  }

  if (!grepl("^[0-9]{2}$", uf)) {
    rlang::abort("`uf` must be a two-digit string like '29'.")
  }

  # Usa piggyback para download dos assets do SC
  .sc_download_with_piggyback(uf = uf, cache = cache, verbose = verbose)
}

#' Download census tract parquet from GitHub releases using piggyback
#' @keywords internal
#' @noRd
.sc_download_with_piggyback <- function(
    uf,
    cache = TRUE,
    verbose = TRUE
) {

  rlang::check_installed(
    "piggyback",
    reason = "to download census tract data from GitHub releases."
  )

  filename <- .sc_asset_filename(uf)
  tag <- .sc_assets_tag()
  repo <- "pedreirajr/cnefetools"

  if (isTRUE(cache)) {
    destfile <- .sc_asset_local_path(uf)

    # Se já existe e é válido, retorna
    if (file.exists(destfile) && .validate_sc_parquet(destfile)) {
      if (verbose) {
        cli::cli_alert_info("Using cached file: {.file {basename(destfile)}}")
      }
      return(destfile)
    }

    # Download com piggyback
    if (verbose) {
      cli::cli_progress_step("Downloading {.file {filename}} from GitHub release")
    }

    tryCatch({
      piggyback::pb_download(
        file = filename,
        repo = repo,
        tag = tag,
        dest = dirname(destfile),
        overwrite = TRUE,
        show_progress = verbose
      )

      if (verbose) {
        cli::cli_progress_done()
      }

      # Valida depois do download
      if (!.validate_sc_parquet(destfile)) {
        cli::cli_abort("Downloaded file failed validation: {.file {filename}}")
      }

      return(destfile)
    }, error = function(e) {
      if (verbose) {
        cli::cli_progress_done()
      }
      cli::cli_abort(
        c(
          "Failed to download {.file {filename}} from GitHub release.",
          "i" = "Error: {conditionMessage(e)}"
        ),
        parent = e
      )
    })
  }

  # No-cache mode: baixa para temp
  tmp_dir <- tempdir()

  if (verbose) {
    cli::cli_progress_step("Downloading {.file {filename}} from GitHub release")
  }

  tryCatch({
    piggyback::pb_download(
      file = filename,
      repo = repo,
      tag = tag,
      dest = tmp_dir,
      overwrite = TRUE,
      show_progress = verbose
    )

    if (verbose) {
      cli::cli_progress_done()
    }

    # O arquivo baixado está em tmp_dir/filename
    downloaded_file <- file.path(tmp_dir, filename)

    if (!file.exists(downloaded_file)) {
      cli::cli_abort("Downloaded file not found at expected location: {.path {downloaded_file}}")
    }

    return(downloaded_file)
  }, error = function(e) {
    if (verbose) {
      cli::cli_progress_done()
    }
    cli::cli_abort(
      c(
        "Failed to download {.file {filename}} from GitHub release.",
        "i" = "Error: {conditionMessage(e)}"
      ),
      parent = e
    )
  })
}

#' @keywords internal
#' @noRd
.sc_create_views_in_duckdb <- function(
  con,
  code_muni,
  cache = TRUE,
  verbose = TRUE
) {

  code_muni <- .normalize_code_muni(code_muni)
  uf <- .uf_from_code_muni(code_muni)

  # Ensure UF parquet is available locally
  parquet_path <- .sc_ensure_parquet_uf(uf, cache = cache, verbose = verbose)
  parquet_path <- normalizePath(parquet_path, winslash = "/", mustWork = TRUE)

  # 7-digit municipality prefix inside 15-digit tract code
  muni_prefix <- sprintf("%07d", code_muni)

  suppressMessages({
  # View with tract attributes + geometry as DuckDB GEOMETRY
    DBI::dbExecute(
      con,
      sprintf(
        "
      CREATE OR REPLACE VIEW sc_uf_raw AS
      SELECT *
      FROM read_parquet('%s');
    ",
        parquet_path
      )
    )

    DBI::dbExecute(
      con,
      sprintf(
        "
      CREATE OR REPLACE VIEW sc_muni AS
      SELECT
        *,
        ST_GeomFromWKB(geom_wkb) AS geom
      FROM sc_uf_raw
      WHERE substr(code_tract, 1, 7) = '%s';
    ",
        muni_prefix
      )
    )
  })

  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.cnefe_create_points_view_in_duckdb <- function(
  con,
  code_muni,
  index = cnefe_index_2022,
  cache = TRUE,
  verbose = TRUE
) {
  code_muni <- .normalize_code_muni(code_muni)

  # Ensure zipfs is available (community extension)
  ok_zipfs <- tryCatch(
    {
      suppressMessages(DBI::dbExecute(con, "LOAD zipfs;"))
      TRUE
    },
    error = function(e) FALSE
  )

  if (!ok_zipfs) {
    suppressMessages({
      DBI::dbExecute(con, "INSTALL zipfs FROM community;")
      DBI::dbExecute(con, "LOAD zipfs;")
    })
  }

  # Ensure the municipality ZIP exists locally (reuses your existing cache logic)
  zip_info <- .cnefe_ensure_zip(
    code_muni = code_muni,
    index = index,
    cache = cache,
    verbose = verbose
  )

  zip_path <- zip_info$zip_path
  zip_norm <- normalizePath(zip_path, winslash = "/", mustWork = TRUE)

  csv_inside <- .cnefe_first_csv_in_zip(zip_norm)

  # DuckDB zipfs URI: zip://<zipfile>/<file_inside_zip>
  uri <- sprintf("zip://%s/%s", zip_norm, csv_inside)
  uri_sql <- gsub("'", "''", uri)

  suppressMessages({
    DBI::dbExecute(
      con,
      sprintf(
        "
      CREATE OR REPLACE VIEW cnefe_raw AS
      SELECT
        CAST(COD_UNICO_ENDERECO AS VARCHAR) AS COD_UNICO_ENDERECO,
        CAST(COD_SETOR         AS VARCHAR) AS COD_SETOR,
        try_cast(COD_ESPECIE   AS INTEGER) AS COD_ESPECIE,
        CAST(LONGITUDE         AS DOUBLE)  AS lon,
        CAST(LATITUDE          AS DOUBLE)  AS lat
      FROM read_csv_auto('%s', delim=';', header=true, strict_mode=false);
    ",
        uri_sql
      )
    )

    DBI::dbExecute(
      con,
      "
      CREATE OR REPLACE VIEW cnefe_pts AS
      SELECT
        COD_UNICO_ENDERECO,
        COD_SETOR,
        COD_ESPECIE,
        lon,
        lat,
        ST_Point(lon, lat) AS geom
      FROM cnefe_raw
      WHERE
        COD_ESPECIE IN (1, 2)
        AND lon IS NOT NULL
        AND lat IS NOT NULL;
    "
    )
  })

  # Clean up temp ZIP if cache = FALSE
  if (isTRUE(zip_info$cleanup_zip)) {
    on.exit(unlink(zip_path), add = TRUE)
  }

  invisible(TRUE)
}
