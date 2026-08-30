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

#' Resolve the cache directory
#'
#' Referee 1 (R1.7) noted that the cache location was hardcoded to
#' `tools::R_user_dir()`, which forces potentially large downloads onto the
#' user's primary partition. The location is now resolvable, from the most
#' specific source to the least:
#'
#' 1. the `cache_dir` argument of the calling function,
#' 2. the `CNEFETOOLS_CACHE_DIR` environment variable,
#' 3. `tools::R_user_dir("cnefetools", which = "cache")`, the previous default.
#'
#' Both mechanisms are offered on purpose. The referee points at `osmextract`,
#' which uses an environment variable, and also observes that an explicit
#' argument is more discoverable for less experienced users. The argument wins
#' so that a single call can be redirected without touching the session.
#'
#' @param cache_dir Optional path. `NULL` falls through to the next source.
#'
#' @return A normalised directory path. The directory is not created here.
#'
#' @keywords internal
#' @noRd
.cnefe_cache_dir <- function(cache_dir = NULL) {
  if (!is.null(cache_dir)) {
    if (!is.character(cache_dir) || length(cache_dir) != 1L || is.na(cache_dir)) {
      cli::cli_abort(c(
        "{.arg cache_dir} must be a single directory path.",
        "i" = "Received: {.val {cache_dir}}"
      ))
    }
    if (!nzchar(cache_dir)) {
      cli::cli_abort("{.arg cache_dir} must not be an empty string.")
    }
    return(path.expand(cache_dir))
  }

  from_env <- Sys.getenv("CNEFETOOLS_CACHE_DIR", unset = "")
  if (nzchar(from_env)) {
    return(path.expand(from_env))
  }

  tools::R_user_dir("cnefetools", which = "cache")
}

# Named vector: two-letter UF abbreviation → numeric IBGE state code
# Official IBGE codes — stable; no runtime dependency needed.
.uf_lookup <- c(
  AC = 12L, AL = 27L, AM = 13L, AP = 16L, BA = 29L,
  CE = 23L, DF = 53L, ES = 32L, GO = 52L, MA = 21L,
  MG = 31L, MS = 50L, MT = 51L, PA = 15L, PB = 25L,
  PE = 26L, PI = 22L, PR = 41L, RJ = 33L, RN = 24L,
  RO = 11L, RR = 14L, RS = 43L, SC = 42L, SE = 28L,
  SP = 35L, TO = 17L
)

#' Resolve a UF identifier to a two-digit integer state code
#'
#' Accepts three input formats:
#' - Two-letter abbreviation: `"BA"` → `29L`
#' - Numeric state code: `29L` → `29L`
#' - Seven-digit municipality code: `2919207` → `29L` (via `.uf_from_code_muni()`)
#'
#' @param uf A UF identifier (character abbreviation, numeric state code, or
#'   7-digit municipality code).
#' @return Integer. Two-digit IBGE state code.
#' @keywords internal
#' @noRd
.resolve_uf <- function(uf) {
  # 7-digit municipality code → extract UF code
  if (is.numeric(uf) && length(uf) == 1L && uf > 99) {
    return(as.integer(.uf_from_code_muni(uf)))
  }
  uf_char <- toupper(trimws(as.character(uf)))
  # Two-letter abbreviation → numeric
  if (nchar(uf_char) == 2L && grepl("^[A-Z]{2}$", uf_char)) {
    code <- .uf_lookup[uf_char]
    if (is.na(code)) {
      cli::cli_abort("Unknown UF abbreviation: {.val {uf_char}}")
    }
    return(code)
  }
  # Numeric string or integer → integer
  code <- suppressWarnings(as.integer(uf_char))
  if (is.na(code)) {
    cli::cli_abort("Cannot resolve UF: {.val {uf}}")
  }
  code
}


# Theme: Download and file handling

#' @keywords internal
#' @noRd
.cnefe_ensure_zip <- function(
  code_muni,
  index,
  cache = TRUE,
  cache_dir = NULL,
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
    cache_dir <- .cnefe_cache_dir(cache_dir)
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
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
    tryCatch(
      .cnefe_download_zip_with_retry(
        url = url,
        destfile = zip_path,
        verbose = verbose,
        retry_timeouts = retry_timeouts
      ),
      # A 404 means the indexed URL no longer resolves, which is the exact
      # failure Referee 1 raised in R1.8a. Try to recover it by scanning the
      # published directory listing before giving up (#92).
      cnefetools_not_found = function(cnd) {
        if (isTRUE(verbose)) {
          cli::cli_alert_info(
            "Indexed URL returned 404. Scanning the IBGE directory listing for a replacement..."
          )
        }

        recovered <- .cnefe_scan_ftp_url(
          code_muni = code_muni,
          known_url = url,
          verbose = verbose
        )

        if (is.null(recovered)) {
          cli::cli_abort(
            c(
              "Could not locate the CNEFE file for municipality {.val {code_muni}}.",
              "i" = "The indexed URL returned 404 and scanning the published directory listing found no replacement.",
              "i" = "The upstream layout has most likely changed in a way {.pkg cnefetools} cannot recover from automatically.",
              "i" = "Please report it at {.url https://github.com/pedreirajr/cnefetools/issues}, quoting the URL below.",
              "i" = "URL: {.url {url}}"
            ),
            parent = cnd,
            class = "cnefetools_not_found"
          )
        }

        .cnefe_download_zip_with_retry(
          url = recovered,
          destfile = zip_path,
          verbose = verbose,
          retry_timeouts = retry_timeouts
        )
      }
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

  # Fail fast and legibly before entering the retry ladder, which can otherwise
  # spend 300 + 600 + 1800 seconds on a URL that will never resolve (R1.8c).
  .cnefe_preflight(url)

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

        # Use fs::file_copy for better Windows compatibility
        fs::file_copy(tmp, destfile, overwrite = TRUE)

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
.read_muni_boundary <- function(code_muni, year = 2022L) {
  # 1. Dependency check with specific reason
  rlang::check_installed(
    "geobr",
    reason = "to read municipality boundaries (needed to build the H3 grid)."
  )

  # 2. Input normalization
  code_muni <- .normalize_code_muni(code_muni)
  year <- .validate_year(year)

  # 3. Argument construction
  # The boundary year follows the CNEFE data year. IBGE geocoded the CNEFE
  # records against the territorial base of that year, and the census tracts of
  # that year nest into the matching municipal mesh, so the grid has to share
  # the same reference frame as the data being aggregated onto it.
  args <- list(
    code_muni = code_muni,
    year = year,
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

  # 4. Isolate the RNG state around the geobr call.
  # geobr (>= 2.0.0) reads boundaries lazily through duckspatial, and dbplyr
  # derives its temporary table name from sample(), i.e. from the global RNG
  # state. Under a fixed seed (e.g. R CMD check examples) repeated calls would
  # generate the same name and collide on a reused DuckDB connection with
  # "Table dbplyr_<...> already exists". Reseed from entropy so each call gets
  # a unique name, then restore the caller's RNG state so we do not disturb
  # their reproducibility.
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(
      assign(".Random.seed", old_seed, envir = .GlobalEnv),
      add = TRUE
    )
  } else {
    on.exit(
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      },
      add = TRUE
    )
  }
  set.seed(NULL)

  # 5. Safe execution with error handling
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
          "Could not read municipality boundary via {.pkg geobr} for {.val {year}}.",
          "i" = "Municipality code: {.val {code_muni}}"
        ),
        parent = cnd
      )
    }
  )

  # 6. Output Validation
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
  getOption("cnefetools.sc_assets_tag", "sc-assets-v2")
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
.sc_cache_dir <- function(cache_dir = NULL) {
  file.path(.cnefe_cache_dir(cache_dir), "sc_assets")
}

#' @keywords internal
#' @noRd
.sc_asset_local_path <- function(uf, cache_dir = NULL) {
  file.path(.sc_cache_dir(cache_dir), .sc_asset_filename(uf))
}


#' @keywords internal
#' @noRd
.validate_sc_parquet <- function(path) {

  # Validation: open Parquet metadata and check required fields
  # Includes v2 variables (pop_ph, pop_ch, race_*) to invalidate old v1 cache
  tryCatch(
    {
      reader <- arrow::ParquetFileReader$create(path)
      schema <- reader$GetSchema()
      fields <- schema$names
      required_fields <- c(
        "code_tract", "geom_wkb",
        "pop_ph", "pop_ch",
        "race_branca", "race_preta", "race_amarela", "race_parda", "race_indigena"
      )
      all(required_fields %in% fields)
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
    cache_dir = NULL,
    verbose = TRUE,
    retry_timeouts = c(300L, 600L, 1800L)  # Ignored, kept for compatibility
) {

  uf <- as.character(uf)

  uf <- trimws(uf)

  if (nchar(uf) == 1L) {
    uf <- paste0("0", uf)
  }

  if (!grepl("^[0-9]{2}$", uf)) {
    rlang::abort("`uf` must be a two-digit string like '29'.")
  }

  # Use piggyback to download the census tract assets
  .sc_download_with_piggyback(uf = uf, cache = cache, cache_dir = cache_dir, verbose = verbose)
}

#' Try to copy file to cache, return FALSE if file is locked
#'
#' On Windows, files may be locked by DuckDB or other processes.
#' This function attempts to copy but returns FALSE instead of erroring
#' if the destination file is locked.
#'
#' @param from Source file path
#' @param to Destination file path
#' @return TRUE if copy succeeded, FALSE if destination is locked
#' @keywords internal
#' @noRd
.try_copy_to_cache <- function(from, to) {
  tryCatch(
    {
      # First try to delete destination if it exists
      if (fs::file_exists(to)) {
        fs::file_delete(to)
      }
      fs::file_copy(from, to, overwrite = TRUE)

      # Verify the copy succeeded
      fs::file_exists(to) && fs::file_size(to) > 0
    },
    error = function(e) {
      # File is locked or other error - return FALSE
      FALSE
    }
  )
}

#' Download census tract parquet from GitHub releases using piggyback
#'
#' This function handles the common Windows issue where cached files are locked
#' by DuckDB or other processes. When the cache file cannot be updated, it falls
#' back to using a temporary file for the current session.
#'
#' Detect a GitHub authentication failure
#'
#' The census tract assets live in public GitHub releases, so no token is
#' needed. But `gh` sends whatever token it can find on every request, resolved
#' from `GITHUB_PAT` / `GITHUB_TOKEN` / `GH_TOKEN` and then from the git
#' credential store. An expired or invalid token there makes GitHub answer 401
#' instead of falling back to anonymous access, which is why this has to be
#' recognised and retried rather than reported.
#'
#' @keywords internal
#' @noRd
.is_github_auth_error <- function(cnd) {
  if (!inherits(cnd, "condition")) {
    return(FALSE)
  }
  msg <- paste(
    conditionMessage(cnd),
    if (!is.null(cnd$parent)) conditionMessage(cnd$parent) else "",
    collapse = " "
  )
  grepl("401|bad credentials|requires authentication", msg, ignore.case = TRUE)
}


#' @keywords internal
#' @noRd
.sc_download_with_piggyback <- function(
    uf,
    cache = TRUE,
    cache_dir = NULL,
    verbose = TRUE
) {

  rlang::check_installed(
    "piggyback",
    reason = "to download census tract data from GitHub releases."
  )

  filename <- .sc_asset_filename(uf)
  tag <- .sc_assets_tag()
  repo <- "pedreirajr/cnefetools"

  # Determine cache destination
  destfile <- NULL
  if (isTRUE(cache)) {
    destfile <- normalizePath(.sc_asset_local_path(uf, cache_dir), winslash = "/", mustWork = FALSE)
    dest_dir <- dirname(destfile)

    # Ensure cache directory exists
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # If file already exists and is valid, return it
    if (file.exists(destfile) && .validate_sc_parquet(destfile)) {
      if (verbose) {
        cli::cli_alert_info("Using cached file: {.file {basename(destfile)}}")
      }
      return(destfile)
    }
  }

  # Download to a unique temp location to avoid conflicts
  tmp_download_dir <- file.path(tempdir(), paste0("sc_download_", Sys.getpid()))
  if (!dir.exists(tmp_download_dir)) {
    dir.create(tmp_download_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (verbose) {
    cli::cli_progress_step("Downloading {.file {filename}} from GitHub release")
  }

  # The first attempt keeps the default token, so users with a valid one still
  # get the authenticated rate limit.
  do_download <- function(token = NULL) {
    args <- list(
      file = filename,
      repo = repo,
      tag = tag,
      dest = tmp_download_dir,
      overwrite = TRUE,
      show_progress = verbose
    )
    if (!is.null(token)) {
      args$.token <- token
    }
    tryCatch(
      {
        do.call(piggyback::pb_download, args)
        list(ok = TRUE, err = NULL)
      },
      error = function(e) list(ok = FALSE, err = e)
    )
  }

  download_result <- do_download()

  # A broken credential in the user environment makes GitHub reject a request
  # that needs no credential at all, since the repo and its releases are public.
  # Retry once anonymously before giving up.
  retried_anonymously <- FALSE
  if (!download_result$ok && .is_github_auth_error(download_result$err)) {
    retried_anonymously <- TRUE
    download_result <- do_download(token = "")
  }

  if (verbose) {
    cli::cli_progress_done()
  }

  if (!download_result$ok) {
    cli::cli_abort(
      c(
        "Failed to download {.file {filename}} from GitHub release.",
        "i" = "Error: {conditionMessage(download_result$err)}",
        if (retried_anonymously) {
          c(
            "!" = "An anonymous retry was attempted and also failed.",
            "i" = "A GitHub token in your environment may be expired or invalid. It can come from {.envvar GITHUB_PAT}, {.envvar GITHUB_TOKEN}, {.envvar GH_TOKEN} or from the git credential store.",
            "i" = "Inspect it with {.run gh::gh_token()} and clear it with {.run gitcreds::gitcreds_delete()}, then restart R."
          )
        }
      ),
      parent = download_result$err
    )
  }

  # File downloaded to temp location
  tmp_file <- file.path(tmp_download_dir, filename)

  if (!file.exists(tmp_file)) {
    cli::cli_abort("Downloaded file not found at expected location: {.path {tmp_file}}")
  }

  # Validate the downloaded file
 if (!.validate_sc_parquet(tmp_file)) {
    cli::cli_abort("Downloaded file failed validation: {.file {filename}}")
  }

  # If cache is disabled, return temp file directly
  if (!isTRUE(cache)) {
    return(tmp_file)
  }

  # Try to copy to cache
  copy_ok <- .try_copy_to_cache(tmp_file, destfile)

  if (copy_ok) {
    # Successfully cached - clean up temp and return cache path
    tryCatch(fs::file_delete(tmp_file), error = function(e) NULL)
    return(destfile)
  }

  # Cache copy failed (file locked) - use temp file for this session
  if (verbose) {
    cli::cli_alert_warning(
      c(
        "Cache file is locked (possibly by another R session or DuckDB).",
        "i" = "Using temporary file for this session.",
        "i" = "Restart R to update the cached file."
      )
    )
  }

  return(tmp_file)
}

#' @keywords internal
#' @noRd
.sc_create_views_in_duckdb <- function(
  con,
  code_muni,
  cache = TRUE,
  cache_dir = NULL,
  verbose = TRUE
) {

  code_muni <- .normalize_code_muni(code_muni)
  uf <- .uf_from_code_muni(code_muni)

  # Ensure UF parquet is available locally
  parquet_path <- .sc_ensure_parquet_uf(uf, cache = cache, cache_dir = cache_dir, verbose = verbose)
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
  cache_dir = NULL,
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
    cache_dir = cache_dir,
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

  # Return zip_info so callers can manage cleanup after materialising the views.
  # Do NOT register on.exit here: cnefe_pts is a lazy VIEW that reads from the
  # ZIP file; if the ZIP were deleted on function exit, any subsequent
  # CREATE TABLE ... AS SELECT * FROM cnefe_pts in the caller would fail.
  invisible(zip_info)
}


## Theme: DuckDB connection and extensions

#' Run an expression while suppressing DuckDB console noise
#'
#' DuckDB writes progress output to stdout and emits startup messages, and both
#' are noise in an interactive session. The previous idiom for silencing them
#' was a nested `capture.output(capture.output(expr, type = "message"),
#' type = "output")`, which also captured the message of any error raised inside
#' `expr`. Failures therefore surfaced with no text at all, which made the
#' DuckDB backend effectively undebuggable (GitHub issue #57).
#'
#' `suppressMessages()` muffles messages while leaving conditions of class
#' `error` untouched, so errors keep propagating with their message intact.
#'
#' @param expr Expression to evaluate. Its value is returned.
#'
#' @keywords internal
#' @noRd
.duckdb_quiet <- function(expr) {
  # `expr` is a promise, so it is forced in the caller's frame. Assignments made
  # inside it therefore land in the caller, which is what the migrated call
  # sites rely on. The local name is dotted to avoid shadowing anything there.
  utils::capture.output(.value <- suppressMessages(expr), type = "output")
  .value
}


#' Open an in-memory DuckDB connection with cleanup registered immediately
#'
#' Both referees noted that the previous pattern registered
#' `on.exit(DBI::dbDisconnect(...))` only after the whole connect-and-load block
#' had completed, so any failure inside it (a failed extension install, a SQL
#' error, a user interrupt) left the connection and its file handles behind.
#'
#' Cleanup is registered here through `withr::defer()` on the caller's frame,
#' immediately after `dbConnect()` returns and before anything that can fail.
#' Referee 1 suggested exactly this pattern. The guard on `DBI::dbIsValid()`
#' prevents a secondary error during cleanup if the connection died earlier.
#'
#' @param extensions Character vector of DuckDB extensions to ensure. Community
#'   extensions by default, `"spatial"` is treated as a core extension.
#' @param spatial Logical. Whether to load duckspatial, installing it into the
#'   connection if the load fails.
#' @param reason Passed to [rlang::check_installed()] to explain the dependency.
#' @param verbose Logical, forwarded to the extension loader.
#' @param .envir Frame to attach the cleanup handler to. Defaults to the caller,
#'   which is what every call site wants.
#'
#' @return A live DuckDB connection.
#'
#' @keywords internal
#' @noRd
.duckdb_connect <- function(
  extensions = character(0),
  spatial = FALSE,
  reason = "to use the DuckDB backend.",
  verbose = TRUE,
  .envir = parent.frame()
) {
  rlang::check_installed("duckdb", reason = reason)

  con <- .duckdb_quiet(
    DBI::dbConnect(
      duckdb::duckdb(),
      dbdir = ":memory:",
      config = list(
        enable_progress_bar = FALSE,
        enable_print_progress = FALSE,
        print_progress_bar = FALSE
      )
    )
  )

  # Registered before anything else can fail. This is the whole point.
  withr::defer(
    {
      if (!is.null(con) && DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con, shutdown = TRUE)
      }
    },
    envir = .envir
  )

  if (isTRUE(spatial)) {
    rlang::check_installed("duckspatial", reason = reason)
    .duckdb_quiet(
      tryCatch(
        duckspatial::ddbs_load(con),
        error = function(e) {
          duckspatial::ddbs_install(con)
          duckspatial::ddbs_load(con)
        }
      )
    )
  }

  for (ext in extensions) {
    .duckdb_quiet(
      .duckdb_ensure_extension(
        con,
        ext,
        repo = if (identical(ext, "spatial")) NULL else "community",
        verbose = verbose
      )
    )
  }

  con
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
  # repo = NULL means core extension (no FROM clause needed)

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
      # if (verbose) {
      #   message("DuckDB: extension '", ext, "' already loaded.")
      # }
      return(invisible(TRUE))
    }
    if (isTRUE(info$installed[[1]])) {
      # if (verbose) {
      #   message("DuckDB: loading extension '", ext, "'...")
      # }
      DBI::dbExecute(con, sprintf("LOAD %s;", ext))
      return(invisible(TRUE))
    }
  }

  ok_load <- tryCatch(
    {
      # if (verbose) {
      #   message("DuckDB: trying to LOAD extension '", ext, "'...")
      # }
      DBI::dbExecute(con, sprintf("LOAD %s;", ext))
      TRUE
    },
    error = function(e) FALSE
  )

  if (ok_load) {
    return(invisible(TRUE))
  }

  # if (verbose) {
  #   message("DuckDB: installing extension '", ext, "' from ", repo, "...")
  # }
  if (is.null(repo)) {
    DBI::dbExecute(con, sprintf("INSTALL %s;", ext))
  } else {
    DBI::dbExecute(con, sprintf("INSTALL %s FROM %s;", ext, repo))
  }
  DBI::dbExecute(con, sprintf("LOAD %s;", ext))

  invisible(TRUE)
}


## Theme: Polygon argument validation

#' Validate a user-supplied polygon argument
#'
#' `cnefe_counts()`, `compute_lumi()` and `tracts_to_polygon()` each carried a
#' near-identical copy of these checks, which Referee 2 flagged as duplication
#' (R2.C1). Consolidating them also fixes GitHub issue #71 for all three
#' functions at once rather than only for `tracts_to_polygon()`.
#'
#' The zero-feature check matters because an empty `sf` object survives every
#' other check here and then fails deep inside the DuckDB step, where
#' `sf::st_union()` on zero rows yields an empty geometry, `sf::st_centroid()`
#' of that yields an empty POINT, and `sf::st_coordinates()` returns a zero-row
#' matrix. The result was `valor ausente onde TRUE/FALSE necessário`, several
#' steps away from the actual cause. Filtering a geobr dataset for a
#' municipality it does not cover produces exactly this input.
#'
#' @param polygon The object to validate.
#' @param crs_output Optional CRS to validate alongside it.
#' @param required_when Optional string naming the condition that makes
#'   `polygon` mandatory, used only to phrase the error. `NULL` means it is
#'   unconditionally required.
#'
#' @return `polygon`, invisibly, so the call can be used inline.
#'
#' @keywords internal
#' @noRd
.validate_polygon_arg <- function(
  polygon,
  crs_output = NULL,
  required_when = NULL
) {
  if (is.null(polygon)) {
    # format_inline() first, otherwise the cli markup inside `required_when`
    # would be pasted in literally rather than expanded.
    head_msg <- if (is.null(required_when)) {
      "{.arg polygon} is required."
    } else {
      paste0(
        "{.arg polygon} is required when ",
        cli::format_inline(required_when),
        "."
      )
    }
    cli::cli_abort(c(
      head_msg,
      "i" = "Provide an {.cls sf} object with polygon geometries."
    ))
  }

  if (!inherits(polygon, "sf")) {
    cli::cli_abort(c(
      "{.arg polygon} must be an {.cls sf} object.",
      "i" = "Received: {.cls {class(polygon)[1]}}"
    ))
  }

  if (nrow(polygon) == 0L) {
    cli::cli_abort(c(
      "{.arg polygon} must contain at least one feature, but has 0 rows.",
      "i" = "Check that your spatial filter returns features before calling this function.",
      "i" = "Filtering a {.pkg geobr} dataset for a municipality it does not cover is a common cause."
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

  invisible(polygon)
}


## Theme: Dasymetric allocation SQL

#' Build the per-point allocation SQL for the dasymetric interpolation
#'
#' `tracts_to_h3()` and `tracts_to_polygon()` carried byte-identical copies of
#' this builder, 90 lines each, which is the largest of the duplications Referee
#' 2 lists under R2.C1. The two functions differ in what they aggregate the
#' allocated points onto, not in how the allocation itself is expressed, so the
#' builder is target-agnostic.
#'
#' The generated expressions assume the aliases used by both callers: `p` for
#' the CNEFE points and `s` for the census tract aggregates, with `s.n_dom_p`
#' and `s.n_dom_c` holding the counts of private and collective dwellings.
#'
#' Allocation rules, kept identical to the documented behaviour:
#' - `pop_ph` and `n_resp` are split across private dwellings only.
#' - `pop_ch` is split across collective dwellings only.
#' - `avg_inc_resp` is assigned, not split, to each private dwelling point.
#' - every other variable goes to private dwellings when the tract has any, and
#'   falls back to collective dwellings when it has none.
#'
#' @param vars Character vector of tract variables to allocate.
#'
#' @return A single SQL string of comma-separated `CASE` expressions, each
#'   aliased as `<var>_pt`.
#'
#' @keywords internal
#' @noRd
.build_alloc_sql <- function(vars) {
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

  paste(alloc_exprs, collapse = ",\n")
}


## Theme: Dasymetric interpolation diagnostics

#' Build the stage 1 diagnostic lines for a dasymetric interpolation
#'
#' `tracts_to_h3()` and `tracts_to_polygon()` carried near-identical copies of
#' this, roughly 200 lines each, listed by Referee 2 under R2.C1. The two had
#' already drifted: one ended the "Tracts with NA totals" line with a full stop
#' and the other did not, and one wrapped every query in `suppressMessages()`
#' while the other did not. That is exactly the divergence the referee warned
#' about, and it is why this is now built once.
#'
#' Stage 1 covers the tracts-to-points half of the interpolation, which is
#' identical for both targets. Stage 2 is left to each caller, since it reports
#' on different things: H3 cell coverage in one case, polygon coverage in the
#' other.
#'
#' Expects the tables both callers create: `sc_muni_tbl`, `sc_muni_w_dom`,
#' `cnefe_sc` and `cnefe_alloc`.
#'
#' @param con A live DuckDB connection.
#' @param vars Character vector of the interpolated variables.
#' @param unmatched_pts Count of CNEFE points that matched no tract.
#' @param total_pts Total CNEFE points considered.
#'
#' @return A character vector of preformatted cli lines, possibly empty.
#'
#' @keywords internal
#' @noRd
.build_interp_diagnostics <- function(con, vars, unmatched_pts, total_pts) {
  q1 <- function(sql, col) .duckdb_quiet(DBI::dbGetQuery(con, sql))[[col]][1]

  warn_lines <- character(0)

  n_tracts <- q1("SELECT COUNT(*) AS n FROM sc_muni_tbl;", "n")

  totals_vars <- setdiff(vars, "avg_inc_resp")

  for (v in totals_vars) {
    total_v <- q1(
      sprintf("SELECT SUM(%s) AS total FROM sc_muni_tbl WHERE %s IS NOT NULL;", v, v),
      "total"
    )
    alloc_v <- q1(
      sprintf("SELECT SUM(%s_pt) AS alloc FROM cnefe_alloc WHERE %s_pt IS NOT NULL;", v, v),
      "alloc"
    )

    total_v <- if (is.null(total_v) || is.na(total_v)) 0 else as.numeric(total_v)
    alloc_v <- if (is.null(alloc_v) || is.na(alloc_v)) 0 else as.numeric(alloc_v)

    # Use threshold >= 0.5 to avoid floating point precision issues
    unalloc <- max(total_v - alloc_v, 0)
    unalloc <- if (unalloc < 0.5) 0 else round(unalloc)
    pct <- if (total_v > 0) 100 * unalloc / total_v else 0

    label <- switch(v,
      "pop_ph" = "population from private households",
      "pop_ch" = "population from collective households",
      v # default: use variable name
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
    eligible_avg <- q1(
      "
      SELECT COUNT(*) AS n
      FROM cnefe_sc p
      JOIN sc_muni_w_dom s USING (code_tract)
      WHERE p.COD_ESPECIE = 1 AND s.n_dom_p > 0;
      ",
      "n"
    )
    assigned_avg <- q1(
      "SELECT COUNT(*) AS n FROM cnefe_alloc WHERE avg_inc_resp_pt IS NOT NULL;",
      "n"
    )

    assigned_pct <- if (eligible_avg > 0) 100 * assigned_avg / eligible_avg else 0
    warn_lines <- c(
      warn_lines,
      cli::format_inline(
        "{.field avg_inc_resp} assigned to {.strong {assigned_avg}} of {.strong {eligible_avg}} eligible points ({.strong {sprintf('%.2f%%', assigned_pct)}} of total points)"
      )
    )

    na_avg_tracts <- q1(
      "SELECT COUNT(*) AS n FROM sc_muni_tbl WHERE avg_inc_resp IS NULL;",
      "n"
    )

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
    unmatched_pct <- if (total_pts > 0) 100 * unmatched_pts / total_pts else 0
    warn_lines <- c(
      warn_lines,
      cli::format_inline(
        "Unmatched CNEFE points (no tract): {.strong {unmatched_pts}} of {.strong {total_pts}} points ({.strong {sprintf('%.2f%%', unmatched_pct)}} of total points)"
      )
    )
  }

  na_totals <- character(0)
  for (v in totals_vars) {
    n_na <- q1(
      sprintf("SELECT COUNT(*) AS n FROM sc_muni_tbl WHERE %s IS NULL;", v),
      "n"
    )
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
        "Tracts with {.strong NA} totals: {paste(na_totals, collapse = '; ')}"
      )
    )
  }

  no_elig <- character(0)
  for (v in totals_vars) {
    sql <- if (v %in% c("pop_ph", "n_resp")) {
      sprintf(
        "SELECT COUNT(*) AS n FROM sc_muni_w_dom
         WHERE %s IS NOT NULL AND %s > 0 AND n_dom_p = 0;",
        v, v
      )
    } else if (v == "pop_ch") {
      "SELECT COUNT(*) AS n FROM sc_muni_w_dom
       WHERE pop_ch IS NOT NULL AND pop_ch > 0 AND n_dom_c = 0;"
    } else {
      sprintf(
        "SELECT COUNT(*) AS n FROM sc_muni_w_dom
         WHERE %s IS NOT NULL AND %s > 0
           AND (CASE
                  WHEN n_dom_p > 0 THEN n_dom_p
                  WHEN n_dom_c > 0 THEN n_dom_c
                  ELSE 0
                END) = 0;",
        v, v
      )
    }
    n0 <- q1(sql, "n")

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

  warn_lines
}


#' Emit the two-stage dasymetric interpolation diagnostics
#'
#' The framing was duplicated alongside the stage 1 builder. Stage 2 lines and
#' the label for that stage come from the caller, since they describe different
#' targets.
#'
#' @param stage1_lines Character vector from `.build_interp_diagnostics()`.
#' @param stage2_lines Character vector of preformatted stage 2 lines.
#' @param stage2_label What the points were aggregated onto, e.g. `"H3 hexagons"`.
#'
#' @keywords internal
#' @noRd
.report_interp_diagnostics <- function(stage1_lines, stage2_lines, stage2_label) {
  cli::cli_h2("Dasymetric interpolation diagnostics")

  cli::cli_h3("Stage 1: Tracts \u2192 CNEFE points")
  if (length(stage1_lines) > 0) {
    cli::cli_bullets(
      stats::setNames(stage1_lines, rep("!", length(stage1_lines)))
    )
  } else {
    cli::cli_alert_success("All tract values fully allocated to CNEFE points.")
  }

  cli::cli_h3("Stage 2: CNEFE points \u2192 {stage2_label}")
  cli::cli_bullets(
    stats::setNames(stage2_lines, rep("i", length(stage2_lines)))
  )

  invisible(NULL)
}


#' An empty `sf` with the `compute_lumi()` hex schema
#'
#' `compute_lumi()` used to return `NULL` when no hexagon survived filtering,
#' which Referee 2 flagged under R2.C6 because callers that pipe the result have
#' no reason to expect it. Returning a zero-row `sf` with the documented columns
#' keeps the contract stable: the shape is always the same, only the row count
#' varies.
#'
#' @keywords internal
#' @noRd
.empty_lumi_sf <- function() {
  sf::st_sf(
    id_hex = character(0),
    p_res = numeric(0),
    ei = numeric(0),
    hhi = numeric(0),
    bal = numeric(0),
    ice = numeric(0),
    hhi_adp = numeric(0),
    bgbi = numeric(0),
    geometry = sf::st_sfc(crs = 4326)
  )
}


## Theme: Aggregation mode

#' Resolve the aggregation mode, honouring the deprecated `polygon_type`
#'
#' Referee 1 (R1.6) observed that `polygon_type` is redundant, since passing an
#' `sf` object to `polygon` already states the intent. The argument is now
#' soft-deprecated: the mode is inferred from whether `polygon` is `NULL`, and
#' code that still passes `polygon_type` keeps working with a warning rather
#' than breaking.
#'
#' Two cases are preserved deliberately. `polygon_type = "user"` with no
#' `polygon` stays an error, because it states an intent the call cannot
#' satisfy, and it was an error before. `polygon_type = "hex"` alongside a
#' supplied `polygon` resolves to user polygons, which is what the previous
#' inference did, only without the three alert lines it used to print.
#'
#' @param polygon The `polygon` argument as received by the caller.
#' @param polygon_type The `polygon_type` argument as received by the caller.
#' @param fn Name of the calling function, for the deprecation message.
#'
#' @return `"hex"` or `"user"`.
#'
#' @keywords internal
#' @noRd
.resolve_polygon_mode <- function(
  polygon,
  polygon_type = lifecycle::deprecated(),
  fn
) {
  if (lifecycle::is_present(polygon_type)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = paste0(fn, "(polygon_type)"),
      details = c(
        "The aggregation mode is now inferred from {.arg polygon}.",
        "i" = "Pass an {.cls sf} object to {.arg polygon} for user polygons, or leave it {.code NULL} for an H3 grid."
      )
    )

    polygon_type <- match.arg(polygon_type, c("hex", "user"))

    # Stating "user" without a polygon was an error before and stays one.
    if (identical(polygon_type, "user") && is.null(polygon)) {
      .validate_polygon_arg(
        NULL,
        required_when = "{.arg polygon_type} is {.val user}"
      )
    }
  }

  if (is.null(polygon)) "hex" else "user"
}


## Theme: Pre-flight availability check

#' Check that a CNEFE URL is reachable before committing to the retry ladder
#'
#' Referee 1 (R1.8c) asked for the two failure modes to be told apart. Without
#' this, a broken upstream path and a dead network connection produced the same
#' message, after burning through retry timeouts of 300, 600 and 1800 seconds.
#'
#' A HEAD request separates them cheaply. The IBGE server answers HEAD
#' correctly, returning 200 with a content-length for a file that exists and 404
#' for one that does not, so a failure at transport level means the server could
#' not be reached at all.
#'
#' The 404 case is the interesting one. The internal index is built from the
#' published directory layout, so a 404 on a URL we generated means that layout
#' changed upstream. That is a package problem rather than a user problem, and
#' the message says so and points at the issue tracker.
#'
#' @param url The URL to probe.
#' @param timeout Seconds to wait. Deliberately short: this is a probe, not the
#'   download.
#'
#' @return `TRUE`, invisibly, when the URL is reachable. Aborts otherwise.
#'
#' @keywords internal
#' @noRd
.cnefe_preflight <- function(url, timeout = 15L) {
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) e
  )

  # Transport-level failure: DNS, refused connection, timeout, TLS.
  if (inherits(resp, "condition")) {
    host <- httr2::url_parse(url)$hostname
    cli::cli_abort(
      c(
        "Could not reach the IBGE server.",
        "i" = "This usually means a connectivity problem on your side, such as no internet access, a proxy, or a firewall.",
        "i" = "Server: {.url {host}}",
        "i" = "If your connection is working, the server may be temporarily down. Try again later."
      ),
      parent = resp,
      class = "cnefetools_unreachable"
    )
  }

  status <- httr2::resp_status(resp)

  if (identical(status, 404L)) {
    cli::cli_abort(
      c(
        "The IBGE server is reachable, but the requested file was not found (HTTP 404).",
        "i" = "The upstream directory structure has most likely changed, which makes this a problem with {.pkg cnefetools} rather than with your setup.",
        "i" = "Please report it at {.url https://github.com/pedreirajr/cnefetools/issues}, quoting the URL below.",
        "i" = "URL: {.url {url}}"
      ),
      class = "cnefetools_not_found"
    )
  }

  if (status >= 400L) {
    cli::cli_abort(
      c(
        "The IBGE server answered with HTTP {status}.",
        "i" = "URL: {.url {url}}",
        "i" = "If this persists, please report it at {.url https://github.com/pedreirajr/cnefetools/issues}."
      ),
      class = "cnefetools_http_error"
    )
  }

  invisible(TRUE)
}


## Theme: Dynamic FTP fallback

#' Recover a CNEFE download URL by scanning the IBGE directory listing
#'
#' Referee 1 (R1.8a) noted that the package resolves URLs entirely from a
#' pre-built internal index, so a change to the IBGE directory layout would
#' break every released version with no way to recover.
#'
#' The IBGE server publishes an Apache autoindex, so the layout can be walked at
#' runtime. Probing it confirmed that the file names follow
#' `<7-digit code>_<NAME>.zip` and that all 645 municipalities of São Paulo can
#' be reconstructed from the listing, matching the internal index byte for byte.
#' A UF listing is around 170 KB, so this reads a page, never a data file.
#'
#' Two levels are attempted, because a layout change can move either the file or
#' the directory that holds it:
#'
#' 1. list the directory the index URL points into and look for the code there,
#' 2. failing that, list its parent and look for a directory whose name starts
#'    with the two-digit UF code, then list that.
#'
#' This is a recovery path, not the normal one. It runs only after a 404 on the
#' indexed URL, and it never replaces the index, so a successful scan repairs a
#' single call rather than mutating package state.
#'
#' @param code_muni Seven-digit IBGE municipality code, already normalised.
#' @param known_url The URL from the internal index, used to locate the tree.
#' @param timeout Seconds allowed per listing request.
#' @param verbose Whether to report the attempt.
#'
#' @return The recovered URL, or `NULL` when the scan finds nothing.
#'
#' @keywords internal
#' @noRd
.cnefe_scan_ftp_url <- function(
  code_muni,
  known_url,
  timeout = 30L,
  verbose = TRUE
) {
  fetch <- function(u) {
    tryCatch(
      {
        resp <- httr2::request(u) |>
          httr2::req_timeout(timeout) |>
          httr2::req_error(is_error = function(x) FALSE) |>
          httr2::req_perform()
        if (httr2::resp_status(resp) >= 400L) NULL else httr2::resp_body_string(resp)
      },
      error = function(e) NULL
    )
  }

  # Apache autoindex entries are relative hrefs.
  links <- function(html, pattern) {
    if (is.null(html)) {
      return(character(0))
    }
    hits <- regmatches(html, gregexpr('href="[^"]+"', html))[[1]]
    hits <- gsub('^href="|"$', "", hits)
    hits <- hits[!grepl("^([a-z]+:|/|[?])", hits)]
    hits[grepl(pattern, hits)]
  }

  code_str <- sprintf("%07d", as.integer(code_muni))
  uf_prefix <- substr(code_str, 1L, 2L)

  find_in_dir <- function(dir_url) {
    zips <- links(fetch(dir_url), "[.]zip$")
    hit <- zips[startsWith(zips, paste0(code_str, "_"))]
    if (length(hit) == 0L) NULL else paste0(dir_url, hit[[1]])
  }

  # Level 1: the directory the index URL already points into.
  dir_url <- sub("[^/]+$", "", known_url)
  found <- find_in_dir(dir_url)

  # Level 2: the UF directory may have been renamed, so list the parent.
  if (is.null(found)) {
    parent_url <- sub("[^/]+/$", "", dir_url)
    uf_dirs <- links(fetch(parent_url), "/$")
    uf_dirs <- uf_dirs[startsWith(uf_dirs, uf_prefix)]
    for (d in uf_dirs) {
      found <- find_in_dir(paste0(parent_url, d))
      if (!is.null(found)) break
    }
  }

  if (!is.null(found) && isTRUE(verbose)) {
    cli::cli_alert_success(
      "Recovered the download URL by scanning the IBGE directory listing."
    )
  }

  found
}
