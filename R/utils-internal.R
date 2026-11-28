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


#' Normalise one or several IBGE municipality codes
#'
#' `cnefe_counts()`, `compute_lumi()` and the two interpolation functions take a
#' vector of codes, so that a region spanning several municipalities is built in
#' one pass: one grid over the union of the boundaries, one reference share `P`
#' over the whole region, and no cell that appears twice (#120, R2 comment 1).
#' Each element goes through [.normalize_code_muni()], repeated codes are
#' dropped, and the first-seen order is kept.
#'
#' @param code_muni One or more seven-digit IBGE municipality codes.
#'
#' @return An integer vector without duplicates, at least one element long.
#'
#' @keywords internal
#' @noRd
.normalize_code_munis <- function(code_muni) {
  if (length(code_muni) < 1L) {
    cli::cli_abort("{.arg code_muni} must contain at least one municipality code.")
  }

  codes <- vapply(
    seq_along(code_muni),
    function(i) .normalize_code_muni(code_muni[[i]]),
    integer(1)
  )

  unique(codes)
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
#' @return A normalized directory path. The directory is not created here.
#'
#' @keywords internal
#' @noRd
.cnefe_cache_dir <- function(cache_dir = NULL, year = NULL) {
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
    return(.cnefe_year_dir(path.expand(cache_dir), year))
  }

  from_env <- Sys.getenv("CNEFETOOLS_CACHE_DIR", unset = "")
  if (nzchar(from_env)) {
    return(.cnefe_year_dir(path.expand(from_env), year))
  }

  .cnefe_year_dir(tools::R_user_dir("cnefetools", which = "cache"), year)
}


#' Append the CNEFE edition to a cache root
#'
#' Cache entries are segregated per edition. The published ZIP names carry no
#' year, so `2919207_LAURO_DE_FREITAS.zip` from a future census would be
#' indistinguishable from the 2022 one, and the package would happily serve a
#' cached 2022 file to someone who asked for another edition. A directory per
#' year makes that collision impossible by construction.
#'
#' Coerce a cache-edition argument to a directory name
#'
#' Looser than `.validate_year()` on purpose. The cache cleaners operate on
#' directories, so they must be able to reach an edition this version does not
#' read.
#'
#' @keywords internal
#' @noRd
.cnefe_cache_year <- function(year) {
  if (is.null(year)) {
    return(NULL)
  }
  if (length(year) != 1L) {
    cli::cli_abort("{.arg year} must be a single value.")
  }
  y <- suppressWarnings(as.integer(year))
  if (is.na(y)) {
    cli::cli_abort("{.arg year} must be coercible to an integer.")
  }
  y
}


#' @keywords internal
#' @noRd
.cnefe_year_dir <- function(root, year) {
  if (is.null(year)) root else file.path(root, as.character(year))
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
  year = NULL,
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

  # The cache stores a gzipped CSV rather than the published ZIP. DuckDB
  # decompresses gzip natively, which removes the community `zipfs` extension
  # from the read path and, measured in data-raw/bench_gz_vs_zip.R, reads 2.29x
  # faster on Fortaleza at the same size on disk (#80 R1.11).
  gz_name <- paste0(sub("[.]zip$", "", basename(url), ignore.case = TRUE), ".csv.gz")

  if (isTRUE(cache)) {
    cache_dir <- .cnefe_cache_dir(cache_dir, year)
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    zip_path <- file.path(cache_dir, gz_name)
    cleanup_zip <- FALSE
  } else {
    zip_path <- tempfile(fileext = ".csv.gz")
    cleanup_zip <- TRUE
  }

  # A truncated or corrupt gzip stream fails on the first read, and a readable
  # first line that is not the CNEFE header means the wrong file is cached.
  if (isTRUE(cache) && file.exists(zip_path)) {
    valid <- tryCatch(
      {
        con <- gzfile(zip_path, "rt")
        on.exit(close(con), add = TRUE)
        header <- readLines(con, n = 1L, warn = FALSE)
        length(header) == 1L && grepl("COD_UNICO_ENDERECO|LONGITUDE", header)
      },
      error = function(e) FALSE
    )

    if (!valid) {
      if (verbose) {
        cli::cli_alert_warning("Cached file appears corrupted. Deleting it...")
      }
      unlink(zip_path)
    }
  }

  # Download if needed. The published ZIP goes to a temporary file and is then
  # converted to the gzipped CSV the cache actually holds, so an interrupted
  # download never leaves a half-written cache entry behind.
  if (!file.exists(zip_path)) {
    tmp_zip <- tempfile(fileext = ".zip")
    on.exit(unlink(tmp_zip), add = TRUE)

    tryCatch(
      .cnefe_download_zip_with_retry(
        url = url,
        destfile = tmp_zip,
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
          destfile = tmp_zip,
          verbose = verbose,
          retry_timeouts = retry_timeouts
        )
      }
    )

    .cnefe_zip_to_gz(tmp_zip, zip_path, verbose = verbose)
    unlink(tmp_zip)
  } else if (verbose) {
    cli::cli_alert_info("Using cached file: {zip_path}")
  }

  list(
    zip_path = zip_path,
    cleanup_zip = cleanup_zip,
    url = url
  )
}

#' Retrieve the CNEFE file of every requested municipality
#'
#' Loops over [.cnefe_ensure_zip()] and registers the cleanup of an uncached
#' file right after each retrieval, so a failure while fetching the second
#' municipality still removes the first one's temporary file.
#'
#' @param code_munis Integer vector of municipality codes.
#' @param envir Frame the cleanup handlers attach to, the caller by default.
#' @inheritParams .cnefe_ensure_zip
#'
#' @return A list of `.cnefe_ensure_zip()` results, one per code, in order.
#'
#' @keywords internal
#' @noRd
.cnefe_ensure_zips <- function(
  code_munis,
  index,
  cache = TRUE,
  cache_dir = NULL,
  year = NULL,
  verbose = TRUE,
  retry_timeouts = c(300L, 600L, 1800L),
  envir = parent.frame()
) {
  infos <- vector("list", length(code_munis))

  for (i in seq_along(code_munis)) {
    if (verbose && length(code_munis) > 1L) {
      cli::cli_alert_info(
        "Municipality {i} of {length(code_munis)}: {.val {code_munis[[i]]}}"
      )
    }
    infos[[i]] <- .cnefe_ensure_zip(
      code_muni = code_munis[[i]],
      index = index,
      cache = cache,
      cache_dir = cache_dir,
      year = year,
      verbose = verbose,
      retry_timeouts = retry_timeouts
    )
    .cnefe_defer_cleanup(infos[[i]], envir = envir)
  }

  infos
}


#' SQL that reads the address points of several municipalities as one table
#'
#' Builds one `SELECT ... FROM read_csv_auto(...)` per file and joins them with
#' `UNION ALL`. Each file is read, and its types inferred, on its own exactly as
#' a single-municipality call does, and DuckDB streams the union, so the memory
#' of an aggregation over it does not depend on how many files there are.
#'
#' The columns are `code_muni`, `lon`, `lat` and `COD_ESPECIE`, plus
#' `COD_UNICO_ENDERECO` and `COD_SETOR` when `ids = TRUE`.
#'
#' @param con A live DuckDB connection, used to load `zipfs` for a legacy ZIP.
#' @param zip_infos List of `.cnefe_ensure_zip()` results.
#' @param code_munis Integer vector of codes, the same length and order.
#' @param ids Whether to carry the address id and the census tract code.
#' @param verbose Forwarded to the extension loader.
#'
#' @return A single SQL string, to be used as a subquery.
#'
#' @keywords internal
#' @noRd
.cnefe_points_union_sql <- function(
  con,
  zip_infos,
  code_munis,
  ids = FALSE,
  verbose = TRUE
) {
  parts <- character(length(zip_infos))
  zipfs_ready <- FALSE

  for (i in seq_along(zip_infos)) {
    src <- .cnefe_csv_uri(zip_infos[[i]]$zip_path)
    if (isTRUE(src$needs_zipfs) && !zipfs_ready) {
      # A cache written by an older version is still a ZIP.
      .duckdb_quiet(.duckdb_ensure_extension(con, "zipfs", verbose = verbose))
      zipfs_ready <- TRUE
    }
    uri_sql <- gsub("'", "''", src$uri)

    id_cols <- if (isTRUE(ids)) {
      paste0(
        "CAST(COD_UNICO_ENDERECO AS VARCHAR) AS COD_UNICO_ENDERECO,
",
        "      CAST(COD_SETOR AS VARCHAR) AS COD_SETOR,
      "
      )
    } else {
      ""
    }

    parts[[i]] <- sprintf(
      paste0(
        "SELECT
",
        "      %d AS code_muni,
",
        "      %sCAST(LONGITUDE AS DOUBLE) AS lon,
",
        "      CAST(LATITUDE AS DOUBLE) AS lat,
",
        "      try_cast(COD_ESPECIE AS INTEGER) AS COD_ESPECIE
",
        "    FROM read_csv_auto('%s', delim=';', header=true, strict_mode=false)"
      ),
      as.integer(code_munis[[i]]),
      id_cols,
      uri_sql
    )
  }

  paste(parts, collapse = "
    UNION ALL
    ")
}


#' Combine the spatial-join results of several municipalities
#'
#' The R backend joins one municipality file at a time and keeps only the
#' counts per polygon, so memory follows the largest municipality. The pieces
#' are additive: counts per polygon (and species) are summed, and so are the
#' coverage totals. A single result is returned untouched.
#'
#' @param results List of the lists built by `.cnefe_counts_user_poly_r()` or
#'   `.compute_lumi_user_poly_r()`.
#'
#' @return One list of the same shape.
#'
#' @keywords internal
#' @noRd
.combine_poly_joins <- function(results) {
  if (length(results) == 1L) {
    return(results[[1L]])
  }

  counts <- dplyr::bind_rows(lapply(results, function(x) x$counts))
  value_cols <- intersect(c("n", "n_res", "n_tot"), names(counts))
  key_cols <- setdiff(names(counts), value_cols)

  counts <- counts |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(value_cols), ~ as.integer(sum(.x))),
      .groups = "drop"
    )

  out <- list(counts = counts)
  for (nm in c(
    "total_points", "points_matched", "points_outside", "points_multi",
    "total_n_res", "total_n_tot", "matched_n_res", "matched_n_tot"
  )) {
    vals <- lapply(results, function(x) x[[nm]])
    if (!all(vapply(vals, is.null, logical(1)))) {
      out[[nm]] <- sum(unlist(vals))
    }
  }

  out
}


#' Delete a temporary data file when the calling function exits
#'
#' With `cache = FALSE`, [.cnefe_ensure_zip()] writes the municipality to a
#' temporary file and flags it with `cleanup_zip = TRUE`, and the census tract
#' download leaves its Parquet in a temporary folder. The caller owns those
#' files and has to delete them on success and on error alike, which the
#' functions did not do before comment 3 of the second-round review (#121).
#'
#' The handler is registered with `priority = "last"` so that it runs after the
#' DuckDB disconnect registered by [.duckdb_connect()] on the same frame. A
#' DuckDB view reads the file, and on Windows an open handle would make the
#' deletion fail silently.
#'
#' Call it right after the file is created, before anything that can fail.
#'
#' @param path Path of the temporary file. `NULL` is a no-op.
#' @param envir Frame whose exit triggers the deletion. Defaults to the caller.
#'
#' @return `path`, invisibly.
#'
#' @keywords internal
#' @noRd
.defer_unlink <- function(path, envir = parent.frame()) {
  if (is.null(path) || !nzchar(path)) {
    return(invisible(path))
  }
  withr::defer(
    if (file.exists(path)) unlink(path),
    envir = envir,
    priority = "last"
  )
  invisible(path)
}

#' Register deletion of the file returned by `.cnefe_ensure_zip()` if it is
#' temporary
#'
#' @param zip_info The list returned by [.cnefe_ensure_zip()].
#' @param envir Frame whose exit triggers the deletion. Defaults to the caller.
#'
#' @return `zip_info`, invisibly.
#'
#' @keywords internal
#' @noRd
.cnefe_defer_cleanup <- function(zip_info, envir = parent.frame()) {
  if (is.list(zip_info) && isTRUE(zip_info$cleanup_zip)) {
    .defer_unlink(zip_info$zip_path, envir = envir)
  }
  invisible(zip_info)
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
.sc_assets_tag <- function(year = 2022L) {
  # Advanced users can override via options() without changing the API. The
  # override is edition-agnostic on purpose, since it exists for pointing a
  # single session at a test release.
  override <- getOption("cnefetools.sc_assets_tag", NULL)
  if (!is.null(override)) {
    return(override)
  }

  # One GitHub release per census edition. Tract boundaries are redrawn every
  # census, so the 2030 assets are a different dataset rather than a new version
  # of the 2022 ones, and they get their own release instead of new file names
  # inside this one. That keeps each edition independently re-cuttable and lets
  # the 2022 assets stay frozen once published.
  # TODO: When new CNEFE editions become available, cut a release per edition
  # and add it here. Do not rename the existing entry: once published, an
  # edition's assets stay frozen so the provenance checksums keep matching.
  #   tags <- c(
  #     "2022" = "sc-assets-v3",
  #     "2030" = "sc-assets-2030-v1"
  #   )
  #
  # sc-assets-v3 replaced sc-assets-v2 for the 2022 edition. The data is
  # identical, verified across all 27 states, but v3 is reproducible from
  # data-raw/sc_assets_build.R and v2 was not. v2 stays published, so older
  # package versions keep working, and its checksums stay valid.
  #
  # sc-assets-v4 holds the same rows and values as v3, rewritten by
  # data-raw/sc_assets_rowgroups.R in row groups of 256 rows, so the tracts of
  # one municipality can be read remotely through httpfs without the rest of
  # the state (#120). v3 stays published for the versions that use it.
  tags <- c("2022" = "sc-assets-v4")

  key <- as.character(as.integer(year %||% 2022L))
  if (!key %in% names(tags)) {
    cli::cli_abort(c(
      "No census tract assets are published for {.val {key}}.",
      "i" = "Available editions: {.val {names(tags)}}."
    ))
  }

  unname(tags[[key]])
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
.sc_cache_dir <- function(cache_dir = NULL, year = NULL) {
  file.path(.cnefe_cache_dir(cache_dir, year), "sc_assets")
}

#' @keywords internal
#' @noRd
.sc_asset_local_path <- function(uf, cache_dir = NULL, year = NULL) {
  file.path(.sc_cache_dir(cache_dir, year), .sc_asset_filename(uf))
}


#' @keywords internal
#' @noRd
.validate_sc_parquet <- function(path, required_fields = NULL) {

  # Validation: open Parquet metadata and check required fields
  # Includes v2 variables (pop_ph, pop_ch, race_*) to invalidate old v1 cache
  tryCatch(
    {
      # Without mmap: on Windows a mapped file stays locked until the reader
      # is garbage collected, so the temporary copy could not be deleted on
      # exit, nor overwritten by the next call with cache = FALSE
      reader <- arrow::ParquetFileReader$create(path, mmap = FALSE)
      schema <- reader$GetSchema()
      fields <- schema$names
      required_fields <- required_fields %||% c(
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
    year = NULL,
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
  .sc_download_with_piggyback(uf = uf, cache = cache, cache_dir = cache_dir, year = year, verbose = verbose)
}

#' URL of the census tract asset of a state
#'
#' The option `cnefetools.sc_url`, a `sprintf()` template with `%s` for the
#' two-digit state code, overrides it. The tests use it to point at a local
#' file.
#'
#' @keywords internal
#' @noRd
.sc_url <- function(uf, year = NULL) {
  opt <- getOption("cnefetools.sc_url")
  if (!is.null(opt)) {
    return(sprintf(opt, uf))
  }
  sprintf(
    "https://github.com/pedreirajr/cnefetools/releases/download/%s/%s",
    .sc_assets_tag(year), .sc_asset_filename(uf)
  )
}


#' Folder of the census tracts cached by municipality
#'
#' @keywords internal
#' @noRd
.sc_muni_cache_dir <- function(cache_dir = NULL, year = NULL) {
  file.path(.sc_cache_dir(cache_dir, year), "muni")
}


#' Census tract Parquet files for some municipalities of one state
#'
#' The files that `.sc_create_views_in_duckdb()` reads for the municipalities in
#' `code_muni`, all of state `uf` (#120):
#'
#' 1. The state file, when it is already in the cache (downloaded by an earlier
#'    version, or by the fallback below). It holds every municipality of the
#'    state, so nothing is downloaded.
#' 2. Otherwise one file per municipality, under `sc_assets/muni/`. The ones
#'    missing are read from the release through httpfs, in one query that only
#'    touches the row groups of those municipalities, and written to the cache.
#'    With `cache = FALSE` they go to the temporary download folder instead,
#'    and the caller deletes them.
#' 3. If the remote read fails, the whole state file is downloaded with
#'    `.sc_ensure_parquet_uf()`, as before, with a message.
#'
#' @return A character vector of Parquet paths.
#'
#' @keywords internal
#' @noRd
.sc_ensure_parquet_munis <- function(
    uf,
    code_muni,
    cache = TRUE,
    cache_dir = NULL,
    year = NULL,
    verbose = TRUE
) {
  state_file <- .sc_asset_local_path(uf, cache_dir, year)
  if (isTRUE(cache) && file.exists(state_file) && .validate_sc_parquet(state_file)) {
    if (verbose) {
      cli::cli_alert_info("Using cached file: {.file {basename(state_file)}}")
    }
    return(state_file)
  }

  got <- tryCatch(
    .sc_fetch_munis(uf, code_muni, cache = cache, cache_dir = cache_dir, year = year),
    error = function(e) e
  )
  if (!inherits(got, "error")) {
    return(got)
  }

  cli::cli_inform(c(
    "!" = "Could not read the census tracts of {.val {code_muni}} from the {.pkg cnefetools} release, so the whole file of state {.val {uf}} is downloaded instead.",
    "i" = "{conditionMessage(got)}"
  ))
  .sc_ensure_parquet_uf(uf, cache = cache, cache_dir = cache_dir, year = year, verbose = verbose)
}


#' Release tag stored in a census tract file cached by municipality
#'
#' @return The tag, or `NULL` when the file has none or cannot be opened.
#'
#' @keywords internal
#' @noRd
.sc_file_tag <- function(path) {
  tryCatch(
    arrow::ParquetFileReader$create(path, mmap = FALSE)$GetSchema()$metadata$cnefetools_tag,
    error = function(e) NULL
  )
}


#' Read the tracts of some municipalities from the release, one file each
#'
#' The asset is sorted by `code_tract`, whose first seven digits are the
#' municipality code, and written in row groups of 256 rows, so a range on
#' `code_tract` lets DuckDB skip the row groups of every other municipality.
#' Files are written under a temporary name and renamed, so an interrupted
#' write never leaves a partial file that a later call would trust. If a write
#' fails, the files written by this call are removed.
#'
#' @keywords internal
#' @noRd
.sc_fetch_munis <- function(uf, code_muni, cache = TRUE, cache_dir = NULL, year = NULL) {
  code_muni <- as.integer(code_muni)
  dir <- if (isTRUE(cache)) .sc_muni_cache_dir(cache_dir, year) else .sc_tmp_download_dir()
  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  files <- file.path(dir, paste0(code_muni, ".parquet"))

  tag <- .sc_assets_tag(year)
  have <- if (isTRUE(cache)) {
    # A cached file is reused only when it opens with the key and the
    # geometry and was written from the release in use, whose tag is stored
    # in the file. A file from another release, which may lack a variable of
    # the current one, is fetched again (#120).
    vapply(files, function(f) {
      file.exists(f) && .validate_sc_parquet(f, c("code_tract", "geom_wkb")) &&
        identical(.sc_file_tag(f), tag)
    }, logical(1), USE.NAMES = FALSE)
  } else {
    rep(FALSE, length(files))
  }
  if (all(have)) {
    return(files)
  }
  need <- code_muni[!have]

  url <- .sc_url(uf, year)
  con <- .duckdb_httpfs_connect(url, reason = "to read the census tract data.")
  ranges <- paste(
    sprintf("(code_tract >= '%07d' AND code_tract < '%07d')", need, need + 1L),
    collapse = " OR "
  )
  .duckdb_quiet(DBI::dbExecute(con, sprintf(
    "CREATE TEMP TABLE sc_fetched AS SELECT * FROM read_parquet('%s') WHERE %s;",
    gsub("'", "''", url), ranges
  )))

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  written <- character(0)
  done <- FALSE
  on.exit(if (!done) unlink(c(written, paste0(written, ".tmp"))), add = TRUE)
  for (cm in need) {
    f <- file.path(dir, paste0(cm, ".parquet"))
    tmp <- paste0(f, ".tmp")
    written <- c(written, f)
    .duckdb_quiet(DBI::dbExecute(con, sprintf(
      "COPY (SELECT * FROM sc_fetched WHERE substr(code_tract, 1, 7) = '%07d'
             ORDER BY code_tract) TO '%s'
             (FORMAT parquet, KV_METADATA {cnefetools_tag: '%s'});",
      cm, gsub("'", "''", tmp), gsub("'", "''", tag)
    )))
    if (!file.rename(tmp, f)) {
      cli::cli_abort("Could not write {.path {f}}.")
    }
  }
  done <- TRUE
  files
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
#' recognized and retried rather than reported.
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
    year = NULL,
    verbose = TRUE
) {

  rlang::check_installed(
    "piggyback",
    reason = "to download census tract data from GitHub releases."
  )

  filename <- .sc_asset_filename(uf)
  tag <- .sc_assets_tag(year)
  repo <- "pedreirajr/cnefetools"

  # Determine cache destination
  destfile <- NULL
  if (isTRUE(cache)) {
    destfile <- normalizePath(.sc_asset_local_path(uf, cache_dir, year), winslash = "/", mustWork = FALSE)
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
  tmp_download_dir <- .sc_tmp_download_dir()
  if (!dir.exists(tmp_download_dir)) {
    dir.create(tmp_download_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (verbose) {
    cli::cli_progress_step("Downloading {.file {filename}} from GitHub release")
  }

  # The text progress bar is only useful in a console. Anywhere else (knitr,
  # Quarto, Rscript logs) it is written out line by line, hundreds of lines per
  # download, and no chunk option suppresses it. `rlang::is_interactive()` is
  # FALSE while knitr is running, which `interactive()` alone does not detect.
  show_progress <- isTRUE(verbose) && rlang::is_interactive()

  # The first attempt keeps the default token, so users with a valid one still
  # get the authenticated rate limit.
  do_download <- function(token = NULL) {
    args <- list(
      file = filename,
      repo = repo,
      tag = tag,
      dest = tmp_download_dir,
      overwrite = TRUE,
      show_progress = show_progress
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

#' Folder that holds census tract downloads before they reach the cache
#'
#' With `cache = FALSE`, or when the cached copy is locked, the Parquet is used
#' straight from here, which makes a file in this folder a temporary one that
#' the calling function has to delete.
#'
#' @keywords internal
#' @noRd
.sc_tmp_download_dir <- function() {
  file.path(tempdir(), paste0("sc_download_", Sys.getpid()))
}

#' Whether a census tract Parquet is a temporary download
#'
#' Decided by location rather than by `cache`, since a locked cache file also
#' falls back to the temporary copy. A `cache_dir` the user places under
#' `tempdir()` is not mistaken for it, because only this exact folder counts.
#'
#' @keywords internal
#' @noRd
.sc_is_tmp_download <- function(path) {
  norm <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)
  identical(norm(dirname(path)), norm(.sc_tmp_download_dir()))
}

#' @param .envir Frame whose exit deletes a temporary Parquet. Defaults to the
#'   caller, which also owns the DuckDB connection whose views read the file.
#' @keywords internal
#' @noRd
.sc_create_views_in_duckdb <- function(
  con,
  code_muni,
  cache = TRUE,
  cache_dir = NULL,
  year = NULL,
  verbose = TRUE,
  .envir = parent.frame()
) {

  code_muni <- .normalize_code_munis(code_muni)
  ufs <- unique(vapply(code_muni, .uf_from_code_muni, character(1)))

  # Ensure each UF parquet is available locally. A region that crosses a state
  # border needs more than one. A temporary download is deleted when the caller
  # exits, after its DuckDB connection closes. The views below read it lazily,
  # so it cannot go any sooner (#121). It is registered as soon as the path is
  # known, so a failure while fetching the next UF still removes it.
  #
  # Only the tracts of the municipalities read are fetched, one file each,
  # unless the state file is already cached (#120, see
  # .sc_ensure_parquet_munis()).
  uf_of <- vapply(code_muni, .uf_from_code_muni, character(1))
  parquet_paths <- character(0)
  for (uf in ufs) {
    paths <- .sc_ensure_parquet_munis(
      uf, code_muni[uf_of == uf],
      cache = cache, cache_dir = cache_dir, year = year, verbose = verbose
    )
    for (path in paths) {
      path <- normalizePath(path, winslash = "/", mustWork = TRUE)
      if (.sc_is_tmp_download(path)) {
        .defer_unlink(path, envir = .envir)
      }
      parquet_paths <- c(parquet_paths, path)
    }
  }

  # 7-digit municipality prefixes inside the 15-digit tract code
  muni_prefixes <- sprintf("%07d", code_muni)
  prefix_sql <- paste(sprintf("'%s'", muni_prefixes), collapse = ", ")
  paths_sql <- paste(
    sprintf("'%s'", gsub("'", "''", parquet_paths)),
    collapse = ", "
  )

  suppressMessages({
  # View with tract attributes + geometry as DuckDB GEOMETRY
    DBI::dbExecute(
      con,
      sprintf(
        "
      CREATE OR REPLACE VIEW sc_uf_raw AS
      SELECT *
      FROM read_parquet([%s], union_by_name = true);
    ",
        paths_sql
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
      WHERE substr(code_tract, 1, 7) IN (%s);
    ",
        prefix_sql
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
  year = NULL,
  verbose = TRUE,
  .envir = parent.frame()
) {
  code_muni <- .normalize_code_munis(code_muni)

  # Ensure each municipality file exists locally (reuses the existing cache
  # logic). An uncached file is registered for removal on the caller's frame at
  # once: the view built below reads from it lazily, so it has to outlive this
  # function, and it has to go when the caller exits, on success or on error.
  zip_infos <- .cnefe_ensure_zips(
    code_muni,
    index = index,
    cache = cache,
    cache_dir = cache_dir,
    year = year,
    verbose = verbose,
    envir = .envir
  )

  # One SELECT per municipality file, joined by UNION ALL. The cache holds a
  # gzipped CSV, which DuckDB reads from a plain path. Only a legacy ZIP still
  # needs the community zipfs extension, so it is loaded on demand rather than
  # on every connection (#80 R1.11).
  src_sql <- .cnefe_points_union_sql(
    con,
    zip_infos,
    code_muni,
    ids = TRUE,
    verbose = verbose
  )

  suppressMessages({
    DBI::dbExecute(
      con,
      sprintf(
        "
      CREATE OR REPLACE VIEW cnefe_raw AS
      %s;
    ",
        src_sql
      )
    )

    DBI::dbExecute(
      con,
      "
      CREATE OR REPLACE VIEW cnefe_pts AS
      SELECT
        code_muni,
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

  invisible(zip_infos)
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


#' Apply the user's DuckDB resource limits to a fresh connection
#'
#' DuckDB sizes itself against the whole machine: it takes one thread per
#' logical core and sets `memory_limit` to 80% of installed RAM. That is the
#' right default on a dedicated machine and the wrong one on a shared node, a
#' laptop running other work, or a CI runner. Referee 2 raised the same point
#' from the reporting side (R2.8b, R2.8c), asking what the package actually
#' needs and how it behaves on modest hardware.
#'
#' `options(cnefetools.duckdb_config = list(threads = 4, memory_limit = "4GB"))`
#' holds it back. Names are passed to DuckDB's `SET` verbatim, so any setting
#' DuckDB accepts works, not only these two.
#'
#' Exceeding `memory_limit` makes DuckDB spill to its temp directory rather
#' than fail, so a low value costs time, not correctness.
#'
#' @param con A live DuckDB connection.
#'
#' @return `con`, invisibly.
#'
#' @keywords internal
#' @noRd
.duckdb_apply_user_config <- function(con) {
  cfg <- getOption("cnefetools.duckdb_config", default = NULL)
  if (is.null(cfg)) {
    return(invisible(con))
  }

  if (!is.list(cfg) || is.null(names(cfg)) || any(!nzchar(names(cfg)))) {
    cli::cli_abort(c(
      "{.code options(cnefetools.duckdb_config)} must be a named list.",
      "i" = 'For example {.code list(threads = 4, memory_limit = "4GB")}.'
    ))
  }

  for (nm in names(cfg)) {
    value <- cfg[[nm]]
    if (length(value) != 1L || is.na(value)) {
      cli::cli_abort(
        "{.code cnefetools.duckdb_config${nm}} must be a single non-missing value."
      )
    }

    # Quoted unconditionally: DuckDB accepts a quoted integer for `threads` and
    # requires quoting for string settings such as `memory_limit`.
    stmt <- sprintf(
      "SET %s = '%s';",
      nm,
      gsub("'", "''", as.character(value))
    )

    tryCatch(
      .duckdb_quiet(DBI::dbExecute(con, stmt)),
      error = function(e) {
        cli::cli_abort(
          c(
            "Could not apply {.code cnefetools.duckdb_config${nm}}.",
            "x" = conditionMessage(e)
          ),
          parent = e
        )
      }
    )
  }

  invisible(con)
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
#' Resource limits are read from the `cnefetools.duckdb_config` option, which
#' Referee 2 (R2.8b, R2.8c) motivated: DuckDB sizes itself against the whole
#' machine, taking every core and 80% of RAM, and a user on a small machine had
#' no way to hold it back. `threads` and `memory_limit` are the two knobs that
#' matter, and they are what the benchmark harness varies to report a
#' constrained-hardware profile.
#'
#' They are applied with `SET` after connecting rather than through the
#' `config` list so that an unrecognized name fails loudly here instead of
#' aborting `dbConnect()` with a message that names no option.
#'
#' @param extensions Character vector of DuckDB extensions to ensure. Community
#'   extensions by default, `"spatial"` is treated as a core extension.
#' @param spatial Logical. Whether to load duckspatial, installing it into the
#'   connection if the load fails.
#' @param reason Passed to [rlang::check_installed()] to explain the dependency.
#' @param fallback Optional code the caller can suggest when an extension has no
#'   usable build for the platform, such as `backend = "r"`. `NULL` for the
#'   functions that have no non-DuckDB path.
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
  fallback = NULL,
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

  # Registered before anything else can fail. This is the whole point. The
  # user configuration is applied only after this, since an unknown setting or
  # a malformed option errors and would otherwise leave the connection open
  # (comment 3 of the second-round review, #121).
  withr::defer(
    {
      if (!is.null(con) && DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con, shutdown = TRUE)
      }
    },
    envir = .envir
  )

  .duckdb_apply_user_config(con)

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
        verbose = verbose,
        fallback = fallback,
        # Report the failure against the function the user actually called,
        # not against the connection helper.
        call = .envir
      )
    )
  }

  con
}


#' A DuckDB connection that can read a remote Parquet file
#'
#' Used by the boundary and census tract readers (#120). For an http(s) URL
#' the httpfs extension is loaded, and installed first if needed. A local path,
#' as the tests use, needs nothing. The connection closes when `.envir` exits.
#'
#' @keywords internal
#' @noRd
.duckdb_httpfs_connect <- function(url, reason, .envir = parent.frame()) {
  con <- .duckdb_connect(reason = reason, verbose = FALSE, .envir = .envir)
  if (grepl("^https?://", url)) {
    loaded <- tryCatch(
      {
        .duckdb_quiet(DBI::dbExecute(con, "LOAD httpfs;"))
        TRUE
      },
      error = function(e) FALSE
    )
    if (!loaded) {
      .duckdb_quiet(DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;"))
    }
  }
  con
}


# -----------------------------------------------------------------------------
# Internal: Turn a failed extension load into an actionable message
# -----------------------------------------------------------------------------
# DuckDB surfaces a failed `LOAD` as a raw `dlopen()` dump, which reads as a
# package bug on a fresh look. The architecture-mismatch case is real and
# current: since extension v1.5.5 the community repository serves an arm64
# build of `h3` under the osx_amd64 path, so the DuckDB backend is unusable on
# Intel macOS through no fault of ours (issue #99).
#
# `fallback` is the code the caller can suggest instead, or NULL when the
# calling function has no non-DuckDB path. `tracts_to_h3()` and
# `tracts_to_polygon()` are the NULL case on purpose, see R2.6 in issue #80.
.duckdb_extension_abort <- function(
  ext,
  err,
  repo = "community",
  fallback = NULL,
  call = rlang::caller_env()
) {
  detail <- conditionMessage(err)
  arch_mismatch <- grepl("incompatible architecture", detail, fixed = TRUE)
  repo_label <- if (is.null(repo)) "core" else repo

  if (arch_mismatch) {
    bullets <- c(
      "The DuckDB {.val {ext}} extension has no usable build for this platform.",
      "x" = "The {repo_label} extension repository served a binary for a different architecture.",
      "i" = "This is a packaging problem upstream in DuckDB, not in {.pkg cnefetools}."
    )
    # The dlopen dump repeats the same path four times, so only the head of it
    # earns a place in the message.
    detail <- paste0(substr(detail, 1, 200), "...")
  } else {
    bullets <- c(
      "The DuckDB {.val {ext}} extension could not be loaded."
    )
  }

  if (!is.null(fallback)) {
    bullets <- c(
      bullets,
      "i" = "You can run this on the pure-R backend instead, with {.code {fallback}}."
    )
  }

  cli::cli_abort(
    c(bullets, "i" = "DuckDB reported: {detail}"),
    class = "cnefetools_extension_unavailable",
    call = call
  )
}


# -----------------------------------------------------------------------------
# Internal: Helper to ensure DuckDB extension is loaded
# -----------------------------------------------------------------------------
.duckdb_ensure_extension <- function(
  con,
  ext,
  repo = "community",
  verbose = TRUE,
  fallback = NULL,
  call = rlang::caller_env()
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
      tryCatch(
        DBI::dbExecute(con, sprintf("LOAD %s;", ext)),
        error = function(e) {
          .duckdb_extension_abort(ext, e, repo = repo, fallback = fallback, call = call)
        }
      )
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
  tryCatch(
    {
      if (is.null(repo)) {
        DBI::dbExecute(con, sprintf("INSTALL %s;", ext))
      } else {
        DBI::dbExecute(con, sprintf("INSTALL %s FROM %s;", ext, repo))
      }
      DBI::dbExecute(con, sprintf("LOAD %s;", ext))
    },
    error = function(e) {
      .duckdb_extension_abort(ext, e, repo = repo, fallback = fallback, call = call)
    }
  )

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
#' Allocation rules, kept identical to the documented behavior:
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
        "{.field avg_inc_resp} assigned to {.strong {assigned_avg}} of {.strong {eligible_avg}} eligible points ({.strong {sprintf('%.2f%%', assigned_pct)}})"
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
          "{.field avg_inc_resp} is {.strong NA} in {.strong {na_avg_tracts}} of {.strong {n_tracts}} tracts ({.strong {sprintf('%.2f%%', na_avg_pct)}} of the tracts read)"
        )
      )
    }
  }

  if (unmatched_pts > 0) {
    unmatched_pct <- if (total_pts > 0) 100 * unmatched_pts / total_pts else 0
    warn_lines <- c(
      warn_lines,
      cli::format_inline(
        "Unmatched CNEFE points of the municipalities in {.arg code_muni} (no tract): {.strong {unmatched_pts}} of {.strong {total_pts}} points ({.strong {sprintf('%.2f%%', unmatched_pct)}})"
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
        cli::format_inline("{.field {v}} in {.strong {n_na}} of {.strong {n_tracts}} tracts ({.strong {sprintf('%.2f%%', na_pct)}} of the tracts read)")
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
        cli::format_inline("{.field {v}} in {.strong {n0}} of {.strong {n_tracts}} tracts ({.strong {sprintf('%.2f%%', n0_pct)}} of the tracts read)")
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
#' @param stage2_warn_lines Optional stage 2 lines shown with a warning bullet,
#'   such as the points that fell in H3 cells outside the grid (#120).
#' @param head_lines Optional lines shown with a warning bullet right under the
#'   title, before stage 1, such as the dwellings that fall in more than one
#'   polygon (#120).
#'
#' @keywords internal
#' @noRd
.report_interp_diagnostics <- function(
  stage1_lines,
  stage2_lines,
  stage2_label,
  stage2_warn_lines = character(0),
  head_lines = character(0)
) {
  cli::cli_h2("Dasymetric interpolation diagnostics")
  if (length(head_lines) > 0) {
    cli::cli_bullets(
      stats::setNames(head_lines, rep("!", length(head_lines)))
    )
  }

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
  if (length(stage2_warn_lines) > 0) {
    cli::cli_bullets(
      stats::setNames(stage2_warn_lines, rep("!", length(stage2_warn_lines)))
    )
  }

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
      # lifecycle passes `details` through as-is, so cli markup would print
      # literally. Expanded here instead.
      details = c(
        cli::format_inline("The aggregation mode is now inferred from {.arg polygon}."),
        "i" = cli::format_inline(
          "Pass an {.cls sf} object to {.arg polygon} for user polygons, or leave it {.code NULL} for an H3 grid."
        )
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


## Theme: scope and the municipalities read

#' Check the `code_muni` argument before it is normalized
#'
#' In polygon mode with `scope = "geom"`, `code_muni` is optional (#120), so
#' `polygon` can be the only spatial input. A polygon passed by position then
#' lands in `code_muni`, which used to fail with an unhelpful message about
#' 7-digit codes. In H3 mode the grid is built from the boundaries of
#' `code_muni`, so it is always required there.
#'
#' @param code_muni The `code_muni` argument as received by the caller.
#' @param polygon_type `"hex"` or `"user"`.
#'
#' @return `TRUE` when `code_muni` was supplied, `FALSE` when it is `NULL` and
#'   has to be detected from the polygons.
#'
#' @keywords internal
#' @noRd
.check_code_muni_arg <- function(code_muni, polygon_type) {
  if (inherits(code_muni, "sf") || inherits(code_muni, "sfc")) {
    cli::cli_abort(c(
      "{.arg code_muni} received an {.cls sf} object.",
      "i" = "Pass the polygons by name, as {.code polygon = <your sf object>}."
    ))
  }

  if (is.null(code_muni)) {
    if (identical(polygon_type, "hex")) {
      cli::cli_abort(c(
        "{.arg code_muni} is required for the H3 grid.",
        "i" = "Pass one or more 7-digit IBGE codes, or pass {.arg polygon} to aggregate onto your own polygons."
      ))
    }
    return(FALSE)
  }

  TRUE
}


#' Resolve `scope` and decide whether the municipalities are detected
#'
#' `scope` decides which CNEFE addresses a call works with (#120):
#'
#' - `"muni"`: every address of the municipalities in `code_muni`, which is
#'   then required in both modes.
#' - `"geom"`: the addresses inside the output geometries, whichever
#'   municipality they belong to. In polygon mode the municipalities are
#'   detected from the polygons, so a `code_muni` that was passed is ignored.
#'   In H3 mode `code_muni` still defines the grid, and the municipalities the
#'   grid reaches are added later by `.grid_neighbors()`.
#'
#' @param scope The `scope` argument as received.
#' @param code_muni_given The result of `.check_code_muni_arg()`.
#' @param polygon_type `"hex"` or `"user"`.
#' @param call The environment of the exported function, so the error names it.
#'
#' @return A list with `scope` (`"muni"` or `"geom"`) and `detect` (`TRUE`
#'   when the municipalities have to be detected from the polygons).
#'
#' @keywords internal
#' @noRd
.resolve_scope <- function(scope, code_muni_given, polygon_type, call = rlang::caller_env()) {
  scope <- match.arg(scope, c("muni", "geom"))

  if (!identical(polygon_type, "user")) {
    return(list(scope = scope, detect = FALSE))
  }

  if (identical(scope, "muni")) {
    if (!code_muni_given) {
      cli::cli_abort(c(
        "{.arg code_muni} is required when {.code scope = \"muni\"}.",
        "i" = "Pass the 7-digit IBGE codes of the municipalities to read, or use {.code scope = \"geom\"} to read the municipalities the polygons intersect."
      ), call = call)
    }
    return(list(scope = scope, detect = FALSE))
  }

  if (code_muni_given) {
    cli::cli_inform(c(
      "i" = "{.arg code_muni} is ignored with {.code scope = \"geom\"}, which reads the municipalities the polygons intersect.",
      "i" = "Use {.code scope = \"muni\"} to read only the municipalities in {.arg code_muni}."
    ))
  }
  list(scope = scope, detect = TRUE)
}


#' Municipalities outside `code_muni` that an H3 grid reaches
#'
#' With `scope = "geom"` the cells along the border of the grid are completed
#' with the addresses of the neighboring municipalities (#120). The grid holds
#' the cells whose center falls inside `code_muni` plus the neighboring cells
#' that intersect the boundary, so it reaches every municipality that borders
#' the area. They are found with the same test as the polygons, and listed in a
#' message.
#'
#' @param hex_grid The sf grid from `build_h3_grid()`.
#' @param code_muni The normalized codes the grid was built from.
#' @param year Territorial base year.
#'
#' @return An integer vector of codes, possibly empty, not in `code_muni`.
#'
#' @keywords internal
#' @noRd
.grid_neighbors <- function(hex_grid, code_muni, year = 2022L, cache = TRUE,
                            cache_dir = NULL) {
  detected <- .detect_code_munis(hex_grid, year = year, cache = cache, cache_dir = cache_dir)
  extra <- detected[!detected$code_muni %in% as.integer(code_muni), , drop = FALSE]

  if (nrow(extra) > 0L) {
    .inform_detected_munis(
      extra,
      intro = "The H3 grid also reaches {n} municipalit{?y/ies} outside {.arg code_muni}, read to complete the cells along the border: ",
      hint = "Use {.code scope = \"muni\"} to read only the municipalities in {.arg code_muni}."
    )
  }

  extra$code_muni
}


#' Keep only the neighbor tracts that hold a dwelling inside the H3 grid
#'
#' Used by `tracts_to_h3(scope = "geom")` (#120). The neighbors read to complete
#' the border cells are read whole, but only the tracts with at least one
#' dwelling in a cell of the grid can send anything to it. Those tracts are
#' found by joining the neighbor dwellings that fall in the grid, usually a few
#' thousand, to the tracts. Every other tract of the neighbors is deleted from
#' `sc_muni_tbl`, so the main spatial join runs against far fewer tracts. The
#' tracts kept stay whole, and all their dwellings are matched later, so the
#' allocation is the same as in a call that lists the neighbors in `code_muni`.
#'
#' A dwelling is matched only to the tracts of its own municipality, as in the
#' main join, and that filter runs after the spatial join for the same reason
#' given there.
#'
#' @param con DuckDB connection holding `cnefe_pts_tbl` and `sc_muni_tbl`, the
#'   latter with an RTREE index `sc_muni_geom_idx`.
#' @param grid_ids Character vector of the cells of the grid.
#' @param h3_resolution H3 resolution of the grid.
#' @param own_list The codes of `code_muni`, as a comma-separated string.
#'
#' @keywords internal
#' @noRd
.sc_keep_tracts_touching_grid <- function(con, grid_ids, h3_resolution, own_list) {
  exec <- function(sql) invisible(.duckdb_quiet(DBI::dbExecute(con, sql)))
  own_chr <- paste0("'", strsplit(own_list, ", ", fixed = TRUE)[[1]], "'", collapse = ", ")

  .duckdb_quiet(DBI::dbWriteTable(
    con, "grid_ids_tbl", data.frame(id_hex = as.character(grid_ids)),
    temporary = TRUE, overwrite = TRUE
  ))

  exec(sprintf(
    "
    CREATE OR REPLACE TEMP TABLE nb_pts_in_grid AS
    SELECT p.code_muni, p.geom
    FROM cnefe_pts_tbl p
    WHERE p.code_muni NOT IN (%s)
      AND h3_latlng_to_cell_string(p.lat, p.lon, %d) IN (SELECT id_hex FROM grid_ids_tbl);
    ",
    own_list, as.integer(h3_resolution)
  ))

  exec(
    "
    CREATE OR REPLACE TEMP TABLE nb_tract_hits AS
    SELECT p.code_muni, s.code_tract
    FROM nb_pts_in_grid p, sc_muni_tbl s
    WHERE ST_Within(p.geom, s.geom);
    "
  )

  # The index is rebuilt after the delete
  exec("DROP INDEX IF EXISTS sc_muni_geom_idx;")
  exec(sprintf(
    "
    DELETE FROM sc_muni_tbl
    WHERE substr(code_tract, 1, 7) NOT IN (%s)
      AND code_tract NOT IN (
        SELECT DISTINCT code_tract
        FROM nb_tract_hits
        WHERE substr(code_tract, 1, 7) = CAST(code_muni AS VARCHAR)
      );
    ",
    own_chr
  ))
  exec("CREATE INDEX sc_muni_geom_idx ON sc_muni_tbl USING RTREE (geom);")

  invisible(NULL)
}


#' Build the H3 grid and decide which municipalities to read
#'
#' The grid always comes from the boundaries of `code_muni`. With
#' `scope = "geom"`, the municipalities the grid reaches beyond them are read
#' too, so the cells along the border hold the addresses of both sides (#120).
#'
#' @param code_muni Normalized codes.
#' @param scope `"muni"` or `"geom"`.
#' @param h3_resolution H3 resolution.
#' @param year Territorial base year.
#' @param cache,cache_dir Passed to the boundary reader.
#'
#' @return A list with `hex_grid` (sf) and `read_munis` (integer codes, with
#'   `code_muni` first).
#'
#' @keywords internal
#' @noRd
.h3_grid_and_read_set <- function(code_muni, scope, h3_resolution, year = 2022L,
                                  cache = TRUE, cache_dir = NULL) {
  hex_grid <- build_h3_grid(
    h3_resolution = h3_resolution,
    code_muni = code_muni,
    year = year,
    cache = cache,
    cache_dir = cache_dir
  )

  read_munis <- code_muni
  if (identical(scope, "geom")) {
    read_munis <- c(
      code_muni,
      .grid_neighbors(hex_grid, code_muni, year = year, cache = cache, cache_dir = cache_dir)
    )
  }

  list(hex_grid = hex_grid, read_munis = as.integer(read_munis))
}


#' Call a geobr reader with the RNG state isolated and errors wrapped
#'
#' The same isolation `.read_muni_boundary()` does inline: geobr (>= 2.0.0)
#' derives dbplyr table names from the global RNG, so repeated calls under a
#' fixed seed collide. The caller's RNG state is restored on exit.
#'
#' @param fun A geobr function.
#' @param args A named list of arguments.
#' @param what Short description used in the error message.
#'
#' @return The sf object returned by geobr.
#'
#' @keywords internal
#' @noRd
.geobr_read <- function(fun, args, what) {
  rlang::check_installed(
    "geobr",
    reason = "to find the municipalities that the polygons intersect."
  )

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  } else {
    on.exit(
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      },
      add = TRUE
    )
  }
  set.seed(NULL)

  out <- tryCatch(
    suppressMessages(suppressWarnings(rlang::exec(fun, !!!args))),
    error = function(cnd) {
      cli::cli_abort(
        "Could not read {what} via {.pkg geobr}.",
        parent = cnd
      )
    }
  )

  if (!inherits(out, "sf") || nrow(out) == 0L) {
    cli::cli_abort(c(
      "{.pkg geobr} returned an empty or invalid object for {what}.",
      "i" = "Try updating {.pkg geobr}."
    ))
  }

  out
}


#' Municipality boundaries of some states, for the territorial base of `year`
#'
#' Simplified geobr boundaries, used only when the release cannot be read (see
#' `.read_muni_boundaries()`).
#'
#' @param code_states Two-digit IBGE state codes.
#'
#' @keywords internal
#' @noRd
.read_muni_boundaries_of_states <- function(code_states, year = 2022L) {
  args_base <- list(year = year, simplified = TRUE, showProgress = FALSE)
  if ("keep_areas_operacionais" %in% names(formals(geobr::read_municipality))) {
    args_base$keep_areas_operacionais <- FALSE
  }

  parts <- lapply(code_states, function(uf) {
    .geobr_read(
      geobr::read_municipality,
      c(list(code_muni = as.integer(uf)), args_base),
      what = paste("the municipality boundaries of state", uf)
    )
  })

  do.call(rbind, parts)
}


#' Tell the user which municipalities were detected
#'
#' Long lists are truncated to the first few names, so a layer that covers a
#' metropolitan region does not print dozens of lines. The message is emitted
#' whatever `verbose` says, because the detected set decides what is downloaded.
#'
#' @param detected The data frame returned by `.detect_code_munis()`.
#' @param max_shown How many municipalities to name before truncating.
#' @param intro cli text that opens the message. `{n}` is the number listed.
#' @param hint cli text of the closing hint.
#'
#' @keywords internal
#' @noRd
.inform_detected_munis <- function(
  detected,
  max_shown = 5L,
  intro = "The polygons intersect {n} municipalit{?y/ies}: ",
  hint = "Use {.code scope = \"muni\"} with {.arg code_muni} to read only the municipalities you choose."
) {
  n <- nrow(detected)
  labels <- sprintf("%s (%s)", detected$name_muni, detected$code_muni)
  shown <- utils::head(labels, max_shown)
  rest <- n - length(shown)

  listing <- paste(shown, collapse = ", ")
  if (rest > 0L) {
    listing <- paste0(listing, cli::format_inline(", and {rest} more"))
  }

  cli::cli_inform(c(
    "i" = cli::format_inline(intro),
    " " = listing,
    "i" = hint
  ))
}


## Theme: P for compute_lumi()

#' Validate `p_target`
#'
#' @keywords internal
#' @noRd
.validate_p_target <- function(p_target) {
  if (is.null(p_target)) {
    return(NULL)
  }
  if (
    !is.numeric(p_target) || length(p_target) != 1L ||
      is.na(p_target) || p_target <= 0 || p_target >= 1
  ) {
    cli::cli_abort(c(
      "{.arg p_target} must be a single number strictly between 0 and 1.",
      "i" = "Value received: {.val {p_target}}"
    ))
  }
  as.numeric(p_target)
}


## Theme: Polygon coverage messages

#' Abort when no CNEFE point falls inside the polygons
#'
#' @param code_muni The municipality codes that were read.
#' @param detected `NULL` when `code_muni` was supplied, otherwise the data
#'   frame from `.detect_code_munis()`.
#'
#' @keywords internal
#' @noRd
.abort_no_points_in_polygons <- function(code_muni, detected = NULL) {
  if (is.null(detected)) {
    cli::cli_abort(c(
      "No CNEFE coordinates were captured within the provided polygon.",
      "i" = "This may indicate that:",
      "*" = "The municipality code {.val {code_muni}} does not correspond to the polygon's municipality, or",
      "*" = "The polygon is not located within municipality {.val {code_muni}}."
    ))
  }
  cli::cli_abort(c(
    "No CNEFE coordinates were captured within the provided polygon.",
    "i" = "None of the addresses of the {nrow(detected)} municipalit{?y/ies} the polygons intersect falls inside them."
  ))
}


#' Report how many CNEFE points of the municipalities fell inside the polygons
#'
#' The warning depends on `scope` (#120):
#'
#' - `scope = "muni"`: the user chose the municipalities, so the points of
#'   those municipalities left outside the polygons are worth a warning. In
#'   `compute_lumi()` without `p_target` they are still part of P, and the
#'   warning says so, along with what `scope = "geom"` would change (it does
#'   not bring these points into the polygons).
#' - `scope = "geom"` (`detected` is not `NULL`): the municipalities are read
#'   whole, so most of their points fall outside the polygons by construction,
#'   and they enter neither the counts nor P. Nothing is reported.
#'
#' @param points_matched Points inside at least one polygon.
#' @param total_points Points read.
#' @param detected `NULL` or the data frame from `.detect_code_munis()`.
#' @param p_note `TRUE` when P is computed from the data (compute_lumi() without
#'   `p_target`), `FALSE` otherwise (including cnefe_counts()).
#' @param overlap The line from `.polygon_overlap_line()`, or `character(0)`.
#'   It opens the coverage warning, and is a warning of its own when there is
#'   no coverage warning to open.
#'
#' @keywords internal
#' @noRd
.report_polygon_coverage <- function(
  points_matched,
  total_points,
  detected = NULL,
  p_note = FALSE,
  overlap = character(0)
) {
  warn_overlap_alone <- function() {
    if (length(overlap) > 0L) {
      cli::cli_warn("{overlap}")
    }
    invisible(NULL)
  }

  if (!is.null(detected)) {
    return(warn_overlap_alone())
  }

  points_outside <- total_points - points_matched

  if (points_outside <= 0L) {
    cli::cli_inform(c(
      "v" = "All {.val {total_points}} CNEFE points were captured within the provided polygon."
    ))
    return(warn_overlap_alone())
  }
  overlap_line <- if (length(overlap) > 0L) c("!" = "{overlap}") else character(0)

  coverage_pct <- (points_matched / total_points) * 100

  p_line <- if (p_note) {
    c("i" = "Under {.code scope = \"muni\"}, P covers every address of these municipalities, these points included. On the other hand, {.code scope = \"geom\"} computes P from the addresses inside the polygons only, and also adds to the polygons the addresses of neighboring municipalities that fall inside them.")
  } else {
    character(0)
  }
  cli::cli_warn(c(
    "Polygon coverage: {.val {sprintf('%.1f', coverage_pct)}%} of the CNEFE points of the municipalities in {.arg code_muni} were captured.",
    overlap_line,
    "i" = "{.val {points_matched}} of {.val {total_points}} points are within the provided polygon.",
    "i" = "{.val {points_outside}} points from the municipalities in {.arg code_muni} fell outside the polygons and were not counted. Possible causes are polygons that don't cover the whole municipality, a coastline the polygons don't follow, or geocoding errors.",
    p_line
  ))
  invisible(NULL)
}


## Theme: Repeated and overlapping polygons

#' Drop the features of `polygon` that repeat the geometry of an earlier one
#'
#' Some public layers repeat a feature with the same geometry, such as the
#' geobr neighborhoods, where a neighborhood appears once per subdistrict it
#' falls in. Each copy would receive the same addresses, so sums over the
#' output would count them two or three times, and the copies would also be
#' reported as overlapping polygons (#120). The copies are compared as WKB,
#' which catches exact repeats in about 0.1 s even for tens of thousands of
#' features. The first occurrence of each geometry is kept, with its
#' attributes.
#'
#' @param polygon An `sf` object.
#'
#' @return `polygon` without the repeated features.
#'
#' @keywords internal
#' @noRd
.drop_repeated_polygons <- function(polygon) {
  wkb <- lapply(sf::st_as_binary(sf::st_geometry(polygon)), as.raw)
  repeated <- duplicated(wkb)
  n_rep <- sum(repeated)
  if (n_rep == 0L) {
    return(polygon)
  }
  n <- length(repeated)
  n_kept <- n - n_rep
  cli::cli_inform(c(
    "i" = "{n_rep} of the {n} features in {.arg polygon} repeat the geometry of an earlier feature and were removed, so {n_kept} feature{?s} {?is/are} used.",
    " " = "The first occurrence of each geometry is kept, with its attributes."
  ))
  polygon[!repeated, ]
}


#' Describe the CNEFE points that fall in more than one polygon
#'
#' Overlapping polygons are a legitimate choice, so they are kept. Each point
#' still goes to every polygon that contains it, and the values summed over the
#' polygons exceed those of the addresses. The line built here opens the
#' diagnostics of the call (#120): the coverage warning of `cnefe_counts()` and
#' `compute_lumi()`, or the interpolation diagnostics of `tracts_to_polygon()`.
#' The count comes from the point-polygon join each function already makes.
#'
#' @param points_multi Points inside two or more polygons.
#' @param points_matched Points inside at least one polygon.
#' @param consequence The end of the sentence, on what the repeat does to the
#'   output.
#' @param what What the points are, for the message.
#'
#' @return One formatted line, or `character(0)` when no point repeats.
#'
#' @keywords internal
#' @noRd
.polygon_overlap_line <- function(points_multi, points_matched, consequence,
                                  what = "CNEFE points") {
  if (is.null(points_multi) || is.na(points_multi) || points_multi <= 0L) {
    return(character(0))
  }
  pct <- .fmt_small_pct(100 * points_multi / points_matched)
  cli::format_inline(
    "Overlapping polygons: {.val {points_multi}} of the {.val {points_matched}} {what} inside the polygons ({pct}) fall in more than one polygon, so {consequence}"
  )
}


## Theme: Addresses outside the H3 grid

#' Format a share that is often tiny
#'
#' The addresses outside the grid are usually a handful among millions, and a
#' fixed three decimals printed them as "0.000%", which reads as none.
#' Values below 0.001% print as "<0.001%", values below 1% keep two
#' significant digits, and larger ones one decimal.
#'
#' @param pct A percentage, between 0 and 100.
#'
#' @return A string such as "<0.001%", "0.0013%", "0.42%" or "12.3%".
#'
#' @keywords internal
#' @noRd
.fmt_small_pct <- function(pct) {
  if (is.na(pct) || pct <= 0) {
    return("0%")
  }
  if (pct < 0.001) {
    return("<0.001%")
  }
  if (pct < 1) {
    return(paste0(format(signif(pct, 2), scientific = FALSE, trim = TRUE), "%"))
  }
  sprintf("%.1f%%", pct)
}


#' Warn about addresses whose H3 cell is not in the grid
#'
#' The grid is built from the 2022 IBGE boundary of the municipalities, while the
#' cell of an address comes from its own coordinates. An address geocoded just
#' outside the boundary, beyond the ring of border cells, falls in a cell that
#' the grid does not hold, and the join from the grid drops it (#120). The
#' grid is kept as it is, and the loss is reported instead.
#'
#' @param n_outside Addresses in cells outside the grid.
#' @param n_total All addresses counted.
#' @param extra Optional extra bullet lines.
#'
#' @keywords internal
#' @noRd
.warn_outside_grid <- function(n_outside, n_total, extra = character(0)) {
  if (n_outside <= 0L) {
    return(invisible(NULL))
  }
  pct <- if (n_total > 0) 100 * n_outside / n_total else 0
  # Formatted first: cli reads "{.name" as a style, not as a function call
  pct_txt <- .fmt_small_pct(pct)
  cli::cli_warn(c(
    "{.val {n_outside}} of {.val {n_total}} addresses ({pct_txt}) of the municipalities in {.arg code_muni} fall in H3 cells outside the grid and were not counted.",
    "i" = "The grid covers the 2022 IBGE municipal boundaries plus the cells along them, and these addresses were geocoded beyond it.",
    extra
  ))
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
#' @param code_muni Seven-digit IBGE municipality code, already normalized.
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


## Theme: Local file ingestion

#' Read a CNEFE file already on disk into an Arrow table
#'
#' Referee 1 (R1.11) asked for a path from the transient cache to a persistent,
#' optimized store, citing `spanishoddata` as a model. Referee 2 (R2.7) asked,
#' from the other direction, whether `read_cnefe()` could ingest a locally
#' obtained file independently of the FTP download step. Both are answered by
#' separating retrieval from parsing, which this does.
#'
#' Supported formats, chosen by extension:
#' - `.parquet`, read lazily by Arrow,
#' - `.csv` and `.csv.gz`, read with the CNEFE delimiter,
#' - `.zip`, the shape IBGE publishes, extracted to a temporary directory.
#'
#' @param file Path to a local file.
#' @param verbose Whether to report progress.
#'
#' @return An [arrow::Table].
#'
#' @keywords internal
#' @noRd
.cnefe_read_local <- function(file, verbose = TRUE) {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    cli::cli_abort("{.arg file} must be a single path.")
  }
  if (!file.exists(file)) {
    cli::cli_abort(c(
      "{.arg file} does not exist.",
      "i" = "Path: {.path {file}}"
    ))
  }

  lower <- tolower(file)

  if (endsWith(lower, ".parquet")) {
    if (verbose) {
      cli::cli_progress_step("Reading {.file {basename(file)}} as Parquet")
    }
    return(arrow::read_parquet(file, as_data_frame = FALSE))
  }

  if (endsWith(lower, ".zip")) {
    if (verbose) {
      cli::cli_progress_step("Extracting {.file {basename(file)}}")
    }
    tmp_dir <- tempfile("cnefe_local_")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    csv_inside <- .cnefe_first_csv_in_zip(file)
    utils::unzip(zipfile = file, files = csv_inside, exdir = tmp_dir)
    file <- file.path(tmp_dir, csv_inside)
  } else if (!grepl("[.]csv([.]gz)?$", lower)) {
    cli::cli_abort(c(
      "Unsupported file type: {.path {basename(file)}}",
      "i" = "Supported extensions are {.val .zip}, {.val .csv}, {.val .csv.gz} and {.val .parquet}."
    ))
  }

  if (verbose) {
    cli::cli_progress_step("Reading {.file {basename(file)}} with {.pkg arrow}")
  }

  # Arrow decompresses .gz transparently from the extension.
  suppressWarnings(
    arrow::read_delim_arrow(
      file,
      delim = ";",
      col_names = TRUE,
      as_data_frame = FALSE
    )
  )
}


## Theme: Cache storage format

#' Convert a downloaded CNEFE ZIP into a gzipped CSV
#'
#' Referee 1 (R1.11) proposed re-compressing the cache to Gzip, which DuckDB
#' decompresses natively, avoiding the community `zipfs` extension. Our own
#' measurement, in `data-raw/bench_gz_vs_zip.R`, reproduces their figures: on
#' Fortaleza a DuckDB read is 2.29x faster from `.csv.gz` than through `zipfs`,
#' against their reported 2.2x, and the gzipped file is the same size on disk as
#' the ZIP. Raw CSV is faster still but needs 6.7x the disk, so it is not a
#' sensible cache format.
#'
#' The conversion is paid once, on first download. It is streamed in chunks so
#' peak memory does not scale with the file: the CSV inside São Paulo's archive
#' is roughly 900 MB.
#'
#' @param zip_path Path to the downloaded ZIP.
#' @param gz_path Destination for the gzipped CSV.
#' @param verbose Whether to report progress.
#'
#' @return `gz_path`, invisibly.
#'
#' @keywords internal
#' @noRd
.cnefe_zip_to_gz <- function(zip_path, gz_path, verbose = TRUE) {
  if (verbose) {
    cli::cli_progress_step("Converting the archive to {.file .csv.gz} (done once)")
  }

  tmp_dir <- tempfile("cnefe_convert_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  csv_inside <- .cnefe_first_csv_in_zip(zip_path)
  utils::unzip(zipfile = zip_path, files = csv_inside, exdir = tmp_dir)
  csv_path <- file.path(tmp_dir, csv_inside)

  if (!file.exists(csv_path)) {
    cli::cli_abort("Failed to extract {.file {csv_inside}} from the archive.")
  }

  # Write to a temporary name first, so an interrupted conversion cannot leave a
  # truncated file behind that later looks like a valid cache entry.
  part <- paste0(gz_path, ".part")
  inc <- file(csv_path, "rb")
  outc <- gzfile(part, "wb")
  on.exit(
    {
      try(close(inc), silent = TRUE)
      try(close(outc), silent = TRUE)
      if (file.exists(part)) unlink(part)
    },
    add = TRUE
  )

  repeat {
    buf <- readBin(inc, "raw", n = 1e7)
    if (length(buf) == 0L) break
    writeBin(buf, outc)
  }
  close(inc)
  close(outc)

  if (!file.rename(part, gz_path)) {
    file.copy(part, gz_path, overwrite = TRUE)
    unlink(part)
  }

  if (verbose) {
    cli::cli_progress_done()
  }

  invisible(gz_path)
}


#' Build the URI DuckDB should read a cached CNEFE file from
#'
#' The cache holds `.csv.gz`, which DuckDB reads natively from a plain path.
#' ZIP is still understood so that a cache written by an older version, or a
#' file the user supplies directly, keeps working through the `zipfs` route.
#'
#' @param path Path to a cached or user-supplied CNEFE file.
#'
#' @return A list with the `uri` to read and whether `zipfs` is needed.
#'
#' @keywords internal
#' @noRd
.cnefe_csv_uri <- function(path) {
  norm <- normalizePath(path, winslash = "/", mustWork = TRUE)

  if (grepl("[.]zip$", norm, ignore.case = TRUE)) {
    csv_inside <- .cnefe_first_csv_in_zip(norm)
    return(list(
      uri = sprintf("zip://%s/%s", norm, csv_inside),
      needs_zipfs = TRUE
    ))
  }

  list(uri = norm, needs_zipfs = FALSE)
}
