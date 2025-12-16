# Internal helper functions for cnefetools (not exported)


## Theme: Input validation

#' @keywords internal
#' @noRd
.normalize_code_muni <- function(code_muni) {
  if (length(code_muni) != 1L) {
    rlang::abort("`code_muni` must be a single value.")
  }

  if (is.factor(code_muni)) code_muni <- as.character(code_muni)

  if (is.character(code_muni)) {
    code_muni <- trimws(code_muni)
    if (!nzchar(code_muni) || !grepl("^\\d{7}$", code_muni)) {
      rlang::abort("`code_muni` must be coercible to a valid integer IBGE code.")
    }
    code_muni <- suppressWarnings(as.integer(code_muni))
  } else {
    code_muni <- suppressWarnings(as.integer(code_muni))
  }

  if (is.na(code_muni) || !is.finite(code_muni)) {
    rlang::abort("`code_muni` must be coercible to a valid integer IBGE code.")
  }

  if (nchar(sprintf("%d", code_muni)) != 7) {
    rlang::abort("`code_muni` must be coercible to a valid integer IBGE code.")
  }

  code_muni
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


# Theme: Download and archive handling

#' @keywords internal
#' @noRd
.cnefe_ensure_zip <- function(code_muni,
                              index,
                              cache = TRUE,
                              verbose = TRUE,
                              base_timeout = 300L,
                              timeouts = c(300L, 600L, 1800L)) {

  info <- index[index$code_muni == code_muni, , drop = FALSE]
  if (nrow(info) == 0) {
    rlang::abort(
      sprintf("Municipality code not found in internal CNEFE index: %s", code_muni)
    )
  }

  url <- info$zip_url[1]
  if (is.na(url) || !nzchar(url)) {
    rlang::abort(
      sprintf("Missing `zip_url` in internal index for municipality: %s", code_muni)
    )
  }

  ext <- tools::file_ext(url)
  if (!nzchar(ext)) ext <- "zip"

  if (isTRUE(cache)) {
    cache_dir   <- .cnefe_cache_dir()
    zip_path    <- file.path(cache_dir, basename(url))
    cleanup_zip <- FALSE
  } else {
    zip_path    <- tempfile(fileext = paste0(".", ext))
    cleanup_zip <- TRUE
  }

  # If cached file exists, validate it; if invalid, delete and re-download
  if (isTRUE(cache) && file.exists(zip_path)) {
    valid <- tryCatch({
      archive::archive(zip_path)
      TRUE
    }, error = function(e) FALSE)

    if (!valid) {
      if (verbose) message("Cached ZIP appears corrupted. Deleting it...")
      unlink(zip_path)
    }
  }

  # Download if needed
  if (!file.exists(zip_path)) {
    .cnefe_download_zip_with_retry(
      url          = url,
      destfile     = zip_path,
      verbose      = verbose,
      base_timeout = base_timeout,
      timeouts     = timeouts
    )
  } else if (verbose) {
    message("Using cached file: ", zip_path)
  }

  list(
    zip_path    = zip_path,
    cleanup_zip = cleanup_zip,
    url         = url
  )
}

#' @keywords internal
#' @noRd
.cnefe_download_zip_with_retry <- function(url,
                                           destfile,
                                           verbose = TRUE,
                                           base_timeout = 300L,
                                           timeouts = c(300L, 600L, 1800L)) {

  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)

  timeouts <- unique(as.integer(c(base_timeout, timeouts)))
  timeouts <- timeouts[!is.na(timeouts) & timeouts > 0]
  timeouts <- sort(timeouts)

  last_err <- NULL

  for (t in timeouts) {
    options(timeout = t)

    tmp <- tempfile(fileext = ".zip")
    if (file.exists(tmp)) unlink(tmp)

    if (verbose) message(sprintf("Downloading (timeout = %ss): %s", t, url))

    ok <- tryCatch({
      utils::download.file(
        url,
        destfile = tmp,
        mode     = "wb",
        quiet    = !verbose
      )

      if (!file.exists(tmp) || is.na(file.size(tmp)) || file.size(tmp) == 0) {
        stop("Download produced an empty file.")
      }

      # Validate ZIP integrity by listing contents
      archive::archive(tmp)

      # Move into place only after validation
      if (file.exists(destfile)) unlink(destfile)
      ok_copy <- file.copy(tmp, destfile, overwrite = TRUE)
      if (!isTRUE(ok_copy)) stop("Failed to copy downloaded ZIP into destination.")

      TRUE
    }, error = function(e) {
      last_err <<- e
      FALSE
    }, finally = {
      if (file.exists(tmp)) unlink(tmp)
    })

    if (isTRUE(ok)) return(invisible(TRUE))

    if (verbose && !is.null(last_err)) {
      message("Download attempt failed: ", conditionMessage(last_err))
    }
  }

  if (!is.null(last_err)) {
    rlang::abort(conditionMessage(last_err))
  }
  rlang::abort("Download failed for unknown reasons.")
}

#' @keywords internal
#' @noRd
.cnefe_first_csv_in_archive <- function(arch_info) {
  csv_inside <- arch_info$path[grepl("\\.csv$", arch_info$path, ignore.case = TRUE)][1]
  if (is.na(csv_inside)) {
    rlang::abort("No .csv file found inside archive.")
  }
  csv_inside
}


# Theme: Spatial boundaries (geobr)

#' @keywords internal
#' @noRd
.read_muni_boundary_2024 <- function(code_muni) {

  rlang::check_installed(
    "geobr",
    reason = "to read municipality boundaries (needed to build the H3 grid)."
  )

  code_muni <- .normalize_code_muni(code_muni)

  args <- list(
    code_muni    = code_muni,
    year         = 2024L,
    simplified   = TRUE,
    showProgress = FALSE,
    cache        = TRUE
  )

  if ("keep_areas_operacionais" %in% names(formals(geobr::read_municipality))) {
    args$keep_areas_operacionais <- FALSE
  }

  err <- NULL
  muni <- tryCatch(
    suppressMessages(suppressWarnings(do.call(geobr::read_municipality, args))),
    error = function(e) { err <<- e; NULL }
  )

  if (is.null(muni) || !inherits(muni, "sf") || nrow(muni) == 0L) {
    msg <- paste0(
      "Could not read municipality boundary via geobr with year = 2024. ",
      "Try updating {geobr} if needed."
    )
    if (!is.null(err)) {
      msg <- paste0(msg, " Underlying error: ", conditionMessage(err))
    }
    rlang::abort(msg)
  }

  muni
}
