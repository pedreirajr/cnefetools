#' Read CNEFE data for a given municipality
#'
#' @description
#' Downloads and reads the CNEFE CSV file for a given
#' IBGE municipality code, using the official IBGE FTP structure. The function
#' relies on an internal index linking municipality codes to the corresponding
#' ZIP URLs. Data are returned either as an Arrow [Table][arrow::Table]
#' (default) or as an [sf][sf::st_as_sf] object with SIRGAS 2000 coordinates.
#'
#' @details
#'
#' When `output = "arrow"` (default), the function does not perform any spatial
#' conversion and simply returns the Arrow table. When `output = "sf"`, the
#' function converts the result to an [sf][sf::st_as_sf] point object using the
#' `LONGITUDE` and `LATITUDE` columns, with CRS EPSG:4674 (SIRGAS 2000),
#' keeping these columns in the final object (`remove = FALSE`).
#'
#' @section Caching:
#' When `cache = TRUE` (the default), the downloaded ZIP file is stored in a
#' user-level cache directory specific to this package, created via
#' [tools::R_user_dir()] with `which = "cache"`. This avoids re-downloading
#' the same municipality file across sessions.
#'
#' When `cache = FALSE`, the ZIP file is stored in a temporary location and
#' removed when the function exits.
#'
#' @param code_muni Integer. Seven-digit IBGE municipality code. Omit it when
#'   reading a local file through `file`.
#' @param year Integer. The CNEFE data year. Currently only 2022 is supported.
#'   Defaults to 2022.
#' @param verbose Logical; if `TRUE`, print informative messages about
#'   download, extraction, and reading steps.
#' @param cache Logical; if `TRUE`, cache the downloaded ZIP file in a
#'   user-level cache directory specific to this package. If `FALSE`, a
#'   temporary file is used and removed after reading.
#' @param cache_dir Character. Directory to use for cached downloads. If `NULL`
#'   (default), the `CNEFETOOLS_CACHE_DIR` environment variable is used when it
#'   is set, otherwise [tools::R_user_dir()] with `which = "cache"`. Use this to
#'   point large downloads at a secondary drive or a shared volume.
#' @param file Character. Path to a CNEFE file already on disk, read instead of
#'   downloading. Accepts `.zip` as published by IBGE, `.csv`, `.csv.gz` and
#'   `.parquet`, which is what [cnefe_export()] writes. Mutually exclusive with
#'   `code_muni`, and it makes the function independent of the IBGE server.
#' @param output Character. Output format. `"arrow"` (default) returns an
#'   [arrow::Table], whereas `"sf"` returns an [sf][sf::st_as_sf] point object
#'   with coordinates built from `LONGITUDE` / `LATITUDE` in CRS 4674.
#'
#' @return
#' If `output = "arrow"`, an [arrow::Table] containing all CNEFE records for
#' the given municipality.
#'
#' If `output = "sf"`, an [sf][sf::st_as_sf] object with point geometry in
#' EPSG:4674 (SIRGAS 2000), using the `LONGITUDE` and `LATITUDE` columns.
#'
#' @seealso [cnefe_export()] to write a municipality to a persistent, optimised
#'   file that this function can read back through `file`.
#'
#' @examples
#' \donttest{
#' # Read CNEFE data as an Arrow table
#' cnefe <- read_cnefe(code_muni = 2929057, cache = FALSE)
#'
#' # Read a local file instead, with no network access
#' path <- cnefe_export(2929057, path = tempdir(), cache = FALSE)
#' cnefe_local <- read_cnefe(file = path)
#'
#' # Read as an sf spatial object
#' cnefe_sf <- read_cnefe(code_muni = 2929057, output = "sf", cache = FALSE)
#' }
#'
#' @export
read_cnefe <- function(
  code_muni = NULL,
  year = 2022,
  verbose = TRUE,
  cache = TRUE,
  cache_dir = NULL,
  output = c("arrow", "sf"),
  file = NULL
) {
  output <- match.arg(output)

  if (is.null(file) && is.null(code_muni)) {
    cli::cli_abort(c(
      "Supply either {.arg code_muni} or {.arg file}.",
      "i" = "{.arg code_muni} downloads from IBGE, {.arg file} reads a local file."
    ))
  }
  if (!is.null(file) && !is.null(code_muni)) {
    cli::cli_abort(
      "Supply {.arg code_muni} or {.arg file}, not both."
    )
  }

  # Local ingestion path: retrieval is skipped entirely (R2.7, R1.11).
  if (!is.null(file)) {
    tab <- .cnefe_read_local(file, verbose = verbose)

    if (verbose) {
      cli::cli_progress_done()
      cli::cli_alert_success("Read {.val {nrow(tab)}} records from {.file {basename(file)}}")
    }

    return(.cnefe_finalise_output(tab, output = output, verbose = verbose))
  }

  code_muni <- .normalize_code_muni(code_muni)
  year <- .validate_year(year)

  # Get the appropriate index for the requested year
  cnefe_index <- .get_cnefe_index(year)

  if (verbose) {
    cli::cli_alert_info("Processing municipality code {.val {code_muni}}")
  }

  # Ensure ZIP exists (cached or temporary) and is valid
  zip_info <- .cnefe_ensure_zip(
    code_muni = code_muni,
    index = cnefe_index,
    cache = cache,
    cache_dir = cache_dir,
    year = year,
    verbose = verbose,
    retry_timeouts = c(300L, 600L, 1800L)
  )

  zip_path <- zip_info$zip_path
  cleanup_zip <- isTRUE(zip_info$cleanup_zip)

  # Temporary directory to extract the CSV
  tmp_dir <- tempfile("cnefe_unzip_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  on.exit(
    {
      if (cleanup_zip && file.exists(zip_path)) {
        unlink(zip_path)
      }
      if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)
    },
    add = TRUE
  )

  # The cache holds a gzipped CSV, which is read directly. A path written by an
  # older version of the package may still be a ZIP, and .cnefe_read_local()
  # handles both.
  tab <- .cnefe_read_local(zip_path, verbose = verbose)

  if (verbose) {
    cli::cli_progress_done()
    cli::cli_alert_success("Read {.val {nrow(tab)}} records from CNEFE")
  }

  return(.cnefe_finalise_output(tab, output = output, verbose = verbose))
}


#' Turn a CNEFE Arrow table into the requested output
#'
#' Shared by the download path and the local-file path added for R1.11 and
#' R2.7, so both return exactly the same object for the same data.
#'
#' @keywords internal
#' @noRd
.cnefe_finalise_output <- function(tab, output, verbose) {
  if (identical(output, "arrow")) {
    return(tab)
  }

  rlang::check_installed(
    "sf",
    reason = "to use `output = \"sf\"` in `read_cnefe()`."
  )

  if (verbose) {
    cli::cli_progress_step("Converting to {.pkg sf} object")
  }

  df <- as.data.frame(tab)

  if (!all(c("LONGITUDE", "LATITUDE") %in% names(df))) {
    cli::cli_abort(c(
      "Columns {.field LONGITUDE} and {.field LATITUDE} not found in CNEFE data.",
      "i" = "Cannot build {.cls sf} object without coordinates."
    ))
  }

  df$LONGITUDE <- as.numeric(df$LONGITUDE)
  df$LATITUDE <- as.numeric(df$LATITUDE)

  n_before <- nrow(df)
  df <- df[!is.na(df$LONGITUDE) & !is.na(df$LATITUDE), , drop = FALSE]
  n_dropped <- n_before - nrow(df)

  if (n_dropped > 0L && isTRUE(verbose)) {
    pct <- if (n_before > 0L) 100 * n_dropped / n_before else 0
    cli::cli_alert_warning(
      "Dropped {.strong {n_dropped}} of {.strong {n_before}} rows ({.strong {sprintf('%.2f%%', pct)}}) with missing coordinates."
    )
  }

  if (nrow(df) == 0L) {
    cli::cli_abort(c(
      "No rows with valid coordinates were found.",
      "i" = "All {.field LONGITUDE} and {.field LATITUDE} values are {.val NA}."
    ))
  }

  out <- sf::st_as_sf(
    df,
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4674,
    remove = FALSE
  )

  if (verbose) {
    cli::cli_progress_done()
    cli::cli_alert_success("Created {.cls sf} object with {.val {nrow(out)}} points (CRS: EPSG:4674)")
  }

  out
}
