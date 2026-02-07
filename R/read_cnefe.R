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
#' @param code_muni Integer. Seven-digit IBGE municipality code.
#' @param year Integer. The CNEFE data year. Currently only 2022 is supported.
#'   Defaults to 2022.
#' @param verbose Logical; if `TRUE`, print informative messages about
#'   download, extraction, and reading steps.
#' @param cache Logical; if `TRUE`, cache the downloaded ZIP file in a
#'   user-level cache directory specific to this package. If `FALSE`, a
#'   temporary file is used and removed after reading.
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
#' @examples
#' \donttest{
#' try({
#'   # Read CNEFE data as an Arrow table
#'   cnefe <- read_cnefe(code_muni = 2929057)
#'
#'   # Read as an sf spatial object
#'   cnefe_sf <- read_cnefe(code_muni = 2929057, output = "sf")
#' })
#' }
#'
#' @export
read_cnefe <- function(
  code_muni,
  year = 2022,
  verbose = TRUE,
  cache = TRUE,
  output = c("arrow", "sf")
) {
  output <- match.arg(output)
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

  # List files and find first CSV inside
  if (verbose) {
    cli::cli_progress_step("Listing file contents")
  }

  csv_inside <- .cnefe_first_csv_in_zip(zip_path)

  if (verbose) {
    cli::cli_progress_done()
  }

  if (verbose) {
    cli::cli_progress_step("Extracting {.file {csv_inside}}")
  }

  utils::unzip(
    zipfile = zip_path,
    files   = csv_inside,
    exdir   = tmp_dir
  )

  csv_path <- file.path(tmp_dir, csv_inside)
  if (!file.exists(csv_path)) {
    cli::cli_abort("Failed to extract CSV to {.path {csv_path}}")
  }

  if (verbose) {
    cli::cli_progress_done()
  }

  # Read with Arrow
  if (verbose) {
    cli::cli_progress_step("Reading CSV with {.pkg arrow}")
  }

  tab <- suppressWarnings(
    arrow::read_delim_arrow(
      csv_path,
      delim = ";",
      col_names = TRUE,
      as_data_frame = FALSE
    )
  )

  if (verbose) {
    cli::cli_progress_done()
    cli::cli_alert_success("Read {.val {nrow(tab)}} records from CNEFE")
  }

  if (identical(output, "arrow")) {
    return(tab)
  }

  # From here on we need sf installed
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

  df <- df[!is.na(df$LONGITUDE) & !is.na(df$LATITUDE), , drop = FALSE]

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

  return(out)
}
