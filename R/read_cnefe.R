#' Read CNEFE 2022 data for a given municipality
#'
#' @description
#' Downloads and reads the CNEFE 2022 CSV file for a given
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
#' @export
read_cnefe <- function(
  code_muni,
  verbose = TRUE,
  cache = TRUE,
  output = c("arrow", "sf")
) {
  output <- match.arg(output)
  code_muni <- .normalize_code_muni(code_muni)

  # Ensure ZIP exists (cached or temporary) and is valid
  zip_info <- .cnefe_ensure_zip(
    code_muni = code_muni,
    index = cnefe_index_2022,
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
    message("Listing file contents...")
  }
  csv_inside <- .cnefe_first_csv_in_zip(zip_path)

  if (verbose) {
    message("Extracting ", csv_inside, " ...")
  }

  utils::unzip(
    zipfile = zip_path,
    files   = csv_inside,
    exdir   = tmp_dir
  )

  csv_path <- file.path(tmp_dir, csv_inside)
  if (!file.exists(csv_path)) {
    rlang::abort(sprintf("Failed to extract CSV to %s", csv_path))
  }

  # Read with Arrow
  if (verbose) {
    message("Reading CSV with arrow...")
  }
  tab <- suppressWarnings(
    arrow::read_delim_arrow(
      csv_path,
      delim = ";",
      col_names = TRUE,
      as_data_frame = FALSE
    )
  )

  if (identical(output, "arrow")) {
    return(tab)
  }

  # From here on we need sf installed
  rlang::check_installed(
    "sf",
    reason = "to use `output = \"sf\"` in `read_cnefe()`."
  )

  df <- as.data.frame(tab)

  if (!all(c("LONGITUDE", "LATITUDE") %in% names(df))) {
    rlang::abort(
      "Columns 'LONGITUDE' and 'LATITUDE' not found in CNEFE data; cannot build sf object."
    )
  }

  df$LONGITUDE <- as.numeric(df$LONGITUDE)
  df$LATITUDE <- as.numeric(df$LATITUDE)

  df <- df[!is.na(df$LONGITUDE) & !is.na(df$LATITUDE), , drop = FALSE]

  if (nrow(df) == 0L) {
    rlang::abort(
      "No rows with valid coordinates (LONGITUDE/LATITUDE) were found."
    )
  }

  sf::st_as_sf(
    df,
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4674,
    remove = FALSE
  )
}
