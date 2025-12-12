#' Read CNEFE 2022 data for a given municipality
#'
#' @description
#' Downloads (optionally caching) and reads the CNEFE 2022 CSV file for a given
#' IBGE municipality code, using the official IBGE FTP structure. The function
#' relies on an internal index linking municipality codes to the corresponding
#' ZIP URLs. Data are returned as an Arrow [Table][arrow::Table] for efficient
#' downstream processing.
#'
#' @details
#' Internally, `read_cnefe()`:
#'
#' 1. Looks up the given `code_muni` in an internal index (`cnefe_index_2022`),
#'    which maps municipality codes to ZIP URLs.
#' 2. Downloads the corresponding ZIP file (or reuses a cached copy).
#' 3. Finds the first `.csv` file inside the archive.
#' 4. Extracts the CSV to a temporary directory.
#' 5. Reads it with [arrow::read_delim_arrow()] assuming `";"` as delimiter.
#'
#' The function does not attempt to interpret column types beyond Arrow's
#' defaults and does not perform any spatial conversion. These steps are left
#' to the user or to higher level helpers that may be added in future versions.
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
#' @param code_muni Integer or coercible to integer. Seven-digit IBGE
#'   municipality code.
#' @param verbose Logical; if `TRUE`, print informative messages about
#'   download, extraction, and reading steps.
#' @param cache Logical; if `TRUE`, cache the downloaded ZIP file in a
#'   user-level cache directory specific to this package. If `FALSE`, a
#'   temporary file is used and removed after reading.
#'
#' @return An [arrow::Table] containing all CNEFE records for the given
#'   municipality.
#'
#' @examples
#' \dontrun{
#' # Example: read CNEFE data for a municipality (Salvador)
#' tab <- read_cnefe(2927408)
#'
#' # Convert to a data frame
#' df <- tab |> as.data.frame()
#' head(df)
#' }
#'
#' @export
read_cnefe <- function(code_muni,
                       verbose = TRUE,
                       cache   = TRUE) {
  code_muni <- .normalize_code_muni(code_muni)

  # Use the internal index built in data-raw/
  index <- cnefe_index_2022

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

  # Decide where to store the ZIP file
  if (cache) {
    cache_dir <- .cnefe_cache_dir()
    zip_path  <- file.path(cache_dir, basename(url))
    cleanup_zip <- FALSE
  } else {
    zip_path    <- tempfile(fileext = paste0(".", ext))
    cleanup_zip <- TRUE
  }

  # Download if needed
  if (!file.exists(zip_path)) {
    if (verbose) message("Downloading CNEFE file from: ", url)
    utils::download.file(
      url,
      destfile = zip_path,
      mode     = "wb",
      quiet    = !verbose
    )
  } else if (verbose) {
    message("Using cached file: ", zip_path)
  }

  # Temporary directory to extract the CSV
  tmp_dir <- tempfile("cnefe_unzip_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  on.exit({
    if (cleanup_zip && file.exists(zip_path)) {
      unlink(zip_path)
    }
    if (dir.exists(tmp_dir)) {
      unlink(tmp_dir, recursive = TRUE)
    }
  }, add = TRUE)

  if (verbose) message("Listing archive contents...")
  arch_info <- archive::archive(zip_path)
  csv_inside <- arch_info$path[
    grepl("\\.csv$", arch_info$path, ignore.case = TRUE)
  ][1]

  if (is.na(csv_inside)) {
    rlang::abort(
      sprintf(
        "No .csv file found inside archive for municipality: %s",
        code_muni
      )
    )
  }

  if (verbose) message("Extracting ", csv_inside, " ...")
  archive::archive_extract(zip_path, file = csv_inside, dir = tmp_dir)

  csv_path <- file.path(tmp_dir, csv_inside)
  if (!file.exists(csv_path)) {
    rlang::abort(
      sprintf(
        "Failed to extract CSV to %s", csv_path
      )
    )
  }

  if (verbose) message("Reading CSV with arrow...")
  tab <- suppressWarnings(
    arrow::read_delim_arrow(
      csv_path,
      delim        = ";",
      col_names    = TRUE,
      as_data_frame = FALSE
    )
  )

  tab
}

# Internal helper: normalize and validate municipality code
.normalize_code_muni <- function(code_muni) {
  if (length(code_muni) != 1L) {
    rlang::abort("`code_muni` must be a single value.")
  }

  code_muni <- suppressWarnings(as.integer(code_muni))

  if (is.na(code_muni) || !is.finite(code_muni)) {
    rlang::abort("`code_muni` must be coercible to a valid integer IBGE code.")
  }

  code_muni
}

# Internal helper: return and create (if needed) the cache directory
.cnefe_cache_dir <- function() {
  cache_dir <- tools::R_user_dir("cnefetools", which = "cache")

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  cache_dir
}
