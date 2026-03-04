#' Delete cached CNEFE ZIP files
#'
#' @description
#' `clear_cache_muni()` removes CNEFE ZIP files stored in the user cache
#' directory by [cnefe_counts()], [compute_lumi()], [tracts_to_h3()], and
#' related functions.
#'
#' @param code_muni Integer or `"all"`. If `"all"` (default), all cached CNEFE
#'   ZIP files are deleted. If a seven-digit IBGE municipality code is provided,
#'   only the ZIP file for that municipality is deleted.
#' @param verbose Logical; if `TRUE` (default), reports the number of files
#'   deleted and the space freed.
#'
#' @return Invisibly, the character vector of deleted file paths.
#'
#' @examples
#' \dontrun{
#' # Delete all cached CNEFE ZIPs
#' clear_cache_muni()
#'
#' # Delete only the ZIP for Lauro de Freitas-BA
#' clear_cache_muni(2919207)
#' }
#'
#' @export
clear_cache_muni <- function(code_muni = "all", verbose = TRUE) {
  cache_dir <- .cnefe_cache_dir()

  if (!dir.exists(cache_dir)) {
    if (verbose) {
      cli::cli_inform(c("i" = "Cache directory does not exist: {.path {cache_dir}}"))
    }
    return(invisible(character(0)))
  }

  # List all ZIP files in the cache root (not subdirectories)
  all_zips <- list.files(
    path = cache_dir,
    pattern = "\\.zip$",
    full.names = TRUE,
    recursive = FALSE
  )

  if (length(all_zips) == 0L) {
    if (verbose) {
      cli::cli_inform(c("i" = "No cached CNEFE ZIP files found."))
    }
    return(invisible(character(0)))
  }

  # Filter by municipality code if a specific code was provided
  if (!identical(code_muni, "all")) {
    code_muni <- .normalize_code_muni(code_muni)
    code_str <- as.character(code_muni)
    all_zips <- all_zips[grepl(code_str, basename(all_zips), fixed = TRUE)]

    if (length(all_zips) == 0L) {
      if (verbose) {
        cli::cli_inform(c(
          "i" = "No cached ZIP found for municipality {.val {code_muni}}."
        ))
      }
      return(invisible(character(0)))
    }
  }

  # Compute total size before deletion
  sizes <- file.size(all_zips)
  total_mb <- sum(sizes, na.rm = TRUE) / 1024^2

  # Delete files
  unlink(all_zips)

  if (verbose) {
    n <- length(all_zips)
    cli::cli_inform(c(
      "v" = "Deleted {n} cached CNEFE ZIP file{?s} ({round(total_mb, 1)} MB freed)."
    ))
  }

  invisible(all_zips)
}


#' Delete cached census tract Parquet files
#'
#' @description
#' `clear_cache_tracts()` removes census tract Parquet files stored in the
#' user cache directory by [tracts_to_h3()] and [tracts_to_polygon()].
#'
#' @param uf `"all"`, a two-letter UF abbreviation (e.g. `"BA"`), a two-digit
#'   numeric state code (e.g. `29L`), or a seven-digit IBGE municipality code
#'   (e.g. `2919207`). If `"all"` (default), all cached Parquet files are
#'   deleted. Otherwise, only the file for the resolved state is deleted.
#' @param verbose Logical; if `TRUE` (default), reports the number of files
#'   deleted and the space freed.
#'
#' @return Invisibly, the character vector of deleted file paths.
#'
#' @examples
#' \dontrun{
#' # Delete all cached census tract Parquets
#' clear_cache_tracts()
#'
#' # Delete only the Parquet for Bahia (several equivalent calls)
#' clear_cache_tracts("BA")
#' clear_cache_tracts(29)
#' clear_cache_tracts(2919207)  # municipality code → state resolved automatically
#' }
#'
#' @export
clear_cache_tracts <- function(uf = "all", verbose = TRUE) {
  sc_dir <- .sc_cache_dir()

  if (!dir.exists(sc_dir)) {
    if (verbose) {
      cli::cli_inform(c("i" = "Cache directory does not exist: {.path {sc_dir}}"))
    }
    return(invisible(character(0)))
  }

  # List all Parquet files
  all_parquets <- list.files(
    path = sc_dir,
    pattern = "\\.parquet$",
    full.names = TRUE,
    recursive = FALSE
  )

  if (length(all_parquets) == 0L) {
    if (verbose) {
      cli::cli_inform(c("i" = "No cached census tract Parquet files found."))
    }
    return(invisible(character(0)))
  }

  # Filter by UF if a specific UF was provided
  if (!identical(uf, "all")) {
    uf_code <- .resolve_uf(uf)
    filename <- .sc_asset_filename(uf_code)
    all_parquets <- all_parquets[basename(all_parquets) == filename]

    if (length(all_parquets) == 0L) {
      if (verbose) {
        cli::cli_inform(c(
          "i" = "No cached Parquet found for UF {.val {uf}}."
        ))
      }
      return(invisible(character(0)))
    }
  }

  # Compute total size before deletion
  sizes <- file.size(all_parquets)
  total_mb <- sum(sizes, na.rm = TRUE) / 1024^2

  # Delete files
  unlink(all_parquets)

  if (verbose) {
    n <- length(all_parquets)
    cli::cli_inform(c(
      "v" = "Deleted {n} cached Parquet file{?s} ({round(total_mb, 1)} MB freed)."
    ))
  }

  invisible(all_parquets)
}
