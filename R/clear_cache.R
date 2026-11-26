#' Delete cached CNEFE data files
#'
#' @description
#' `clear_cache_muni()` removes CNEFE data files stored in the user cache
#' directory by [cnefe_counts()], [compute_lumi()], [tracts_to_h3()], and
#' related functions.
#'
#' The cache holds gzipped CSVs (`.csv.gz`). Archives left by versions before
#' 0.3.0, which cached the ZIP as published by IBGE, are removed as well.
#'
#' @param code_muni Integer or `"all"`. If `"all"` (default), every cached CNEFE
#'   file is deleted. If a seven-digit IBGE municipality code is provided, only
#'   the file for that municipality is deleted.
#' @param year Integer. Restrict the deletion to one CNEFE edition. `NULL`
#'   (default) clears every edition, which is the previous behavior.
#' @param cache_dir Character. Directory to use for cached downloads. If `NULL`
#'   (default), the `CNEFETOOLS_CACHE_DIR` environment variable is used when it
#'   is set, otherwise [tools::R_user_dir()] with `which = "cache"`. Use this to
#'   point large downloads at a secondary drive or a shared volume.
#' @param verbose Logical; if `TRUE` (default), reports the number of files
#'   deleted and the space freed.
#'
#' @return Invisibly, the character vector of deleted file paths.
#'
#' @examples
#' \donttest{
#' # Delete every cached CNEFE file
#' clear_cache_muni()
#'
#' # Delete only the file for Lauro de Freitas-BA
#' clear_cache_muni(2919207)
#' }
#'
#' @export
clear_cache_muni <- function(
  code_muni = "all",
  verbose = TRUE,
  cache_dir = NULL,
  year = NULL
) {
  # Deliberately not .validate_year(): this deletes directories, so it must be
  # able to clear an edition this version no longer reads, or one left behind by
  # a newer version the user downgraded from.
  year <- .cnefe_cache_year(year)
  cache_dir <- .cnefe_cache_dir(cache_dir, year)

  if (!dir.exists(cache_dir)) {
    if (verbose) {
      cli::cli_inform(c(
        "i" = "Cache directory does not exist: {.path {cache_dir}}"
      ))
    }
    return(invisible(character(0)))
  }

  # Both cache formats. `.csv.gz` is what #93 writes; `.zip` is what versions
  # before 0.3.0 left behind, and those still need clearing. Matching only
  # `.zip` made this function a silent no-op after the format changed: it
  # reported an empty cache while every current entry sat there undeleted.
  # `.parquet` is deliberately absent, since census tract assets are
  # clear_cache_tracts()'s business.
  all_files <- list.files(
    path = cache_dir,
    pattern = "\\.(csv\\.gz|zip)$",
    full.names = TRUE,
    # Cache entries live under a directory per CNEFE edition, so clearing
    # every edition (year = NULL) has to recurse to still mean everything.
    recursive = is.null(year)
  )

  if (length(all_files) == 0L) {
    if (verbose) {
      cli::cli_inform(c("i" = "No cached CNEFE files found."))
    }
    return(invisible(character(0)))
  }

  # Filter by municipality code if a specific code was provided
  if (!identical(code_muni, "all")) {
    code_muni <- .normalize_code_muni(code_muni)
    code_str <- as.character(code_muni)
    all_files <- all_files[grepl(code_str, basename(all_files), fixed = TRUE)]

    if (length(all_files) == 0L) {
      if (verbose) {
        cli::cli_inform(c(
          "i" = "No cached file found for municipality {.val {code_muni}}."
        ))
      }
      return(invisible(character(0)))
    }
  }

  # Compute total size before deletion
  sizes <- file.size(all_files)
  total_mb <- sum(sizes, na.rm = TRUE) / 1024^2

  # Delete files
  unlink(all_files)

  if (verbose) {
    n <- length(all_files)
    cli::cli_inform(c(
      "v" = "Deleted {n} cached CNEFE file{?s} ({round(total_mb, 1)} MB freed)."
    ))
  }

  invisible(all_files)
}


#' Delete cached census tract Parquet files
#'
#' @description
#' `clear_cache_tracts()` removes census tract Parquet files stored in the
#' user cache directory by [tracts_to_h3()] and [tracts_to_polygon()].
#'
#' Those functions download only the census tracts of the municipalities they
#' read, and keep one file per municipality in the cache. A whole-state file is
#' kept instead when it was downloaded by an earlier version of the package, or
#' when the tracts of a single municipality could not be read from the release.
#' Both kinds of file are removed here.
#'
#' @param uf `"all"`, a two-letter UF abbreviation (e.g. `"BA"`), a two-digit
#'   numeric state code (e.g. `29L`), or a seven-digit IBGE municipality code
#'   (e.g. `2919207`). If `"all"` (default), all cached Parquet files are
#'   deleted. Otherwise, only the files of the resolved state are deleted (the
#'   state file and the files of each of its municipalities).
#' @param year Integer. Restrict the deletion to one CNEFE edition. `NULL`
#'   (default) clears every edition, which is the previous behavior.
#' @param cache_dir Character. Directory to use for cached downloads. If `NULL`
#'   (default), the `CNEFETOOLS_CACHE_DIR` environment variable is used when it
#'   is set, otherwise [tools::R_user_dir()] with `which = "cache"`. Use this to
#'   point large downloads at a secondary drive or a shared volume.
#' @param verbose Logical; if `TRUE` (default), reports the number of files
#'   deleted and the space freed.
#'
#' @return Invisibly, the character vector of deleted file paths.
#'
#' @examples
#' \donttest{
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
clear_cache_tracts <- function(
  uf = "all",
  verbose = TRUE,
  cache_dir = NULL,
  year = NULL
) {
  # Deliberately not .validate_year(): this deletes directories, so it must be
  # able to clear an edition this version no longer reads, or one left behind by
  # a newer version the user downgraded from.
  year <- .cnefe_cache_year(year)
  # Tract assets live at <cache>/<year>/sc_assets, so clearing every edition
  # has to start from the cache root and recurse, not from a single sc_assets.
  sc_dir <- if (is.null(year)) {
    .cnefe_cache_dir(cache_dir)
  } else {
    .sc_cache_dir(cache_dir, year)
  }

  if (!dir.exists(sc_dir)) {
    if (verbose) {
      cli::cli_inform(c(
        "i" = "Cache directory does not exist: {.path {sc_dir}}"
      ))
    }
    return(invisible(character(0)))
  }

  # List all Parquet files
  all_parquets <- list.files(
    path = sc_dir,
    pattern = "\\.parquet$",
    full.names = TRUE,
    # Cache entries live under a directory per CNEFE edition, and the tracts
    # cached by municipality under sc_assets/muni, so the listing recurses.
    recursive = TRUE
  )
  # The municipality boundaries are Parquet files too, kept under
  # <cache>/<year>/boundaries. They are clear_cache_boundaries()'s business.
  all_parquets <- all_parquets[
    basename(dirname(all_parquets)) != "boundaries"
  ]

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
    # The state file, and the files of its municipalities, named by code
    by_muni <- basename(dirname(all_parquets)) == "muni" &
      startsWith(basename(all_parquets), sprintf("%02d", as.integer(uf_code)))
    all_parquets <- all_parquets[basename(all_parquets) == filename | by_muni]

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


#' Delete cached municipality boundaries
#'
#' @description
#' `clear_cache_boundaries()` removes the municipality boundaries stored in the
#' user cache directory. The functions that build an H3 grid from `code_muni`,
#' or that find the municipalities a layer reaches with `scope = "geom"`, read
#' the boundaries at full resolution from a file published with the package and
#' keep each municipality they read in the cache, so later calls work offline.
#' Each file is small (Salvador takes about 0.06 MB, and the 39 municipalities
#' of the São Paulo metropolitan region about 1.6 MB), and the whole country
#' would take about 250 MB.
#'
#' @param code_muni Integer or `"all"`. If `"all"` (default), every cached
#'   boundary is deleted. Otherwise, one or more seven-digit IBGE municipality
#'   codes whose boundaries are deleted.
#' @inheritParams clear_cache_muni
#'
#' @return Invisibly, the character vector of deleted file paths.
#'
#' @examples
#' \donttest{
#' # Delete every cached boundary
#' clear_cache_boundaries()
#'
#' # Delete only the boundary of Lauro de Freitas-BA
#' clear_cache_boundaries(2919207)
#' }
#'
#' @export
clear_cache_boundaries <- function(
  code_muni = "all",
  verbose = TRUE,
  cache_dir = NULL,
  year = NULL
) {
  year <- .cnefe_cache_year(year)
  root <- .cnefe_cache_dir(cache_dir, year)

  if (!dir.exists(root)) {
    if (verbose) {
      cli::cli_inform(c("i" = "Cache directory does not exist: {.path {root}}"))
    }
    return(invisible(character(0)))
  }

  # Boundaries live at <cache>/<year>/boundaries, so clearing every edition
  # starts from the cache root and keeps only the files of those folders.
  files <- list.files(
    root,
    pattern = "[.]parquet([.]tmp)?$",
    full.names = TRUE,
    recursive = TRUE
  )
  files <- files[basename(dirname(files)) == "boundaries"]

  if (!identical(code_muni, "all")) {
    codes <- vapply(code_muni, .normalize_code_muni, integer(1))
    files <- files[
      sub("[.]parquet([.]tmp)?$", "", basename(files)) %in% as.character(codes)
    ]
  }

  if (length(files) == 0L) {
    if (verbose) {
      cli::cli_inform(c("i" = "No cached municipality boundaries found."))
    }
    return(invisible(character(0)))
  }

  total_mb <- sum(file.size(files), na.rm = TRUE) / 1024^2
  unlink(files)

  if (verbose) {
    n <- length(files)
    cli::cli_inform(c(
      "v" = "Deleted {n} cached boundar{?y/ies} ({round(total_mb, 2)} MB freed)."
    ))
  }

  invisible(files)
}
