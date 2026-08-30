#' Export CNEFE data to a persistent, optimised file
#'
#' @description
#' `cnefe_export()` downloads a municipality (or reuses the cache) and writes it
#' to a location and format of your choosing, so the data no longer depends on
#' the package cache being present or on the IBGE server being reachable.
#'
#' @details
#' The package cache is designed to be transient: it lives in a directory the
#' package manages, it holds the ZIP exactly as IBGE published it, and
#' [clear_cache_muni()] is expected to empty it. That is the wrong shape for a
#' reproducible analysis that must still run in a year.
#'
#' This function fills that gap. Point it at a project directory, a shared
#' volume or an external drive, choose a format, and use the resulting file
#' directly:
#'
#' ```r
#' path <- cnefe_export(2919207, "data/cnefe")
#' cnefe <- read_cnefe(file = path)
#' ```
#'
#' `read_cnefe()` accepts any file this function writes, and also the raw ZIP as
#' distributed by IBGE, so a file obtained by other means can be read without
#' the download step at all.
#'
#' Parquet is the default because it is columnar, typed and compressed, which
#' makes it markedly smaller and faster to read than the published CSV, and
#' because it is the format Arrow and DuckDB both read natively.
#'
#' @param code_muni Integer. Seven-digit IBGE municipality code.
#' @param path Character. Directory to write into. Created if missing.
#' @param format Character. `"parquet"` (default), `"csv"` or `"csv.gz"`.
#' @param year Integer. The CNEFE data year. Currently only 2022 is supported.
#' @param overwrite Logical. Whether to replace an existing file. Defaults to
#'   `FALSE`, which errors instead, since these files are expensive to produce.
#' @param cache Logical. Whether to use the package cache for the download.
#' @param cache_dir Character. Directory to use for cached downloads. If `NULL`
#'   (default), the `CNEFETOOLS_CACHE_DIR` environment variable is used when it
#'   is set, otherwise [tools::R_user_dir()] with `which = "cache"`.
#' @param verbose Logical. Whether to print progress.
#'
#' @return The path to the written file, invisibly.
#'
#' @seealso [read_cnefe()], which reads the result back through its `file`
#'   argument, and [clear_cache_muni()] for the transient cache.
#'
#' @examples
#' \donttest{
#' # Write a municipality to a project directory as Parquet
#' path <- cnefe_export(2929057, path = tempdir(), cache = FALSE, overwrite = TRUE)
#'
#' # Read it back without touching the network
#' cnefe <- read_cnefe(file = path)
#' }
#'
#' @export
cnefe_export <- function(
  code_muni,
  path,
  format = c("parquet", "csv", "csv.gz"),
  year = 2022,
  overwrite = FALSE,
  cache = TRUE,
  cache_dir = NULL,
  verbose = TRUE
) {
  format <- match.arg(format)
  code_muni <- .normalize_code_muni(code_muni)
  year <- .validate_year(year)

  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a single directory path.")
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  outfile <- file.path(
    path,
    sprintf("cnefe_%s_%s.%s", year, code_muni, format)
  )

  if (file.exists(outfile) && !isTRUE(overwrite)) {
    cli::cli_abort(c(
      "{.path {outfile}} already exists.",
      "i" = "Pass {.code overwrite = TRUE} to replace it."
    ))
  }

  tab <- read_cnefe(
    code_muni = code_muni,
    year = year,
    cache = cache,
    cache_dir = cache_dir,
    verbose = verbose,
    output = "arrow"
  )

  if (verbose) {
    cli::cli_progress_step("Writing {.file {basename(outfile)}}")
  }

  if (identical(format, "parquet")) {
    arrow::write_parquet(tab, outfile)
  } else if (identical(format, "csv")) {
    # The delimiter has to be ';', matching what IBGE publishes and what
    # read_cnefe() expects, otherwise the round trip yields a single column.
    arrow::write_csv_arrow(
      tab,
      outfile,
      write_options = arrow::CsvWriteOptions$create(delimiter = ";")
    )
  } else {
    # write_csv_arrow() does not compress, so .csv.gz goes through a gzfile
    # connection on the collected table.
    con <- gzfile(outfile, open = "wb")
    on.exit(close(con), add = TRUE)
    utils::write.table(
      # Materialised on purpose: the whole table is what is being written.
      as.data.frame(tab), con,
      sep = ";", row.names = FALSE, qmethod = "double"
    )
  }

  if (verbose) {
    cli::cli_progress_done()
    size_mb <- file.size(outfile) / 1024^2
    cli::cli_alert_success(
      "Wrote {.val {nrow(tab)}} records to {.path {outfile}} ({sprintf('%.1f', size_mb)} MB)."
    )
  }

  invisible(outfile)
}
