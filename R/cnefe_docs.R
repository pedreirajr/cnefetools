#' Open the official CNEFE data dictionary
#'
#' Opens the bundled Excel data dictionary in the system's default
#' spreadsheet viewer (e.g., Excel, LibreOffice).
#'
#' @param year Integer. The CNEFE data year. Currently only 2022 is supported.
#'
#' @return Invisibly, the path to the Excel file inside the installed package.
#'
#' @examples
#' \dontrun{
#' cnefe_dictionary()
#' }
#'
#' @export
cnefe_dictionary <- function(year = 2022) {
  year <- .validate_year(year)

  filename <- sprintf("cnefe_dictionary_%d.xls", year)
  path <- system.file(
    "extdata",
    filename,
    package = "cnefetools"
  )

  if (!nzchar(path)) {
    cli::cli_abort(
      "Bundled CNEFE dictionary file for year {.val {year}} not found."
    )
  }

  utils::browseURL(path)
  invisible(path)
}

#' Open the official CNEFE methodological note
#'
#' Opens the bundled PDF methodological document in the system's
#' default PDF viewer.
#'
#' @param year Integer. The CNEFE data year. Currently only 2022 is supported.
#'
#' @return Invisibly, the path to the PDF file inside the installed package.
#'
#' @examples
#' \dontrun{
#' cnefe_doc()
#' }
#'
#' @export
cnefe_doc <- function(year = 2022) {
  year <- .validate_year(year)

  filename <- sprintf("cnefe_metodologica_%d.pdf", year)
  path <- system.file(
    "extdata",
    filename,
    package = "cnefetools"
  )

  if (!nzchar(path)) {
    cli::cli_abort(
      "Bundled CNEFE methodological note for year {.val {year}} not found."
    )
  }

  utils::browseURL(path)
  invisible(path)
}
