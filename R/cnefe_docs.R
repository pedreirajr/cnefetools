#' Open the official CNEFE 2022 data dictionary
#'
#' Opens the bundled Excel data dictionary in the system's default
#' spreadsheet viewer (e.g., Excel, LibreOffice).
#'
#' @return Invisibly, the path to the Excel file inside the installed package.
#' @export
cnefe_dictionary <- function() {
  path <- system.file(
    "extdata",
    "cnefe_dictionary_2022.xls",
    package = "cnefetools"
  )

  if (!nzchar(path)) {
    rlang::abort(
      "Bundled CNEFE dictionary file not found in installed package."
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
#' @return Invisibly, the path to the PDF file inside the installed package.
#' @export
cnefe_doc <- function() {
  path <- system.file(
    "extdata",
    "cnefe_metodologica_2024.pdf",
    package = "cnefetools"
  )

  if (!nzchar(path)) {
    rlang::abort(
      "Bundled CNEFE methodological note not found in installed package."
    )
  }

  utils::browseURL(path)
  invisible(path)
}
