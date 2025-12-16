# data-raw/cnefe_fixture_zip.R
#
# Generates a tiny CNEFE-like ZIP fixture used by tests.
# Output: inst/extdata/cnefe_fixture_cnefe.zip

generate_cnefe_fixture_zip <- function() {

  fixture_dir <- file.path("inst", "extdata")
  dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

  # Quick sanity check: is the directory writable?
  write_test <- file.path(fixture_dir, "_write_test.tmp")
  ok_write <- tryCatch(file.create(write_test), error = function(e) FALSE)
  if (!isTRUE(ok_write)) {
    stop("inst/extdata is not writable: ", normalizePath(fixture_dir, winslash = "\\", mustWork = FALSE))
  }
  unlink(write_test)

  tmp_dir <- file.path(fixture_dir, "_tmp_fixture")
  if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE, force = TRUE)
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  csv_name <- "cnefe_fixture.csv"
  zip_name <- "cnefe_fixture_cnefe.zip"

  csv_path <- file.path(tmp_dir, csv_name)
  zip_path <- file.path(fixture_dir, zip_name)

  csv_lines <- c(
    "LONGITUDE;LATITUDE;COD_ESPECIE",
    "-38.5200;-3.7300;1",
    "-38.5200;-3.7300;2",
    "-38.5500;-3.7500;1",
    "-38.5500;-3.7500;8",
    "-38.5500;-3.7500;7",
    "-38.5400;-3.7400;6",
    "-38.5400;;5",
    "-38.5300;-3.7350;9"
  )

  writeLines(csv_lines, csv_path, useBytes = TRUE)
  stopifnot(file.exists(csv_path))

  if (file.exists(zip_path)) unlink(zip_path)

  # 1) Prefer base R zip (zip.exe). Use -j to store only the filename inside the ZIP.
  ok_zip <- tryCatch({
    utils::zip(
      zipfile = normalizePath(zip_path, winslash = "\\", mustWork = FALSE),
      files   = normalizePath(csv_path, winslash = "\\", mustWork = FALSE),
      flags   = "-j"
    )
    file.exists(zip_path)
  }, error = function(e) FALSE)

  # 2) Fallback: PowerShell Compress-Archive (Windows)
  if (!isTRUE(ok_zip)) {
    cmd <- sprintf(
      "Compress-Archive -Path %s -DestinationPath %s -Force",
      shQuote(normalizePath(csv_path, winslash = "\\", mustWork = FALSE)),
      shQuote(normalizePath(zip_path, winslash = "\\", mustWork = FALSE))
    )
    ok_zip <- tryCatch({
      system2("powershell", c("-NoProfile", "-Command", cmd), stdout = TRUE, stderr = TRUE)
      file.exists(zip_path)
    }, error = function(e) FALSE)
  }

  if (!isTRUE(ok_zip)) {
    stop("Failed to create fixture ZIP at: ", zip_path)
  }

  # Validate: list contents (base unzip)
  lst <- utils::unzip(zip_path, list = TRUE)
  if (!any(tolower(lst$Name) == tolower(csv_name))) {
    stop("Fixture ZIP created, but does not contain ", csv_name)
  }

  # Cleanup
  unlink(tmp_dir, recursive = TRUE, force = TRUE)

  message("Fixture ZIP written to: ", zip_path)
}

generate_cnefe_fixture_zip()
