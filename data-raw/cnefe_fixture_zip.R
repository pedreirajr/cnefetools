# data-raw/cnefe_fixture_zip.R
#
# Generates a tiny CNEFE-like ZIP fixture used by tests.
# Output: inst/extdata/cnefe_fixture_cnefe.zip

if (!requireNamespace("archive", quietly = TRUE)) {
  stop("Package 'archive' is required to generate the fixture ZIP.")
}

fixture_dir <- file.path("inst", "extdata")
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)

csv_name <- "cnefe_fixture.csv"
zip_name <- "cnefe_fixture_cnefe.zip"

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

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(fixture_dir)

writeLines(csv_lines, csv_name, useBytes = TRUE)

if (file.exists(zip_name)) unlink(zip_name)

# Write ZIP with only this file (no extra paths inside the archive)
archive::archive_write_files(archive = zip_name, file = csv_name)

# Validate ZIP integrity
archive::archive(zip_name)

# Clean up plain CSV (keep only the ZIP)
unlink(csv_name)

message("Fixture ZIP written to: ", file.path(old_wd, fixture_dir, zip_name))
