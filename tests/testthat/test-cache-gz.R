# The cache stores a gzipped CSV rather than the published ZIP (#80 R1.11).
# Measured in data-raw/bench_gz_vs_zip.R: 2.29x faster DuckDB reads on
# Fortaleza at the same size on disk, and it removes the community zipfs
# extension from the normal read path.

make_zip <- function(dir, header = "COD_UNICO_ENDERECO;LONGITUDE;LATITUDE") {
  csv <- file.path(dir, "inner.csv")
  writeLines(c(header, "1;-38.5;-12.9", "2;-38.4;-12.8"), csv)
  z <- file.path(dir, "muni.zip")
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  utils::zip(basename(z), "inner.csv", flags = "-q")
  z
}


testthat::test_that(".cnefe_zip_to_gz() produces a readable gzipped CSV (#80 R1.11)", {
  d <- file.path(tempdir(), "gzconv")
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  z <- make_zip(d)
  gz <- file.path(d, "out.csv.gz")
  cnefetools:::.cnefe_zip_to_gz(z, gz, verbose = FALSE)

  testthat::expect_true(file.exists(gz))
  con <- gzfile(gz, "rt")
  on.exit(close(con), add = TRUE)
  testthat::expect_identical(readLines(con, n = 1L), "COD_UNICO_ENDERECO;LONGITUDE;LATITUDE")

  # The intermediate .part must not survive a successful conversion.
  testthat::expect_false(file.exists(paste0(gz, ".part")))
})


testthat::test_that(".cnefe_csv_uri() picks the right reader per format (#80 R1.11)", {
  d <- file.path(tempdir(), "gzuri")
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  gz <- file.path(d, "x.csv.gz")
  con <- gzfile(gz, "wb")
  writeLines("COD_UNICO_ENDERECO;LONGITUDE;LATITUDE", con)
  close(con)

  # Gzip is read from a plain path, so no community extension is needed.
  res <- cnefetools:::.cnefe_csv_uri(gz)
  testthat::expect_false(res$needs_zipfs)
  testthat::expect_false(grepl("^zip://", res$uri))

  # A ZIP, which a cache written by an older version may still hold, keeps
  # working through the zipfs route.
  z <- make_zip(d)
  res <- cnefetools:::.cnefe_csv_uri(z)
  testthat::expect_true(res$needs_zipfs)
  testthat::expect_match(res$uri, "^zip://")
  testthat::expect_match(res$uri, "inner[.]csv$")
})


testthat::test_that("a corrupt cache entry is detected and discarded (#80 R1.11)", {
  d <- file.path(tempdir(), "gzbad")
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  # Not gzip at all.
  bad <- file.path(d, "bad.csv.gz")
  writeLines("this is not gzip", bad)
  testthat::expect_error(cnefetools:::.cnefe_read_local(bad, verbose = FALSE))

  # Valid gzip, but the wrong contents: the header check is what catches this,
  # since a plain-text file opened through gzfile() reads back fine.
  wrong <- file.path(d, "wrong.csv.gz")
  con <- gzfile(wrong, "wb")
  writeLines("a;b;c", con)
  close(con)
  con <- gzfile(wrong, "rt")
  header <- readLines(con, n = 1L, warn = FALSE)
  close(con)
  testthat::expect_false(grepl("COD_UNICO_ENDERECO|LONGITUDE", header))
})
