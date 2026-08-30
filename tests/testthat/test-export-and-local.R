# Local ingestion and export, from #80 R1.11 and R2.7. Everything here works on
# a small in-memory table written to tempdir(), so no test needs the network.

toy <- function(n = 5L) {
  data.frame(
    COD_UNICO_ENDERECO = seq_len(n),
    COD_ESPECIE = rep(1L, n),
    LONGITUDE = seq(-38.5, -38.4, length.out = n),
    LATITUDE = seq(-12.9, -12.8, length.out = n),
    stringsAsFactors = FALSE
  )
}

write_toy <- function(dir, fmt) {
  df <- toy()
  p <- file.path(dir, paste0("toy.", fmt))
  if (fmt == "parquet") {
    arrow::write_parquet(df, p)
  } else if (fmt == "csv") {
    utils::write.table(df, p, sep = ";", row.names = FALSE, qmethod = "double")
  } else if (fmt == "csv.gz") {
    con <- gzfile(p, open = "wb")
    on.exit(close(con), add = TRUE)
    utils::write.table(df, con, sep = ";", row.names = FALSE, qmethod = "double")
  } else if (fmt == "zip") {
    csv <- file.path(dir, "toy_inner.csv")
    utils::write.table(df, csv, sep = ";", row.names = FALSE, qmethod = "double")
    old <- setwd(dir)
    on.exit(setwd(old), add = TRUE)
    utils::zip(basename(p), "toy_inner.csv", flags = "-q")
  }
  p
}


testthat::test_that(".cnefe_read_local() reads every supported format (#80 R2.7)", {
  testthat::skip_if_not_installed("arrow")
  d <- file.path(tempdir(), "readlocal")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  for (fmt in c("parquet", "csv", "csv.gz")) {
    p <- write_toy(d, fmt)
    tab <- cnefetools:::.cnefe_read_local(p, verbose = FALSE)
    testthat::expect_s3_class(tab, "Table")
    testthat::expect_identical(nrow(tab), 5L, info = fmt)
    testthat::expect_true("LONGITUDE" %in% names(tab), info = fmt)
  }
})


testthat::test_that(".cnefe_read_local() rejects bad input clearly (#80 R2.7)", {
  testthat::expect_error(cnefetools:::.cnefe_read_local(c("a", "b")), "single path")
  testthat::expect_error(cnefetools:::.cnefe_read_local("/no/such/file.parquet"), "does not exist")

  bad <- tempfile(fileext = ".txt")
  writeLines("x", bad)
  on.exit(unlink(bad), add = TRUE)
  testthat::expect_error(cnefetools:::.cnefe_read_local(bad), "Unsupported file type")
})


testthat::test_that("read_cnefe() requires exactly one of code_muni and file (#80 R2.7)", {
  testthat::expect_error(read_cnefe(verbose = FALSE), "Supply either")
  testthat::expect_error(
    read_cnefe(code_muni = 2919207, file = "x.parquet", verbose = FALSE),
    "not both"
  )
})


testthat::test_that("read_cnefe(file =) returns the same shape as the download path (#80 R2.7)", {
  testthat::skip_if_not_installed("arrow")
  d <- file.path(tempdir(), "readlocal2")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  p <- write_toy(d, "parquet")

  tab <- read_cnefe(file = p, verbose = FALSE)
  testthat::expect_s3_class(tab, "Table")
  testthat::expect_identical(nrow(tab), 5L)

  # The sf branch is shared with the download path through
  # .cnefe_finalise_output(), so both return the same object for the same data.
  pts <- read_cnefe(file = p, output = "sf", verbose = FALSE)
  testthat::expect_s3_class(pts, "sf")
  testthat::expect_identical(nrow(pts), 5L)
  testthat::expect_identical(sf::st_crs(pts)$epsg, 4674L)
})


testthat::test_that("cnefe_export() validates its arguments before downloading (#80 R1.11)", {
  testthat::expect_error(cnefe_export(2919207, path = c("a", "b")), "single directory path")
  testthat::expect_error(cnefe_export(2919207, path = ""), "single directory path")
  testthat::expect_error(
    cnefe_export(2919207, path = tempdir(), format = "rds"),
    "should be one of"
  )
})


testthat::test_that("cnefe_export() refuses to clobber without overwrite (#80 R1.11)", {
  d <- file.path(tempdir(), "exportguard")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  # The guard fires before any download, which is the point: these files are
  # expensive to produce and must not be lost to a stray re-run.
  existing <- file.path(d, "cnefe_2022_2919207.parquet")
  file.create(existing)

  testthat::expect_error(
    cnefe_export(2919207, path = d, verbose = FALSE),
    "already exists"
  )
})
