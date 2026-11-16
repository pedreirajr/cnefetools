# Connections and temporary files are released on every exit path (#121,
# comment 3 of the second-round review).
#
# Only the two download boundaries are mocked: `.cnefe_download_zip_with_retry()`
# for CNEFE and `piggyback::pb_download()` for the census tracts. Everything
# else runs for real, including `.cnefe_ensure_zip()`, which is what writes the
# temporary `.csv.gz` when `cache = FALSE`. The referee's scripts in
# `paper/review-2/` use the same approach.

cleanup_code_muni <- 2929057L

# A tiny synthetic municipality: five addresses inside one box, packed the way
# IBGE publishes them (a ZIP holding one semicolon-separated CSV).
cleanup_input <- function(env = parent.frame()) {
  dir <- withr::local_tempdir("cnefetools-input-", .local_envir = env)

  box <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(
    c(xmin = -38.6, ymin = -3.8, xmax = -38.5, ymax = -3.7),
    crs = 4326
  )))

  csv <- file.path(dir, paste0(cleanup_code_muni, "_TEST.csv"))
  utils::write.table(
    data.frame(
      COD_UNICO_ENDERECO = 1:5,
      COD_SETOR = "292905705000001",
      LONGITUDE = c(-38.55, -38.56, -38.54, -38.55, -38.53),
      LATITUDE = c(-3.75, -3.74, -3.76, -3.73, -3.75),
      COD_ESPECIE = c(1L, 1L, 2L, 4L, 1L)
    ),
    csv, sep = ";", row.names = FALSE, quote = FALSE
  )
  zip_file <- file.path(dir, "input.zip")
  zip::zipr(zip_file, csv)

  tract <- data.frame(
    code_tract = "292905705000001",
    pop_ph = 100, pop_ch = 0,
    race_branca = 0, race_preta = 0, race_amarela = 0,
    race_parda = 0, race_indigena = 0
  )
  tract$geom_wkb <- unclass(sf::st_as_binary(
    sf::st_cast(sf::st_geometry(box), "MULTIPOLYGON")
  ))

  far_away <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(
    c(xmin = -50, ymin = -20, xmax = -49.9, ymax = -19.9),
    crs = 4326
  )))

  list(box = box, far_away = far_away, zip = zip_file, tract = tract)
}

# Data files created in tempdir() while `code` runs. Anything left there after
# the function returns (or fails) is a file the package forgot to delete.
new_data_files <- function(code) {
  list_data <- function() {
    list.files(tempdir(), pattern = "[.](csv[.]gz|parquet)$",
               recursive = TRUE, full.names = TRUE)
  }
  before <- list_data()
  result <- tryCatch(code, error = function(e) e)
  left <- setdiff(list_data(), before)
  # Removed here so a failing expectation does not leak into later tests.
  unlink(left)
  list(result = result, left = basename(left))
}

# Runs `code` with the downloads replaced by the synthetic input, and counts
# how many times CNEFE was fetched.
with_fake_downloads <- function(input, code) {
  downloads <- 0L
  out <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      new_data_files(code),
      pb_download = function(file, dest, ...) {
        arrow::write_parquet(input$tract, file.path(dest, file))
      },
      .package = "piggyback"
    ),
    .cnefe_download_zip_with_retry = function(url, destfile, ...) {
      downloads <<- downloads + 1L
      stopifnot(file.copy(input$zip, destfile, overwrite = TRUE))
      invisible(destfile)
    },
    .package = "cnefetools"
  )
  out$downloads <- downloads
  out
}


# -----------------------------------------------------------------------------
# Uncached files, R backend (no DuckDB extension needed)
# -----------------------------------------------------------------------------

testthat::test_that("cnefe_counts(cache = FALSE) downloads once and leaves no file (backend r)", {
  testthat::skip_if_not_installed("zip")
  input <- cleanup_input()

  out <- with_fake_downloads(input, suppressWarnings(cnefetools::cnefe_counts(
    cleanup_code_muni, polygon = input$box, backend = "r",
    cache = FALSE, verbose = FALSE
  )))

  testthat::expect_s3_class(out$result, "sf")
  testthat::expect_equal(out$result$addr_type1, 3L)
  testthat::expect_equal(out$downloads, 1L)
  testthat::expect_length(out$left, 0L)
})

testthat::test_that("compute_lumi(cache = FALSE) downloads once and leaves no file (backend r)", {
  testthat::skip_if_not_installed("zip")
  input <- cleanup_input()

  out <- with_fake_downloads(input, suppressWarnings(cnefetools::compute_lumi(
    cleanup_code_muni, polygon = input$box, backend = "r",
    cache = FALSE, verbose = FALSE
  )))

  testthat::expect_s3_class(out$result, "sf")
  testthat::expect_equal(out$downloads, 1L)
  testthat::expect_length(out$left, 0L)
})

testthat::test_that("the temporary file is deleted when the function fails (backend r)", {
  testthat::skip_if_not_installed("zip")
  input <- cleanup_input()

  # A polygon with no address in it aborts after the download.
  for (fn in c("cnefe_counts", "compute_lumi")) {
    out <- with_fake_downloads(input, suppressWarnings(
      getExportedValue("cnefetools", fn)(
        cleanup_code_muni, polygon = input$far_away, backend = "r",
        cache = FALSE, verbose = FALSE
      )
    ))
    testthat::expect_s3_class(out$result, "error")
    testthat::expect_equal(out$downloads, 1L, info = fn)
    testthat::expect_length(out$left, 0L)
  }
})


# -----------------------------------------------------------------------------
# Uncached files, DuckDB backend
# -----------------------------------------------------------------------------

testthat::test_that("cnefe_counts() and compute_lumi() leave no file on the DuckDB backend", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("zip")
  skip_unless_duckdb_extensions("spatial")
  input <- cleanup_input()

  for (fn in c("cnefe_counts", "compute_lumi")) {
    run <- function(polygon) {
      with_fake_downloads(input, suppressWarnings(
        getExportedValue("cnefetools", fn)(
          cleanup_code_muni, polygon = polygon, backend = "duckdb",
          cache = FALSE, verbose = FALSE
        )
      ))
    }

    ok <- run(input$box)
    testthat::expect_s3_class(ok$result, "sf")
    testthat::expect_equal(ok$downloads, 1L)
    testthat::expect_length(ok$left, 0L)

    failed <- run(input$far_away)
    testthat::expect_s3_class(failed$result, "error")
    testthat::expect_length(failed$left, 0L)
  }
})

testthat::test_that("tracts_to_polygon(cache = FALSE) leaves neither the CNEFE file nor the tract Parquet", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("zip")
  skip_unless_duckdb_extensions("spatial")
  input <- cleanup_input()

  out <- with_fake_downloads(input, suppressMessages(cnefetools::tracts_to_polygon(
    cleanup_code_muni, polygon = input$box, vars = "pop_ph",
    cache = FALSE, verbose = FALSE
  )))

  testthat::expect_s3_class(out$result, "sf")
  testthat::expect_equal(sum(out$result$pop_ph), 100)
  testthat::expect_equal(out$downloads, 1L)
  testthat::expect_length(out$left, 0L)
})


# -----------------------------------------------------------------------------
# Connections
# -----------------------------------------------------------------------------

# Keeps the real configuration step and only records the connection it gets,
# so the test can check afterwards that the connection was closed.
capture_config_con <- function(env = parent.frame()) {
  held <- new.env()
  real <- cnefetools:::.duckdb_apply_user_config
  testthat::local_mocked_bindings(
    .duckdb_apply_user_config = function(con) {
      held$con <- con
      real(con)
    },
    .package = "cnefetools",
    .env = env
  )
  held
}

testthat::test_that("a configuration error does not leave the connection open", {
  testthat::skip_if_not_installed("duckdb")

  # The wrapper gives .duckdb_connect() a frame that exits on the error, the
  # same as in a package function.
  open <- function() cnefetools:::.duckdb_connect(verbose = FALSE)

  bad_configs <- list(
    invalid_setting = list(not_a_duckdb_setting = 1),
    malformed_option = "threads = 4",
    vector_value = list(threads = c(2, 4))
  )

  for (nm in names(bad_configs)) {
    held <- capture_config_con()
    withr::with_options(
      list(cnefetools.duckdb_config = bad_configs[[nm]]),
      testthat::expect_error(open())
    )
    testthat::expect_s4_class(held$con, "duckdb_connection")
    testthat::expect_false(DBI::dbIsValid(held$con), label = nm)
  }

  # Control: a valid configuration closes on normal return too.
  held <- capture_config_con()
  withr::with_options(
    list(cnefetools.duckdb_config = list(threads = 1)),
    open()
  )
  testthat::expect_false(DBI::dbIsValid(held$con))
})

testthat::test_that("a configuration error inside cnefe_counts() closes the connection and deletes the file", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("zip")
  input <- cleanup_input()
  held <- capture_config_con()

  withr::local_options(cnefetools.duckdb_config = list(not_a_duckdb_setting = 1))
  out <- with_fake_downloads(input, cnefetools::cnefe_counts(
    cleanup_code_muni, polygon = input$box, backend = "duckdb",
    cache = FALSE, verbose = FALSE
  ))

  testthat::expect_s3_class(out$result, "error")
  testthat::expect_match(conditionMessage(out$result), "not_a_duckdb_setting")
  testthat::expect_false(DBI::dbIsValid(held$con))
  testthat::expect_length(out$left, 0L)
})
