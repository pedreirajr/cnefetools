# Municipality boundaries read from the release, with the disk cache (#120).
# A local GeoParquet with the two synthetic municipalities of
# helper-multi-muni.R stands in for the release asset, through the option
# `cnefetools.boundary_url`, so nothing touches the network.

write_boundary_fixture <- function(path) {
  idx <- mock_boundary_index()
  wkb <- lapply(
    sf::st_as_binary(c(
      sf::st_geometry(multi_muni$boundary_a),
      sf::st_geometry(multi_muni$boundary_b)
    )),
    as.raw
  )
  tab <- arrow::arrow_table(idx, geometry = arrow::Array$create(wkb, type = arrow::binary()))
  tab$metadata$geo <- '{"version":"1.1.0","primary_column":"geometry","columns":{"geometry":{"encoding":"WKB","geometry_types":["Polygon"]}}}'
  arrow::write_parquet(tab, path)
  path
}

with_boundary_fixture <- function(code, url, ...) {
  testthat::with_mocked_bindings(
    cnefetools:::.read_muni_boundaries(code, ...),
    .boundary_index = mock_boundary_index,
    .package = "cnefetools"
  )
}

skip_unless_boundary_deps <- function() {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("sf")
}

both_codes <- c(multi_muni$code_a, multi_muni$code_b)

testthat::test_that("boundaries are fetched once, cached, and then read from disk (#120)", {
  skip_unless_boundary_deps()
  root <- withr::local_tempdir()
  src <- write_boundary_fixture(file.path(withr::local_tempdir(), "muni.parquet"))
  withr::local_options(cnefetools.boundary_url = src)

  b <- with_boundary_fixture(rev(both_codes), cache_dir = root)
  testthat::expect_s3_class(b, "sf")
  # One row per code, in the order asked for
  testthat::expect_equal(b$code_muni, rev(both_codes))
  testthat::expect_equal(sf::st_crs(b)$epsg, 4326L)
  testthat::expect_true(all(sf::st_equals(
    b[b$code_muni == multi_muni$code_a, ], multi_muni$boundary_a, sparse = FALSE
  )))

  cache_files <- file.path(cnefetools:::.boundary_cache_dir(root), paste0(both_codes, ".parquet"))
  testthat::expect_true(all(file.exists(cache_files)))

  # With the source gone, the cache alone answers, without the fallback
  withr::local_options(cnefetools.boundary_url = file.path(root, "missing.parquet"))
  testthat::expect_no_message(again <- with_boundary_fixture(both_codes, cache_dir = root))
  testthat::expect_equal(again$code_muni, both_codes)
})

testthat::test_that("a corrupt cached boundary is fetched again, not sent to the fallback (#120)", {
  skip_unless_boundary_deps()
  root <- withr::local_tempdir()
  src <- write_boundary_fixture(file.path(withr::local_tempdir(), "muni.parquet"))
  withr::local_options(cnefetools.boundary_url = src)

  dir <- cnefetools:::.boundary_cache_dir(root)
  dir.create(dir, recursive = TRUE)
  bad <- file.path(dir, paste0(multi_muni$code_a, ".parquet"))
  writeLines("not a parquet file", bad)

  testthat::expect_no_message(b <- with_boundary_fixture(both_codes, cache_dir = root))
  testthat::expect_equal(b$code_muni, both_codes)
  testthat::expect_true(all(sf::st_equals(
    b[b$code_muni == multi_muni$code_a, ], multi_muni$boundary_a, sparse = FALSE
  )))
  # The bad file was replaced by a readable one
  testthat::expect_equal(
    as.integer(arrow::read_parquet(bad)$code_muni),
    as.integer(multi_muni$code_a)
  )
})

testthat::test_that("cache = FALSE fetches without writing anything (#120)", {
  skip_unless_boundary_deps()
  root <- withr::local_tempdir()
  src <- write_boundary_fixture(file.path(withr::local_tempdir(), "muni.parquet"))
  withr::local_options(cnefetools.boundary_url = src)

  b <- with_boundary_fixture(multi_muni$code_a, cache = FALSE, cache_dir = root)
  testthat::expect_equal(b$code_muni, multi_muni$code_a)
  testthat::expect_false(dir.exists(cnefetools:::.boundary_cache_dir(root)))
})

testthat::test_that("an unreadable release falls back to geobr, with a message (#120)", {
  skip_unless_boundary_deps()
  root <- withr::local_tempdir()
  withr::local_options(cnefetools.boundary_url = file.path(root, "missing.parquet"))

  testthat::expect_message(
    b <- testthat::with_mocked_bindings(
      cnefetools:::.read_muni_boundaries(multi_muni$code_a, cache_dir = root),
      .boundary_index = mock_boundary_index,
      .read_boundaries_geobr = function(codes, year = 2022L) mock_read_muni_boundaries(codes),
      .package = "cnefetools"
    ),
    "simplified.*geobr"
  )
  testthat::expect_equal(b$code_muni, multi_muni$code_a)
})

testthat::test_that("an unknown code is an error, not a fallback (#120)", {
  testthat::expect_error(
    with_boundary_fixture(1234567L),
    "Unknown municipality code"
  )
})

testthat::test_that("clear_cache_boundaries() deletes all or some, and clear_cache_tracts() leaves them (#120)", {
  root <- withr::local_tempdir()
  bdir <- cnefetools:::.boundary_cache_dir(root, 2022)
  sc <- cnefetools:::.sc_cache_dir(root, 2022)
  dir.create(bdir, recursive = TRUE)
  dir.create(sc, recursive = TRUE)
  file.create(file.path(bdir, c("2927408.parquet", "2919207.parquet")))
  file.create(file.path(sc, "sc_29.parquet"))

  testthat::expect_length(clear_cache_tracts(cache_dir = root, verbose = FALSE), 1L)
  testthat::expect_true(all(file.exists(file.path(bdir, c("2927408.parquet", "2919207.parquet")))))

  deleted <- clear_cache_boundaries(2919207, cache_dir = root, verbose = FALSE)
  testthat::expect_equal(basename(deleted), "2919207.parquet")
  testthat::expect_true(file.exists(file.path(bdir, "2927408.parquet")))

  testthat::expect_message(
    clear_cache_boundaries(cache_dir = root),
    "Deleted 1 cached boundary"
  )
  testthat::expect_false(file.exists(file.path(bdir, "2927408.parquet")))
  testthat::expect_message(
    clear_cache_boundaries(cache_dir = root),
    "No cached municipality boundaries"
  )
})
