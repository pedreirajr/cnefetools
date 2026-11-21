# Census tracts read by municipality from the release, with the disk cache
# (#120). A local state file with the tracts of the two synthetic
# municipalities of helper-multi-muni.R stands in for the release asset,
# through the option `cnefetools.sc_url`, so nothing touches the network.

skip_unless_sc_deps <- function() {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("sf")
}

# The fixture state file, at <dir>/sc_30.parquet, and the sprintf template
# that points .sc_url() at it
local_sc_source <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  file.copy(tract_parquet_multi(), file.path(dir, "sc_30.parquet"))
  file.path(dir, "sc_%s.parquet")
}

sc_codes <- c(multi_muni$code_a, multi_muni$code_b)

testthat::test_that("tracts are fetched by municipality, cached, and then read from disk (#120)", {
  skip_unless_sc_deps()
  root <- withr::local_tempdir()
  withr::local_options(cnefetools.sc_url = local_sc_source())

  paths <- cnefetools:::.sc_ensure_parquet_munis("30", sc_codes, cache_dir = root,
                                                 year = 2022L, verbose = FALSE)
  muni_dir <- cnefetools:::.sc_muni_cache_dir(root, 2022L)
  testthat::expect_equal(normalizePath(paths), normalizePath(file.path(muni_dir, paste0(sc_codes, ".parquet"))))

  # Each file holds only the tracts of its municipality, with every column
  a <- arrow::read_parquet(paths[1])
  testthat::expect_equal(a$code_tract, "300000100000001")
  testthat::expect_setequal(names(a), c("code_tract", "pop_ph", "pop_ch", "geom_wkb"))

  # With the source gone, the cache alone answers, without the fallback
  withr::local_options(cnefetools.sc_url = file.path(root, "missing_%s.parquet"))
  testthat::expect_no_message(
    again <- cnefetools:::.sc_ensure_parquet_munis("30", sc_codes, cache_dir = root,
                                                   year = 2022L, verbose = FALSE)
  )
  testthat::expect_equal(normalizePath(again), normalizePath(paths))
})

testthat::test_that("a cached state file is used without any remote read (#120)", {
  skip_unless_sc_deps()
  root <- withr::local_tempdir()
  state <- cnefetools:::.sc_asset_local_path("30", root, 2022L)
  dir.create(dirname(state), recursive = TRUE)
  # A valid state file needs the fields .validate_sc_parquet() checks
  arrow::write_parquet(
    data.frame(code_tract = "300000100000001", geom_wkb = I(list(as.raw(1))),
               pop_ph = 1, pop_ch = 0, race_branca = 0, race_preta = 0,
               race_amarela = 0, race_parda = 0, race_indigena = 0),
    state
  )
  withr::local_options(cnefetools.sc_url = file.path(root, "missing_%s.parquet"))

  testthat::expect_no_message(
    paths <- cnefetools:::.sc_ensure_parquet_munis("30", sc_codes, cache_dir = root,
                                                   year = 2022L, verbose = FALSE)
  )
  testthat::expect_equal(paths, state)
  testthat::expect_false(dir.exists(cnefetools:::.sc_muni_cache_dir(root, 2022L)))
})

testthat::test_that("cache = FALSE writes the tracts to the temporary folder only (#120)", {
  skip_unless_sc_deps()
  root <- withr::local_tempdir()
  withr::local_options(cnefetools.sc_url = local_sc_source())

  paths <- cnefetools:::.sc_ensure_parquet_munis("30", multi_muni$code_a, cache = FALSE,
                                                 cache_dir = root, year = 2022L, verbose = FALSE)
  on.exit(unlink(paths), add = TRUE)
  testthat::expect_true(cnefetools:::.sc_is_tmp_download(paths))
  testthat::expect_false(dir.exists(cnefetools:::.sc_cache_dir(root, 2022L)))
})

testthat::test_that("an unreadable release falls back to the whole-state download (#120)", {
  skip_unless_sc_deps()
  root <- withr::local_tempdir()
  withr::local_options(cnefetools.sc_url = file.path(root, "missing_%s.parquet"))

  state_called <- NULL
  testthat::expect_message(
    paths <- testthat::with_mocked_bindings(
      cnefetools:::.sc_ensure_parquet_munis("30", sc_codes, cache_dir = root,
                                            year = 2022L, verbose = FALSE),
      .sc_ensure_parquet_uf = function(uf, ...) {
        state_called <<- uf
        "state-file.parquet"
      },
      .package = "cnefetools"
    ),
    "whole file of state"
  )
  testthat::expect_equal(state_called, "30")
  testthat::expect_equal(paths, "state-file.parquet")
  # Nothing half-written is left behind
  testthat::expect_length(
    list.files(cnefetools:::.sc_muni_cache_dir(root, 2022L)), 0L
  )
})

testthat::test_that("tracts_to_h3() gives the same result from the per-municipality files (#120)", {
  skip_unless_sc_deps()
  skip_unless_multi_deps("duckdb", polygon = TRUE)
  skip_unless_duckdb_extensions("h3")
  root <- withr::local_tempdir()
  withr::local_options(cnefetools.sc_url = local_sc_source())

  # run_tracts() serves the state file directly, the reference result
  reference <- run_tracts(cnefetools::tracts_to_h3, sc_codes, vars = "pop_ph",
                          h3_resolution = multi_muni$res)

  files <- list(
    "3000001" = write_cnefe_gz(tract_multi$records_a),
    "3000002" = write_cnefe_gz(tract_multi$records_b)
  )
  on.exit(unlink(unlist(files)), add = TRUE)
  by_muni <- testthat::with_mocked_bindings(
    suppressWarnings(suppressMessages(cnefetools::tracts_to_h3(
      sc_codes, vars = "pop_ph", h3_resolution = multi_muni$res,
      cache_dir = root, verbose = FALSE
    ))),
    .cnefe_ensure_zip = function(code_muni, ...) {
      list(zip_path = files[[as.character(code_muni)]], cleanup_zip = FALSE, url = "synthetic")
    },
    .read_muni_boundaries = mock_read_muni_boundaries,
    .boundary_index = mock_boundary_index,
    .package = "cnefetools"
  )

  testthat::expect_true(all(file.exists(file.path(
    cnefetools:::.sc_muni_cache_dir(root, 2022L), paste0(sc_codes, ".parquet")
  ))))
  testthat::expect_equal(
    sf::st_drop_geometry(by_muni)[order(by_muni$id_hex), ],
    sf::st_drop_geometry(reference)[order(reference$id_hex), ],
    ignore_attr = TRUE
  )
})

testthat::test_that("clear_cache_tracts() removes the per-municipality files of a state (#120)", {
  root <- withr::local_tempdir()
  muni_dir <- cnefetools:::.sc_muni_cache_dir(root, 2022L)
  dir.create(muni_dir, recursive = TRUE)
  file.create(file.path(muni_dir, c("2927408.parquet", "2611101.parquet")))
  file.create(file.path(dirname(muni_dir), "sc_29.parquet"))

  deleted <- clear_cache_tracts("BA", cache_dir = root, verbose = FALSE)
  testthat::expect_setequal(basename(deleted), c("2927408.parquet", "sc_29.parquet"))
  testthat::expect_true(file.exists(file.path(muni_dir, "2611101.parquet")))

  testthat::expect_length(clear_cache_tracts(cache_dir = root, year = 2022, verbose = FALSE), 1L)
})
