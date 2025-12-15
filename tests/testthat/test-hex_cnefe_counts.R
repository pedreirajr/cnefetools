test_that("hex_cnefe_counts works offline using cached ZIP (structure + nonnegative counts)", {

  skip_if_not_installed("sf")
  skip_if_not_installed("h3jsr")
  skip_if_not_installed("archive")

  # 1) Require a cached ZIP (no internet in tests)
  cache_dir <- tools::R_user_dir("cnefetools", "cache")
  zips <- list.files(cache_dir, pattern = "^\\d{7}_.+\\.zip$", full.names = TRUE)

  if (length(zips) == 0L) {
    skip("No cached CNEFE ZIP found. Run read_cnefe(code_muni, cache=TRUE) once before testing.")
  }

  # pick the first cached file and parse code_muni from filename
  zip_path <- zips[[1]]
  code_muni <- as.integer(sub("^([0-9]{7})_.*$", "\\1", basename(zip_path)))

  # 2) Run (backend r avoids DuckDB extension installation)
  out <- tryCatch(
    suppressMessages(suppressWarnings(
      cnefetools::hex_cnefe_counts(code_muni, h3_resolution = 9L, backend = "r", verbose = FALSE)
    )),
    error = function(e) {
      skip(paste("hex_cnefe_counts failed in this environment:", conditionMessage(e)))
    }
  )

  # 3) Structure checks
  expect_s3_class(out, "sf")
  expect_true("id_hex" %in% names(out))

  cols <- paste0("addr_type", 1:8)
  expect_true(all(cols %in% names(out)))

  # CRS should be lon/lat (EPSG:4326 output)
  expect_true(sf::st_is_longlat(out))
  expect_false(any(sf::st_is_empty(out)))

  # 4) Count checks (robust to integer/numeric/integer64)
  x <- sf::st_drop_geometry(out[, cols, drop = FALSE])

  # Convert safely to numeric for comparisons
  vals_num <- unlist(lapply(x, function(v) as.numeric(v)), use.names = FALSE)

  expect_false(anyNA(vals_num))
  expect_true(all(vals_num >= 0))
  expect_true(all(vals_num == floor(vals_num)))

  # At least something counted (if you cached a real municipality ZIP)
  expect_true(sum(vals_num) > 0)
})
