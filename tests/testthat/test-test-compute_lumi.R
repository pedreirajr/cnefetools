test_that("compute_lumi returns an sf with expected structure", {
  skip_on_cran()

  # skip_if(
  #   Sys.getenv("CNEFETOOLS_RUN_COMPUTE_LUMI_TESTS") == "",
  #   "Set CNEFETOOLS_RUN_COMPUTE_LUMI_TESTS=1 to run this test (network + heavy)."
  # )

  # Use a small city
  res <- compute_lumi( 2929057, h3_resolution = 8, verbose = FALSE)

  # Basic structure
  expect_s3_class(res, "sf")
  expect_true(nrow(res) > 0)

  # Only the columns we really want to expose
  expected_cols <- c(
    "id_hex", "p_res", "ei", "hhi", "hhi_adp", "bgbi", "geometry"
  )
  expect_true(all(expected_cols %in% names(res)))

  # Type checks
  df <- res %>% sf::st_drop_geometry()

  expect_type(df$id_hex, "character")
  expect_true(is.numeric(df$p_res))
  expect_true(is.numeric(df$ei))
  expect_true(is.numeric(df$hhi))
  expect_true(is.numeric(df$hhi_adp))
  expect_true(is.numeric(df$bgbi))

  # p_res em [0, 1]
  not_na_p <- !is.na(df$p_res)
  expect_true(all(df$p_res[not_na_p] >= 0))
  expect_true(all(df$p_res[not_na_p] <= 1 + 1e-8))

  # ei e hhi em [0, 1]
  for (v in c("ei", "hhi")) {
    vals <- df[[v]]
    vals <- vals[!is.na(vals)]
    expect_true(all(vals >= 0 - 1e-8))
    expect_true(all(vals <= 1 + 1e-8))
  }

  # hhi_adp e bgbi em [-1, 1]
  for (v in c("hhi_adp", "bgbi")) {
    vals <- df[[v]]
    vals <- vals[!is.na(vals)]
    expect_true(all(vals >= -1 - 1e-8))
    expect_true(all(vals <=  1 + 1e-8))
  }
})
