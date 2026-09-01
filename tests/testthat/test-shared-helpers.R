# The helpers extracted in #83 carry the logic that used to sit inside
# tracts_to_h3() and tracts_to_polygon(). Testing them directly is what lets
# that logic have coverage under CRAN defaults: these tests need no DuckDB
# spatial or H3 extension, so none of them calls skip_on_cran().
#
# This is the substance of #80 R1.13, where the referee reports tracts_to_h3()
# at 0%. That figure is what the default configuration produces, because the
# only test exercising the function is skipped on CRAN for a real reason, a
# DuckDB spatial segfault fixed in 0.2.1.

testthat::test_that(".build_alloc_sql() applies the documented allocation rules", {
  sql <- cnefetools:::.build_alloc_sql(c("pop_ph", "pop_ch", "n_resp", "avg_inc_resp", "female"))

  testthat::expect_type(sql, "character")
  testthat::expect_length(sql, 1L)

  # One aliased expression per variable.
  for (v in c("pop_ph", "pop_ch", "n_resp", "avg_inc_resp", "female")) {
    testthat::expect_match(sql, paste0("AS ", v, "_pt"), fixed = TRUE)
  }

  # pop_ph and n_resp are split across private dwellings only.
  testthat::expect_match(sql, "CAST(s.pop_ph AS DOUBLE) / s.n_dom_p", fixed = TRUE)
  testthat::expect_match(sql, "CAST(s.n_resp AS DOUBLE) / s.n_dom_p", fixed = TRUE)

  # pop_ch is split across collective dwellings only.
  testthat::expect_match(sql, "CAST(s.pop_ch AS DOUBLE) / s.n_dom_c", fixed = TRUE)
  testthat::expect_match(sql, "p.COD_ESPECIE = 2", fixed = TRUE)

  # avg_inc_resp is assigned, not divided.
  testthat::expect_match(sql, "THEN CAST(s.avg_inc_resp AS DOUBLE)", fixed = TRUE)
  testthat::expect_false(grepl("s.avg_inc_resp AS DOUBLE) / ", sql, fixed = TRUE))

  # Everything else falls back from private to collective dwellings.
  testthat::expect_match(sql, "WHEN s.n_dom_p > 0 THEN (p.COD_ESPECIE = 1)", fixed = TRUE)
  testthat::expect_match(sql, "WHEN s.n_dom_c > 0 THEN (p.COD_ESPECIE = 2)", fixed = TRUE)
})


testthat::test_that(".build_alloc_sql() joins expressions and handles one variable", {
  one <- cnefetools:::.build_alloc_sql("pop_ph")
  two <- cnefetools:::.build_alloc_sql(c("pop_ph", "female"))

  testthat::expect_false(grepl(",\n", one, fixed = TRUE))
  testthat::expect_true(grepl(",\n", two, fixed = TRUE))
  testthat::expect_identical(cnefetools:::.build_alloc_sql(character(0)), "")
})


testthat::test_that(".build_interp_diagnostics() reports unallocated totals correctly", {
  testthat::skip_if_not_installed("duckdb")

  # Core DuckDB only: no spatial, no H3, nothing from the community repo, which
  # is what keeps this runnable in the default configuration.
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE sc_muni_tbl AS
    SELECT '001' AS code_tract, 100 AS pop_ph, 10 AS n_resp UNION ALL
    SELECT '002', 50, 5 UNION ALL
    SELECT '003', NULL, NULL;
  ")
  DBI::dbExecute(con, "
    CREATE TABLE sc_muni_w_dom AS
    SELECT '001' AS code_tract, 100 AS pop_ph, 10 AS n_resp, 4 AS n_dom_p, 0 AS n_dom_c UNION ALL
    SELECT '002', 50, 5, 0, 0 UNION ALL
    SELECT '003', NULL, NULL, 1, 0;
  ")
  DBI::dbExecute(con, "
    CREATE TABLE cnefe_sc AS
    SELECT 1 AS pt_id, '001' AS code_tract, 1 AS COD_ESPECIE;
  ")
  # Only tract 001 was allocated: 100 of the 150 available.
  DBI::dbExecute(con, "
    CREATE TABLE cnefe_alloc AS
    SELECT 1 AS pt_id, 25.0 AS pop_ph_pt, 2.5 AS n_resp_pt UNION ALL
    SELECT 2, 25.0, 2.5 UNION ALL
    SELECT 3, 25.0, 2.5 UNION ALL
    SELECT 4, 25.0, 2.5;
  ")

  lines <- cnefetools:::.build_interp_diagnostics(
    con,
    vars = c("pop_ph", "n_resp"),
    unmatched_pts = 2L,
    total_pts = 6L
  )

  testthat::expect_type(lines, "character")
  joined <- paste(lines, collapse = " | ")

  # 150 available, 100 allocated, so 50 unallocated, 33.33%.
  testthat::expect_match(joined, "50 of 150")
  testthat::expect_match(joined, "33.33%")
  # Unmatched points are reported against the total given.
  testthat::expect_match(joined, "2 of 6 points")
  # Tract 003 has NA totals for both variables.
  testthat::expect_match(joined, "NA")
  # Tract 002 has a positive total and no eligible dwellings.
  testthat::expect_match(joined, "no eligible dwellings")
})


testthat::test_that(".build_interp_diagnostics() stays silent when everything is allocated", {
  testthat::skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con, "CREATE TABLE sc_muni_tbl AS SELECT '001' AS code_tract, 100 AS pop_ph;")
  DBI::dbExecute(con, "CREATE TABLE sc_muni_w_dom AS SELECT '001' AS code_tract, 100 AS pop_ph, 4 AS n_dom_p, 0 AS n_dom_c;")
  DBI::dbExecute(con, "CREATE TABLE cnefe_sc AS SELECT 1 AS pt_id, '001' AS code_tract, 1 AS COD_ESPECIE;")
  DBI::dbExecute(con, "CREATE TABLE cnefe_alloc AS SELECT 1 AS pt_id, 100.0 AS pop_ph_pt;")

  lines <- cnefetools:::.build_interp_diagnostics(con, "pop_ph", unmatched_pts = 0L, total_pts = 1L)

  # The line is always emitted for consistency, but reports nothing lost.
  testthat::expect_match(paste(lines, collapse = " "), "0 of 100")
  testthat::expect_match(paste(lines, collapse = " "), "0.00%")
  # Nothing about unmatched points or missing totals.
  testthat::expect_false(any(grepl("Unmatched", lines)))
  testthat::expect_false(any(grepl("no eligible dwellings", lines)))
})


testthat::test_that(".report_interp_diagnostics() labels each stage", {
  msgs <- testthat::capture_messages(
    cnefetools:::.report_interp_diagnostics(
      stage1_lines = c("first thing", "second thing"),
      stage2_lines = "third thing",
      stage2_label = "H3 hexagons"
    )
  )
  joined <- paste(msgs, collapse = " ")

  testthat::expect_match(joined, "Dasymetric interpolation diagnostics")
  testthat::expect_match(joined, "Stage 1")
  testthat::expect_match(joined, "Stage 2")
  testthat::expect_match(joined, "H3 hexagons")
  testthat::expect_match(joined, "first thing")
  testthat::expect_match(joined, "third thing")
})


testthat::test_that(".report_interp_diagnostics() says so when nothing was lost", {
  msgs <- testthat::capture_messages(
    cnefetools:::.report_interp_diagnostics(character(0), "all mapped", "Polygons")
  )
  joined <- paste(msgs, collapse = " ")

  testthat::expect_match(joined, "fully allocated")
  testthat::expect_match(joined, "Polygons")
})
