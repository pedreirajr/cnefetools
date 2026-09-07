# Since extension v1.5.5 the DuckDB community repository serves an arm64 build
# of `h3` under the osx_amd64 path, so `LOAD h3` fails on Intel macOS with a raw
# dlopen() dump that reads as a cnefetools bug. These tests pin the translation
# of that dump into an actionable message (#99). They construct the condition
# directly rather than reproducing the platform, so they run everywhere and need
# no DuckDB extension.

.arch_error <- function() {
  simpleError(paste0(
    "Invalid Error: IO Error: Extension ",
    "\"/tmp/duckdb/extensions/v1.5.5/osx_amd64/h3.duckdb_extension\" could not ",
    "be loaded: dlopen(/tmp/duckdb/extensions/v1.5.5/osx_amd64/",
    "h3.duckdb_extension, 0x0006): tried: '/tmp/duckdb/extensions/v1.5.5/",
    "osx_amd64/h3.duckdb_extension' (mach-o file, but is an incompatible ",
    "architecture (have 'arm64', need 'x86_64h' or 'x86_64'))"
  ))
}


testthat::test_that(".duckdb_extension_abort() names the platform, not the package", {
  err <- testthat::expect_error(
    cnefetools:::.duckdb_extension_abort("h3", .arch_error()),
    class = "cnefetools_extension_unavailable"
  )

  msg <- conditionMessage(err)

  testthat::expect_match(msg, "no usable build for this platform")
  testthat::expect_match(msg, "different architecture")
  # The point of the message: this is not our bug.
  testthat::expect_match(msg, "upstream in DuckDB")
  testthat::expect_match(msg, "community")
})


testthat::test_that(".duckdb_extension_abort() suggests the fallback only when there is one", {
  with_fallback <- testthat::expect_error(
    cnefetools:::.duckdb_extension_abort(
      "h3",
      .arch_error(),
      fallback = "backend = \"r\""
    ),
    class = "cnefetools_extension_unavailable"
  )
  testthat::expect_match(conditionMessage(with_fallback), "backend = ", fixed = TRUE)

  # tracts_to_h3() and tracts_to_polygon() have no non-DuckDB path by design
  # (#80 R2.6), so they must not be told to use one that does not exist.
  without <- testthat::expect_error(
    cnefetools:::.duckdb_extension_abort("h3", .arch_error()),
    class = "cnefetools_extension_unavailable"
  )
  testthat::expect_false(grepl("pure-R backend", conditionMessage(without), fixed = TRUE))
})


testthat::test_that(".duckdb_extension_abort() trims the dlopen dump but keeps the original", {
  err <- testthat::expect_error(
    cnefetools:::.duckdb_extension_abort("h3", .arch_error()),
    class = "cnefetools_extension_unavailable"
  )

  msg <- conditionMessage(err)
  testthat::expect_match(msg, "DuckDB reported:")
  # The dump repeats the same path four times and is far longer than this.
  testthat::expect_lt(nchar(msg), 900L)
})


testthat::test_that(".duckdb_extension_abort() passes other failures through plainly", {
  err <- testthat::expect_error(
    cnefetools:::.duckdb_extension_abort(
      "zipfs",
      simpleError("Invalid Error: could not reach the extension repository"),
      fallback = "backend = \"r\""
    ),
    class = "cnefetools_extension_unavailable"
  )

  msg <- conditionMessage(err)
  testthat::expect_match(msg, "could not be loaded")
  # No architecture claim when the error says nothing about architecture.
  testthat::expect_false(grepl("different architecture", msg, fixed = TRUE))
  testthat::expect_match(msg, "could not reach the extension repository")
})


testthat::test_that(".duckdb_ensure_extension() routes a failed LOAD through the helper", {
  fake_con <- structure(list(), class = "cnefetools_fake_con")

  testthat::expect_error(
    testthat::with_mocked_bindings(
      cnefetools:::.duckdb_ensure_extension(
        fake_con,
        "h3",
        fallback = "backend = \"r\""
      ),
      dbGetQuery = function(...) data.frame(installed = TRUE, loaded = FALSE),
      dbExecute = function(...) stop(.arch_error()),
      .package = "DBI"
    ),
    class = "cnefetools_extension_unavailable"
  )
})
