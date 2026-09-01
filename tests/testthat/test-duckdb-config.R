# The `cnefetools.duckdb_config` option (#80 R2.8b, R2.8c).
#
# Left unset, DuckDB takes one thread per logical core and 80% of installed
# RAM. These tests pin the behaviour that lets a user hold it back, since the
# response to the referee reports measurements taken through this option and a
# silent regression would invalidate them.
#
# No extension is needed, so nothing here is skipped on CRAN.

threads_of <- function(con) {
  as.integer(DBI::dbGetQuery(con, "SELECT current_setting('threads') AS v")$v[1])
}

testthat::test_that("the option is opt-in and does not leak between connections", {
  testthat::skip_if_not_installed("duckdb")

  # Asserted relative to DuckDB's own default rather than against a fixed
  # number: the default is machine-dependent, and what matters is that the
  # package leaves it alone unless asked, and restores it afterwards.
  withr::local_options(cnefetools.duckdb_config = NULL)
  default_threads <- threads_of(cnefetools:::.duckdb_connect(verbose = FALSE))
  testthat::expect_gt(default_threads, 0L)

  set_threads <- withr::with_options(
    list(cnefetools.duckdb_config = list(threads = 1)),
    threads_of(cnefetools:::.duckdb_connect(verbose = FALSE))
  )
  testthat::expect_equal(set_threads, 1L)

  # Back to the default once the option is gone.
  testthat::expect_equal(
    threads_of(cnefetools:::.duckdb_connect(verbose = FALSE)),
    default_threads
  )
})


testthat::test_that("the option is applied to the connection", {
  testthat::skip_if_not_installed("duckdb")

  withr::local_options(cnefetools.duckdb_config = list(
    threads = 2,
    memory_limit = "1GB"
  ))
  con <- cnefetools:::.duckdb_connect(verbose = FALSE)

  got <- DBI::dbGetQuery(
    con,
    "SELECT current_setting('threads') AS threads,
            current_setting('memory_limit') AS mem"
  )

  testthat::expect_equal(as.integer(got$threads), 2L)

  # DuckDB normalises the limit into its own units and picks the unit by
  # magnitude, so "1GB" comes back as "953.6 MiB". Assert that a limit was
  # applied and that it is far below the untouched default, rather than
  # matching a literal string that depends on DuckDB's formatting.
  default_mem <- withr::with_options(
    list(cnefetools.duckdb_config = NULL),
    DBI::dbGetQuery(
      cnefetools:::.duckdb_connect(verbose = FALSE),
      "SELECT current_setting('memory_limit') AS mem"
    )$mem[1]
  )
  testthat::expect_false(identical(got$mem[1], default_mem))
  testthat::expect_match(got$mem[1], "^[0-9.]+ [KMG]iB$")
})


testthat::test_that("an unrecognised setting fails loudly and names itself", {
  testthat::skip_if_not_installed("duckdb")

  withr::local_options(cnefetools.duckdb_config = list(
    not_a_duckdb_setting = 1
  ))

  testthat::expect_error(
    cnefetools:::.duckdb_connect(verbose = FALSE),
    "not_a_duckdb_setting"
  )
})


testthat::test_that("a malformed option is rejected before reaching DuckDB", {
  testthat::skip_if_not_installed("duckdb")

  withr::local_options(cnefetools.duckdb_config = "threads = 4")
  testthat::expect_error(
    cnefetools:::.duckdb_connect(verbose = FALSE),
    "named list"
  )

  withr::local_options(cnefetools.duckdb_config = list(4))
  testthat::expect_error(
    cnefetools:::.duckdb_connect(verbose = FALSE),
    "named list"
  )

  withr::local_options(cnefetools.duckdb_config = list(threads = c(2, 4)))
  testthat::expect_error(
    cnefetools:::.duckdb_connect(verbose = FALSE),
    "single non-missing value"
  )
})
