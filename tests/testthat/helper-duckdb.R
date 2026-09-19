# Skip unless the DuckDB extensions a test needs can be loaded.
#
# This goes through the package's own .duckdb_connect(), so a test is skipped
# for exactly the reasons the package itself would fail: it knows that h3 is a
# community extension and spatial a core one, which a plain `INSTALL h3` in the
# test does not. The tests used to require zipfs as well, which the read path
# stopped needing once the cache became a gzipped CSV (#93), and since zipfs
# cannot be installed on the CI machines, the tests were skipped there on every
# run (#114).
skip_unless_duckdb_extensions <- function(extensions) {
  testthat::skip_if_not_installed("duckdb")

  problem <- tryCatch(
    local({
      cnefetools:::.duckdb_connect(extensions = extensions, verbose = FALSE)
      NULL
    }),
    error = function(e) conditionMessage(e)
  )

  if (!is.null(problem)) {
    testthat::skip(paste("DuckDB extension not available:", problem))
  }
}
