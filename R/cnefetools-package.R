#' @section Package options:
#'
#' `cnefetools.duckdb_config` takes a named list of DuckDB settings, applied to
#' every connection the package opens:
#'
#' ```r
#' options(cnefetools.duckdb_config = list(threads = 4, memory_limit = "4GB"))
#' ```
#'
#' Left unset, DuckDB sizes itself against the whole machine, taking one thread
#' per logical core and a `memory_limit` of 80% of installed RAM. That is the
#' right default on a dedicated machine and the wrong one on a shared node, a
#' laptop running other work, or a CI runner.
#'
#' Names are passed to DuckDB's `SET` verbatim, so any setting DuckDB accepts
#' works, not only these two. An unrecognised name raises an error naming it.
#'
#' Going over `memory_limit` makes DuckDB spill to its temporary directory
#' rather than fail, so a low value costs time, not correctness.
#'
#' The download cache location is set through the `CNEFETOOLS_CACHE_DIR`
#' environment variable rather than an option. See [clear_cache_muni()] and
#' [clear_cache_tracts()].
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

utils::globalVariables(
  c(
    ".data",
    "n",
    "CODIGO_ESPECIE",
    "COD_ESPECIE",
    "id_hex",
    "count",
    "COD_ESPECIE1",
    "tot",
    "p_res",
    "q_rest",
    "k",
    "hhi",
    "min_hhi",
    "hhi_sc",
    "ei",
    "bal",
    "ice",
    "hhi_adp",
    "bgbi",
    "geometry"
  )
)
