# Spot-check a subset of the golden reference from #83 without re-running the
# whole suite. Uses the same normalise() as refactor_baseline.R, so the two
# cannot drift.
#
#   Rscript data-raw/baseline_check.R counts_hex_duckdb lumi_hex_duckdb
#   Rscript data-raw/baseline_check.R            # all cases

devtools::load_all(quiet = TRUE)

normalise <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (!inherits(x, "sf")) {
    return(x)
  }
  df <- sf::st_drop_geometry(x)
  df <- df[, sort(names(df)), drop = FALSE]
  key <- if ("id_hex" %in% names(df)) df$id_hex else seq_len(nrow(df))
  ord <- order(key)
  df <- df[ord, , drop = FALSE]
  rownames(df) <- NULL
  list(
    attrs = df,
    geom = sf::st_as_binary(sf::st_geometry(x)[ord], EWKB = TRUE),
    crs = sf::st_crs(x)$input,
    class = class(x)
  )
}

CODE_MUNI <- 2919207L
H3_RES <- 9L
VARS <- c("pop_ph", "pop_ch", "female", "age_70m", "n_resp", "avg_inc_resp")

nei <- subset(
  suppressMessages(geobr::read_neighborhood(year = 2022, showProgress = FALSE)),
  code_muni == CODE_MUNI
)

cases <- list(
  counts_hex_duckdb = function() cnefe_counts(CODE_MUNI, h3_resolution = H3_RES, backend = "duckdb", verbose = FALSE),
  counts_hex_r = function() cnefe_counts(CODE_MUNI, h3_resolution = H3_RES, backend = "r", verbose = FALSE),
  counts_user_duckdb = function() cnefe_counts(CODE_MUNI, polygon = nei, backend = "duckdb", verbose = FALSE),
  counts_user_r = function() cnefe_counts(CODE_MUNI, polygon = nei, backend = "r", verbose = FALSE),
  lumi_hex_duckdb = function() compute_lumi(CODE_MUNI, h3_resolution = H3_RES, backend = "duckdb", verbose = FALSE),
  lumi_hex_r = function() compute_lumi(CODE_MUNI, h3_resolution = H3_RES, backend = "r", verbose = FALSE),
  lumi_user_duckdb = function() compute_lumi(CODE_MUNI, polygon = nei, backend = "duckdb", verbose = FALSE),
  lumi_user_r = function() compute_lumi(CODE_MUNI, polygon = nei, backend = "r", verbose = FALSE),
  tracts_h3 = function() tracts_to_h3(CODE_MUNI, h3_resolution = H3_RES, vars = VARS, verbose = FALSE),
  tracts_polygon = function() tracts_to_polygon(CODE_MUNI, polygon = nei, vars = VARS, verbose = FALSE)
)

want <- commandArgs(trailingOnly = TRUE)
if (!length(want)) want <- names(cases)

base <- utils::read.csv("data-raw/baseline_digests_before.csv", stringsAsFactors = FALSE)
bad <- 0L

for (nm in want) {
  res <- tryCatch(cases[[nm]](), error = function(e) e)
  if (inherits(res, "error")) {
    cat(sprintf("%-20s *** ERROR *** %s\n", nm, conditionMessage(res)))
    bad <- bad + 1L
    next
  }
  got <- digest::digest(normalise(res), algo = "sha256")
  exp <- base$digest[base$case == nm]
  ok <- identical(got, exp)
  if (!ok) bad <- bad + 1L
  cat(sprintf(
    "%-20s %s\n", nm,
    if (ok) "IDENTICAL" else sprintf("*** DIFFERS *** got %s want %s", substr(got, 1, 12), substr(exp, 1, 12))
  ))
}

cat("\n")
if (bad == 0L) cat("all", length(want), "checked cases match the baseline\n") else quit(status = 1L)
