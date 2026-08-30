# Golden reference for the shared-helpers refactor (#83, #84, #57, #71).
#
# The refactor touches all five main files, and the test suite is thin by the
# referees' own account (#80 items R1.13 and R2.C8), with the fix for that
# scheduled last in #94. This script is the safety net in the meantime: it runs
# every exported function across every backend and aggregation mode, and freezes
# the results so the refactor can be proven output-identical rather than assumed
# to be.
#
# Usage, from the package root:
#
#   Rscript data-raw/refactor_baseline.R before   # freeze, run on the pre-refactor code
#   Rscript data-raw/refactor_baseline.R after    # re-run and diff against the freeze
#
# Results go to data-raw/baseline/, which is gitignored: the digests in
# baseline_digests.csv are the committed artefact.

devtools::load_all(quiet = TRUE)

mode <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode) || !mode %in% c("before", "after")) {
  stop("Usage: Rscript data-raw/refactor_baseline.R [before|after]")
}

outdir <- file.path("data-raw", "baseline", mode)
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

CODE_MUNI <- 2919207L # Lauro de Freitas-BA, the municipality used in the docs
H3_RES <- 9L
VARS <- c("pop_ph", "pop_ch", "female", "age_70m", "n_resp", "avg_inc_resp")

# User polygons: the neighbourhoods of Lauro de Freitas, as in the examples.
nei <- subset(
  suppressMessages(geobr::read_neighborhood(year = 2022, showProgress = FALSE)),
  code_muni == CODE_MUNI
)

# Normalise an sf result so the comparison is about values, not about row order,
# column order or geometry representation.
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

cases <- list(
  counts_hex_duckdb = function() {
    cnefe_counts(CODE_MUNI, h3_resolution = H3_RES, backend = "duckdb", verbose = FALSE)
  },
  counts_hex_r = function() {
    cnefe_counts(CODE_MUNI, h3_resolution = H3_RES, backend = "r", verbose = FALSE)
  },
  counts_user_duckdb = function() {
    cnefe_counts(CODE_MUNI, polygon_type = "user", polygon = nei, backend = "duckdb", verbose = FALSE)
  },
  counts_user_r = function() {
    cnefe_counts(CODE_MUNI, polygon_type = "user", polygon = nei, backend = "r", verbose = FALSE)
  },
  lumi_hex_duckdb = function() {
    compute_lumi(CODE_MUNI, h3_resolution = H3_RES, backend = "duckdb", verbose = FALSE)
  },
  lumi_hex_r = function() {
    compute_lumi(CODE_MUNI, h3_resolution = H3_RES, backend = "r", verbose = FALSE)
  },
  lumi_user_duckdb = function() {
    compute_lumi(CODE_MUNI, polygon_type = "user", polygon = nei, backend = "duckdb", verbose = FALSE)
  },
  lumi_user_r = function() {
    compute_lumi(CODE_MUNI, polygon_type = "user", polygon = nei, backend = "r", verbose = FALSE)
  },
  tracts_h3 = function() {
    tracts_to_h3(CODE_MUNI, h3_resolution = H3_RES, vars = VARS, verbose = FALSE)
  },
  tracts_polygon = function() {
    tracts_to_polygon(CODE_MUNI, polygon = nei, vars = VARS, verbose = FALSE)
  }
)

rows <- list()

for (nm in names(cases)) {
  cat(sprintf("running %-22s ... ", nm))
  t0 <- Sys.time()
  res <- tryCatch(cases[[nm]](), error = function(e) e)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  if (inherits(res, "error")) {
    cat(sprintf("ERROR (%s)\n", conditionMessage(res)))
    rows[[nm]] <- data.frame(
      case = nm, status = "error", nrow = NA_integer_, ncol = NA_integer_,
      digest = NA_character_, secs = round(secs, 1),
      stringsAsFactors = FALSE
    )
    next
  }

  saveRDS(res, file.path(outdir, paste0(nm, ".rds")))
  dg <- digest::digest(normalise(res), algo = "sha256")
  cat(sprintf("ok  rows=%-6s digest=%s  (%.1fs)\n", nrow(res), substr(dg, 1, 16), secs))

  rows[[nm]] <- data.frame(
    case = nm, status = "ok", nrow = nrow(res), ncol = ncol(res),
    digest = dg, secs = round(secs, 1),
    stringsAsFactors = FALSE
  )
}

summary_df <- do.call(rbind, rows)
rownames(summary_df) <- NULL
csv_path <- file.path("data-raw", sprintf("baseline_digests_%s.csv", mode))
utils::write.csv(summary_df[, c("case", "status", "nrow", "ncol", "digest")], csv_path, row.names = FALSE)
cat("\nwrote", csv_path, "\n")

if (mode == "after") {
  before <- utils::read.csv("data-raw/baseline_digests_before.csv", stringsAsFactors = FALSE)
  after <- summary_df
  cmp <- merge(before, after, by = "case", suffixes = c("_before", "_after"), all = TRUE)

  cat("\n== comparison ==\n")
  bad <- 0L
  for (i in seq_len(nrow(cmp))) {
    same <- identical(cmp$digest_before[i], cmp$digest_after[i])
    if (!same) bad <- bad + 1L
    cat(sprintf(
      "%-22s %s   (%s -> %s)\n",
      cmp$case[i],
      if (same) "IDENTICAL" else "*** DIFFERS ***",
      substr(cmp$digest_before[i], 1, 12),
      substr(cmp$digest_after[i], 1, 12)
    ))
  }

  cat("\n")
  if (bad == 0L) {
    cat("All", nrow(cmp), "cases are byte-identical to the baseline.\n")
  } else {
    cat(bad, "case(s) differ. Inspect the saved .rds pairs under data-raw/baseline/.\n")
    quit(status = 1L)
  }
}
