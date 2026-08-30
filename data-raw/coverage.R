# Test coverage measurement for #94 (#80 items R1.13 and R2.C8).
#
# Referee 1 reports "~25%" overall, with tracts_to_h3() at 0% and
# tracts_to_polygon() at ~10%. Those figures predate the revision work, so the
# baseline is re-measured here rather than quoted.
#
#   Rscript data-raw/coverage.R before
#   Rscript data-raw/coverage.R after

mode <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(mode) || !mode %in% c("before", "after")) {
  stop("Usage: Rscript data-raw/coverage.R [before|after]")
}

# Measured in both configurations, deliberately.
#
# The referee reports tracts_to_h3() at 0%. That is exactly what CRAN defaults
# produce, because the only test exercising it calls skip_on_cran(), added in
# 0.2.1 for a real reason: a DuckDB spatial segfault on
# r-devel-linux-x86_64-fedora-clang. With NOT_CRAN=true the same file measures
# 90%. A test that runs only when someone opts in is close to invisible, which
# is the substance of the referee's point even though the number they saw is an
# artefact of the skip.
measure <- function(not_cran) {
  Sys.setenv(NOT_CRAN = if (not_cran) "true" else "false")
  cov <- covr::package_coverage(type = "tests", quiet = TRUE)
  bf <- covr::coverage_to_list(cov)$filecoverage
  list(
    by_file = stats::setNames(round(as.numeric(bf), 1), names(bf)),
    overall = covr::percent_coverage(cov)
  )
}

cran <- measure(FALSE)
full <- measure(TRUE)

files <- union(names(cran$by_file), names(full$by_file))
res <- data.frame(
  file = files,
  cran_defaults = as.numeric(cran$by_file[files]),
  not_cran = as.numeric(full$by_file[files]),
  stringsAsFactors = FALSE
)
res <- res[order(res$cran_defaults), ]
rownames(res) <- NULL
overall <- full$overall

out <- file.path("data-raw", sprintf("coverage_%s.csv", mode))
utils::write.csv(res, out, row.names = FALSE)

cat(sprintf("\n== coverage (%s) ==\n", mode))
print(res, row.names = FALSE)
cat(sprintf("\noverall: %.1f%% with CRAN defaults, %.1f%% with NOT_CRAN=true\n", cran$overall, full$overall))
cat("wrote", out, "\n")

if (mode == "after" && file.exists("data-raw/coverage_before.csv")) {
  b <- utils::read.csv("data-raw/coverage_before.csv", stringsAsFactors = FALSE)
  cmp <- merge(b, res, by = "file", suffixes = c("_before", "_after"), all = TRUE)
  cmp$delta <- round(cmp$not_cran_after - cmp$not_cran_before, 1)
  cat("\n== change ==\n")
  print(cmp[order(-abs(cmp$delta)), ], row.names = FALSE)
}
