# Re-measure only the constrained ("modest") configurations of the R2.8
# benchmark and splice them back into data-raw/bench_r2_8.csv.
#
# The first full run pinned CPU affinity to processors 0-3 as well as limiting
# DuckDB's threads. That produced 79% and 82% spread on those two rows, against
# roughly 20% everywhere else, with elapsed time tracking the recorded
# `other_cpu_s` almost exactly: Windows puts other work on the low-numbered
# processors by preference, so the pinned runs fought for the busiest cores
# instead of simply running with less parallelism. bench_r2_8.R now limits
# thread count only.
#
# Re-running the whole grid to fix two rows would cost another 51 minutes and
# would also replace measurements that are fine. This re-runs only what changed.
#
#   Rscript data-raw/bench_r2_8_redo_modest.R

BENCH_DEFINE_ONLY <- TRUE
source("data-raw/bench_r2_8.R")

CSV <- "data-raw/bench_r2_8.csv"
REPS <- 5L

old <- utils::read.csv(CSV, stringsAsFactors = FALSE)
todo <- Filter(function(x) identical(x$block, "modest"), CONFIGS)

cat(sprintf("Re-measuring %d constrained configurations, %d replicates each.\n",
            length(todo), REPS))
cat("DuckDB config: ", paste(names(CONSTRAINED), unlist(CONSTRAINED),
                             sep = " = ", collapse = ", "), "\n\n")

# Warm-up, discarded, same as the main harness.
for (one in todo) invisible(run_one(one, NA_integer_))

rows <- list()
for (k in seq_len(REPS)) {
  for (one in todo) {
    r <- run_one(one, k)
    rows[[length(rows) + 1L]] <- r
    cat(sprintf("  pass %d  %-17s %8.2fs %7.0f MB  (other cpu %.1fs)\n",
                k, one$fn, r$elapsed_s, r$peak_rss_mb, r$other_cpu_s))
  }
}
new <- do.call(rbind, rows)

merged <- rbind(old[old$block != "modest", , drop = FALSE], new)
utils::write.csv(merged, CSV, row.names = FALSE)

cat(sprintf("\nReplaced %d rows with %d in %s (%d total).\n",
            sum(old$block == "modest"), nrow(new), CSV, nrow(merged)))

for (f in unique(new$fn)) {
  d <- new[new$fn == f, ]
  cat(sprintf("  %-17s median %.2fs  range %.2f-%.2f  spread %.0f%%  peak %.0f MB\n",
              f, stats::median(d$elapsed_s), min(d$elapsed_s), max(d$elapsed_s),
              100 * (max(d$elapsed_s) - min(d$elapsed_s)) / stats::median(d$elapsed_s),
              stats::median(d$peak_rss_mb)))
}
