# Benchmark harness for #80 R2.8 (referee 2, benchmark completeness).
#
# Produces data-raw/bench_r2_8.csv, one row per replicate. Both the manuscript
# (paper/figures/benchmark.R) and the pkgdown article
# (vignettes/articles/bench_duckdb.Rmd.orig) plot from that CSV instead of
# measuring on their own, so the two can no longer disagree.
#
# WHAT R2.8 ASKED FOR
#   (a) benchmark the other heavy functions, not only cnefe_counts()
#   (b) report peak memory, not only elapsed time
#   (c) report on more modest hardware
#
# WHY THE OLD DESIGN COULD NOT ANSWER IT
# The previous scripts ran every DuckDB case, then every pure-R case, once
# each. This machine drifts: the same call on the same warm input measured
# 1.63s to 3.77s within one session, with the slow runs at the END, and a
# fresh session minutes later ran uniformly at 3.2-3.6s. That is CPU thermal
# and frequency drift, not page cache (cold cache would be slow first) and not
# thread scheduling (pinning affinity changed almost nothing). Blocking by
# backend confounds that drift perfectly with the factor being measured.
#
# WHAT THIS DOES INSTEAD
#   * round-robin over configurations, so drift becomes noise, not bias
#   * a discarded warm-up per configuration
#   * N replicates, median reported with the min-max range
#   * a fresh subprocess per measurement, so peak RSS is clean
#   * peak memory as OS peak working set, not R-level allocation
#   * per-replicate machine load recorded, so contention is visible in the CSV
#
# WHY NOT bench::mark()
# Its mem_alloc counts R heap allocations only. DuckDB allocates in C++, in
# this process but outside the R heap, and DuckDB's memory is exactly what
# R2.8b asks about. ps::ps_memory_info()$peak_wset is the kernel's monotonic
# high-water mark for the whole process, so it captures both.
#
# HOW TO RUN
#   Rscript data-raw/bench_r2_8_prewarm.R    # once, downloads everything
#   Rscript data-raw/bench_r2_8.R --pilot    # one pass, no warm-up, for timing
#   Rscript data-raw/bench_r2_8.R            # the real run
#
# Run it on an idle machine, plugged in, on a high-performance power plan.
# Interleaving protects the speedup RATIO from contention but not the absolute
# times, and not peak memory: memory pressure makes Windows trim the working
# set, which deflates the peak and inflates the time at the same time.

stopifnot(requireNamespace("callr", quietly = TRUE))
stopifnot(requireNamespace("ps", quietly = TRUE))

PILOT <- "--pilot" %in% commandArgs(trailingOnly = TRUE)
PKG_DIR <- normalizePath(".", winslash = "/")
OUT_CSV <- "data-raw/bench_r2_8.csv"

MUNIS <- list(
  vca = list(label = "Vitoria da Conquista-BA", code = 2933307L, n = "~200k"),
  ctb = list(label = "Curitiba-PR",             code = 4106902L, n = "~900k"),
  spo = list(label = "Sao Paulo-SP",            code = 3550308L, n = "~5.7M")
)

# The constrained profile for R2.8c. Four threads and a 4 GB DuckDB budget
# stand in for a modest laptop. This is a simulation of the resource envelope,
# not of a different machine: the SSD, the RAM speed, the CPU generation and
# the OS are all still ours, and the response letter has to say so.
#
# An earlier version also pinned CPU affinity to processors 0-3. That was a
# mistake and the recorded `other_cpu_s` column is what exposed it: those runs
# came out at 79% and 82% spread, with elapsed time tracking background load
# almost exactly, while every other configuration stayed near 20%. Windows
# schedules other work onto the low-numbered processors by preference, so
# pinning there made the constrained runs compete for the busiest cores on the
# machine rather than simply running with less parallelism. Limiting DuckDB's
# thread count alone gives the intended four-thread budget without that
# artefact.
CONSTRAINED <- list(
  threads = 4L,
  memory_limit = "4GB"
)


# -----------------------------------------------------------------------------
# Configuration grid
# -----------------------------------------------------------------------------
# `block` names the artefact each row feeds:
#   cities  -> Figure 5, cnefe_counts() by municipality size
#   h3res   -> Figure 6, cnefe_counts() by H3 resolution
#   lumi    -> R2.8a, does compute_lumi() track cnefe_counts()?
#   tracts  -> R2.8a, the dasymetric functions
#   modest  -> R2.8c, the constrained-resource run

cfg <- function(block, fn, muni, backend = NA_character_, h3_res = 8L,
                reps = 5L, constrained = FALSE) {
  m <- MUNIS[[muni]]
  list(
    block = block, fn = fn,
    muni = muni, muni_label = m$label, code_muni = m$code, n_addresses = m$n,
    backend = backend, h3_res = as.integer(h3_res),
    reps = as.integer(reps), constrained = constrained
  )
}

CONFIGS <- c(
  # Figure 5: three sizes, both backends, resolution 8.
  # Sao Paulo pure-R is ~165s per replicate, so it gets 3 rather than 5.
  list(
    cfg("cities", "cnefe_counts", "vca", "duckdb", 8L),
    cfg("cities", "cnefe_counts", "vca", "r",      8L),
    cfg("cities", "cnefe_counts", "ctb", "duckdb", 8L),
    cfg("cities", "cnefe_counts", "ctb", "r",      8L),
    cfg("cities", "cnefe_counts", "spo", "duckdb", 8L),
    cfg("cities", "cnefe_counts", "spo", "r",      8L, reps = 3L)
  ),

  # Figure 6: Curitiba across resolutions, both backends.
  list(
    cfg("h3res", "cnefe_counts", "ctb", "duckdb",  7L),
    cfg("h3res", "cnefe_counts", "ctb", "r",       7L),
    cfg("h3res", "cnefe_counts", "ctb", "duckdb",  9L),
    cfg("h3res", "cnefe_counts", "ctb", "r",       9L),
    cfg("h3res", "cnefe_counts", "ctb", "duckdb", 11L),
    cfg("h3res", "cnefe_counts", "ctb", "r",      11L)
  ),

  # R2.8a, part 1. compute_lumi() shares the scan, the H3 assignment and the
  # GROUP BY with cnefe_counts(), skips the 8-way pivot, and adds arithmetic
  # that is O(cells) rather than O(addresses). Mirrors the Figure 5 grid so the
  # claim can be checked at every size rather than asserted.
  list(
    cfg("lumi", "compute_lumi", "vca", "duckdb", 8L),
    cfg("lumi", "compute_lumi", "vca", "r",      8L),
    cfg("lumi", "compute_lumi", "ctb", "duckdb", 8L),
    cfg("lumi", "compute_lumi", "ctb", "r",      8L),
    cfg("lumi", "compute_lumi", "spo", "duckdb", 8L),
    cfg("lumi", "compute_lumi", "spo", "r",      8L, reps = 3L)
  ),

  # R2.8a, part 2. The dasymetric functions really are a different workload:
  # a point-in-polygon overlay against census tracts, DuckDB only, no backend
  # argument. Fewer replicates because each run is heavy.
  list(
    cfg("tracts", "tracts_to_h3",      "vca", NA_character_, 9L),
    cfg("tracts", "tracts_to_h3",      "ctb", NA_character_, 9L),
    cfg("tracts", "tracts_to_h3",      "spo", NA_character_, 9L, reps = 3L),
    cfg("tracts", "tracts_to_polygon", "vca", NA_character_, 9L),
    cfg("tracts", "tracts_to_polygon", "ctb", NA_character_, 9L),
    cfg("tracts", "tracts_to_polygon", "spo", NA_character_, 9L, reps = 3L)
  ),

  # R2.8c. Sao Paulo is the stress case, so it is the one worth constraining.
  list(
    cfg("modest", "cnefe_counts", "spo", "duckdb", 8L, reps = 3L, constrained = TRUE),
    cfg("modest", "tracts_to_h3", "spo", NA_character_, 9L, reps = 3L, constrained = TRUE)
  )
)


# -----------------------------------------------------------------------------
# One measurement, in a fresh subprocess
# -----------------------------------------------------------------------------
# A fresh process per replicate is what makes peak_wset meaningful: it is a
# monotonic high-water mark that never resets within a process, so reusing a
# session would report the largest run so far for every later run.

measure_once <- function(one, pkg_dir, constrained) {
  callr::r(
    function(pkg_dir, one, constrained) {
      suppressMessages(pkgload::load_all(pkg_dir, quiet = TRUE, helpers = FALSE))

      if (!is.null(constrained)) {
        options(cnefetools.duckdb_config = constrained)
      }

      # Built before the clock starts: supplying the polygon is the user's job,
      # not the function's, so its cost is not part of what we report.
      #
      # An H3 grid over the municipality stands in for a user polygon layer.
      # It is deterministic, needs no extra download, and holds the geometry
      # constant against tracts_to_h3(), so the difference between the two
      # measurements is the user-polygon code path rather than the shapes.
      # `build_h3_grid()` is internal, hence the triple colon.
      poly <- NULL
      if (identical(one$fn, "tracts_to_polygon")) {
        poly <- cnefetools:::build_h3_grid(
          h3_resolution = one$h3_res,
          code_muni = one$code_muni,
          year = 2022L
        )
      }

      call_it <- function() {
        switch(
          one$fn,
          cnefe_counts = cnefetools::cnefe_counts(
            code_muni = one$code_muni, h3_resolution = one$h3_res,
            backend = one$backend, verbose = FALSE
          ),
          compute_lumi = cnefetools::compute_lumi(
            code_muni = one$code_muni, h3_resolution = one$h3_res,
            backend = one$backend, verbose = FALSE
          ),
          tracts_to_h3 = cnefetools::tracts_to_h3(
            code_muni = one$code_muni, h3_resolution = one$h3_res,
            verbose = FALSE
          ),
          tracts_to_polygon = cnefetools::tracts_to_polygon(
            code_muni = one$code_muni, polygon = poly, verbose = FALSE
          ),
          stop("unknown fn: ", one$fn)
        )
      }

      self <- ps::ps_handle()
      sys0 <- ps::ps_system_cpu_times()
      own0 <- ps::ps_cpu_times(self)

      t0 <- proc.time()[["elapsed"]]
      res <- call_it()
      elapsed <- proc.time()[["elapsed"]] - t0

      sys1 <- ps::ps_system_cpu_times()
      own1 <- ps::ps_cpu_times(self)

      # Contention indicator. `other_cpu_s` is CPU seconds burned by everything
      # on the machine except this process during the timed region. Near zero
      # means the machine was free and the number is trustworthy.
      sysd <- sys1 - sys0
      busy_s <- unname(sysd[["user"]] + sysd[["system"]])
      own_s <- unname((own1[["user"]] - own0[["user"]]) +
                        (own1[["system"]] - own0[["system"]]))

      list(
        elapsed_s = elapsed,
        peak_rss_mb = ps::ps_memory_info(self)[["peak_wset"]] / 1024^2,
        own_cpu_s = own_s,
        other_cpu_s = max(0, busy_s - own_s),
        n_rows = nrow(res)
      )
    },
    args = list(pkg_dir = pkg_dir, one = one, constrained = constrained)
  )
}


# -----------------------------------------------------------------------------
# Driver
# -----------------------------------------------------------------------------

label_of <- function(one) {
  sprintf(
    "%-17s %-23s %-6s res%-3d%s",
    one$fn, one$muni_label,
    if (is.na(one$backend)) "-" else one$backend,
    one$h3_res,
    if (isTRUE(one$constrained)) " [constrained]" else ""
  )
}

run_one <- function(one, rep_id) {
  con <- if (isTRUE(one$constrained)) CONSTRAINED else NULL
  t_wall <- Sys.time()

  # Starting a subprocess failed once during development, transiently and
  # before any package code ran. Losing a two-hour run to that would be
  # expensive, so retry twice. A failure that reproduces three times is a real
  # one and should still stop the run.
  r <- NULL
  for (attempt in 1:3) {
    r <- tryCatch(measure_once(one, PKG_DIR, con), error = function(e) e)
    if (!inherits(r, "error")) break
    cat(sprintf("  ! attempt %d failed (%s), retrying\n",
                attempt, conditionMessage(r)))
    Sys.sleep(2)
  }
  if (inherits(r, "error")) stop(r)

  data.frame(
    block = one$block, fn = one$fn,
    muni = one$muni_label, code_muni = one$code_muni, n_addresses = one$n_addresses,
    backend = one$backend, h3_res = one$h3_res,
    constrained = isTRUE(one$constrained),
    rep = rep_id,
    elapsed_s = round(r$elapsed_s, 3),
    peak_rss_mb = round(r$peak_rss_mb, 1),
    own_cpu_s = round(r$own_cpu_s, 2),
    other_cpu_s = round(r$other_cpu_s, 2),
    n_rows = r$n_rows,
    timestamp = format(t_wall, "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
}

# Everything above is definitions. bench_r2_8_redo_modest.R sources this file
# to reuse the configuration grid and the measurement functions without
# starting a run, so guard the driver rather than have that script slice the
# source by line number.
if (exists("BENCH_DEFINE_ONLY", inherits = FALSE) && isTRUE(BENCH_DEFINE_ONLY)) {
  message("bench_r2_8.R loaded, definitions only.")
} else {

cat(sprintf(
  "R2.8 benchmark | %s | %d configurations\n\n",
  if (PILOT) "PILOT (1 pass, no warm-up)" else "FULL RUN",
  length(CONFIGS)
))

rows <- list()
t_start <- Sys.time()

if (PILOT) {
  for (one in CONFIGS) {
    r <- run_one(one, 0L)
    rows[[length(rows) + 1L]] <- r
    cat(sprintf("  %s  %8.2fs  %7.0f MB  (other cpu %.1fs)\n",
                label_of(one), r$elapsed_s, r$peak_rss_mb, r$other_cpu_s))
  }
} else {
  # Warm-up, discarded. Fills the OS page cache and pays the one-off extension
  # load, so replicate 1 is not systematically the slowest.
  cat("-- warm-up (discarded) --\n")
  for (one in CONFIGS) {
    invisible(run_one(one, NA_integer_))
    cat(sprintf("  %s  done\n", label_of(one)))
  }

  # Round-robin. Every configuration is visited once per pass, so slow thermal
  # drift spreads across all of them instead of loading onto whichever block
  # happened to run last.
  max_reps <- max(vapply(CONFIGS, function(x) x$reps, integer(1)))
  for (k in seq_len(max_reps)) {
    cat(sprintf("\n-- pass %d of %d --\n", k, max_reps))
    for (one in CONFIGS) {
      if (k > one$reps) next
      r <- run_one(one, k)
      rows[[length(rows) + 1L]] <- r
      cat(sprintf("  %s  %8.2fs  %7.0f MB  (other cpu %.1fs)\n",
                  label_of(one), r$elapsed_s, r$peak_rss_mb, r$other_cpu_s))
    }
    # Written after every pass, not only at the end. A run this long should not
    # be all-or-nothing, and a partial CSV is still analysable.
    utils::write.csv(do.call(rbind, rows), OUT_CSV, row.names = FALSE)
  }
}

res <- do.call(rbind, rows)

if (!PILOT) {
  utils::write.csv(res, OUT_CSV, row.names = FALSE)
  cat(sprintf("\nWrote %s (%d rows)\n", OUT_CSV, nrow(res)))
}

cat(sprintf("\nTotal wall time: %.1f min\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

key <- with(res, paste(block, fn, muni, backend, h3_res, constrained, sep = " | "))
agg <- do.call(rbind, lapply(split(res, key), function(d) {
  data.frame(
    config = d$block[1],
    what = sprintf("%s %s %s res%d%s", d$fn[1], d$muni[1],
                   if (is.na(d$backend[1])) "-" else d$backend[1], d$h3_res[1],
                   if (d$constrained[1]) " [constr]" else ""),
    n = nrow(d),
    median_s = round(stats::median(d$elapsed_s), 2),
    min_s = round(min(d$elapsed_s), 2),
    max_s = round(max(d$elapsed_s), 2),
    spread_pct = round(100 * (max(d$elapsed_s) - min(d$elapsed_s)) /
                         stats::median(d$elapsed_s)),
    peak_rss_mb = round(stats::median(d$peak_rss_mb)),
    max_other_cpu_s = round(max(d$other_cpu_s), 1),
    stringsAsFactors = FALSE
  )
}))
agg <- agg[order(agg$config, agg$what), ]

cat("\n== summary ==\n")
print(agg, row.names = FALSE)

if (!PILOT) {
  bad <- agg[agg$spread_pct > 25, ]
  if (nrow(bad) > 0L) {
    cat("\n!! spread above 25% in these configurations. Drift beat the",
        "interleaving, so either raise reps or re-run on a quieter machine.",
        "Check max_other_cpu_s first: if it is high, the machine was busy and",
        "the fix is a quieter re-run, not more replicates:\n")
    print(bad[, c("what", "n", "median_s", "spread_pct", "max_other_cpu_s")],
          row.names = FALSE)
  }
}

}  # end of the BENCH_DEFINE_ONLY guard
