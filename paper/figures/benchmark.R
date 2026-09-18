# Figures and derived numbers for Section 5 (Performance) of the manuscript.
#
# THIS SCRIPT DOES NOT MEASURE ANYTHING.
#
# Measurement happens once, in data-raw/bench_r2_8.R, and is committed to
# data-raw/bench_r2_8.csv with every replicate kept. This script and the
# pkgdown article (vignettes/articles/bench_duckdb.Rmd.orig) both read that
# one file through the shared helpers in data-raw/bench_r2_8_plots.R, so
# neither can disagree with the other about a speedup, a rounding rule or a
# label. That divergence is what Referee 1 flagged as R1.3 for the submitted
# version, where the article's opening claimed "up to 20x" while its own
# table below showed 13.33.
#
# An earlier version of this script measured on its own, one run per
# configuration, all DuckDB cases followed by all pure-R cases. That ordering
# confounds CPU thermal and frequency drift with the backend being tested.
# The committed data comes from a round-robin harness instead, with replicates
# and per-replicate machine-load recorded. See data-raw/bench_r2_8.R.
#
# WHY THE NUMBERS ARE NOT COMPUTED IN THE MANUSCRIPT
# The manuscript is submitted as a self-contained archive and the R Journal
# build environment does not have data-raw/. So the figures ship as PNGs and
# the tables are literal markdown in cnefetools.Rmd. This script prints the
# derived tables to the console so they can be transcribed, and re-running it
# is how you check that the manuscript still agrees with the measurements.
#
# OUTPUT
#   bench-cities.png  -- elapsed time by municipality size
#   bench-h3.png      -- elapsed time by H3 resolution
#   bench-memory.png  -- peak memory by municipality size
#   plus the tracts_to_*(), compute_lumi() and constrained-envelope tables,
#   printed to the console
#
# HOW TO RUN
# Run with this directory as the working directory, so that the relative path
# to the committed CSV resolves and the PNGs land alongside this file:
#   setwd("paper/figures")
#   source("benchmark.R")

library(dplyr)
library(ggplot2)

source("../../data-raw/bench_r2_8_plots.R")

bench <- bench_load()
bench_med <- bench_summarise(bench)

# The manuscript figures carry a caption, so the in-plot title and subtitle
# would only repeat it. The article keeps them because it has no captions.
fig_w <- 7
fig_h <- 4.2
fig_dpi <- 300

# ---------------------------------------------------------------------------
# Figure: elapsed time by municipality size (H3 resolution fixed at 8)
# ---------------------------------------------------------------------------

cities <- bench_cities(bench_med)

p_cities <- bench_plot_time(cities, city_lbl, title = NULL)

ggsave("bench-cities.png", plot = p_cities,
       width = fig_w, height = fig_h, dpi = fig_dpi, path = ".")

# ---------------------------------------------------------------------------
# Figure: peak memory by municipality size
#
# This is the R2.8b figure. It runs opposite to the direction Referee 2
# anticipated, so it earns its own panel rather than a sentence.
# ---------------------------------------------------------------------------

p_memory <- bench_plot_memory(cities, city_lbl, title = NULL)

ggsave("bench-memory.png", plot = p_memory,
       width = fig_w, height = fig_h, dpi = fig_dpi, path = ".")

# ---------------------------------------------------------------------------
# Figure: elapsed time by H3 resolution (municipality fixed at Curitiba-PR)
# ---------------------------------------------------------------------------

h3res <- bench_med |>
  filter(block == "h3res") |>
  mutate(
    res_lbl = factor(h3_res, levels = sort(unique(h3_res))),
    backend_lbl = bench_backend(backend)
  )

p_h3 <- bench_plot_time(h3res, res_lbl, xlab = "H3 resolution", title = NULL)

ggsave("bench-h3.png", plot = p_h3,
       width = fig_w, height = fig_h, dpi = fig_dpi, path = ".")

# ---------------------------------------------------------------------------
# Derived numbers for the prose and the tables, printed for transcription
# ---------------------------------------------------------------------------

cat("\n== Speedup, DuckDB over pure R, by municipality ==\n")
bench_med |>
  filter(block == "cities") |>
  mutate(muni = bench_city(muni)) |>
  bench_speedup("muni") |>
  print()

cat("\n== Speedup, DuckDB over pure R, by H3 resolution ==\n")
bench_med |>
  filter(block == "h3res") |>
  bench_speedup("h3_res") |>
  print()

cat("\n== Elapsed and peak memory, cities block ==\n")
bench_med |>
  filter(block == "cities") |>
  transmute(
    muni = bench_city(muni), backend, n_reps,
    seconds = round(seconds, 2),
    peak_gb = round(peak_mb / 1024, 2)
  ) |>
  arrange(muni, backend) |>
  print(n = Inf)

# R2.8a, first half: compute_lumi() against cnefe_counts(). The ratio is the
# point. It stays at 1, so the manuscript states the equivalence instead of
# plotting the same curve twice.
cat("\n== compute_lumi() vs cnefe_counts(), H3 res 8 ==\n")
bench_med |>
  filter(block %in% c("cities", "lumi"), h3_res == 8) |>
  transmute(muni = bench_city(muni), backend, fn, seconds) |>
  tidyr::pivot_wider(names_from = fn, values_from = seconds) |>
  mutate(ratio = round(compute_lumi / cnefe_counts, 2),
         across(c(cnefe_counts, compute_lumi), \(x) round(x, 2))) |>
  arrange(muni, backend) |>
  print(n = Inf)

# R2.8a, second half: the dasymetric functions, which do not behave like
# cnefe_counts() and so need their own numbers.
cat("\n== tracts_to_*(), DuckDB only ==\n")
bench_med |>
  filter(block == "tracts") |>
  transmute(
    muni = bench_city(muni), fn,
    seconds = round(seconds, 2),
    peak_gb = round(peak_mb / 1024, 2)
  ) |>
  arrange(muni, fn) |>
  print(n = Inf)

# R2.8c: the constrained resource envelope, 4 threads and 4 GB, against the
# same calls on the full machine.
#
# `backend` is NA for the tracts_to_*() rows, because those functions are
# DuckDB-only and take no backend argument. Testing it for equality would
# silently drop them.
cat("\n== Constrained envelope (4 threads, 4 GB) vs full machine ==\n")
bench_med |>
  filter(muni == "Sao Paulo-SP", is.na(backend) | backend == "duckdb",
         fn %in% c("cnefe_counts", "tracts_to_h3")) |>
  transmute(
    fn, constrained,
    seconds = round(seconds, 2),
    peak_gb = round(peak_mb / 1024, 2)
  ) |>
  arrange(fn, constrained) |>
  print(n = Inf)

cat("\nFigures written: bench-cities.png, bench-memory.png, bench-h3.png\n")
