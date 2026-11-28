# Cache pre-warm for the R2.8 benchmark.
#
# Every measurement in data-raw/bench_r2_8.R must read from a warm, uniform
# cache. Two failure modes this guards against:
#
#   1. A run that downloads is timing the network, not the code.
#   2. A cache left over from before #93 is still a .zip, so DuckDB reads it
#      through the `zipfs` extension instead of native gzip. Half the runs would
#      then measure a code path we no longer ship, and the numbers would be
#      meaningless without anything looking wrong.
#
# Run this to completion before bench_r2_8.R:
#   Rscript data-raw/bench_r2_8_prewarm.R

devtools::load_all(quiet = TRUE)

MUNIS <- c(
  "Vitoria da Conquista-BA" = 2933307L,
  "Curitiba-PR"             = 4106902L,
  "Sao Paulo-SP"            = 3550308L
)

cat("== CNEFE caches ==\n")
for (nm in names(MUNIS)) {
  cm <- MUNIS[[nm]]
  t0 <- Sys.time()
  info <- .cnefe_ensure_zip(
    code_muni = cm,
    index = cnefe_index_2022,
    cache = TRUE,
    year = 2022L,
    verbose = FALSE,
    retry_timeouts = c(300L, 600L, 1800L)
  )
  p <- info$zip_path
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf(
    "  %-24s %-34s %7.1f MB  %6.1fs\n",
    nm, basename(p), file.size(p) / 1024^2, dt
  ))

  # The whole point of this script: fail loudly on a legacy .zip cache rather
  # than silently benchmarking the zipfs path.
  if (!grepl("\\.csv\\.gz$", p)) {
    stop(
      "Cache for ", nm, " is not a .csv.gz: ", p,
      "\nDelete it with clear_cache_muni() and re-run, otherwise the benchmark ",
      "measures the legacy zipfs path."
    )
  }
}

cat("\n== municipal boundaries and census tracts ==\n")
# What the current version reads: the boundary of each municipality and its
# census tracts, one Parquet file per municipality (#120). A whole-state tract
# file in the cache takes precedence over the per-municipality one, and the
# benchmark would then time a path new users no longer take, so stop on it.
# .uf_from_code_muni() gives the two-digit IBGE state code, not the acronym.
for (nm in names(MUNIS)) {
  cm <- MUNIS[[nm]]
  uf <- .uf_from_code_muni(cm)
  state_file <- .sc_asset_local_path(uf, year = 2022L)
  if (file.exists(state_file)) {
    stop(
      "The cache holds the whole-state tract file ", state_file,
      "\nUse a clean cache (CNEFETOOLS_CACHE_DIR) or remove it with ",
      "clear_cache_tracts(), otherwise the benchmark measures the old path."
    )
  }
  t0 <- Sys.time()
  invisible(.read_muni_boundaries(cm, year = 2022L, cache = TRUE))
  invisible(.sc_fetch_munis(uf, cm, cache = TRUE, year = 2022L))
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("  %-24s uf %-3s %6.1fs\n", nm, uf, dt))
}

cat("\nPre-warm complete. Caches are uniform (.csv.gz, per-municipality tracts) and local.\n")
