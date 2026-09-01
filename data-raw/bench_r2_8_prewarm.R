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

cat("\n== census tract assets ==\n")
# .sc_ensure_parquet_uf() takes the two-digit IBGE state code, not the acronym,
# so derive it from the municipality codes above rather than hardcoding it.
for (nm in names(MUNIS)) {
  uf <- .uf_from_code_muni(MUNIS[[nm]])
  t0 <- Sys.time()
  p <- .sc_ensure_parquet_uf(uf = uf, cache = TRUE, year = 2022L, verbose = FALSE)
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf(
    "  %-24s uf %-3s %-28s %7.1f MB  %6.1fs\n",
    nm, uf, basename(p), file.size(p) / 1024^2, dt
  ))
}

cat("\nPre-warm complete. Caches are uniform (.csv.gz) and local.\n")
