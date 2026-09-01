# Measurement for #93 option B: is re-compressing the cache to .gz worth it?
#
# Referee 1 (R1.11) reports Gzip 2.2x faster than ZIP and raw CSV 4.7x faster,
# measured on their own machine with Salvador. Their benchmark_gz_vs_zip.R was
# offered as a starting point. This is our own run, because the decision turns
# on a cost their figures do not include: the one-time conversion from the ZIP
# IBGE publishes into whatever we would cache instead.
#
#   Rscript data-raw/bench_gz_vs_zip.R

devtools::load_all(quiet = TRUE)

MUNIS <- c("Lauro de Freitas-BA" = 2919207L, "Fortaleza-CE" = 2304400L)
REPS <- 3L

timeit <- function(expr) {
  t0 <- Sys.time()
  force(expr)
  as.numeric(difftime(Sys.time(), t0, units = "secs"))
}

# One DuckDB read of the whole file, which is what the package actually does.
read_via <- function(uri, ext) {
  con <- .duckdb_connect(extensions = if (ext == "zipfs") "zipfs" else character(0),
                         verbose = FALSE)
  sql <- sprintf(
    "SELECT COUNT(*) AS n FROM read_csv_auto('%s', delim=';', header=true, strict_mode=false);",
    gsub("'", "''", uri)
  )
  .duckdb_quiet(DBI::dbGetQuery(con, sql))$n[1]
}

rows <- list()

for (nm in names(MUNIS)) {
  cm <- MUNIS[[nm]]
  cat("\n==", nm, "==\n")

  zip_info <- .cnefe_ensure_zip(cm, index = cnefe_index_2022, cache = TRUE,
                                year = 2022L, verbose = FALSE)
  zip_path <- normalizePath(zip_info$zip_path, winslash = "/")
  csv_inside <- .cnefe_first_csv_in_zip(zip_path)
  zip_uri <- sprintf("zip://%s/%s", zip_path, csv_inside)

  # Conversion cost: unzip, then gzip. Paid once per municipality, on the first
  # download, and it is the term missing from the referee's figures.
  workdir <- file.path(tempdir(), paste0("bench_", cm))
  unlink(workdir, recursive = TRUE)
  dir.create(workdir, recursive = TRUE)

  t_convert <- timeit({
    utils::unzip(zip_path, files = csv_inside, exdir = workdir)
    raw_csv <- file.path(workdir, csv_inside)
    gz_path <- paste0(raw_csv, ".gz")
    inc <- file(raw_csv, "rb")
    outc <- gzfile(gz_path, "wb")
    repeat {
      buf <- readBin(inc, "raw", n = 1e7)
      if (length(buf) == 0L) break
      writeBin(buf, outc)
    }
    close(inc); close(outc)
  })

  raw_csv <- file.path(workdir, csv_inside)
  gz_path <- paste0(raw_csv, ".gz")

  sizes <- c(zip = file.size(zip_path), csv = file.size(raw_csv), gz = file.size(gz_path))
  cat(sprintf("  sizes: zip %.1f MB | csv %.1f MB | gz %.1f MB\n", sizes[["zip"]]/1024^2,
              sizes[["csv"]]/1024^2, sizes[["gz"]]/1024^2))
  cat(sprintf("  one-time conversion: %.2fs\n", t_convert))

  for (lbl in c("zip", "gz", "csv")) {
    uri <- switch(lbl, zip = zip_uri, gz = gz_path, csv = raw_csv)
    ext <- if (lbl == "zip") "zipfs" else "none"
    invisible(read_via(uri, ext)) # warm
    ts <- replicate(REPS, timeit(read_via(uri, ext)))
    cat(sprintf("  read %-4s median %.2fs  (n = %d reps)\n", lbl, stats::median(ts), REPS))
    rows[[length(rows) + 1L]] <- data.frame(
      muni = nm, format = lbl, median_s = round(stats::median(ts), 3),
      size_mb = round(sizes[[lbl]] / 1024^2, 1),
      convert_s = if (lbl == "zip") 0 else round(t_convert, 2),
      stringsAsFactors = FALSE
    )
  }
  unlink(workdir, recursive = TRUE)
}

res <- do.call(rbind, rows)
utils::write.csv(res, "data-raw/bench_gz_vs_zip.csv", row.names = FALSE)

cat("\n== summary ==\n")
print(res, row.names = FALSE)

cat("\n== break-even: reads needed before .gz pays for its conversion ==\n")
for (nm in unique(res$muni)) {
  z <- res$median_s[res$muni == nm & res$format == "zip"]
  g <- res$median_s[res$muni == nm & res$format == "gz"]
  cv <- res$convert_s[res$muni == nm & res$format == "gz"]
  saved <- z - g
  cat(sprintf("  %-22s zip %.2fs, gz %.2fs, saving %.2fs/read, conversion %.2fs -> %s\n",
      nm, z, g, saved,
      if (saved <= 0) "never pays off" else sprintf("%.1f reads", cv / saved)))
}
