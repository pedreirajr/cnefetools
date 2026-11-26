# Census tract assets in small row groups, release sc-assets-v4 (#120)
#
# The package reads only the tracts of the municipalities it needs, straight
# from the release through DuckDB's httpfs extension (see
# .sc_ensure_parquet_munis() in R/utils-internal.R). That only saves anything if
# a remote read can skip most of the file, which takes two things: rows sorted
# by code_tract, whose first seven digits are the municipality code, and many
# small row groups, whose min/max statistics let DuckDB skip the others.
#
# The sc-assets-v3 files are already sorted by code_tract but hold a single row
# group each. This script rewrites them in row groups of 256 rows, with the
# same rows, columns, values and compression. Statistics are written for
# code_tract only, since no query filters on the other columns, which halves
# the footer every remote read has to fetch (511 KB to 250 KB for Bahia).
#
# One file per state, not one for the whole country: the footer grows with the
# number of row groups, and a national file (about 1,840 groups) would carry a
# footer of several MB into every call.
#
#   Rscript data-raw/sc_assets_rowgroups.R
#
# Output: data-raw/sc_assets/v4/sc_<UF>.parquet, to be uploaded as release
# sc-assets-v4, after which data-raw/sc_assets_checksums.R is rerun.

v3 <- "https://github.com/pedreirajr/cnefetools/releases/download/sc-assets-v3"
ufs <- c(11:17, 21:29, 31:33, 35, 41:43, 50:53)
stopifnot(length(ufs) == 27L)

src_dir <- file.path(tempdir(), "sc_assets_v3")
out_dir <- "data-raw/sc_assets/v4"
dir.create(src_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

log <- list()
for (uf in ufs) {
  name <- sprintf("sc_%d.parquet", uf)
  src <- file.path(src_dir, name)
  out <- file.path(out_dir, name)
  if (!file.exists(src)) {
    httr2::req_perform(httr2::request(file.path(v3, name)), path = src)
  }

  tab <- arrow::read_parquet(src, as_data_frame = FALSE)
  stopifnot(!is.unsorted(tab$code_tract$as_vector()))

  stats <- stats::setNames(names(tab) == "code_tract", names(tab))
  arrow::write_parquet(tab, out, chunk_size = 256L, compression = "snappy",
                       write_statistics = stats)

  # Same rows and values, in the same order
  same <- DBI::dbGetQuery(con, sprintf(
    "SELECT count(*) AS n FROM (
       (SELECT * FROM read_parquet('%1$s') EXCEPT ALL SELECT * FROM read_parquet('%2$s'))
       UNION ALL
       (SELECT * FROM read_parquet('%2$s') EXCEPT ALL SELECT * FROM read_parquet('%1$s'))
     );",
    src, out
  ))$n
  stopifnot(same == 0L)

  rg <- arrow::ParquetFileReader$create(out)$num_row_groups
  log[[length(log) + 1L]] <- data.frame(
    uf = uf, rows = tab$num_rows, row_groups = rg,
    mb_v3 = round(file.size(src) / 2^20, 1), mb_v4 = round(file.size(out) / 2^20, 1)
  )
  cat(sprintf("%s  %6d rows  %4d groups  %6.1f MB -> %6.1f MB\n",
              name, tab$num_rows, rg, file.size(src) / 2^20, file.size(out) / 2^20))
  unlink(src)
}

log <- do.call(rbind, log)
print(colSums(log[, c("rows", "row_groups", "mb_v3", "mb_v4")]))
