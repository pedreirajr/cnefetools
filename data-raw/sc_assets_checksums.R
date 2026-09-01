# SHA-256 checksums for the census tract assets served from GitHub Releases.
#
# Referee 1 (R1.9) asked for enough information to reproduce or audit the
# derived assets the package downloads, naming checksums specifically. This
# records what is actually published, so a user can verify that the file they
# received is the file we shipped.
#
# Each asset is downloaded to a temporary file, hashed, and deleted, so the run
# needs bandwidth but not 1.2 GB of disk.
#
#   Rscript data-raw/sc_assets_checksums.R
#
# Output: data-raw/sc_assets_checksums.csv

devtools::load_all(quiet = TRUE)

tag <- .sc_assets_tag(2022L)
repo <- "pedreirajr/cnefetools"

assets <- system2(
  "gh",
  c("release", "view", tag, "--repo", repo, "--json", "assets",
    "-q", shQuote(".assets[] | [.name, (.size|tostring)] | @tsv")),
  stdout = TRUE
)
assets <- do.call(rbind, strsplit(assets, "\t"))
assets <- data.frame(
  name = assets[, 1],
  size = as.numeric(assets[, 2]),
  stringsAsFactors = FALSE
)
assets <- assets[order(assets$name), ]

cat(sprintf("release %s: %d assets, %.1f MB total\n\n", tag, nrow(assets), sum(assets$size) / 1024^2))

tmp <- file.path(tempdir(), "sc_checksums")
unlink(tmp, recursive = TRUE)
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

rows <- vector("list", nrow(assets))

for (i in seq_len(nrow(assets))) {
  nm <- assets$name[i]
  cat(sprintf("%2d/%d  %-18s %7.1f MB ... ", i, nrow(assets), nm, assets$size[i] / 1024^2))

  ok <- system2(
    "gh",
    c("release", "download", tag, "--repo", repo, "--pattern", shQuote(nm),
      "--dir", shQuote(tmp), "--clobber"),
    stdout = FALSE, stderr = FALSE
  )

  path <- file.path(tmp, nm)
  if (!identical(ok, 0L) || !file.exists(path)) {
    cat("FAILED\n")
    rows[[i]] <- data.frame(asset = nm, size_bytes = assets$size[i],
                            sha256 = NA_character_, stringsAsFactors = FALSE)
    next
  }

  h <- digest::digest(path, algo = "sha256", file = TRUE)
  cat(substr(h, 1, 16), "\n")
  unlink(path)

  rows[[i]] <- data.frame(asset = nm, size_bytes = assets$size[i],
                          sha256 = h, stringsAsFactors = FALSE)
}

res <- do.call(rbind, rows)
res$release_tag <- tag
res$edition <- 2022L

out <- "data-raw/sc_assets_checksums.csv"
utils::write.csv(res, out, row.names = FALSE)

cat("\nwrote", out, "\n")
cat("failed:", sum(is.na(res$sha256)), "of", nrow(res), "\n")
