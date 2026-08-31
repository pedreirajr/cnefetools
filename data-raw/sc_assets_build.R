# Build the census tract assets served from GitHub Releases.
#
# Referee 1 (#80 R1.9) noted that the package relies on derived assets whose
# generation was not documented or reproducible. The index and the reference
# table had scripts in data-raw/; the UF-level sc_##.parquet files did not. This
# is that missing script.
#
# It was reconstructed from the published assets rather than from notes, and
# then verified: every variable reproduces exactly for every tract that carries
# a value. See data-raw/sc_assets_build_verification.csv for the check.
#
#   Rscript data-raw/sc_assets_build.R          # all 27 UFs
#   Rscript data-raw/sc_assets_build.R 16 29    # selected UFs
#
# Output: data-raw/sc_assets/sc_<UF>.parquet, to be uploaded to a release whose
# tag matches .sc_assets_tag() for the edition. Do not rename a published tag:
# the checksums in data-raw/sc_assets_checksums.csv are computed against it.

suppressMessages({
  library(dplyr)
  library(sf)
})

YEAR <- 2022L

# --- sources -----------------------------------------------------------------
#
# Attributes come from censobr, which repackages the IBGE tract aggregates and
# is what the variable codes in `tracts_variables_ref` refer to. censobr
# prefixes each variable with its source table, so V00005 arrives as
# domicilio01_V00005, except in ResponsavelRenda where the codes are bare.
#
# Geometry comes from geobr, the same source used elsewhere in the package.

VARS <- tibble::tribble(
  ~out,             ~dataset,           ~code,
  "n_inhab",        "Basico",           "V0001",
  "pop_ph",         "Domicilio",        "V00005",
  "pop_ch",         "Domicilio",        "V00007",
  "male",           "Pessoas",          "V01007",
  "female",         "Pessoas",          "V01008",
  "age_0_4",        "Pessoas",          "V01031",
  "age_5_9",        "Pessoas",          "V01032",
  "age_10_14",      "Pessoas",          "V01033",
  "age_15_19",      "Pessoas",          "V01034",
  "age_20_24",      "Pessoas",          "V01035",
  "age_25_29",      "Pessoas",          "V01036",
  "age_30_39",      "Pessoas",          "V01037",
  "age_40_49",      "Pessoas",          "V01038",
  "age_50_59",      "Pessoas",          "V01039",
  "age_60_69",      "Pessoas",          "V01040",
  "age_70m",        "Pessoas",          "V01041",
  "race_branca",    "Pessoas",          "V01317",
  "race_preta",     "Pessoas",          "V01318",
  "race_amarela",   "Pessoas",          "V01319",
  "race_parda",     "Pessoas",          "V01320",
  "race_indigena",  "Pessoas",          "V01321",
  "n_resp",         "ResponsavelRenda", "V06001",
  "avg_inc_resp",   "ResponsavelRenda", "V06004"
)

# Column order of the published assets, which must not change: the package
# reads them by name, but a stable order keeps diffs and checksums meaningful.
SCHEMA <- c(
  "code_tract", "code_type", "n_inhab", "male", "female",
  "age_0_4", "age_5_9", "age_10_14", "age_15_19", "age_20_24", "age_25_29",
  "age_30_39", "age_40_49", "age_50_59", "age_60_69", "age_70m",
  "race_branca", "race_preta", "race_amarela", "race_parda", "race_indigena",
  "n_resp", "avg_inc_resp", "pop_ph", "pop_ch", "geom_wkb"
)

# --- helpers -----------------------------------------------------------------

read_dataset <- function(ds) {
  censobr::read_tracts(
    year = YEAR, dataset = ds,
    showProgress = FALSE, as_data_frame = FALSE
  ) |>
    collect()
}

# censobr prefixes variables with their source table, except ResponsavelRenda.
resolve_col <- function(nms, code) {
  hit <- grep(paste0("(^|_)", code, "$"), nms, value = TRUE)
  if (!length(hit)) {
    stop("variable not found in censobr output: ", code)
  }
  hit[[1]]
}

build_uf <- function(uf, attrs, verbose = TRUE) {
  uf <- sprintf("%02d", as.integer(uf))
  if (verbose) cat(sprintf("  UF %s ... ", uf))

  # Geometry, from the same source the rest of the package uses.
  geo <- suppressMessages(geobr::read_census_tract(
    code_tract = as.integer(uf), year = YEAR,
    simplified = FALSE, showProgress = FALSE
  ))
  geo <- sf::st_transform(geo, 4326)

  out <- data.frame(
    code_tract = as.character(geo$code_tract),
    stringsAsFactors = FALSE
  )

  for (ds in names(attrs)) {
    d <- attrs[[ds]]
    d <- d[substr(as.character(d$code_tract), 1, 2) == uf, , drop = FALSE]
    d$code_tract <- as.character(d$code_tract)

    if (ds == "Basico" && !"code_type" %in% names(out)) {
      out <- merge(out, d[, c("code_tract", "code_type")], by = "code_tract", all.x = TRUE)
    }

    want <- VARS[VARS$dataset == ds, ]
    for (i in seq_len(nrow(want))) {
      src <- resolve_col(names(d), want$code[i])
      tmp <- d[, c("code_tract", src)]
      names(tmp)[2] <- want$out[i]
      out <- merge(out, tmp, by = "code_tract", all.x = TRUE)
    }
  }

  num_cols <- setdiff(names(out), c("code_tract"))
  for (cc in num_cols) out[[cc]] <- suppressWarnings(as.numeric(as.character(out[[cc]])))

  # Geometry travels as WKB so the parquet needs no spatial extension to read.
  geo <- geo[match(out$code_tract, as.character(geo$code_tract)), ]
  out$geom_wkb <- sf::st_as_binary(sf::st_geometry(geo))

  out <- out[, SCHEMA]
  if (verbose) cat(sprintf("%d tracts\n", nrow(out)))
  out
}

# --- run ---------------------------------------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  ufs <- if (length(args)) args else sprintf("%02d", c(11:17, 21:29, 31:33, 35, 41:43, 50:53))

  outdir <- file.path("data-raw", "sc_assets")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  cat("reading censobr datasets once, for all UFs\n")
  attrs <- list()
  for (ds in unique(VARS$dataset)) {
    cat("  ", ds, "\n")
    attrs[[ds]] <- read_dataset(ds)
  }

  cat("\nbuilding\n")
  for (uf in ufs) {
    d <- build_uf(uf, attrs)
    arrow::write_parquet(d, file.path(outdir, sprintf("sc_%s.parquet", uf)))
  }

  cat("\nwrote", length(ufs), "assets to", outdir, "\n")
  cat("Upload them to the release named by .sc_assets_tag(), then refresh\n")
  cat("data-raw/sc_assets_checksums.csv.\n")
}
