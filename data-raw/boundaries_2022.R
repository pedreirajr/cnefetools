# Municipality boundaries, 2022 edition (#120)
#
# Builds the two pieces behind .read_muni_boundaries() and .detect_code_munis(),
# and records the checksum of the first in data-raw/boundaries_checksums.csv:
#
# 1. muni_2022.parquet, published as an asset of the GitHub release
#    `boundaries-v1`. It holds the 5570 municipalities of the 2022 IBGE mesh at
#    full resolution (geobr, simplified = FALSE), in WGS 84, with the geometry
#    as WKB and the GeoParquet "geo" metadata. Rows are sorted by immediate
#    region and then by code, and written in row groups of 32, so a remote
#    query by code_immediate downloads only the row groups of those regions.
#
# 2. boundary_index_2022, stored in R/sysdata.rda next to cnefe_index_2022: the
#    code, name, immediate region and bounding box of every municipality. The
#    package reads it offline, both to pick the regions to download and to pick
#    the candidate municipalities of a polygon layer.
#
# Run from the repo root: Rscript data-raw/boundaries_2022.R
# Then upload the asset:
#   gh release create boundaries-v1 data-raw/muni_2022.parquet --title ... --notes ...
# The Parquet itself is not committed (about 230 MB).

suppressMessages(library(sf))
sf_use_s2(FALSE)

out <- "data-raw/muni_2022.parquet"

m <- geobr::read_municipality(
  year = 2022, simplified = FALSE, showProgress = FALSE,
  keep_areas_operacionais = FALSE
)
m <- st_transform(m, 4326)
stopifnot(nrow(m) == 5570L, !anyDuplicated(m$code_muni))

# Immediate region (IBGE 2017 division, geobr layer of 2019) of each
# municipality, by a point on its surface. Every municipality must get one.
ri <- geobr::read_immediate_region(year = 2019, simplified = TRUE, showProgress = FALSE)
ri <- st_transform(ri, 4326)
pos <- suppressWarnings(st_point_on_surface(st_geometry(m)))
hit <- st_intersects(pos, ri)
stopifnot(all(lengths(hit) >= 1L))
m$code_immediate <- as.integer(ri$code_immediate[vapply(hit, function(i) i[1], 1L)])

# An immediate region never crosses a state line, so a mismatch means a bad match
stopifnot(all(substr(m$code_immediate, 1, 2) == substr(m$code_muni, 1, 2)))

bb <- t(vapply(st_geometry(m), function(g) as.numeric(st_bbox(g)), numeric(4)))
df <- data.frame(
  code_muni = as.integer(m$code_muni),
  name_muni = as.character(m$name_muni),
  code_immediate = m$code_immediate,
  xmin = bb[, 1], ymin = bb[, 2], xmax = bb[, 3], ymax = bb[, 4]
)
wkb <- lapply(st_as_binary(st_geometry(m)), as.raw)

ord <- order(df$code_immediate, df$code_muni)
df <- df[ord, ]
wkb <- wkb[ord]
rownames(df) <- NULL

# DuckDB writes row groups of at least 2048 rows and ignores a smaller
# ROW_GROUP_SIZE, while arrow honors chunk_size
tab <- arrow::arrow_table(
  df,
  geometry = arrow::Array$create(wkb, type = arrow::binary())
)
# No "crs" key: GeoParquet then means OGC:CRS84 (WGS 84, lon/lat)
tab$metadata$geo <- as.character(jsonlite::toJSON(list(
  version = "1.1.0",
  primary_column = "geometry",
  columns = list(geometry = list(
    encoding = "WKB",
    geometry_types = list("MultiPolygon", "Polygon")
  ))
), auto_unbox = TRUE))
arrow::write_parquet(tab, out, chunk_size = 32L, compression = "zstd")
cat("Wrote", out, "-", round(file.size(out) / 2^20, 1), "MB\n")

# SHA-256 and size of the published asset, as for the census tract assets
# (data-raw/sc_assets_checksums.csv), so a user can confirm that the file
# served by the release is the one built here. A new build is published under
# a new tag, and its row is added without removing the earlier ones.
sums_out <- "data-raw/boundaries_checksums.csv"
sums <- data.frame(
  asset = basename(out),
  size_bytes = file.size(out),
  sha256 = digest::digest(out, algo = "sha256", file = TRUE),
  release_tag = "boundaries-v1",
  edition = 2022L
)
if (file.exists(sums_out)) {
  old <- utils::read.csv(sums_out, stringsAsFactors = FALSE)
  sums <- rbind(old[old$release_tag != sums$release_tag, names(sums)], sums)
}
utils::write.csv(sums, sums_out, row.names = FALSE)

# Internal index, saved with the objects already in sysdata.rda
boundary_index_2022 <- df
sysdata <- new.env()
load("R/sysdata.rda", envir = sysdata)
assign("boundary_index_2022", boundary_index_2022, envir = sysdata)
save(list = ls(sysdata), envir = sysdata, file = "R/sysdata.rda", compress = "xz")
cat("Saved boundary_index_2022 in R/sysdata.rda:", paste(ls(sysdata), collapse = ", "), "\n")
