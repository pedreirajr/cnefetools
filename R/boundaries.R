# Municipality boundaries (#120)
#
# The boundaries come from a GeoParquet of the 2022 IBGE mesh at full
# resolution, published as an asset of the GitHub release `boundaries-v1` and
# read remotely through DuckDB's httpfs extension, so only the row groups of
# the immediate regions needed are downloaded (about 1.5 MB for Salvador,
# against 257 MB for the whole-Brazil geobr file). What is downloaded is kept
# in the package cache, one file per municipality, and reused across sessions.
# The internal index `boundary_index_2022` (code, name, immediate region and
# bounding box of every municipality, built by data-raw/boundaries_2022.R)
# tells which regions to ask for and which municipalities a polygon layer can
# touch, without any download. If the release or the extension cannot be
# reached, the simplified geobr boundaries are used instead, as before.
# Benchmarks: paper/review-2/benchmark-simplified.md.

#' URL of the boundary GeoParquet
#'
#' The option `cnefetools.boundary_url` overrides it, which the tests use to
#' point at a local file.
#'
#' @keywords internal
#' @noRd
.boundary_url <- function(year = 2022L) {
  opt <- getOption("cnefetools.boundary_url")
  if (!is.null(opt)) {
    return(opt)
  }
  sprintf(
    "https://github.com/pedreirajr/cnefetools/releases/download/boundaries-v1/muni_%d.parquet",
    as.integer(year)
  )
}


#' Index of the municipality boundaries
#'
#' @return A data frame with `code_muni`, `name_muni`, `code_immediate` and the
#'   bounding box (`xmin`, `ymin`, `xmax`, `ymax`, WGS 84) of every
#'   municipality of the edition.
#'
#' @keywords internal
#' @noRd
.boundary_index <- function(year = 2022L) {
  year <- .validate_year(year)
  boundary_index_2022
}


#' Folder of the cached boundaries
#'
#' @keywords internal
#' @noRd
.boundary_cache_dir <- function(cache_dir = NULL, year = 2022L) {
  file.path(.cnefe_cache_dir(cache_dir, year), "boundaries")
}


#' Read the boundaries of some municipalities
#'
#' The boundaries already in the cache are read from disk, and the missing ones
#' are fetched from the release in a single remote query. With `cache = FALSE`
#' nothing is written to disk and everything is fetched. If the release cannot
#' be read, the simplified geobr boundaries are used instead, with a message.
#'
#' @param code_muni Seven-digit codes.
#' @param year Edition.
#' @param cache Logical. Keep what is downloaded in the package cache.
#' @param cache_dir Optional cache root, as in the exported functions.
#'
#' @return An sf object in WGS 84 with `code_muni` and the geometry, one row
#'   per code, in the order given.
#'
#' @keywords internal
#' @noRd
.read_muni_boundaries <- function(code_muni, year = 2022L, cache = TRUE, cache_dir = NULL) {
  codes <- unique(as.integer(code_muni))
  # Checked before the fallback, since geobr would not know these codes either
  unknown <- setdiff(codes, .boundary_index(year)$code_muni)
  if (length(unknown) > 0L) {
    cli::cli_abort("Unknown municipality code{?s}: {.val {unknown}}.")
  }
  out <- tryCatch(
    .read_boundaries_release(codes, year = year, cache = cache, cache_dir = cache_dir),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    cli::cli_inform(c(
      "!" = "Could not read the municipality boundaries from the {.pkg cnefetools} release, so the simplified {.pkg geobr} boundaries are used instead.",
      "i" = "{conditionMessage(out)}"
    ))
    out <- .read_boundaries_geobr(codes, year = year)
  }
  out[match(codes, out$code_muni), ]
}


#' Boundaries from the release, through the cache
#'
#' @keywords internal
#' @noRd
.read_boundaries_release <- function(codes, year = 2022L, cache = TRUE, cache_dir = NULL) {
  idx <- .boundary_index(year)
  dir <- .boundary_cache_dir(cache_dir, year)
  files <- file.path(dir, paste0(codes, ".parquet"))
  cached <- if (isTRUE(cache)) codes[file.exists(files)] else integer(0)

  # A cached file that cannot be read, or that holds another municipality, is
  # deleted and fetched again. Otherwise the error would send every later call
  # to the simplified geobr fallback, and the grid would differ from one
  # machine to another without the cause ever being fixed (#120).
  from_disk <- list()
  for (cm in cached) {
    f <- file.path(dir, paste0(cm, ".parquet"))
    one <- tryCatch({
      # Without mmap, so the file is not locked on Windows afterwards
      tab <- arrow::read_parquet(f, as_data_frame = FALSE, mmap = FALSE)
      out <- list(code_muni = as.integer(tab$code_muni$as_vector()),
                  wkb = tab$geometry$as_vector())
      if (!identical(out$code_muni, as.integer(cm)) || length(out$wkb) != 1L) NULL else out
    }, error = function(e) NULL)
    if (is.null(one)) {
      unlink(f)
    } else {
      from_disk[[length(from_disk) + 1L]] <- one
    }
  }
  missing <- setdiff(codes, unlist(lapply(from_disk, `[[`, "code_muni")))

  fetched <- NULL
  if (length(missing) > 0L) {
    fetched <- .fetch_boundaries(missing, idx, year)
    if (isTRUE(cache)) {
      .write_boundary_cache(fetched, dir)
    }
  }

  all_codes <- c(fetched$code_muni, unlist(lapply(from_disk, `[[`, "code_muni")))
  all_wkb <- c(fetched$wkb, unlist(lapply(from_disk, `[[`, "wkb"), recursive = FALSE))

  sf::st_sf(
    code_muni = all_codes,
    geometry = sf::st_as_sfc(structure(lapply(all_wkb, as.raw), class = "WKB"), crs = 4326)
  )
}


#' Fetch boundaries from the release in one query
#'
#' The file is sorted by immediate region, so filtering on `code_immediate`
#' lets DuckDB skip every row group of the other regions. A local path (from
#' the option, in the tests) is read without httpfs.
#'
#' @return A list with `code_muni` and `wkb` (a list of raw vectors).
#'
#' @keywords internal
#' @noRd
.fetch_boundaries <- function(codes, idx, year = 2022L) {
  url <- .boundary_url(year)
  regions <- unique(idx$code_immediate[idx$code_muni %in% codes])

  con <- .duckdb_httpfs_connect(url, reason = "to read the municipality boundaries.")
  # If spatial happens to be loaded, keep the column as WKB, not GEOMETRY
  try(DBI::dbExecute(con, "SET enable_geoparquet_conversion = false;"), silent = TRUE)

  d <- .duckdb_quiet(DBI::dbGetQuery(con, sprintf(
    "SELECT code_muni, geometry FROM read_parquet('%s')
     WHERE code_immediate IN (%s) AND code_muni IN (%s);",
    url, paste(regions, collapse = ", "), paste(codes, collapse = ", ")
  )))

  if (!setequal(d$code_muni, codes)) {
    cli::cli_abort("The boundary file does not hold every municipality asked for.")
  }
  list(code_muni = as.integer(d$code_muni), wkb = unclass(d$geometry))
}


#' Write fetched boundaries to the cache, one file per municipality
#'
#' Each file is written under a temporary name and renamed, so an interrupted
#' write never leaves a partial file that a later call would trust.
#'
#' @keywords internal
#' @noRd
.write_boundary_cache <- function(fetched, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_along(fetched$code_muni)) {
    f <- file.path(dir, paste0(fetched$code_muni[i], ".parquet"))
    tmp <- paste0(f, ".tmp")
    tab <- arrow::arrow_table(
      code_muni = fetched$code_muni[i],
      geometry = arrow::Array$create(list(as.raw(fetched$wkb[[i]])), type = arrow::binary())
    )
    arrow::write_parquet(tab, tmp)
    file.rename(tmp, f)
  }
  invisible(NULL)
}


#' Boundaries from geobr, the fallback
#'
#' A few municipalities are read one by one. Many are read state by state,
#' since each geobr call filters the whole-Brazil file.
#'
#' @keywords internal
#' @noRd
.read_boundaries_geobr <- function(codes, year = 2022L) {
  if (length(codes) <= 3L) {
    parts <- lapply(codes, function(cm) {
      sf::st_transform(.read_muni_boundary(cm, year = year), 4326)
    })
    geoms <- do.call(c, lapply(parts, sf::st_geometry))
    return(sf::st_sf(code_muni = as.integer(codes), geometry = geoms))
  }
  ufs <- unique(as.integer(substr(codes, 1, 2)))
  munis <- sf::st_transform(.read_muni_boundaries_of_states(ufs, year = year), 4326)
  munis <- munis[as.integer(munis$code_muni) %in% codes, ]
  sf::st_sf(code_muni = as.integer(munis$code_muni), geometry = sf::st_geometry(munis))
}


#' Find the municipalities that a polygon layer intersects
#'
#' Used with `scope = "geom"`, on the polygons or on the H3 grid (#120). The
#' candidates are the municipalities whose bounding box (from the internal
#' index, no download) meets some feature of the layer. Only their boundaries
#' are read, and [sf::st_intersects()] gives the exact answer. The polygons are
#' never unioned, which is the expensive operation on a layer with many
#' features.
#'
#' The test is planar on longitude and latitude. That is enough to decide
#' whether two areas touch, and it avoids s2 failures on invalid rings.
#'
#' Boundaries from another source rarely coincide with the IBGE mesh, so a
#' polygon along a municipal border can touch the neighbor by a few meters and
#' pull it in. This is documented rather than filtered: the user passes
#' `code_muni` with `scope = "muni"` to control the set.
#'
#' @param polygon An sf object with polygon geometries.
#' @param year Territorial base year.
#' @param cache,cache_dir As in `.read_muni_boundaries()`.
#'
#' @return A data frame with `code_muni` (integer) and `name_muni`, ordered by
#'   code.
#'
#' @keywords internal
#' @noRd
.detect_code_munis <- function(polygon, year = 2022L, cache = TRUE, cache_dir = NULL) {
  polygon_4326 <- sf::st_geometry(sf::st_transform(sf::st_make_valid(polygon), 4326))

  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  idx <- .boundary_index(year)
  boxes <- sf::st_sfc(
    lapply(seq_len(nrow(idx)), function(i) {
      x <- c(idx$xmin[i], idx$xmax[i])
      y <- c(idx$ymin[i], idx$ymax[i])
      sf::st_polygon(list(cbind(x[c(1, 2, 2, 1, 1)], y[c(1, 1, 2, 2, 1)])))
    }),
    crs = 4326
  )
  cand <- lengths(suppressMessages(sf::st_intersects(boxes, polygon_4326))) > 0L

  if (!any(cand)) {
    cli::cli_abort(c(
      "The polygons do not intersect any municipality.",
      "i" = "Check the CRS of {.arg polygon}, or pass {.arg code_muni} explicitly."
    ))
  }

  munis <- .read_muni_boundaries(idx$code_muni[cand], year = year, cache = cache, cache_dir = cache_dir)
  munis <- sf::st_make_valid(munis)
  hit <- lengths(suppressMessages(sf::st_intersects(munis, polygon_4326))) > 0L

  if (!any(hit)) {
    cli::cli_abort(c(
      "The polygons do not intersect any municipality.",
      "i" = "Check the CRS of {.arg polygon}, or pass {.arg code_muni} explicitly."
    ))
  }

  codes <- sort(unique(as.integer(munis$code_muni[hit])))
  data.frame(
    code_muni = codes,
    name_muni = as.character(idx$name_muni[match(codes, idx$code_muni)]),
    stringsAsFactors = FALSE
  )
}
