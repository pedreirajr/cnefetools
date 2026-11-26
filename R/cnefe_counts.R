#' Count CNEFE address species on a spatial grid
#'
#' @description
#' `cnefe_counts()` reads CNEFE records for one or more municipalities, assigns
#' each address point to spatial units (either H3 hexagonal cells or user-provided
#' polygons), and returns per-unit counts of `COD_ESPECIE` as `addr_type1` to
#' `addr_type8`.
#'
#' @param code_muni Numeric or character. One or more seven-digit IBGE
#'   municipality codes, for example `2927408` or `c(2927408, 2919207)`.
#'   Repeated codes are dropped. With several codes, their addresses are
#'   counted together (see "Several municipalities").
#'
#'   Required for the H3 grid, which is built from the boundaries of these
#'   municipalities. With `polygon`, it's required under `scope = "muni"` and
#'   ignored under `scope = "geom"`, which finds the municipalities that the
#'   polygons intersect (see "Municipalities detected from the polygons").
#' @param year Integer. The CNEFE data year. Currently only 2022 is supported.
#'   Defaults to 2022.
#' @param polygon_type `r lifecycle::badge("deprecated")` The aggregation mode is
#'   now inferred from `polygon`: leave it `NULL` for an H3 grid, or pass an
#'   [`sf::sf`] object for user polygons. Passing `polygon_type` still works and
#'   warns.
#' @param polygon An [`sf::sf`] object with polygon geometries. When it's
#'   supplied, the counts are returned for these polygons instead of an H3
#'   grid. Features with the same geometry as an earlier feature are dropped,
#'   with a message, and the first one is kept. Under `scope = "muni"`, a
#'   warning reports how many addresses of the municipalities in `code_muni`
#'   fall outside the polygons. When polygons overlap, a warning also reports
#'   how many addresses fall in more than one of them, since those are counted
#'   in each. If no address falls inside the polygons, the function stops with
#'   an error.
#' @param crs_output The CRS for the output object. Only used when `polygon` is
#'   supplied. Default is `NULL`, which uses the original CRS of
#'   the `polygon` argument. Can be an EPSG code (e.g., 4326, 31983) or any CRS
#'   object accepted by [sf::st_transform()].
#' @param h3_resolution Integer. H3 grid resolution (default: 9). Only used for
#'   the H3 grid, so it is ignored when `polygon` is supplied.
#' @param scope Character. Which CNEFE addresses the function works with.
#'   `"muni"` (the default, and the faster option) uses every address of the
#'   municipalities in `code_muni`. `"geom"` uses the addresses that fall inside
#'   the output geometries (the H3 cells, or the polygons in `polygon`),
#'   whichever municipality they belong to, so the municipalities those
#'   geometries reach are read as well. The H3 grid holds the cells whose
#'   center falls inside the boundaries of `code_muni`, plus the neighboring
#'   cells that intersect those boundaries. See the section "Border cells at the
#'   edge of the region".
#' @param verbose Logical; if `TRUE`, prints messages and timing information.
#' @param cache Logical. If `TRUE` (default), the downloaded files are kept in
#'   the user cache directory and reused in later calls. These are the CNEFE
#'   file of each municipality, stored as a gzipped CSV, and the municipal
#'   boundaries used to build the H3 grid or to find the municipalities that
#'   the polygons reach, one small file per municipality. If `FALSE`, temporary
#'   files are used and deleted after the call. See [clear_cache_muni()] and
#'   [clear_cache_boundaries()] to delete the cached files.
#' @param cache_dir Character. Directory to use for cached downloads. If `NULL`
#'   (default), the `CNEFETOOLS_CACHE_DIR` environment variable is used when it
#'   is set, otherwise [tools::R_user_dir()] with `which = "cache"`. Use this to
#'   point large downloads at a secondary drive or a shared volume.
#' @param backend Character. `"duckdb"` (default) uses DuckDB with the H3
#'   extension, and the spatial extension as well when `polygon` is supplied.
#'   `"r"` uses h3jsr and sf in R instead, and needs no DuckDB extension for
#'   the counts. The municipal boundaries of the H3 grid are still downloaded
#'   through DuckDB's httpfs extension the first time, on either backend, and if
#'   that fails the simplified geobr boundaries are used instead.
#'
#'   `"r"` exists for environments where DuckDB extensions cannot be installed,
#'   such as some restricted computing clusters. It is **not** the lighter
#'   option: it materializes the filtered address table in R memory, so its
#'   footprint grows with the municipality, while DuckDB aggregates in a
#'   streaming fashion and stays nearly flat. On São Paulo (5.7 million
#'   addresses) the measured peak is about 9 GB under `"r"` against 0.7 GB
#'   under `"duckdb"`, alongside being roughly 13 times slower.
#'
#'   If the constraint is memory rather than installability, keep the DuckDB
#'   backend and cap it with the `cnefetools.duckdb_config` option instead. See
#'   `?cnefetools` for that option, and the benchmark article at
#'   <https://pedreirajr.github.io/cnefetools/articles/bench_duckdb.html> for
#'   the measurements.
#'
#' @return An [`sf::sf`] object containing:
#' - `id_hex` (when `polygon` is `NULL`): H3 cell identifier
#' - Original columns from `polygon` (when `polygon` is supplied)
#' - `addr_type1` ... `addr_type8`: counts per address type
#' - `geometry`: polygon geometry
#'
#' When `polygon` is supplied, the output CRS matches the original `polygon` CRS
#' (or `crs_output` if specified).
#'
#' @details
#' The counts in the columns `addr_type1` to `addr_type8` correspond to:
#' - `addr_type1`: Private household (Domicílio particular)
#' - `addr_type2`: Collective household (Domicílio coletivo)
#' - `addr_type3`: Agricultural establishment (Estabelecimento agropecuário)
#' - `addr_type4`: Educational establishment (Estabelecimento de ensino)
#' - `addr_type5`: Health establishment (Estabelecimento de saúde)
#' - `addr_type6`: Establishment for other purposes (Estabelecimento de outras finalidades)
#' - `addr_type7`: Building under construction or renovation (Edificação em construção ou reforma)
#' - `addr_type8`: Religious establishment (Estabelecimento religioso)
#'
#' All eight types are reported. In particular, `addr_type7` is retained here,
#' whereas [compute_lumi()] excludes it when computing land-use mix indices.
#'
#' ## Several municipalities
#'
#' Pass a vector of codes to cover a region that spans more than one
#' municipality. The records of all of them are counted together on one grid
#' built over the union of the boundaries, so every cell appears once.
#'
#' Calling the function once per municipality and binding the results isn't the
#' same. A cell on a shared border belongs to the grid of both municipalities,
#' so it comes back from both calls, each with only the addresses of its own
#' municipality (under the default `scope = "muni"`). Counts add up, so those
#' outputs can be combined by summing `addr_type1` to `addr_type8` by `id_hex`,
#' but dropping the repeated rows (for example with `dplyr::distinct()`) loses
#' one side of the border. Shares and indices shouldn't be averaged across
#' municipalities. Compute them from the summed counts, or use [compute_lumi()]
#' with a vector of codes.
#'
#' ## Border cells at the edge of the region
#'
#' Under the default `scope = "muni"`, only the addresses of the municipalities
#' in `code_muni` are read. A cell or polygon on the outer edge of the
#' requested area, one that also overlaps a municipality you did not request,
#' therefore describes only the part of it that lies inside the area: its
#' counts leave out the addresses of the neighbor. Adding municipalities moves
#' the edge outward but does not remove it.
#'
#' Under `scope = "geom"`, the municipalities that the output geometries reach
#' are read too, and every cell or polygon receives all the addresses inside
#' it, so the edge is complete. On the H3 grid, `code_muni` still defines the
#' grid, and a message lists the neighboring municipalities read to complete
#' it. This costs a download and a read of each of them in full, which is why
#' it is not the default.
#'
#' ## Addresses outside the H3 grid
#'
#' The grid is built from the 2022 IBGE municipal boundaries, at full
#' resolution, plus the ring of cells along them, but each address goes to the
#' cell of its own coordinates. An address geocoded beyond that ring, for
#' example on the coast or on a small island the boundary doesn't follow, lands
#' in a cell outside the grid and isn't counted. A warning reports how many addresses of the municipalities in
#' `code_muni` this affects, so the counts can be reconciled with their CNEFE
#' totals. Under `scope = "geom"`, the addresses of the neighboring
#' municipalities that fall beyond the grid are expected and not reported.
#'
#' ## Municipalities detected from the polygons
#'
#' When `polygon` is supplied with `scope = "geom"`, the function finds the
#' municipalities that the polygons intersect, using the 2022 IBGE municipal
#' boundaries, and reads all of their addresses. A message lists them, and a
#' `code_muni` passed in this case is ignored.
#'
#' Polygons from another source rarely follow the IBGE boundaries exactly, so
#' a polygon along a municipal border can touch a neighboring municipality by
#' a few meters. That neighbor is then downloaded and read too. The counts are
#' not affected, since only the points inside the polygons are counted, but use
#' `scope = "muni"` with `code_muni` whenever you know which municipalities
#' the polygons belong to, which is faster.
#'
#' @seealso [compute_lumi()] for land-use mix indices on the same spatial units.
#'
#' @examples
#' \donttest{
#' # Count addresses per H3 hexagon (resolution 9)
#' hex_counts <- cnefe_counts(code_muni = 2929057, cache = FALSE)
#'
#' # Several municipalities in one call: one grid over their union, with no cell
#' # repeated along the shared border (Salvador and its neighbor Lauro de Freitas)
#' region_counts <- cnefe_counts(code_muni = c(2927408, 2919207), cache = FALSE)
#'
#' # The grid of Lauro de Freitas, with the cells along its border completed by
#' # the addresses of the neighboring municipalities
#' ldf_full <- cnefe_counts(code_muni = 2919207, scope = "geom", cache = FALSE)
#'
#' # Count addresses per user-provided polygon (neighborhoods of Lauro de Freitas-BA)
#' # Using geobr to download neighborhood boundaries
#' library(geobr)
#' nei_ldf <- subset(
#'   read_neighborhood(year = 2022),
#'   code_muni == 2919207
#' )
#' nei_counts <- cnefe_counts(
#'   code_muni = 2919207,
#'   polygon = nei_ldf,
#'   cache = FALSE
#' )
#'
#' # With scope = "geom", the municipalities the polygons intersect are read
#' nei_counts_auto <- cnefe_counts(polygon = nei_ldf, scope = "geom", cache = FALSE)
#' }
#'
#' @export
cnefe_counts <- function(
  code_muni = NULL,
  year = 2022,
  polygon_type = lifecycle::deprecated(),
  polygon = NULL,
  crs_output = NULL,
  h3_resolution = 9,
  scope = c("muni", "geom"),
  verbose = TRUE,
  cache = TRUE,
  cache_dir = NULL,
  backend = c("duckdb", "r")
) {
  polygon_type <- .resolve_polygon_mode(polygon, polygon_type, fn = "cnefe_counts")
  backend <- match.arg(backend)
  code_muni_given <- .check_code_muni_arg(code_muni, polygon_type)
  scope <- .resolve_scope(scope, code_muni_given, polygon_type)
  year <- .validate_year(year)

  # Validate polygon argument
  if (polygon_type == "user") {
    .validate_polygon_arg(polygon, crs_output = crs_output)
    # Repeated features would count the same addresses again (#120)
    polygon <- .drop_repeated_polygons(polygon)
  }

  # scope = "geom" on polygons: read the municipalities they intersect (#120)
  detected <- NULL
  if (scope$detect) {
    detected <- .detect_code_munis(polygon, year = year, cache = cache, cache_dir = cache_dir)
    .inform_detected_munis(detected)
    code_muni <- detected$code_muni
  }
  code_muni <- .normalize_code_munis(code_muni)

  # Get the appropriate index for the requested year
  cnefe_index <- .get_cnefe_index(year)

  # ---------------------------------------------------------------------------
  # Branch: H3 grid vs user-provided polygon
  # ---------------------------------------------------------------------------
  if (polygon_type == "hex") {
    out <- .cnefe_counts_h3(
      code_muni = code_muni,
      year = year,
      h3_resolution = h3_resolution,
      scope = scope$scope,
      backend = backend,
      cnefe_index = cnefe_index,
      verbose = verbose,
      cache = cache,
      cache_dir = cache_dir
    )
  } else {
    out <- .cnefe_counts_user_poly(
      code_muni = code_muni,
      year = year,
      polygon = polygon,
      crs_output = crs_output,
      backend = backend,
      cnefe_index = cnefe_index,
      verbose = verbose,
      cache = cache,
      cache_dir = cache_dir,
      detected = detected
    )
  }

  return(out)
}


# -----------------------------------------------------------------------------
# Internal: H3 grid aggregation (original behavior)
# -----------------------------------------------------------------------------
.cnefe_counts_h3 <- function(
  code_muni,
  year,
  h3_resolution,
  scope = "muni",
  backend,
  cnefe_index,
  verbose,
  cache = TRUE,
  cache_dir = NULL
) {
  # ---------------------------------------------------------------------------
  # Step 1/3: Build the H3 grid over the municipal boundaries. It comes first
  # because, with scope = "geom", it decides which municipalities are read.
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 1/3: Building full H3 grid over the municipal boundaries...",
                           msg_done = "Step 1/3 (H3 grid built)")
  }

  grid_set <- .h3_grid_and_read_set(code_muni, scope, h3_resolution, year = year,
                                    cache = cache, cache_dir = cache_dir)
  hex_grid <- grid_set$hex_grid
  read_munis <- grid_set$read_munis

  if (verbose) {
    cli::cli_progress_done("Step 1/3: Building full H3 grid over the municipal boundaries...")
  }

  # ---------------------------------------------------------------------------
  # Step 2/3: Ensure the cached data files exist
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 2/3: Ensuring the CNEFE data file...",
                           msg_done = "Step 2/3 (CNEFE data ready)")
  }

  zip_infos <- .cnefe_ensure_zips(
    read_munis,
    index = cnefe_index,
    cache = cache,
    cache_dir = cache_dir,
    year = year,
    verbose = verbose
  )

  if (verbose) {
    cli::cli_progress_done("Step 2/3: Ensuring the CNEFE data file...")
  }


  # ---------------------------------------------------------------------------
  # Step 3/3: Count address species per hexagon
  # ---------------------------------------------------------------------------
  if (verbose) {
  cli::cli_progress_step("Step 3/3: Counting address species per hexagon...",
                         msg_done = "Step 3/3 (Addresses counted)")

  }

  counts_long <- NULL

  if (identical(backend, "duckdb")) {
    rlang::check_installed(
      "DBI",
      reason = "to use backend = 'duckdb' in `cnefe_counts()`."
    )

    con <- .duckdb_connect(
      extensions = "h3",
      reason = "to use backend = 'duckdb' in `cnefe_counts()`.",
      fallback = "backend = \"r\"",
      verbose = verbose
    )

    # One SELECT per municipality file, joined by UNION ALL. The H3 cell is
    # computed from each address's own coordinates, never from a municipal
    # boundary, so a cell on a shared border receives the addresses of both
    # sides in the same GROUP BY and appears once (#120, R2 comment 1).
    src_sql <- .cnefe_points_union_sql(
      con,
      zip_infos,
      read_munis,
      verbose = verbose
    )

    # `own` flags the addresses of the municipalities in code_muni, the only
    # ones the report of addresses outside the grid is about. The neighbors read
    # under scope = "geom" fall outside the grid by construction (#120).
    sql <- sprintf(
      "
        WITH src AS (
          %s
        )
        SELECT
          lower(hex(CAST(h3_latlng_to_cell(lat, lon, %d) AS UBIGINT))) AS id_hex,
          COD_ESPECIE,
          code_muni IN (%s) AS own,
          COUNT(*)::BIGINT AS n
        FROM src
        WHERE
          lon IS NOT NULL AND lat IS NOT NULL
          AND COD_ESPECIE BETWEEN 1 AND 8
        GROUP BY 1, 2, 3;
      ",
      src_sql,
      as.integer(h3_resolution),
      paste(as.integer(code_muni), collapse = ", ")
    )

    counts_long <- .duckdb_quiet(DBI::dbGetQuery(con, sql)) |>
      dplyr::as_tibble() |>
      dplyr::mutate(
        id_hex = as.character(.data$id_hex),
        COD_ESPECIE = as.integer(.data$COD_ESPECIE),
        own = as.logical(.data$own),
        n = as.integer(.data$n)
      )

  } else {
    # Backend "r" (slower): read Arrow, compute H3 in R. Each municipality is
    # reduced to its counts per cell before the next one is read, so the peak
    # memory is that of the largest municipality, not of their sum.
    counts_long <- lapply(
      seq_along(zip_infos),
      function(i) {
        .cnefe_counts_h3_r(zip_infos[[i]]$zip_path, h3_resolution) |>
          dplyr::mutate(own = read_munis[[i]] %in% code_muni)
      }
    ) |>
      dplyr::bind_rows()
  }

  # Addresses of code_muni whose cell is not in the grid are dropped by the
  # join below, so they are reported (#120)
  if (nrow(counts_long) > 0L) {
    in_grid <- counts_long$id_hex %in% hex_grid$id_hex
    own <- counts_long$own
    .warn_outside_grid(
      n_outside = sum(counts_long$n[own & !in_grid]),
      n_total = sum(counts_long$n[own])
    )
  }

  counts_long <- counts_long |>
    dplyr::group_by(.data$id_hex, .data$COD_ESPECIE) |>
    dplyr::summarise(n = as.integer(sum(.data$n)), .groups = "drop")

  # Wide with addr_type1..addr_type8 and robust typing
  if (nrow(counts_long) == 0L) {
    out <- hex_grid
    for (k in 1:8) {
      out[[paste0("addr_type", k)]] <- 0L
    }
  } else {
    counts_wide <- counts_long |>
      tidyr::pivot_wider(
        id_cols = "id_hex",
        names_from = "COD_ESPECIE",
        values_from = "n",
        names_prefix = "addr_type",
        values_fill = list(n = 0L)
      )

    for (k in 1:8) {
      nm <- paste0("addr_type", k)
      if (!nm %in% names(counts_wide)) counts_wide[[nm]] <- 0L
    }

    counts_wide <- counts_wide |>
      dplyr::select("id_hex", dplyr::all_of(paste0("addr_type", 1:8))) |>
      dplyr::mutate(
        dplyr::across(dplyr::starts_with("addr_type"), ~ as.integer(.x))
      )

    out <- hex_grid |>
      dplyr::left_join(counts_wide, by = "id_hex") |>
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("addr_type"),
          ~ dplyr::coalesce(as.integer(.x), 0L)
        )
      )
  }

  # Final safety: force integer and non-negative
  out <- out |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("addr_type"),
        ~ pmax(as.integer(.x), 0L)
      )
    )

  if (verbose) {
    cli::cli_progress_done("Step 3/3: Counting address species per hexagon...")
  }

  return(out)
}


# -----------------------------------------------------------------------------
# Internal: R backend, counts per H3 cell for one municipality file
# -----------------------------------------------------------------------------
.cnefe_counts_h3_r <- function(zip_path, h3_resolution) {
  tab <- read_cnefe(
    file = zip_path,
    output = "arrow",
    verbose = FALSE
  )

  # Verbs are pushed down to the Arrow table and collected last (#80 R1.10).
  # as.data.frame() first would materialize all 34 columns as an R data frame
  # before three of them are kept. Measured on Fortaleza, 1.19M rows: peak
  # memory falls from 108.4 MB to 68.3 MB.
  df <- tab |>
    dplyr::transmute(
      LONGITUDE = as.numeric(.data$LONGITUDE),
      LATITUDE = as.numeric(.data$LATITUDE),
      COD_ESPECIE = as.integer(.data$COD_ESPECIE)
    ) |>
    dplyr::filter(
      !is.na(.data$LONGITUDE),
      !is.na(.data$LATITUDE),
      !is.na(.data$COD_ESPECIE),
      .data$COD_ESPECIE %in% 1L:8L
    ) |>
    dplyr::collect()

  if (nrow(df) == 0L) {
    return(dplyr::tibble(
      id_hex = character(0),
      COD_ESPECIE = integer(0),
      n = integer(0)
    ))
  }

  coords <- df |>
    dplyr::transmute(lon = .data$LONGITUDE, lat = .data$LATITUDE)

  id_hex <- suppressMessages(
    h3jsr::point_to_cell(coords, res = h3_resolution, simple = TRUE)
  )

  df |>
    dplyr::mutate(id_hex = as.character(id_hex)) |>
    dplyr::filter(!is.na(.data$id_hex)) |>
    dplyr::count(.data$id_hex, .data$COD_ESPECIE, name = "n") |>
    dplyr::mutate(
      COD_ESPECIE = as.integer(.data$COD_ESPECIE),
      n = as.integer(.data$n)
    )
}


# -----------------------------------------------------------------------------
# Internal: User-provided polygon aggregation
# -----------------------------------------------------------------------------
.cnefe_counts_user_poly <- function(
  code_muni,
  year,
  polygon,
  crs_output,
  backend,
  cnefe_index,
  verbose,
  cache = TRUE,
  cache_dir = NULL,
  detected = NULL
) {
  # ---------------------------------------------------------------------------
  # Step 1/2: Ensure the cached data file exists and prepare polygon
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 1/2: Ensuring data and preparing polygon...",
                           msg_done = "Step 1/2 (Data and polygon ready)")
  }

  zip_infos <- .cnefe_ensure_zips(
    code_muni,
    index = cnefe_index,
    cache = cache,
    cache_dir = cache_dir,
    year = year,
    verbose = verbose
  )

  # Store original CRS for output transformation
  original_crs <- sf::st_crs(polygon)

  # Determine output CRS: use crs_output if provided, otherwise use original
  if (is.null(crs_output)) {
    output_crs <- original_crs
  } else {
    output_crs <- sf::st_crs(crs_output)
  }

  # Fix invalid geometries before any spatial operation
  polygon <- sf::st_make_valid(polygon)

  # Transform polygon to WGS84 internally for spatial join with CNEFE points
  polygon_4326 <- sf::st_transform(polygon, 4326)

  # Add row ID for joining
  polygon_4326 <- polygon_4326 |>
    dplyr::mutate(.poly_row_id = dplyr::row_number())

  if (verbose) {
    cli::cli_progress_done("Step 1/2: Ensuring data and preparing polygon...")
  }

  # ---------------------------------------------------------------------------
  # Step 2/2: Read CNEFE, spatial join, and count addresses per polygon
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 2/2: Counting addresses per polygon...",
                           msg_done = "Step 2/2 (Addresses counted)")
  }

  if (identical(backend, "duckdb")) {
    rlang::check_installed(
      "DBI",
      reason = "to use backend = 'duckdb' in `cnefe_counts()`."
    )
    rlang::check_installed(
      "duckdb",
      reason = "to use backend = 'duckdb' in `cnefe_counts()`."
    )

    join_result <- .cnefe_counts_user_poly_duckdb(
      zip_infos = zip_infos,
      code_munis = code_muni,
      polygon = polygon_4326,
      verbose = verbose
    )
  } else {
    # Backend "r": read via Arrow, spatial join via sf, one municipality at a
    # time. Each file is reduced to its counts per polygon before the next one
    # is read, so the peak memory is that of the largest municipality.
    join_result <- .combine_poly_joins(lapply(
      zip_infos,
      function(info) {
        .cnefe_counts_user_poly_r(
          zip_path = info$zip_path,
          polygon = polygon_4326,
          verbose = verbose
        )
      }
    ))
  }

  # Extract coverage statistics
  total_points <- join_result$total_points
  points_matched <- join_result$points_matched
  points_outside <- join_result$points_outside
  counts_long <- join_result$counts

  # Check for zero coverage
  if (points_matched == 0L) {
    .abort_no_points_in_polygons(code_muni, detected)
  }

  # Points of the municipalities left outside the polygons: a warning when the
  # user chose the municipalities, nothing when they were detected. Points in
  # more than one polygon open that warning, or warn alone without it (#120).
  .report_polygon_coverage(
    points_matched = points_matched,
    total_points = total_points,
    detected = detected,
    overlap = .polygon_overlap_line(
      join_result$points_multi,
      points_matched,
      "each is counted in every polygon that contains it, and the counts summed over the polygons exceed the number of addresses."
    )
  )

  # Pivot to wide format
  if (nrow(counts_long) == 0L || all(is.na(counts_long$.poly_row_id))) {
    # No points in any polygon
    counts_wide <- dplyr::tibble(
      .poly_row_id = integer(0)
    )
    for (k in 1:8) {
      counts_wide[[paste0("addr_type", k)]] <- integer(0)
    }
  } else {
    counts_wide <- counts_long |>
      dplyr::filter(!is.na(.data$.poly_row_id)) |>
      tidyr::pivot_wider(
        id_cols = ".poly_row_id",
        names_from = "COD_ESPECIE",
        values_from = "n",
        names_prefix = "addr_type",
        values_fill = list(n = 0L)
      )
  }

  # Ensure all addr_type columns exist
  for (k in 1:8) {
    nm <- paste0("addr_type", k)
    if (!nm %in% names(counts_wide)) counts_wide[[nm]] <- 0L
  }

  counts_wide <- counts_wide |>
    dplyr::select(".poly_row_id", dplyr::all_of(paste0("addr_type", 1:8))) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("addr_type"), ~ as.integer(.x))
    )

  # Join back to polygon (using the 4326 version with row IDs)
  out <- polygon_4326 |>
    dplyr::left_join(counts_wide, by = ".poly_row_id") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("addr_type"),
        ~ dplyr::coalesce(as.integer(.x), 0L)
      )
    ) |>
    dplyr::select(-".poly_row_id")

  # Final safety: force integer and non-negative
  out <- out |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("addr_type"),
        ~ pmax(as.integer(.x), 0L)
      )
    )

  # Transform to output CRS
  out <- sf::st_transform(out, output_crs)

  if (verbose) {
    cli::cli_progress_done("Step 2/2: Counting addresses per polygon...")
  }

  return(out)
}


# -----------------------------------------------------------------------------
# Internal: DuckDB backend for user polygon aggregation
# -----------------------------------------------------------------------------
.cnefe_counts_user_poly_duckdb <- function(
  zip_infos,
  code_munis,
  polygon,
  verbose
) {
  con <- .duckdb_connect(
      extensions = "spatial",
    reason = "to use backend = 'duckdb' in `cnefe_counts()`.",
    fallback = "backend = \"r\"",
    verbose = verbose
  )

  # One SELECT per municipality file, joined by UNION ALL: the points of the
  # whole region go through a single spatial join, so a polygon that crosses a
  # municipal border receives the addresses of both sides (#120).
  src_sql <- .cnefe_points_union_sql(
    con,
    zip_infos,
    code_munis,
    verbose = verbose
  )

  # Create CNEFE points table in DuckDB with point geometry
  DBI::dbExecute(con, sprintf(
    "
    CREATE TABLE cnefe_pts AS
    SELECT
      ROW_NUMBER() OVER () AS pt_id,
      lon,
      lat,
      COD_ESPECIE,
      ST_Point(lon, lat) AS geom
    FROM (
      %s
    )
    WHERE
      lon IS NOT NULL AND lat IS NOT NULL
      AND COD_ESPECIE BETWEEN 1 AND 8;
    ",
    src_sql
  ))

  total_points <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cnefe_pts;")$n[1]

  if (total_points == 0L) {
    return(list(
      counts = dplyr::tibble(
        .poly_row_id = integer(0),
        COD_ESPECIE = integer(0),
        n = integer(0)
      ),
      total_points = 0L,
      points_matched = 0L,
      points_outside = 0L,
      points_multi = 0L
    ))
  }

  # Write user polygon to DuckDB via duckspatial
  invisible(
    .duckdb_quiet(
      duckspatial::ddbs_write_table(
        conn = con,
        # Normalize geometry column to "geom"; duckspatial preserves the
        # input sf geometry name, but the SQL below hardcodes "geom".
        data = sf::st_set_geometry(polygon[, ".poly_row_id"], "geom"),
        name = "user_polygons",
        overwrite = TRUE
      )
    )
  )

  # duckspatial 1.0.0 (DuckDB 1.5+) writes GEOMETRY with embedded CRS metadata
  # (e.g. GEOMETRY('OGC:CRS84')), which DuckDB's RTREE index does not accept.
  # A WKB round-trip strips the CRS parameter and yields plain GEOMETRY.
  DBI::dbExecute(con,
    "ALTER TABLE user_polygons ALTER COLUMN geom SET DATA TYPE GEOMETRY
     USING ST_GeomFromWKB(ST_AsWKB(geom));"
  )

  # Spatial index on user polygons for faster joins
  DBI::dbExecute(
    con,
    "CREATE INDEX IF NOT EXISTS poly_geom_idx ON user_polygons USING RTREE (geom);"
  )

  # Spatial join in DuckDB via ST_Within (LEFT JOIN to track coverage)
  DBI::dbExecute(con,
    "
    CREATE TABLE joined AS
    SELECT
      p.pt_id,
      p.COD_ESPECIE,
      u.\".poly_row_id\" AS poly_row_id
    FROM cnefe_pts p
    LEFT JOIN user_polygons u
      ON ST_Within(p.geom, u.geom);
    "
  )

  # Coverage stats (computed inside DuckDB)
  coverage <- DBI::dbGetQuery(con,
    "
    SELECT
      COUNT(*) FILTER (WHERE k >= 1) AS matched,
      COUNT(*) FILTER (WHERE k > 1) AS multi
    FROM (
      SELECT pt_id, COUNT(poly_row_id) AS k
      FROM joined
      GROUP BY pt_id
    );
    "
  )
  unique_pts_matched <- as.integer(coverage$matched[1])
  # Points inside two or more polygons, for the overlap warning (#120)
  points_multi <- as.integer(coverage$multi[1])
  points_outside <- total_points - unique_pts_matched

  # Aggregate counts per polygon and species (inside DuckDB)
  counts_long <- DBI::dbGetQuery(con,
    "
    SELECT
      poly_row_id AS \".poly_row_id\",
      COD_ESPECIE,
      COUNT(*)::INTEGER AS n
    FROM joined
    WHERE poly_row_id IS NOT NULL
    GROUP BY poly_row_id, COD_ESPECIE;
    "
  ) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      COD_ESPECIE = as.integer(.data$COD_ESPECIE),
      n = as.integer(.data$n)
    )

  return(list(
    counts = counts_long,
    total_points = total_points,
    points_matched = unique_pts_matched,
    points_outside = points_outside,
    points_multi = points_multi
  ))
}


# -----------------------------------------------------------------------------
# Internal: R backend for user polygon aggregation
# -----------------------------------------------------------------------------
.cnefe_counts_user_poly_r <- function(
  zip_path,
  polygon,
  verbose
) {
  # Read CNEFE data via Arrow
  tab <- read_cnefe(
      file = zip_path,
      output = "arrow",
      verbose = FALSE
    )

  # Verbs are pushed down to the Arrow table and collected last (#80 R1.10).
  # as.data.frame() first would materialize all 34 columns as an R data frame
  # before three of them are kept. Measured on Fortaleza, 1.19M rows: peak
  # memory falls from 108.4 MB to 68.3 MB.
  df <- tab |>
    dplyr::transmute(
      LONGITUDE = as.numeric(.data$LONGITUDE),
      LATITUDE = as.numeric(.data$LATITUDE),
      COD_ESPECIE = as.integer(.data$COD_ESPECIE)
    ) |>
    dplyr::filter(
      !is.na(.data$LONGITUDE),
      !is.na(.data$LATITUDE),
      !is.na(.data$COD_ESPECIE),
      .data$COD_ESPECIE %in% 1L:8L
    ) |>
    dplyr::collect()

  total_points <- nrow(df)

  if (total_points == 0L) {
    return(list(
      counts = dplyr::tibble(
        .poly_row_id = integer(0),
        COD_ESPECIE = integer(0),
        n = integer(0)
      ),
      total_points = 0L,
      points_matched = 0L,
      points_outside = 0L,
      points_multi = 0L
    ))
  }

  # Convert to sf points and add point ID
  cnefe_pts <- sf::st_as_sf(
    df,
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4326
  )
  cnefe_pts$.pt_id <- seq_len(nrow(cnefe_pts))

  # Spatial join
  joined <- sf::st_join(cnefe_pts, polygon[, ".poly_row_id"], join = sf::st_within)

  # Count unique points that matched at least one polygon
  hits <- tabulate(joined$.pt_id[!is.na(joined$.poly_row_id)], nbins = total_points)
  unique_pts_matched <- sum(hits > 0L)
  # Points inside two or more polygons, for the overlap warning (#120)
  points_multi <- sum(hits > 1L)
  points_outside <- total_points - unique_pts_matched

  # Count by polygon and species
  counts_long <- joined |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(.data$.poly_row_id)) |>
    dplyr::count(.data$.poly_row_id, .data$COD_ESPECIE, name = "n") |>
    dplyr::mutate(
      COD_ESPECIE = as.integer(.data$COD_ESPECIE),
      n = as.integer(.data$n)
    )

  return(list(
    counts = counts_long,
    total_points = total_points,
    points_matched = unique_pts_matched,
    points_outside = points_outside,
    points_multi = points_multi
  ))
}

