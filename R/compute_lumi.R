#' Compute land-use mix indicators on a spatial grid
#'
#' @description
#' `compute_lumi()` reads CNEFE records for one or more municipalities,
#' assigns each address point to spatial units (either H3 hexagonal cells or
#' user-provided polygons), and computes the residential proportion (`p_res`) and land-use mix
#' indices, such as the Entropy Index (`ei`), the Herfindahl-Hirschman Index (`hhi`),
#' the Balance Index (`bal`), the Index of Concentration at Extremes (`ice`), the adapted HHI (`hhi_adp`),
#' and the Bidirectional Global-centered Balance Index (`bgbi`), following the methodology
#' proposed in Pedreira Junior et al. (2025, 2026). The 2026 article introduces the BGBI,
#' and the adapted HHI is documented in the 2025 preprint.
#'
#' @param code_muni Numeric or character. One or more seven-digit IBGE
#'   municipality codes, for example `2927408` or `c(2927408, 2919207)`.
#'   Repeated codes are dropped. With several codes, their addresses are
#'   pooled into one analysis (see "Several municipalities").
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
#'   supplied, the indices are computed for these polygons instead of an H3
#'   grid. Features with the same geometry as an earlier feature are dropped,
#'   with a message, and the first one is kept. Under `scope = "muni"`, a
#'   warning reports how many addresses of the municipalities in `code_muni`
#'   fall outside the polygons. When polygons overlap, a warning also reports how many
#'   addresses fall in more than one of them, since those enter the indices of
#'   each. If no address falls inside the polygons, the function stops with an
#'   error.
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
#'   cells that intersect those boundaries. The scope also decides which
#'   addresses P comes from (see "The citywide baseline P").
#' @param p_target Numeric or `NULL`. A single number strictly between 0 and 1
#'   that replaces the observed P, for example a residential share set by a
#'   land-use plan (`P*` in Pedreira Junior et al., 2026). `NULL` (the default)
#'   computes P from the data (see "The citywide baseline P").
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
#'   extension, reading the cached gzipped CSV directly. `"r"` computes H3 in R
#'   using h3jsr instead, and needs no DuckDB extension for the indices. The
#'   municipal boundaries of the H3 grid are still downloaded through DuckDB's
#'   httpfs extension the first time, on either backend, and if that fails the
#'   simplified geobr boundaries are used instead.
#'
#'   `"r"` exists for environments where DuckDB extensions cannot be installed,
#'   such as some restricted computing clusters. It is **not** the lighter
#'   option: it materializes the filtered address table in R memory, so its
#'   footprint grows with the municipality, while DuckDB aggregates in a
#'   streaming fashion and stays nearly flat. On São Paulo (5.7 million
#'   addresses) the measured peak is about 8.4 GB under `"r"` against 0.6 GB
#'   under `"duckdb"`, alongside being roughly 15 times slower.
#'
#'   If the constraint is memory rather than installability, keep the DuckDB
#'   backend and cap it with the `cnefetools.duckdb_config` option instead. See
#'   `?cnefetools` for that option, and the benchmark article at
#'   <https://pedreirajr.github.io/cnefetools/articles/bench_duckdb.html> for
#'   the measurements.
#'
#' @return An [`sf::sf`] object containing:
#' \describe{
#'   \item{When `polygon` is `NULL` (H3 grid):}{
#'     \itemize{
#'       \item `id_hex`: H3 cell identifier
#'       \item `p_res`, `ei`, `hhi`, `bal`, `ice`, `hhi_adp`, `bgbi`: land-use
#'         mix indicators
#'       \item `geometry`: hexagon geometry (CRS 4326)
#'     }
#'   }
#'   \item{When `polygon` is supplied:}{
#'     \itemize{
#'       \item Original columns from `polygon`
#'       \item `p_res`, `ei`, `hhi`, `bal`, `ice`, `hhi_adp`, `bgbi`: land-use
#'         mix indicators
#'       \item `geometry`: polygon geometry (in the original or `crs_output` CRS)
#'     }
#'   }
#' }
#'
#' @details
#' ## Binary land-use classification
#'
#' The indices computed here rest on a binary split. An address is counted as
#' residential when `COD_ESPECIE == 1` (private household), and as
#' non-residential otherwise. This follows the formulation of the indices as
#' published in Pedreira Junior et al. (2026), where the measures are defined
#' and empirically validated on that two-category basis.
#'
#' ## Exclusion of buildings under construction
#'
#' `compute_lumi()` drops records with `COD_ESPECIE == 7` (building under
#' construction or renovation), because such records describe a transitional
#' state rather than a realized land use. Note that [cnefe_counts()] does **not**
#' apply this exclusion and reports these records as `addr_type7`.
#'
#' ## The citywide baseline P
#'
#' Two indices use a citywide residential share P: the `bgbi` index, which is
#' referenced against it, and the Balance Index (`bal`), which uses it through
#' r = P / (1 - P). The other indices are computed entirely within each spatial
#' unit. Two properties of P are worth stating.
#'
#' First, P is computed from CNEFE address-type counts rather than from census
#' population, so it describes the distribution of address types and not the
#' distribution of residents.
#'
#' Second, by default (`scope = "muni"`) P is computed from every CNEFE
#' address of the municipalities in `code_muni`, including when `polygon` is
#' supplied, so it does not adapt to the area the supplied polygons happen to
#' cover. This is intended, as P describes the context the addresses sit in,
#' which is the municipality (or the region, when several are read), and a
#' sub-area of a city is still part of that wider context.
#'
#' `scope = "geom"` computes P from the addresses inside the output geometries
#' instead, each counted once where polygons overlap, which measures mix
#' relative to the area covered rather than relative to the city. For polygons that cover only part of a municipality,
#' such as a set of central neighborhoods, P is then the residential share of
#' those polygons alone. On an H3 grid, P becomes the share of the cells of the
#' grid, including the addresses of the neighboring municipalities that fall
#' in the cells along the border.
#'
#' `p_target` replaces P with a composition chosen by the analyst, such as a
#' goal set by a land-use plan, a zoning reform or a transit-oriented
#' development strategy. This is the extension proposed in the conclusions of
#' Pedreira Junior et al. (2026). With it, `bgbi` measures the distance to that
#' goal rather than to the observed city, and the same target can be used
#' across municipalities so that their values become comparable. `bal` with a
#' target is no longer the original Balance Index of Song et al. (2013), which
#' is defined against the observed citywide ratio.
#'
#' ## Several municipalities
#'
#' Pass a vector of codes to cover a region that spans more than one
#' municipality. The records of all of them are pooled before anything is
#' computed, the H3 grid is built over the union of their boundaries, and P is
#' the residential share of the whole region. Each cell appears once, and a
#' cell on a shared border holds the addresses of both sides.
#'
#' Because P belongs to the region, `bal` and `bgbi` are measured against the
#' regional share, not against each municipality's own, and adding a
#' municipality to `code_muni` changes them in every unit, not only near it. A
#' call per municipality gives each of them a different P, so indices from such
#' calls aren't comparable with one another, nor with a regional call.
#'
#' ## Border cells at the edge of the region
#'
#' Under the default `scope = "muni"`, only the addresses of the municipalities
#' in `code_muni` are read. A cell or polygon on the outer edge of the
#' requested area, one that also overlaps a municipality you did not request,
#' therefore describes only the part of it that lies inside the area: its
#' values (counts, `p_res` and indices) leave out the addresses of the
#' neighbor. Adding municipalities moves the edge outward but does not remove
#' it, and changes P.
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
#' `code_muni` this affects. With `scope = "muni"` they are still part of P,
#' and with `scope = "geom"` they are not.
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
#' a few meters. That neighbor is then downloaded and read too. Neither P nor
#' the indices change, since only the addresses inside the polygons are used.
#' If you know which municipalities the polygons belong to and want P to
#' describe them, use `scope = "muni"` with `code_muni`, which is also faster.
#'
#' @references
#' Pedreira Junior, J. U.; Louro, T. V.; Assis, L. B. M.; Brito, P. L.;
#' Bomfim, F. G. (2026).
#' BGBI: A citywide-referenced and bidirectional land use mix index for
#' planning and policy evaluation.
#' *Land Use Policy*, 169, 108135.
#' https://doi.org/10.1016/j.landusepol.2026.108135
#'
#' Pedreira Junior, J. U.; Louro, T. V.; Assis, L. B. M.; Brito, P. L. (2025).
#' Measuring land use mix with address-level census data.
#' *engrXiv* preprint. https://engrxiv.org/preprint/view/5975
#' (where the adapted HHI (`hhi_adp`) is documented)
#'
#' Massey, D. S. (2001).
#' The prodigal paradigm returns: ecology comes back to sociology.
#' In A. Booth & A. C. Crouter (Eds.),
#' *Does It Take a Village? Community Effects on Children, Adolescents, and Families*.
#' Lawrence Erlbaum.
#'
#' Song, Y.; Merlin, L.; Rodriguez, D. (2013).
#' Comparing measures of urban land use mix.
#' *Computers, Environment and Urban Systems*, 42, 1--13.
#' https://doi.org/10.1016/j.compenvurbsys.2013.08.001
#'
#' @examples
#' \donttest{
#' # Compute land-use mix indices on H3 hexagons
#' lumi <- compute_lumi(code_muni = 2929057, cache = FALSE)
#'
#' # Several municipalities in one call: one grid over their union and one P for
#' # the whole region (Salvador and its neighbor Lauro de Freitas)
#' lumi_region <- compute_lumi(code_muni = c(2927408, 2919207), cache = FALSE)
#'
#' # Compute land-use mix indices on user-provided polygons (neighborhoods of Lauro de Freitas-BA)
#' # Using geobr to download neighborhood boundaries
#' library(geobr)
#' nei_ldf <- subset(
#'   read_neighborhood(year = 2022),
#'   code_muni == 2919207
#' )
#' lumi_poly <- compute_lumi(
#'   code_muni = 2919207,
#'   polygon = nei_ldf,
#'   cache = FALSE
#' )
#'
#' # With scope = "geom", the municipalities the polygons intersect are read, and
#' # P comes from the addresses inside the neighborhoods
#' lumi_poly_geom <- compute_lumi(polygon = nei_ldf, scope = "geom", cache = FALSE)
#'
#' # BGBI against a target residential share of 70% instead of the observed one
#' lumi_target <- compute_lumi(code_muni = 2919207, p_target = 0.7, cache = FALSE)
#' }
#'
#' @export
compute_lumi <- function(
  code_muni = NULL,
  year = 2022,
  polygon_type = lifecycle::deprecated(),
  polygon = NULL,
  crs_output = NULL,
  h3_resolution = 9,
  scope = c("muni", "geom"),
  p_target = NULL,
  verbose = TRUE,
  cache = TRUE,
  cache_dir = NULL,
  backend = c("duckdb", "r")
) {
  polygon_type <- .resolve_polygon_mode(polygon, polygon_type, fn = "compute_lumi")
  backend <- match.arg(backend)
  p_target <- .validate_p_target(p_target)
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

  if (verbose) {
    cli::cli_alert_info("Processing municipality code {.val {code_muni}}...")
  }

  # We will return sf objects
  rlang::check_installed(
    "sf",
    reason = "to return an sf grid in `compute_lumi()`."
  )

  # Branch: H3 grid vs user-provided polygon
  if (polygon_type == "hex") {
    out <- .compute_lumi_h3(
      code_muni = code_muni,
      year = year,
      h3_resolution = h3_resolution,
      backend = backend,
      cnefe_index = cnefe_index,
      verbose = verbose,
      cache = cache,
      cache_dir = cache_dir,
      p_target = p_target,
      scope = scope$scope
    )
  } else {
    out <- .compute_lumi_user_poly(
      code_muni = code_muni,
      year = year,
      polygon = polygon,
      crs_output = crs_output,
      backend = backend,
      cnefe_index = cnefe_index,
      verbose = verbose,
      cache = cache,
      cache_dir = cache_dir,
      p_target = p_target,
      scope = scope$scope,
      detected = detected
    )
  }

  return(out)
}


# -----------------------------------------------------------------------------
# Internal: Shared LUMI index computation helpers
# -----------------------------------------------------------------------------

# Balance index (BAL)
.bal_fun <- function(p, P) {
  num <- abs(p - (P / (1 - P)) * (1 - p))
  den <- p + (P / (1 - P)) * (1 - p)
  ifelse(is.na(num) | is.na(den), NA_real_, 1 - num / den)
}

# BGBI function
.bgbi_fun <- function(p, P) {
  num <- (2 * p - 1) - (2 * P - 1)
  den <- 1 - (2 * p - 1) * (2 * P - 1)
  den[den == 0] <- NA_real_
  ifelse(is.na(num) | is.na(den), NA_real_, num / den)
}

# Numerically safe entropy term: treat 0*log(0) as 0
.safe_plogp <- function(x) {
  ifelse(is.na(x) | x <= 0, 0, x * log(x))
}

#' Compute LUMI indices from n_res and n_tot columns
#'
#' Adds p_res, ei, hhi, bal, ice, hhi_adp, bgbi columns to a data frame.
#'
#' @param df A data frame with `n_res` and `n_tot` columns.
#' @param P Global residential proportion for the municipality.
#' @return The data frame with LUMI columns added.
#' @keywords internal
#' @noRd
.compute_lumi_indices <- function(df, P) {
  df |>
    dplyr::mutate(
      p_res = dplyr::if_else(
        .data$n_tot > 0,
        .data$n_res / .data$n_tot,
        NA_real_
      ),
      q_rest = dplyr::if_else(!is.na(.data$p_res), 1 - .data$p_res, NA_real_),

      # EI (k=2)
      ei = dplyr::if_else(
        !is.na(.data$p_res),
        -(.safe_plogp(.data$p_res) + .safe_plogp(.data$q_rest)) / log(2),
        NA_real_
      ),

      # HHI (2 categories)
      hhi = dplyr::if_else(
        !is.na(.data$p_res),
        (.data$p_res^2 + .data$q_rest^2),
        NA_real_
      ),

      # BAL
      bal = dplyr::if_else(
        !is.na(.data$p_res),
        .bal_fun(.data$p_res, P),
        NA_real_
      ),

      # scaled HHI (min=0.5 for k=2)
      hhi_sc = dplyr::if_else(
        !is.na(.data$hhi),
        (.data$hhi - 0.5) / (1 - 0.5),
        NA_real_
      ),

      # ICE (Index of Concentration at Extremes)
      ice = dplyr::if_else(
        !is.na(.data$p_res),
        .data$p_res - .data$q_rest,
        NA_real_
      ),

      hhi_adp = dplyr::if_else(
        !is.na(.data$p_res) & !is.na(.data$hhi_sc),
        sign(.data$p_res - .data$q_rest) * .data$hhi_sc,
        NA_real_
      ),

      bgbi = dplyr::if_else(
        !is.na(.data$p_res),
        .bgbi_fun(.data$p_res, P),
        NA_real_
      )
    )
}


# -----------------------------------------------------------------------------
# Internal: H3 grid aggregation (original behavior)
# -----------------------------------------------------------------------------
.compute_lumi_h3 <- function(
  code_muni,
  year,
  h3_resolution,
  backend,
  cnefe_index,
  verbose,
  cache = TRUE,
  cache_dir = NULL,
  p_target = NULL,
  scope = "muni"
) {
  # ---------------------------------------------------------------------------
  # Step 1/3: Build the H3 grid and ensure the cached data files exist. The
  # grid comes first because, with scope = "geom", it decides which
  # municipalities are read (#120).
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 1/3: Building the H3 grid and ensuring the CNEFE data...",
                           msg_done = "Step 1/3 (Grid and CNEFE data ready)")
  }

  grid_set <- .h3_grid_and_read_set(code_muni, scope, h3_resolution, year = year,
                                    cache = cache, cache_dir = cache_dir)
  hex_grid <- grid_set$hex_grid
  read_munis <- grid_set$read_munis

  zip_infos <- .cnefe_ensure_zips(
    read_munis,
    index = cnefe_index,
    cache = cache,
    cache_dir = cache_dir,
    year = year,
    verbose = verbose
  )


  if (verbose) {
    cli::cli_progress_done("Step 1/3: Building the H3 grid and ensuring the CNEFE data...")
  }

  # ---------------------------------------------------------------------------
  # Step 2/3: Aggregate counts per hex (n_res, n_tot)
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 2/3: Counting addresses per H3 cell...",
                           msg_done = "Step 2/3 (Addresses counted)")
  }

  counts_hex <- NULL

  if (identical(backend, "duckdb")) {
    rlang::check_installed(
      "DBI",
      reason = "to use backend = 'duckdb' in `compute_lumi()`."
    )
    con <- .duckdb_connect(
      extensions = "h3",
      reason = "to use backend = 'duckdb' in `compute_lumi()`.",
      fallback = "backend = \"r\"",
      verbose = verbose
    )

    # One SELECT per municipality file, joined by UNION ALL. The H3 cell comes
    # from the coordinates of each address, so a cell on a shared border gets the
    # addresses of both sides in the same GROUP BY and appears once, and the
    # totals that make up P cover the whole region (#120, R2 comment 1).
    src_sql <- .cnefe_points_union_sql(
      con,
      zip_infos,
      read_munis,
      verbose = verbose
    )

    # only keep the columns needed; exclude COD_ESPECIE == 7. `own` flags the
    # addresses of the municipalities in code_muni, the only ones the report of
    # addresses outside the grid is about (#120).
    sql <- sprintf(
      "
        WITH src AS (
          %s
        ),
        filtered AS (
          SELECT
            lower(hex(CAST(h3_latlng_to_cell(lat, lon, %d) AS UBIGINT))) AS id_hex,
            code_muni IN (%s) AS own,
            COD_ESPECIE AS cod
          FROM src
          WHERE
            lon IS NOT NULL AND lat IS NOT NULL
            AND COD_ESPECIE BETWEEN 1 AND 8
            AND COD_ESPECIE != 7
        )
        SELECT
          id_hex,
          own,
          SUM(CASE WHEN cod = 1 THEN 1 ELSE 0 END)::BIGINT AS n_res,
          COUNT(*)::BIGINT AS n_tot
        FROM filtered
        GROUP BY 1, 2;
      ",
      src_sql,
      as.integer(h3_resolution),
      paste(as.integer(code_muni), collapse = ", ")
    )

    counts_hex <- .duckdb_quiet(DBI::dbGetQuery(con, sql)) |>
      dplyr::as_tibble() |>
      dplyr::mutate(
        id_hex = as.character(.data$id_hex),
        own = as.logical(.data$own),
        n_res = as.integer(.data$n_res),
        n_tot = as.integer(.data$n_tot)
      )

  } else {
    # backend "r": each municipality is reduced to its counts per cell before
    # the next one is read, so the peak memory is that of the largest one.
    counts_hex <- lapply(
      seq_along(zip_infos),
      function(i) {
        .compute_lumi_h3_r(zip_infos[[i]]$zip_path, h3_resolution) |>
          dplyr::mutate(own = read_munis[[i]] %in% code_muni)
      }
    ) |>
      dplyr::bind_rows()

    if (nrow(counts_hex) == 0L) {
      if (verbose) {
        cli::cli_alert_warning(
          "No valid CNEFE points after filtering (COD_ESPECIE 1:8, excluding 7). Returning an empty {.cls sf}."
        )
      }
      return(.empty_lumi_sf())
    }
  }

  if (verbose) {
    cli::cli_progress_done("Step 2/3: Counting addresses per H3 cell...")
  }

  if (is.null(counts_hex) || nrow(counts_hex) == 0L) {
    if (verbose) {
      cli::cli_alert_warning("No hexagons found after aggregation. Returning an empty {.cls sf}.")
    }
    return(.empty_lumi_sf())
  }

  # ---------------------------------------------------------------------------
  # Step 3/3: Compute P and the indices on the grid
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 3/3: Computing LUMI on the grid...",
                           msg_done = "Step 3/3 (Land use mix indices computed)")
  }

  # Addresses whose cell is not in the grid are dropped by the join below (#120)
  in_grid <- counts_hex$id_hex %in% hex_grid$id_hex

  # Citywide residential proportion P (COD_ESPECIE == 7 already excluded): the
  # analyst's target, every address of code_muni (scope = "muni", where only
  # code_muni is read), or the addresses in the cells of the grid, whichever
  # municipality they belong to (scope = "geom").
  P <- if (!is.null(p_target)) {
    p_target
  } else if (identical(scope, "geom")) {
    sum(counts_hex$n_res[in_grid]) / sum(counts_hex$n_tot[in_grid])
  } else {
    sum(counts_hex$n_res, na.rm = TRUE) / sum(counts_hex$n_tot, na.rm = TRUE)
  }

  p_note <- if (!is.null(p_target)) {
    character(0)
  } else if (identical(scope, "geom")) {
    c("i" = "P was computed from the cells of the grid only, so they are not part of it either.")
  } else {
    c("i" = "Under {.code scope = \"muni\"}, P covers every address of these municipalities, these addresses included. On the other hand, {.code scope = \"geom\"} computes P from the cells of the grid only, and also adds to the border cells the addresses of neighboring municipalities that fall inside them.")
  }
  own <- counts_hex$own
  .warn_outside_grid(
    n_outside = sum(counts_hex$n_tot[own & !in_grid]),
    n_total = sum(counts_hex$n_tot[own]),
    extra = p_note
  )

  counts_hex <- counts_hex |>
    dplyr::group_by(.data$id_hex) |>
    dplyr::summarise(
      n_res = as.integer(sum(.data$n_res)),
      n_tot = as.integer(sum(.data$n_tot)),
      .groups = "drop"
    )

  out <- hex_grid |>
    dplyr::left_join(counts_hex, by = "id_hex") |>
    dplyr::mutate(
      n_res = dplyr::coalesce(as.integer(.data$n_res), 0L),
      n_tot = dplyr::coalesce(as.integer(.data$n_tot), 0L)
    ) |>
    .compute_lumi_indices(P) |>
    dplyr::select(
      "id_hex",
      "p_res",
      "ei",
      "hhi",
      "bal",
      "ice",
      "hhi_adp",
      "bgbi",
      "geometry"
    )

  if (verbose) {
    cli::cli_progress_done("Step 3/3: Computing LUMI on the grid...")
  }

  return(out)
}


# -----------------------------------------------------------------------------
# Internal: R backend, counts per H3 cell for one municipality file
# -----------------------------------------------------------------------------
.compute_lumi_h3_r <- function(zip_path, h3_resolution) {
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
      .data$COD_ESPECIE %in% 1L:8L,
      .data$COD_ESPECIE != 7L
    ) |>
    dplyr::collect()

  if (nrow(df) == 0L) {
    return(dplyr::tibble(
      id_hex = character(0),
      n_res = integer(0),
      n_tot = integer(0)
    ))
  }

  coords <- df |>
    dplyr::transmute(lon = .data$LONGITUDE, lat = .data$LATITUDE)

  id_hex <- suppressMessages(h3jsr::point_to_cell(
    coords,
    res = h3_resolution,
    simple = TRUE
  ))

  df |>
    dplyr::mutate(id_hex = as.character(id_hex)) |>
    dplyr::filter(!is.na(.data$id_hex)) |>
    dplyr::group_by(.data$id_hex) |>
    dplyr::summarise(
      n_res = sum(.data$COD_ESPECIE == 1L, na.rm = TRUE),
      n_tot = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      n_res = as.integer(.data$n_res),
      n_tot = as.integer(.data$n_tot)
    )
}


# -----------------------------------------------------------------------------
# Internal: User-provided polygon aggregation
# -----------------------------------------------------------------------------
.compute_lumi_user_poly <- function(
  code_muni,
  year,
  polygon,
  crs_output,
  backend,
  cnefe_index,
  verbose,
  cache = TRUE,
  cache_dir = NULL,
  p_target = NULL,
  scope = "muni",
  detected = NULL
) {
  # ---------------------------------------------------------------------------
  # Step 1/3: Ensure the cached data file exists and prepare polygon
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 1/3: Ensuring data and preparing polygon...",
                           msg_done = "Step 1/3 (Data and polygon ready)")
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
    cli::cli_progress_done("Step 1/3: Ensuring data and preparing polygon...")
  }

  # ---------------------------------------------------------------------------
  # Step 2/3: Read CNEFE points and perform spatial join
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 2/3: Counting addresses per polygon...",
                           msg_done = "Step 2/3 (Addresses counted)")
  }

  if (identical(backend, "duckdb")) {
    rlang::check_installed(
      "DBI",
      reason = "to use backend = 'duckdb' in `compute_lumi()`."
    )
    rlang::check_installed(
      "duckdb",
      reason = "to use backend = 'duckdb' in `compute_lumi()`."
    )

    join_result <- .compute_lumi_user_poly_duckdb(
      zip_infos = zip_infos,
      code_munis = code_muni,
      polygon = polygon_4326,
      verbose = verbose
    )
  } else {
    # One municipality at a time, reduced to counts per polygon before the next
    # file is read, so the peak memory is that of the largest municipality.
    join_result <- .combine_poly_joins(lapply(
      zip_infos,
      function(info) {
        .compute_lumi_user_poly_r(
          zip_path = info$zip_path,
          polygon = polygon_4326,
          verbose = verbose
        )
      }
    ))
  }

  if (verbose) {
    cli::cli_progress_done("Step 2/3: Counting addresses per polygon...")
  }

  # ---------------------------------------------------------------------------
  # Step 3/3: Report coverage, compute LUMI indices, and join back to polygon
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_progress_step("Step 3/3: Computing land use mix indices...",
                           msg_done = "Step 3/3 (Land use mix indices computed)")
  }

  # Extract coverage statistics
  total_points <- join_result$total_points
  points_matched <- join_result$points_matched
  points_outside <- join_result$points_outside
  counts <- join_result$counts
  total_n_res <- join_result$total_n_res
  total_n_tot <- join_result$total_n_tot

  # Check for zero coverage
  if (points_matched == 0L) {
    .abort_no_points_in_polygons(code_muni, detected)
  }

  .report_polygon_coverage(
    points_matched = points_matched,
    total_points = total_points,
    detected = detected,
    p_note = is.null(p_target),
    overlap = .polygon_overlap_line(
      join_result$points_multi,
      points_matched,
      "each enters the indices of every polygon that contains it."
    )
  )

  # P: the analyst's target, the addresses inside the polygons, each counted
  # once even where polygons overlap (scope = "geom"), or every address of
  # code_muni (scope = "muni") (#120).
  P <- if (!is.null(p_target)) {
    p_target
  } else if (identical(scope, "geom")) {
    join_result$matched_n_res / join_result$matched_n_tot
  } else {
    total_n_res / total_n_tot
  }

  # Join counts to polygon and compute LUMI indices
  if (nrow(counts) == 0L || all(is.na(counts$.poly_row_id))) {
    # No matched points: add empty LUMI columns
    out <- polygon_4326 |>
      dplyr::mutate(
        n_res = 0L,
        n_tot = 0L
      ) |>
      .compute_lumi_indices(P) |>
      dplyr::select(
        -"n_res", -"n_tot", -"q_rest", -"hhi_sc",
        -".poly_row_id"
      )
  } else {
    counts_per_poly <- counts |>
      dplyr::filter(!is.na(.data$.poly_row_id))

    out <- polygon_4326 |>
      dplyr::left_join(counts_per_poly, by = ".poly_row_id") |>
      dplyr::mutate(
        n_res = dplyr::coalesce(as.integer(.data$n_res), 0L),
        n_tot = dplyr::coalesce(as.integer(.data$n_tot), 0L)
      ) |>
      .compute_lumi_indices(P) |>
      dplyr::select(
        -"n_res", -"n_tot", -"q_rest", -"hhi_sc",
        -".poly_row_id"
      )
  }

  # Transform to output CRS
  out <- sf::st_transform(out, output_crs)

  if (verbose) {
    cli::cli_progress_done("Step 3/3: Computing land use mix indices...")
  }

  return(out)
}


# -----------------------------------------------------------------------------
# Internal: DuckDB backend for user polygon spatial join (compute_lumi)
# -----------------------------------------------------------------------------
.compute_lumi_user_poly_duckdb <- function(
  zip_infos,
  code_munis,
  polygon,
  verbose
) {
  res <- NULL

  con <- .duckdb_connect(
      extensions = "spatial",
    reason = "to use backend = 'duckdb' in `compute_lumi()`.",
    fallback = "backend = \"r\"",
    verbose = verbose
  )

  .duckdb_quiet({
    {
      # One SELECT per municipality file, joined by UNION ALL: the points of the
      # whole region go through a single spatial join, and the global totals
      # that make up P cover all of them (#120, R2 comment 1).
      src_sql <- .cnefe_points_union_sql(
        con,
        zip_infos,
        code_munis,
        verbose = verbose
      )

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
        AND COD_ESPECIE BETWEEN 1 AND 8
        AND COD_ESPECIE != 7;
      ",
        src_sql
      ))

      total_points <- DBI::dbGetQuery(
        con,
        "SELECT COUNT(*) AS n FROM cnefe_pts;"
      )$n[1]

  if (total_points == 0L) {
    return(list(
      counts = dplyr::tibble(
        .poly_row_id = integer(0),
        n_res = integer(0),
        n_tot = integer(0)
      ),
      total_points = 0L,
      points_matched = 0L,
      points_outside = 0L,
      points_multi = 0L,
      total_n_res = 0L,
      total_n_tot = 0L,
      matched_n_res = 0L,
      matched_n_tot = 0L
    ))
  }

  # Global totals from all municipality points (computed inside DuckDB)
  global_totals <- DBI::dbGetQuery(con,
    "
    SELECT
      SUM(CASE WHEN COD_ESPECIE = 1 THEN 1 ELSE 0 END)::INTEGER AS total_n_res,
      COUNT(*)::INTEGER AS total_n_tot
    FROM cnefe_pts;
    "
  )
  total_n_res <- as.integer(global_totals$total_n_res[1])
  total_n_tot <- as.integer(global_totals$total_n_tot[1])

  # Write user polygon to DuckDB via duckspatial
  duckspatial::ddbs_write_table(
    conn = con,
    # Normalize geometry column to "geom"; duckspatial preserves the
    # input sf geometry name, but the SQL below hardcodes "geom".
    data = sf::st_set_geometry(polygon[, ".poly_row_id"], "geom"),
    name = "user_polygons",
    overwrite = TRUE
  )

  # duckspatial 1.0.0 (DuckDB 1.5+) writes GEOMETRY with embedded CRS metadata,
  # which RTREE does not accept. Strip CRS via WKB round-trip first.
  DBI::dbExecute(
    con,
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

  # Totals over the points inside any polygon, each point once even where
  # polygons overlap, for scope = "geom" (#120)
  matched_totals <- DBI::dbGetQuery(con,
    "
    SELECT
      COALESCE(SUM(CASE WHEN COD_ESPECIE = 1 THEN 1 ELSE 0 END), 0)::INTEGER AS n_res,
      COUNT(*)::INTEGER AS n_tot
    FROM (
      SELECT DISTINCT pt_id, COD_ESPECIE
      FROM joined
      WHERE poly_row_id IS NOT NULL
    );
    "
  )

  # Aggregate n_res and n_tot per polygon (inside DuckDB)
  counts <- DBI::dbGetQuery(con,
    "
    SELECT
      poly_row_id AS \".poly_row_id\",
      SUM(CASE WHEN COD_ESPECIE = 1 THEN 1 ELSE 0 END)::INTEGER AS n_res,
      COUNT(*)::INTEGER AS n_tot
    FROM joined
    WHERE poly_row_id IS NOT NULL
    GROUP BY poly_row_id;
    "
  ) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      n_res = as.integer(.data$n_res),
      n_tot = as.integer(.data$n_tot)
    )

  res <- list(
    counts = counts,
    total_points = total_points,
    points_matched = unique_pts_matched,
    points_outside = points_outside,
    points_multi = points_multi,
    total_n_res = total_n_res,
    total_n_tot = total_n_tot,
    matched_n_res = as.integer(matched_totals$n_res[1]),
    matched_n_tot = as.integer(matched_totals$n_tot[1])
  )

    }
  })

  return(res)

}


# -----------------------------------------------------------------------------
# Internal: R backend for user polygon spatial join (compute_lumi)
# -----------------------------------------------------------------------------
.compute_lumi_user_poly_r <- function(
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
      .data$COD_ESPECIE %in% 1L:8L,
      .data$COD_ESPECIE != 7L
    ) |>
    dplyr::collect()

  total_points <- nrow(df)

  if (total_points == 0L) {
    return(list(
      counts = dplyr::tibble(
        .poly_row_id = integer(0),
        n_res = integer(0),
        n_tot = integer(0)
      ),
      total_points = 0L,
      points_matched = 0L,
      points_outside = 0L,
      points_multi = 0L,
      total_n_res = 0L,
      total_n_tot = 0L,
      matched_n_res = 0L,
      matched_n_tot = 0L
    ))
  }

  # Global totals from all municipality points
  total_n_res <- as.integer(sum(df$COD_ESPECIE == 1L, na.rm = TRUE))
  total_n_tot <- as.integer(nrow(df))

  # Convert to sf points and add point ID
  cnefe_pts <- sf::st_as_sf(
    df,
    coords = c("LONGITUDE", "LATITUDE"),
    crs = 4326
  )
  cnefe_pts$.pt_id <- seq_len(nrow(cnefe_pts))

  # Spatial join
  joined <- sf::st_join(cnefe_pts, polygon[, ".poly_row_id"], join = sf::st_within)

  # Count unique points matched
  hits <- tabulate(joined$.pt_id[!is.na(joined$.poly_row_id)], nbins = total_points)
  matched_ids <- which(hits > 0L)
  unique_pts_matched <- length(matched_ids)
  # Points inside two or more polygons, for the overlap warning (#120)
  points_multi <- sum(hits > 1L)
  points_outside <- total_points - unique_pts_matched

  # Totals over the points inside any polygon, each point once even where
  # polygons overlap, for scope = "geom" (#120)
  matched_n_res <- as.integer(sum(cnefe_pts$COD_ESPECIE[matched_ids] == 1L))

  # Aggregate n_res and n_tot per polygon
  counts <- joined |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(.data$.poly_row_id)) |>
    dplyr::group_by(.data$.poly_row_id) |>
    dplyr::summarise(
      n_res = as.integer(sum(.data$COD_ESPECIE == 1L, na.rm = TRUE)),
      n_tot = as.integer(dplyr::n()),
      .groups = "drop"
    )

  return(list(
    counts = counts,
    total_points = total_points,
    points_matched = unique_pts_matched,
    points_outside = points_outside,
    points_multi = points_multi,
    total_n_res = total_n_res,
    total_n_tot = total_n_tot,
    matched_n_res = matched_n_res,
    matched_n_tot = as.integer(unique_pts_matched)
  ))
}
