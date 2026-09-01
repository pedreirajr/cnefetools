# Shared summarising and plotting for the R2.8 benchmark.
#
# Sourced by both consumers of data-raw/bench_r2_8.csv:
#
#   - vignettes/articles/bench_duckdb.Rmd.orig  (the pkgdown article)
#   - paper/figures/benchmark.R                 (the manuscript figures)
#
# Neither measures anything. Measurement happens once, in data-raw/bench_r2_8.R,
# and both read the committed CSV. Keeping the derived quantities here as well
# means the two cannot disagree on a speedup, a rounding rule or a label, which
# is the failure the R Journal referees flagged for the submitted version: the
# article's opening claimed "up to 20x" while its own table showed 13.33.
#
# Requires dplyr and ggplot2 to be attached by the caller.
#
#   bench <- bench_load()                 # or bench_load(path) from elsewhere
#   bench_med <- bench_summarise(bench)

BENCH_CSV_DEFAULT <- "../../data-raw/bench_r2_8.csv"

# The harness writes plain-ASCII municipality names so the CSV is encoding-safe.
# Accents are restored at display time, here.
BENCH_CITY_KEY <- c("Vitoria da Conquista-BA", "Curitiba-PR", "Sao Paulo-SP")
BENCH_CITY_FULL <- c("Vitória da Conquista-BA", "Curitiba-PR",
                     "São Paulo-SP")
BENCH_CITY_SHORT <- c("Vitória da C.", "Curitiba", "São Paulo")

BENCH_FILL <- c("Pure R" = "#E74C3C", "DuckDB" = "#2C3E50")


bench_load <- function(path = BENCH_CSV_DEFAULT) {
  utils::read.csv(path, stringsAsFactors = FALSE)
}


#' Median across replicates, keeping the observed range for the error bars.
bench_summarise <- function(d) {
  d |>
    dplyr::group_by(
      .data$block, .data$fn, .data$muni, .data$n_addresses,
      .data$backend, .data$h3_res, .data$constrained
    ) |>
    dplyr::summarise(
      n_reps  = dplyr::n(),
      seconds = stats::median(.data$elapsed_s),
      lo      = min(.data$elapsed_s),
      hi      = max(.data$elapsed_s),
      peak_mb = stats::median(.data$peak_rss_mb),
      .groups = "drop"
    )
}


bench_city <- function(x, short = FALSE) {
  lv <- if (short) BENCH_CITY_SHORT else BENCH_CITY_FULL
  factor(lv[match(x, BENCH_CITY_KEY)], levels = lv)
}


bench_backend <- function(x) {
  factor(ifelse(x == "r", "Pure R", "DuckDB"), levels = c("Pure R", "DuckDB"))
}


#' Speedup of DuckDB over pure R, within whatever `group_col` names.
bench_speedup <- function(d, group_col) {
  d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) |>
    dplyr::summarise(
      speedup = .data$seconds[.data$backend == "r"] /
        .data$seconds[.data$backend == "duckdb"],
      .groups = "drop"
    ) |>
    dplyr::mutate(speedup = round(.data$speedup, 2))
}


#' The cities block, with display labels attached.
bench_cities <- function(bench_med) {
  levs <- paste0(sub("-..$", "", BENCH_CITY_FULL), "\n(",
                 c("~200k", "~900k", "~5.7M"), " addresses)")
  bench_med |>
    dplyr::filter(.data$block == "cities") |>
    dplyr::mutate(
      city_pretty = bench_city(.data$muni),
      city_lbl = factor(
        paste0(sub("-..$", "", .data$city_pretty), "\n(",
               .data$n_addresses, " addresses)"),
        levels = levs
      ),
      backend_lbl = bench_backend(.data$backend)
    )
}


#' Grouped bars of elapsed time by backend, with min-max whiskers.
#'
#' `x` is a bare column name.
bench_plot_time <- function(d, x, xlab = NULL, subtitle = NULL,
                            title = "Performance comparison: DuckDB vs pure R",
                            base_size = 13) {
  ggplot2::ggplot(d, ggplot2::aes(x = {{ x }}, y = .data$seconds,
                                  fill = .data$backend_lbl)) +
    ggplot2::geom_col(width = 0.6,
                      position = ggplot2::position_dodge(width = 0.65)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
      width = 0.12,
      position = ggplot2::position_dodge(width = 0.65),
      linewidth = 0.4, colour = "grey25"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f s", .data$seconds), y = .data$hi),
      position = ggplot2::position_dodge(width = 0.65),
      vjust = -0.6, size = 3
    ) +
    ggplot2::scale_fill_manual(values = BENCH_FILL) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.14))
    ) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  y = "Elapsed time (seconds)", x = xlab, fill = "Backend") +
    bench_theme(base_size)
}


#' Grouped bars of peak memory by backend. Labelled in MB below 1 GB.
bench_plot_memory <- function(d, x, xlab = NULL, subtitle = NULL,
                              title = "Peak memory: DuckDB vs pure R",
                              base_size = 13) {
  ggplot2::ggplot(d, ggplot2::aes(x = {{ x }}, y = .data$peak_mb / 1024,
                                  fill = .data$backend_lbl)) +
    ggplot2::geom_col(width = 0.6,
                      position = ggplot2::position_dodge(width = 0.65)) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(
        .data$peak_mb >= 1024,
        sprintf("%.1f GB", .data$peak_mb / 1024),
        sprintf("%.0f MB", .data$peak_mb)
      )),
      position = ggplot2::position_dodge(width = 0.65),
      vjust = -0.4, size = 3
    ) +
    ggplot2::scale_fill_manual(values = BENCH_FILL) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.14))
    ) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  y = "Peak memory (GB)", x = xlab, fill = "Backend") +
    bench_theme(base_size)
}


bench_theme <- function(base_size = 13) {
  list(
    ggplot2::theme_minimal(base_size = base_size),
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold",
                                         size = base_size + 2, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      panel.grid.major.x = ggplot2::element_blank()
    )
  )
}
