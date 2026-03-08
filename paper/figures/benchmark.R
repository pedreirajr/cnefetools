# Benchmark script: DuckDB vs pure-R backend for cnefe_counts()
#
# PURPOSE
# This script documents how the benchmark figures included in the paper
# (bench-cities.png and bench-h3.png) were generated. It is provided
# for transparency and is not intended to be run as part of the main
# reproducibility script (cnefetools.R).
#
# IMPORTANT NOTES ON REPRODUCIBILITY
# Absolute elapsed times vary across machines, operating systems, and
# runs due to CPU load, I/O scheduling, and DuckDB's JIT warm-up.
# The figures embedded in the paper reflect a single run on the authors'
# machine (14-core Intel Core i9-13900H, 32 GB RAM, Windows 11,
# R 4.3.2, DuckDB 1.4.1). A fresh run on a different machine will
# produce different absolute values. However, the speedup ratios
# between the DuckDB and pure-R backends are stable across machines,
# because both backends are subject to the same I/O and hardware
# constraints and the relative advantage of DuckDB's in-process
# columnar engine over row-oriented R operations does not depend on
# absolute clock speed.
#
# RUNTIME
# This script downloads CNEFE data for three large municipalities.
# ZIP files are cached after the first download (see cnefetools caching
# documentation), but the benchmark itself can take 10-30 minutes
# depending on the machine.
#
# OUTPUT
# bench-cities.png  -- Figure 5 in the paper
# bench-h3.png      -- Figure 6 in the paper
#
# HOW TO RUN
# Run this script with its directory as the working directory so that
# the output PNGs are saved alongside it:
#   setwd("paper/figures")
#   source("benchmark.R")

library(cnefetools)
library(dplyr)
library(ggplot2)

# Municipality codes
cod_spo <- 3550308  # São Paulo-SP      (~5.7 million addresses)
cod_ctb <- 4106902  # Curitiba-PR       (~900,000 addresses)
cod_vca <- 2933307  # Vitória da Conquista-BA (~200,000 addresses)

cods <- c(cod_vca, cod_ctb, cod_spo)

# ---------------------------------------------------------------------------
# Benchmark 1: Municipality size (H3 resolution fixed at 8)
# ---------------------------------------------------------------------------

time_duckdb_cities <- sapply(cods, function(i) {
  system.time(
    cnefe_counts(code_muni = i, polygon_type = "hex",
                 h3_resolution = 8, backend = "duckdb", verbose = FALSE)
  )
})

time_r_cities <- sapply(cods, function(i) {
  system.time(
    cnefe_counts(code_muni = i, polygon_type = "hex",
                 h3_resolution = 8, backend = "r", verbose = FALSE)
  )
})

city_labels <- c("Vitória da Conquista\n(~200k addresses)",
                 "Curitiba\n(~900k addresses)",
                 "São Paulo\n(~5.7M addresses)")

benchmark_cities <- data.frame(
  city    = rep(city_labels, 2),
  backend = rep(c("Pure R", "DuckDB"), each = 3),
  elapsed = c(time_r_cities["elapsed", ], time_duckdb_cities["elapsed", ])
) |>
  mutate(
    city    = factor(city, levels = city_labels),
    backend = factor(backend, levels = c("Pure R", "DuckDB"))
  )

p_cities <- ggplot(benchmark_cities,
                   aes(x = city, y = elapsed, fill = backend)) +
  geom_col(width = 0.6, position = position_dodge(width = 0.65)) +
  geom_text(
    aes(label = sprintf("%.2f s", elapsed)),
    position = position_dodge(width = 0.65),
    vjust = -0.4, size = 3
  ) +
  scale_fill_manual(values = c("Pure R" = "#E74C3C", "DuckDB" = "#2C3E50")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    x = NULL,
    y = "Elapsed time (s)",
    fill = "Backend"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position     = "top"
  )

ggsave("bench-cities.png", plot = p_cities,
       width = 7, height = 4, dpi = 300,
       path = ".")

# ---------------------------------------------------------------------------
# Benchmark 2: H3 resolution (municipality fixed at Curitiba-PR)
# ---------------------------------------------------------------------------

h3_res <- c(7, 9, 11)

time_duckdb_h3 <- sapply(h3_res, function(r) {
  system.time(
    cnefe_counts(code_muni = cod_ctb, polygon_type = "hex",
                 h3_resolution = r, backend = "duckdb", verbose = FALSE)
  )
})

time_r_h3 <- sapply(h3_res, function(r) {
  system.time(
    cnefe_counts(code_muni = cod_ctb, polygon_type = "hex",
                 h3_resolution = r, backend = "r", verbose = FALSE)
  )
})

benchmark_h3 <- data.frame(
  resolution = rep(as.character(h3_res), 2),
  backend    = rep(c("Pure R", "DuckDB"), each = 3),
  elapsed    = c(time_r_h3["elapsed", ], time_duckdb_h3["elapsed", ])
) |>
  mutate(
    resolution = factor(resolution, levels = as.character(h3_res)),
    backend    = factor(backend, levels = c("Pure R", "DuckDB"))
  )

p_h3 <- ggplot(benchmark_h3,
               aes(x = resolution, y = elapsed, fill = backend)) +
  geom_col(width = 0.6, position = position_dodge(width = 0.65)) +
  geom_text(
    aes(label = sprintf("%.2f s", elapsed)),
    position = position_dodge(width = 0.65),
    vjust = -0.4, size = 3
  ) +
  scale_fill_manual(values = c("Pure R" = "#E74C3C", "DuckDB" = "#2C3E50")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    x = "H3 resolution",
    y = "Elapsed time (s)",
    fill = "Backend"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position     = "top"
  )

ggsave("bench-h3.png", plot = p_h3,
       width = 7, height = 4, dpi = 300,
       path = ".")
