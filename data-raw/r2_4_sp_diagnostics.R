# R Journal 2026-37, R1 revision. Referee 2, comment R2.4.
#
# The referee asks what the dasymetric unallocation rate looks like in a large,
# complex municipality such as Sao Paulo, given that the Lauro de Freitas
# example reports 0.00%. This script produces that number.
#
# It mirrors the tracts_to_h3() call used for Fortaleza in the tracts_to
# article (same vars, same resolution) so the two diagnostics are directly
# comparable. The full log is written to data-raw/r2_4_sp_diagnostics.log.
#
# Run from the package root: Rscript data-raw/r2_4_sp_diagnostics.R
# Note that the cli diagnostics block is written to stderr by the function, so
# capture it from the console when re-running, not from the log file.

devtools::load_all(quiet = TRUE)

log_path <- file.path("data-raw", "r2_4_sp_diagnostics.log")
con <- file(log_path, open = "wt")
sink(con, type = "output")
sink(con, type = "message")

cat("run started:", format(Sys.time(), tz = "UTC", usetz = TRUE), "\n")
cat("cnefetools:", as.character(utils::packageVersion("cnefetools")), "\n")
cat("R:", R.version.string, "\n\n")

elapsed <- system.time({
  hex_sp <- tracts_to_h3(
    code_muni = 3550308, # Sao Paulo-SP
    h3_resolution = 9,
    vars = c("pop_ph", "race_preta"),
    cache = TRUE,
    verbose = TRUE
  )
})

cat("\nelapsed (s):\n")
print(elapsed)
cat("\nhexagons returned:", nrow(hex_sp), "\n")
cat("\ncolumn totals after interpolation:\n")
print(colSums(sf::st_drop_geometry(hex_sp)[, c("pop_ph", "race_preta")], na.rm = TRUE))
cat("\nrun finished:", format(Sys.time(), tz = "UTC", usetz = TRUE), "\n")

sink(type = "message")
sink(type = "output")
close(con)
