# Re-knit all pre-computed articles
# Run from the vignettes/articles/ directory:
#   source("_build.R")

orig_files <- list.files(pattern = "\\.Rmd\\.orig$")
for (f in orig_files) {
  output <- sub("\\.orig$", "", f)
  message("Knitting ", f, " -> ", output)
  knitr::knit(f, output)
}
