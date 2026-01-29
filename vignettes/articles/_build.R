# Re-knit all pre-computed articles
# Run from the repo root:
#   source("vignettes/articles/_build.R")

devtools::load_all()

owd <- setwd("vignettes/articles")
on.exit(setwd(owd))

# Tell knitr the output target is HTML so htmlwidgets (mapview, leafsync)
# embed their HTML directly instead of attempting a screenshot.
knitr::opts_knit$set(rmarkdown.pandoc.to = "html")

orig_files <- list.files(pattern = "\\.Rmd\\.orig$")
for (f in orig_files) {
  output <- sub("\\.orig$", "", f)
  message("Knitting ", f, " -> ", output)
  knitr::knit(f, output)
}
