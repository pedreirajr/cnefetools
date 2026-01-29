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

  # Post-process: inject an executable chunk that forces pkgdown to include

  # htmlwidget JS/CSS dependencies when it renders the pre-computed .Rmd.
  # Without this, the embedded widget HTML has no JavaScript to render it.
  lines <- readLines(output)
  uses_leafsync <- any(grepl("leafsync", readLines(f)))

  dep_chunk <- c(
    "",
    "```{r include=FALSE}",
    "# Hidden chunk to ensure pkgdown includes htmlwidget dependencies",
    "library(mapview)"
  )
  if (uses_leafsync) {
    dep_chunk <- c(dep_chunk, "library(leafsync)")
  }
  dep_chunk <- c(
    dep_chunk,
    "mapview::mapview()",
    "```",
    ""
  )

  # Insert after YAML front matter (second '---' line)
  yaml_markers <- which(lines == "---")
  insert_pos <- yaml_markers[2]
  lines <- c(lines[1:insert_pos], dep_chunk, lines[(insert_pos + 1):length(lines)])
  writeLines(lines, output)
  message("  Injected dependency chunk into ", output)
}
