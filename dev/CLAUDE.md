# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Package Overview

**cnefetools** is an R package for working with 2022 Brazilian CNEFE
(National Address File for Statistical Purposes) data from IBGE. It
provides tools for downloading, reading, and analyzing address-level
data with spatial aggregation capabilities.

## Common Commands

``` r
# Development workflow
devtools::load_all()         # Load package for interactive development
devtools::document()         # Regenerate documentation from roxygen2 comments
devtools::test()             # Run all tests
devtools::check()            # Full R CMD check

# Run a single test file
testthat::test_file("tests/testthat/test-read_cnefe.R")

# Build package
R CMD BUILD . --no-manual --compact-vignettes=gs+qpdf
```

## Architecture

### Core Functions (R/)

- **read_cnefe.R** - Downloads and reads CNEFE CSV files for a
  municipality; returns Arrow tables or sf spatial objects
- **cnefe_counts.R** - Aggregates address counts to H3 hexagonal grid
  cells or user-provided polygons (DuckDB or pure-R backend)
- **compute_lumi.R** - Computes land-use mix indices (Entropy Index,
  HHI, BGBI, Balance Index)
- **tracts_to_h3.R** - Dasymetric interpolation from census tracts to H3
  via CNEFE dwelling points
- **build_h3_grid.R** - Internal utility to create H3 grids from IDs or
  municipality boundary
- **utils-internal.R** - Input validation, cache management, download
  helpers, DuckDB extension utilities

### Key Design Patterns

- **Dual backends**: Functions support both DuckDB (SQL-based, fast) and
  pure-R backends via `use_duckdb` parameter
- **Caching**: ZIP files are cached in user directory
  (`tools::R_user_dir("cnefetools", "cache")`) for automatic reuse
- **DuckDB extensions**: Uses h3 and zipfs extensions loaded via helper
  functions in utils-internal.R

### Testing

Tests use fixture-based mocking to avoid network dependencies: -
`helper-fixture.R` provides `mock_ensure_zip_fixture()` for mocked
downloads - Offline fixture at `inst/extdata/cnefe_fixture_cnefe.zip` -
Use
[`testthat::with_mocked_bindings()`](https://testthat.r-lib.org/reference/local_mocked_bindings.html)
pattern for network isolation

### Key Dependencies

- **Spatial**: sf, geobr, h3jsr, duckspatial
- **Data**: arrow, dplyr, tidyr, DBI, duckdb
- **Utilities**: cli (messages), checkmate (validation), httr2
  (downloads)

## GitHub Workflow

Always use GitHub CLI (`gh`) to manage issues, branches, and PRs:

``` bash
# List open issues
gh issue list

# View issue details
gh issue view <number>

# Create branch from an issue (format: <number>-<slug>)
gh issue develop <number> --checkout

# Create PR linked to the issue
gh pr create --fill
```

When working on an issue: 1. Review the issue with `gh issue view` 2.
Create branch with `gh issue develop <number> --checkout` 3. Implement
the changes 4. Create PR with `gh pr create` referencing the issue
(e.g., “Closes \#”)

## Code Conventions

- **All comments in code must be written in English** (including TODO
  comments, inline comments, and roxygen2 documentation)
- All documentation uses roxygen2 inline comments (regenerate with
  `devtools::document()`)
- Municipality codes must be 7-digit IBGE codes (validated by
  `.normalize_code_muni()`)
- Internal functions prefixed with `.` (e.g., `.cnefe_cache_dir()`,
  `.normalize_code_muni()`)

## Rendering GitHub Pages Articles

Articles live in `vignettes/articles/` and use a **pre-rendered
workflow** to work around mapview/leaflet HTML widgets that cannot be
rendered by pkgdown alone. The flow is:

1.  **Source files are `.Rmd.orig`** — these are the editable sources.
    Always edit the `.orig` files, never the `.Rmd` directly.
2.  **Render `.orig` → `.Rmd`** using `_build.R`. This executes all R
    code, embeds mapview widget HTML as interactive
    `<!--html_preserve-->` blocks and generates static PNG figures for
    ggplots.
3.  **pkgdown** then builds the final HTML site from the pre-rendered
    `.Rmd` files (which already contain all output).

### How to render — ALWAYS use `_build.R`

**CRITICAL**: Always render articles by running `_build.R` from the
package root:

``` r
source("vignettes/articles/_build.R")
```

**NEVER** call
[`knitr::knit()`](https://rdrr.io/pkg/knitr/man/knit.html) directly on
`.Rmd.orig` files. The `_build.R` script does three essential things
that a bare [`knitr::knit()`](https://rdrr.io/pkg/knitr/man/knit.html)
call does not:

1.  **Sets `knitr::opts_knit$set(rmarkdown.pandoc.to = "html")`** —
    Without this, knitr does not know the output target is HTML and will
    render `mapview()` widgets as static PNG screenshots (via
    webshot/phantomjs) instead of embedding interactive HTML. The
    [`leafsync::sync()`](https://rdrr.io/pkg/leafsync/man/latticeView.html)
    function is not affected by this because it uses a different print
    method, but standalone `mapview()` calls will silently produce PNGs.
    This is the most common rendering pitfall.
2.  **Sets the working directory to `vignettes/articles/`** — So
    `fig.path` resolves correctly and figures land in the right place.
3.  **Injects a hidden dependency chunk into each `.Rmd`** — pkgdown
    needs to discover mapview/leaflet JS/CSS dependencies. Since the
    pre-rendered `.Rmd` contains raw HTML (not R chunks that produce
    widgets), `_build.R` injects a hidden `include=FALSE` chunk after
    the YAML header that loads mapview/leafsync and calls
    [`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html).
    This forces pkgdown to include the required JS/CSS. Without it,
    interactive maps appear blank.

### Key rendering rules for `.Rmd.orig` files

#### `fig.dpi` must NOT be set globally

The `opts_chunk$set()` in each `.Rmd.orig` setup chunk must **not**
include `fig.dpi`. knitr uses `fig.width * dpi` to compute the inline
`style="width:..."` attribute of htmlwidgets. With the default dpi (72),
a widget gets `width:504px`. Setting `fig.dpi = 600` globally inflates
widgets to `width:4200px`, making maps unusable.

**If a ggplot chunk needs high resolution**, set `fig.dpi = 600` on that
specific chunk only:

``` r
```{r, fig.width = 8, fig.height = 4, fig.dpi = 600}
ggplot(...) + ...
```

\`\`\`

Currently, only two chunks use this: one in `compute_lumi.Rmd.orig` and
one in `tracts_to.Rmd.orig`.

#### `out.width = "100%"` is set globally

All articles set `out.width = "100%"` in `opts_chunk$set()` so that both
mapview widgets and ggplot images adapt to the page width instead of
having a fixed pixel size.

#### Do NOT add `cat()`-based dependency injection to `.Rmd.orig`

The dependency chunk injection is handled exclusively by `_build.R`
post-processing. Do **not** add ```` cat('```{r include=FALSE}\n') ````
tricks to `.Rmd.orig` files — this creates duplicate dependency chunks
and is unnecessary.

### Diagnosing rendering problems

If mapview widgets render as static images (PNGs) instead of interactive
maps: - **Cause**: Articles were rendered without
`rmarkdown.pandoc.to = "html"`. - **Fix**: Re-render using
`source("vignettes/articles/_build.R")`. - **How to verify**: Check the
`.Rmd` for `<!--html_preserve-->` markers. Interactive widgets produce
these; PNG fallbacks produce `![plot of chunk ...]()` image references
instead.

If mapview widgets are extremely large (need to zoom out to see them): -
**Cause**: `fig.dpi` is set too high in the global `opts_chunk$set()`. -
**Fix**: Remove `fig.dpi` from the global setup; only set it per-chunk
on ggplot chunks. - **How to verify**: Check the `.Rmd` for
`style="width:...px"` on leaflet divs. Should be `width:100%`, not
`width:4200px`.

If interactive maps appear as blank white boxes on the deployed site: -
**Cause**: The dependency injection chunk is missing from the `.Rmd`. -
**Fix**: Re-render using `_build.R`, which injects it automatically. -
**How to verify**: Check the `.Rmd` for a ```` ```{r include=FALSE} ````
chunk containing
[`mapview::mapview()`](https://r-spatial.github.io/mapview/reference/mapView.html)
near the top of the file.

### What gets committed

Both `.Rmd.orig` (source) and `.Rmd` (pre-rendered output) are committed
to the repo, along with the `*_files/figure-html/` directories
containing generated PNGs for ggplot figures.

## AI Assistant Guidelines

- **Always read all relevant `.R` files** in `R/` and `tests/testthat/`
  directories before making changes
- Understand the existing code structure and patterns before proposing
  modifications
- Ensure consistency with the package’s coding style and conventions
- **Do NOT add `Co-Authored-By` lines for Claude in commits or PRs** (no
  AI co-authorship attribution)

### Model Usage Optimization

To optimize token usage, prefer different models for different task
types:

- **Opus 4.5** (higher token cost): Use for planning, architectural
  decisions, complex investigation, and design work
- **Sonnet 4** (lower token cost): Use for execution of well-defined
  tasks, repetitive edits, and straightforward implementations

When starting a complex task, first use Opus for planning and analysis,
then switch to Sonnet for the execution phase.
