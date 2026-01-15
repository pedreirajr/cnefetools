# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**cnefetools** is an R package for working with 2022 Brazilian CNEFE (National Address File for Statistical Purposes) data from IBGE. It provides tools for downloading, reading, and analyzing address-level data with spatial aggregation capabilities.

## Common Commands

```r
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

- **read_cnefe.R** - Downloads and reads CNEFE CSV files for a municipality; returns Arrow tables or sf spatial objects
- **cnefe_counts.R** - Aggregates address counts to H3 hexagonal grid cells or user-provided polygons (DuckDB or pure-R backend)
- **compute_lumi.R** - Computes land-use mix indices (Entropy Index, HHI, BGBI, Balance Index)
- **tracts_to_h3.R** - Dasymetric interpolation from census tracts to H3 via CNEFE dwelling points
- **build_h3_grid.R** - Internal utility to create H3 grids from IDs or municipality boundary
- **utils-internal.R** - Input validation, cache management, download helpers, DuckDB extension utilities

### Key Design Patterns

- **Dual backends**: Functions support both DuckDB (SQL-based, fast) and pure-R backends via `use_duckdb` parameter
- **Caching**: ZIP files are cached in user directory (`tools::R_user_dir("cnefetools", "cache")`) for automatic reuse
- **DuckDB extensions**: Uses h3 and zipfs extensions loaded via helper functions in utils-internal.R

### Testing

Tests use fixture-based mocking to avoid network dependencies:
- `helper-fixture.R` provides `mock_ensure_zip_fixture()` for mocked downloads
- Offline fixture at `inst/extdata/cnefe_fixture_cnefe.zip`
- Use `testthat::with_mocked_bindings()` pattern for network isolation

### Key Dependencies

- **Spatial**: sf, geobr, h3jsr, duckspatial
- **Data**: arrow, dplyr, tidyr, DBI, duckdb
- **Utilities**: cli (messages), checkmate (validation), httr2 (downloads)

## GitHub Workflow

Always use GitHub CLI (`gh`) to manage issues, branches, and PRs:

```bash
# List open issues
gh issue list

# View issue details
gh issue view <number>

# Create branch from an issue (format: <number>-<slug>)
gh issue develop <number> --checkout

# Create PR linked to the issue
gh pr create --fill
```

When working on an issue:
1. Review the issue with `gh issue view`
2. Create branch with `gh issue develop <number> --checkout`
3. Implement the changes
4. Create PR with `gh pr create` referencing the issue (e.g., "Closes #<number>")

## Code Conventions

- **All comments in code must be written in English** (including TODO comments, inline comments, and roxygen2 documentation)
- All documentation uses roxygen2 inline comments (regenerate with `devtools::document()`)
- Municipality codes must be 7-digit IBGE codes (validated by `.normalize_code_muni()`)
- Internal functions prefixed with `.` (e.g., `.cnefe_cache_dir()`, `.normalize_code_muni()`)

## AI Assistant Guidelines

- **Always read all relevant `.R` files** in `R/` and `tests/testthat/` directories before making changes
- Understand the existing code structure and patterns before proposing modifications
- Ensure consistency with the package's coding style and conventions
