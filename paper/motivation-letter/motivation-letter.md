---
output: pdf_document
fontsize: 12pt
---

\thispagestyle{empty}
\today

The Editors
The R Journal
\bigskip

Dear Editors,
\bigskip

Please consider our manuscript titled **"cnefetools: Access and Analysis of Brazilian CNEFE Address Data in R"** for publication in The R Journal. The package is available on CRAN (v0.2.2) at https://CRAN.R-project.org/package=cnefetools.

**The problem.** The 2022 Brazilian National Address File for Statistical Purposes (CNEFE) is a geocoded register of approximately 100 million addresses published by the Instituto Brasileiro de Geografia e Estatística (IBGE) alongside the 2022 Brazilian Census. It is, to our knowledge, the most comprehensive publicly available geocoded address dataset for any country in the Global South. Despite its potential for urban research and public policy analysis, the dataset is effectively inaccessible to most researchers: data are distributed as individual ZIP-compressed, semicolon-delimited CSV files via an FTP server (one file per municipality, ~5,570 in total), with no unified API, inconsistent column typing, and no existing R interface.

**Our contribution.** `cnefetools` provides a unified R interface for the CNEFE with four capabilities: (1) downloading and caching CNEFE data for any municipality, with automatic management of the IBGE FTP structure and persistent user-level caching via `tools::R_user_dir()`; (2) aggregating address counts to H3 hexagonal grids or user-supplied polygons; (3) computing six land-use mix indices from the geocoded address data, including the novel Bidirectional Global-centered Balance Index (BGBI); and (4) performing dasymetric interpolation of census tract variables to finer spatial units using CNEFE dwelling points as ancillary data. A dual DuckDB/R backend reads CSV data directly from cached ZIP archives through the `zipfs` extension, performing H3 cell assignment and spatial joins entirely in-process, with speedups of up to 13× over a pure-R fallback (benchmark on São Paulo, ~5.7 million addresses, H3 resolution 8).

**Fit with The R Journal.** The manuscript is a software contribution focused on the package design and its four analytical capabilities, not on methodological derivation. We highlight two aspects of broader interest to the R community: (i) the dual-backend architecture—defaulting to DuckDB for performance, with a pure-R fallback for portability—represents a transferable pattern for R packages that process large geospatial files; and (ii) the dasymetric interpolation workflow (`tracts_to_h3()` / `tracts_to_polygon()`) may serve as a reference for similar packages targeting other national census systems. The package includes full unit-test coverage, six pre-rendered pkgdown articles, and an active GitHub issue tracker.

**Relationship to a companion publication.** A companion manuscript (Pedreira Jr. et al., 2025, *engrXiv*, doi:10.31224/5975) presents the statistical methodology underlying the BGBI and evaluates it against existing land-use mix measures using CNEFE data. That paper focuses on the statistical science; this submission focuses on the software. The two papers are complementary and cite each other, with no content duplication and no overlap in journal scope.

**Reproducibility.** The worked example in Section 6 uses Vitória da Conquista (IBGE code 2933307, ~200,000 addresses) and completes in under ten minutes from scratch on a modern laptop. All code requires only CRAN packages.

We hope the manuscript will be of interest to the readership of The R Journal. We look forward to the editors' and reviewers' feedback.
\bigskip
\bigskip

Sincerely,
\bigskip
\bigskip
\bigskip

Jorge Ubirajara Pedreira Junior
Escola Politécnica, Universidade Federal da Bahia
Salvador, Bahia, Brazil
jorge.ubirajara@ufba.br
https://orcid.org/0000-0002-8243-5395
\bigskip

Bruno Mioto
Departamento de Estatística, Universidade Estadual de Maringá
Maringá, Paraná, Brazil
brunomioto97@gmail.com
