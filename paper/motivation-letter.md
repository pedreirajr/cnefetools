Dear Editors,

We submit for your consideration the manuscript "cnefetools: Access and
Analysis of Brazilian CNEFE Address Data in R" by Jorge Ubirajara
Pedreira Junior and Bruno Henrique Mioto Stabile.

The paper introduces the **cnefetools** R package, which provides
programmatic access to the 2022 Brazilian National Address File for
Statistical Purposes (CNEFE), a geocoded register of approximately 110
million addresses produced by the Instituto Brasileiro de Geografia e
Estatística (IBGE) alongside the 2022 national census. The package is
available on CRAN at <https://CRAN.R-project.org/package=cnefetools>.

We believe this paper is suitable for the R Journal for the following
reasons:

1. **Novel data access for the R community.** The CNEFE is a uniquely
   rich open dataset covering an entire country at address resolution,
   yet it has been inaccessible to most R users due to its fragmented
   distribution across thousands of municipality-level files with no
   unified programmatic interface. cnefetools removes this barrier
   through a pre-built index and a persistent caching layer.

2. **Methodological contribution.** Beyond data access, the package
   implements dasymetric interpolation of census tract variables using
   CNEFE dwelling points as ancillary data. This approach is
   methodologically superior to areal-weighted interpolation provided by
   existing R packages such as sf and areal, and more precise than the
   building-footprint dasymetry of populR in verticalised urban
   environments, where address counts better reflect the number of
   dwellings than ground-floor area does. The package also provides the
   first R implementation of a suite of land use mix indices, including
   the novel Bidirectional Global-centered Balance Index (BGBI).

3. **Performance architecture of general interest.** The dual-backend
   design, in which a DuckDB engine reads ZIP-compressed CSV files
   in-process and performs H3 cell assignment and spatial joins entirely
   in SQL, demonstrates a pattern applicable to other large open
   government datasets. Benchmarks show speedups of up to 13× over the
   pure-R fallback for large municipalities.

4. **Reproducibility.** All examples in the paper use publicly available
   data downloaded at runtime from the official IBGE FTP server and from
   the geobr package. The package includes an offline fixture for
   automated testing without network access. The paper was rendered using
   rjtools and all code chunks are fully reproducible.

**Note on the package name.** The automated title-case checker suggests
capitalising the package name as "Cnefetools". However, the official
name of the package, as registered on CRAN, is **cnefetools** (all
lowercase), following the convention of several widely used R packages
(e.g., ggplot2, dplyr, tidyr). We therefore retain the original
capitalisation in the title.

Thank you for considering this submission. We look forward to your
response.

Sincerely,

Jorge Ubirajara Pedreira Junior
Escola Politécnica, Universidade Federal da Bahia
Salvador, Bahia, Brazil
jorge.ubirajara@ufba.br

Bruno Henrique Mioto Stabile
Programa de Pós-Graduação em Ecologia de Ambientes Aquáticos Continentais
Universidade Estadual de Maringá
Maringá, Paraná, Brazil
bhmstabile@gmail.com
