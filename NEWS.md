# cnefetools 0.0.5

-   Makes unit tests more reproducible and CI-friendly by removing dependencies on network access and local cache state (offline ZIP fixture and mocked downloads)
-   Improves robustness of `read_cnefe(output = "sf")` by handling missing coordinates before converting to `sf`
-   Updates `COD_ESPECIE` documentation and removes tidyselect deprecation warnings in tests and internals

# cnefetools 0.0.4

-   Major speed-up for H3 assignment and hex-level aggregation via DuckDB + H3 extension (SQL), with runtimes dropping from minutes to seconds for large municipalities
-   Adds a configurable backend with backwards compatibility: `backend = "duckdb"` (default) or `backend = "r"` for `hex_cnefe_counts()` and `compute_lumi()`
-   Refactors internals to reuse cached ZIPs and consolidate common helpers, reducing repeated overhead and improving maintainability

# cnefetools 0.0.3

-   More robust downloads with retry logic and increased timeout when needed
-   Avoids leaving partial or corrupted ZIP files in the cache
-   Automatically detects corrupted cached ZIPs and re-downloads before extraction

# cnefetools 0.0.2

-   Add `compute_lumi()` to compute land-use mix indicators (EI, HHI, adapted HHI, BGBI) on H3 grids.
-   Extend README with examples for Fortaleza (code 2304400) and BGBI maps.

# cnefetools 0.0.1

# cnefetools 0.0.0.9000

-   Initial development version. Basic package infrastructure set up.
