# Benchmark re-run for the second-round revision (#120)

The benchmark behind the package article `bench_duckdb` and the manuscript (Figures 5 and 6, and the R2.8 results) was run again on 2026-11-25. The run used the development version that will be released as 0.4.0, which is what the revised manuscript describes. The new measurements replace `bench_r2_8.csv`. The previous ones, taken on 2026-09-18 with the code released as 0.3.0, are kept in `bench_r2_8_v030.csv` for this comparison.

## How the two runs differ

Both runs used the same harness (`bench_r2_8.R`), the same machine, the same configurations and the same number of replicates. Each replicate ran in a fresh R subprocess, after a discarded warm-up, and the configurations were visited round-robin.

| | 0.3.0 | Current |
|---|---|---|
| Date | 2026-09-18 | 2026-11-25 |
| cnefetools | 0.3.0 (then 0.3.0.9000, before release) | 0.3.0.9000 (0.4.0 in development) |
| duckdb (R package) | 1.5.2 | 1.5.6 |
| R | 4.6.0 | 4.6.0 |
| Municipal boundaries for the H3 grid | simplified geobr, whole-Brazil file | 2022 IBGE mesh at full resolution, cached per municipality |
| Census tracts | whole-state Parquet file | per-municipality Parquet file |

The current run used a separate cache (via `CNEFETOOLS_CACHE_DIR`) holding only what the new version downloads by itself, so the tracts were read from the per-municipality files a new user gets, not from a whole-state file left by an older version. Every configuration had a spread (max minus min over the median) below 25%, and all but two stayed below 7%.

Since the duckdb version also changed, the differences below can't be attributed to the package alone.

## Median elapsed time and peak memory

Peak memory is the operating system's peak working set of the R process.

| Block | Function | Municipality | Backend | Res. | Time 0.3.0 (s) | Time now (s) | Change | Memory 0.3.0 (MB) | Memory now (MB) | Change |
|---|---|---|---|---|---|---|---|---|---|---|
| cities | `cnefe_counts` | Curitiba-PR | duckdb | 8 | 3.53 | 1.53 | -57% | 444 | 399 | -10% |
| cities | `cnefe_counts` | Curitiba-PR | r | 8 | 9.19 | 7.50 | -18% | 1759 | 1710 | -3% |
| cities | `cnefe_counts` | Sao Paulo-SP | duckdb | 8 | 5.58 | 4.36 | -22% | 720 | 698 | -3% |
| cities | `cnefe_counts` | Sao Paulo-SP | r | 8 | 93.25 | 98.18 | +5% | 9246 | 8840 | -4% |
| cities | `cnefe_counts` | Vitoria da Conquista-BA | duckdb | 8 | 3.79 | 1.93 | -49% | 468 | 433 | -7% |
| cities | `cnefe_counts` | Vitoria da Conquista-BA | r | 8 | 4.69 | 3.14 | -33% | 716 | 695 | -3% |
| h3res | `cnefe_counts` | Curitiba-PR | duckdb | 7 | 4.26 | 1.36 | -68% | 441 | 355 | -20% |
| h3res | `cnefe_counts` | Curitiba-PR | r | 7 | 9.25 | 7.27 | -21% | 1764 | 1704 | -3% |
| h3res | `cnefe_counts` | Curitiba-PR | duckdb | 9 | 4.14 | 2.25 | -46% | 476 | 446 | -6% |
| h3res | `cnefe_counts` | Curitiba-PR | r | 9 | 9.88 | 8.11 | -18% | 1795 | 1744 | -3% |
| h3res | `cnefe_counts` | Curitiba-PR | duckdb | 11 | 35.65 | 38.19 | +7% | 1721 | 1732 | +1% |
| h3res | `cnefe_counts` | Curitiba-PR | r | 11 | 41.77 | 45.12 | +8% | 2758 | 2388 | -13% |
| lumi | `compute_lumi` | Curitiba-PR | duckdb | 8 | 3.48 | 1.48 | -57% | 360 | 399 | +11% |
| lumi | `compute_lumi` | Curitiba-PR | r | 8 | 9.26 | 7.41 | -20% | 1640 | 1689 | +3% |
| lumi | `compute_lumi` | Sao Paulo-SP | duckdb | 8 | 5.58 | 4.31 | -23% | 572 | 693 | +21% |
| lumi | `compute_lumi` | Sao Paulo-SP | r | 8 | 90.61 | 96.23 | +6% | 8605 | 8733 | +1% |
| lumi | `compute_lumi` | Vitoria da Conquista-BA | duckdb | 8 | 4.50 | 1.92 | -57% | 391 | 433 | +11% |
| lumi | `compute_lumi` | Vitoria da Conquista-BA | r | 8 | 4.45 | 2.99 | -33% | 600 | 693 | +16% |
| tracts | `tracts_to_h3` | Curitiba-PR | - | 9 | 6.91 | 5.67 | -18% | 622 | 673 | +8% |
| tracts | `tracts_to_h3` | Sao Paulo-SP | - | 9 | 15.65 | 18.17 | +16% | 1981 | 1995 | +1% |
| tracts | `tracts_to_h3` | Vitoria da Conquista-BA | - | 9 | 9.77 | 9.49 | -3% | 681 | 560 | -18% |
| tracts | `tracts_to_polygon` | Curitiba-PR | - | 9 | 5.25 | 4.61 | -12% | 1118 | 1062 | -5% |
| tracts | `tracts_to_polygon` | Sao Paulo-SP | - | 9 | 15.53 | 13.13 | -15% | 4384 | 4237 | -3% |
| tracts | `tracts_to_polygon` | Vitoria da Conquista-BA | - | 9 | 7.27 | 4.86 | -33% | 821 | 758 | -8% |
| modest | `cnefe_counts` | Sao Paulo-SP | duckdb | 8 (constrained) | 7.01 | 5.72 | -18% | 459 | 427 | -7% |
| modest | `tracts_to_h3` | Sao Paulo-SP | - | 9 (constrained) | 20.93 | 24.48 | +17% | 1791 | 1682 | -6% |

## What changed

- **Time.** Almost every configuration got faster. The largest gains are on the DuckDB backend for the small and mid-sized municipalities (about 50% to 70% less time), where fixed costs such as building the grid weigh most, and that's consistent with reading the cached per-municipality boundaries instead of filtering the whole-Brazil geobr file. `tracts_to_polygon()` is 12% to 33% faster.
- **Slower.** The configurations dominated by work per address changed little or got slightly slower: pure R on São Paulo (+5% to +6%) and resolution 11 on Curitiba (+7% to +8%). `tracts_to_h3()` on São Paulo is 16% to 17% slower, with and without the constrained profile. The current version does more work there (it keeps only the tracts that touch the grid, including those of neighboring municipalities, and computes extra diagnostics), but the share of each step wasn't measured.
- **Memory.** About the same. Changes range from -20% to +21% with no consistent direction, and the order-of-magnitude gap between the backends on São Paulo (8.8 GB for pure R against 698 MB for DuckDB) is unchanged.

## Effect on the text

The qualitative conclusions hold. DuckDB's advantage grows with the number of addresses, shrinks as the H3 resolution rises, and is much larger in memory than in time. Three statements of the article and the manuscript no longer match the data and were rewritten:

- pure R is no longer ahead in any configuration (it was marginally ahead in one before);
- at resolution 11 the backends are 18% apart, not "within a couple of percent";
- the memory gap does narrow at resolution 11, from about 4.8 times at resolution 7 to 1.4 times (this was already true in the 0.3.0 run).

DuckDB speedups in the current run: 1.63, 4.90 and 22.5 for Vitória da Conquista, Curitiba and São Paulo at resolution 8, and 5.35, 3.60 and 1.18 for Curitiba at resolutions 7, 9 and 11.

## Reproducing

```sh
# from the package root, with an idle machine on a high-performance power plan
CNEFETOOLS_CACHE_DIR=/path/to/clean/cache Rscript data-raw/bench_r2_8_prewarm.R
CNEFETOOLS_CACHE_DIR=/path/to/clean/cache Rscript data-raw/bench_r2_8.R
```

`BENCH_PKG_DIR` points the harness at another checkout of the package (for example a worktree of a release tag), and `BENCH_OUT_CSV` writes the results somewhere other than `bench_r2_8.csv`.
