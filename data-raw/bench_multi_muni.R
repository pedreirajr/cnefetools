# Benchmark for #120 (R2 comment 1): does accepting a vector of municipality
# codes cost anything?
#
# Two questions, one CSV (data-raw/bench_multi_muni.csv):
#   single   a call with ONE municipality, the code before and after the change
#            (`pkg = "old"` is a worktree at the commit the referee reviewed)
#   pair     two adjacent municipalities: one call with the vector against the
#            loop-and-bind the vector replaces
#
# Same method as data-raw/bench_r2_8.R, on a smaller scale:
#   * a fresh subprocess per measurement, so peak memory is clean
#   * peak memory as the OS peak working set (ps), which sees DuckDB's C++
#     allocations that bench::mark() does not
#   * round-robin over configurations, with one discarded warm-up replicate
#   * the cache is warm, so downloads are not measured
#
# HOW TO RUN (idle machine, plugged in)
#   git worktree add <dir> bbc10d6          # the "old" code
#   Rscript data-raw/bench_multi_muni.R <dir>
#   Rscript data-raw/bench_multi_muni.R <dir> --pilot   # one pass, no warm-up
#   --fn=tracts_to_h3 --out=<csv>   # only one function, to another file
#
# On R < 4.4 the census tract path needs `%||%`, which base R only gained in
# 4.4.0, so the harness defines it in the subprocess.

stopifnot(requireNamespace("callr", quietly = TRUE))
stopifnot(requireNamespace("ps", quietly = TRUE))

args <- commandArgs(trailingOnly = TRUE)
PILOT <- "--pilot" %in% args
OLD_DIR <- normalizePath(args[!grepl("^--", args)][1], winslash = "/")
NEW_DIR <- normalizePath(".", winslash = "/")
OUT_CSV <- sub("^--out=", "", grep("^--out=", args, value = TRUE)[1])
if (is.na(OUT_CSV)) OUT_CSV <- "data-raw/bench_multi_muni.csv"
ONLY_FN <- sub("^--fn=", "", grep("^--fn=", args, value = TRUE)[1])
REPS <- if (PILOT) 1L else 3L

SALVADOR <- 2927408L
LAURO <- 2919207L
FORTALEZA <- 2304400L
RECIFE <- 2611606L

cfg <- function(group, fn, backend, approach, munis, pkg = "new") {
  list(
    group = group, fn = fn, backend = backend, approach = approach,
    munis = munis, pkg = pkg
  )
}

configs <- c(
  # single municipality, before and after
  lapply(c("cnefe_counts", "compute_lumi"), function(f) {
    lapply(c("old", "new"), function(p) {
      cfg("single", f, "duckdb", "call", SALVADOR, pkg = p)
    })
  }) |> unlist(recursive = FALSE),

  # two adjacent municipalities, DuckDB backend only: the pure-R backend is
  # slower by an order of magnitude and says nothing about the vector interface
  lapply(c("cnefe_counts", "compute_lumi"), function(f) {
    list(
      cfg("pair", f, "duckdb", "vector", c(LAURO, SALVADOR)),
      cfg("pair", f, "duckdb", "loop", c(LAURO, SALVADOR))
    )
  }) |> unlist(recursive = FALSE),
  list(
    cfg("pair", "tracts_to_h3", "duckdb", "vector", c(LAURO, SALVADOR)),
    cfg("pair", "tracts_to_h3", "duckdb", "loop", c(LAURO, SALVADOR))
  )
)

# Runs inside the subprocess and returns timing plus peak memory.
measure <- function(pkg_dir, fn, backend, approach, munis) {
  assign("%||%", function(x, y) if (is.null(x)) y else x, envir = globalenv())
  suppressMessages(devtools::load_all(pkg_dir, quiet = TRUE))

  f <- get(fn, envir = asNamespace("cnefetools"))
  call_one <- function(m) {
    if (fn == "tracts_to_h3") {
      f(m, vars = c("pop_ph", "pop_ch"), verbose = FALSE)
    } else {
      f(m, backend = backend, verbose = FALSE)
    }
  }

  t0 <- proc.time()[["elapsed"]]
  out <- if (approach == "loop") {
    parts <- lapply(munis, call_one)
    do.call(rbind, parts)
  } else if (approach == "vector") {
    call_one(munis)
  } else {
    call_one(munis[[1L]])
  }
  elapsed <- proc.time()[["elapsed"]] - t0

  mem <- ps::ps_memory_info()
  list(
    elapsed_s = elapsed,
    peak_mb = unname(mem[["peak_wset"]]) / 1024^2,
    n_rows = nrow(out),
    n_dup_ids = if ("id_hex" %in% names(out)) sum(duplicated(out$id_hex)) else NA_integer_
  )
}

run_once <- function(cf) {
  pkg_dir <- if (cf$pkg == "old") OLD_DIR else NEW_DIR
  res <- callr::r(
    measure,
    args = list(pkg_dir, cf$fn, cf$backend, cf$approach, cf$munis),
    show = FALSE
  )
  data.frame(
    group = cf$group, fn = cf$fn, backend = cf$backend,
    approach = cf$approach, pkg = cf$pkg,
    n_munis = length(cf$munis),
    elapsed_s = round(res$elapsed_s, 2),
    peak_mb = round(res$peak_mb, 1),
    n_rows = res$n_rows,
    n_dup_ids = res$n_dup_ids,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

if (!is.na(ONLY_FN)) configs <- Filter(function(cf) cf$fn == ONLY_FN, configs)

rows <- list()

if (!PILOT) {
  message("Warm-up pass (discarded)")
  for (cf in configs) invisible(try(run_once(cf), silent = TRUE))
}

for (rep in seq_len(REPS)) {
  for (i in sample(seq_along(configs))) {
    cf <- configs[[i]]
    message(sprintf(
      "rep %d  %-14s %-6s %-7s %-6s %s", rep, cf$fn, cf$backend, cf$approach,
      cf$pkg, paste(cf$munis, collapse = ",")
    ))
    r <- tryCatch(run_once(cf), error = function(e) {
      message("  failed: ", conditionMessage(e))
      NULL
    })
    if (!is.null(r)) {
      r$rep <- rep
      rows[[length(rows) + 1L]] <- r
      utils::write.csv(do.call(rbind, rows), OUT_CSV, row.names = FALSE)
    }
  }
}

message("Wrote ", OUT_CSV)
