testthat::test_that(".is_github_auth_error() recognises auth failures only", {
  expect_auth <- function(msg) {
    testthat::expect_true(
      cnefetools:::.is_github_auth_error(simpleError(msg)),
      info = msg
    )
  }
  expect_not_auth <- function(msg) {
    testthat::expect_false(
      cnefetools:::.is_github_auth_error(simpleError(msg)),
      info = msg
    )
  }

  expect_auth("GitHub API error (401): Bad credentials")
  expect_auth("bad credentials")
  expect_auth("Resource protected by organization SAML enforcement, requires authentication")

  expect_not_auth("GitHub API error (404): Not Found")
  expect_not_auth("Could not resolve host: api.github.com")
  expect_not_auth("Timeout was reached")

  testthat::expect_false(cnefetools:::.is_github_auth_error(NULL))
  testthat::expect_false(cnefetools:::.is_github_auth_error("401"))
})


testthat::test_that(".is_github_auth_error() inspects the parent condition", {
  parent <- simpleError("GitHub API error (401): Bad credentials")
  cnd <- rlang::error_cnd(
    message = "Cannot access release data for repo.",
    parent = parent
  )
  testthat::expect_true(cnefetools:::.is_github_auth_error(cnd))
})


testthat::test_that(".sc_download_with_piggyback() retries anonymously after a 401 (#79)", {
  calls <- list()
  dest_dir <- file.path(tempdir(), paste0("sc_download_", Sys.getpid()))

  fake_pb_download <- function(file, dest, repo, tag, overwrite, show_progress, ...) {
    dots <- list(...)
    calls[[length(calls) + 1L]] <<- list(token = dots$.token)

    # First call: the environment holds a broken credential.
    if (length(calls) == 1L) {
      rlang::abort("GitHub API error (401): Bad credentials")
    }

    # Second call: anonymous, succeeds. Materialise the file piggyback would
    # have written so the caller's existence check passes.
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    writeLines("stub", file.path(dest, file))
    invisible(TRUE)
  }

  testthat::local_mocked_bindings(
    pb_download = fake_pb_download,
    .package = "piggyback"
  )
  # cache = FALSE returns the temp file straight after validation, so the
  # parquet validator is the only internal that needs stubbing.
  testthat::local_mocked_bindings(
    .validate_sc_parquet = function(...) TRUE,
    .package = "cnefetools"
  )
  on.exit(unlink(dest_dir, recursive = TRUE), add = TRUE)

  res <- cnefetools:::.sc_download_with_piggyback(
    uf = "29",
    cache = FALSE,
    verbose = FALSE
  )

  testthat::expect_length(calls, 2L)
  # First attempt keeps the default token, so a valid one still gets the
  # authenticated rate limit.
  testthat::expect_null(calls[[1L]]$token)
  # Second attempt forces an anonymous request.
  testthat::expect_identical(calls[[2L]]$token, "")
  testthat::expect_true(file.exists(res))
})


testthat::test_that(".sc_download_with_piggyback() does not retry on non-auth errors (#79)", {
  n_calls <- 0L

  testthat::local_mocked_bindings(
    pb_download = function(...) {
      n_calls <<- n_calls + 1L
      rlang::abort("GitHub API error (404): Not Found")
    },
    .package = "piggyback"
  )

  testthat::expect_error(
    cnefetools:::.sc_download_with_piggyback(uf = "29", cache = FALSE, verbose = FALSE),
    "Failed to download"
  )
  testthat::expect_identical(n_calls, 1L)
})


testthat::test_that(".sc_download_with_piggyback() hints at a broken credential when both attempts fail (#79)", {
  testthat::local_mocked_bindings(
    pb_download = function(...) rlang::abort("GitHub API error (401): Bad credentials"),
    .package = "piggyback"
  )

  cnd <- tryCatch(
    cnefetools:::.sc_download_with_piggyback(uf = "29", cache = FALSE, verbose = FALSE),
    error = function(e) e
  )

  msg <- paste(conditionMessage(cnd), collapse = " ")
  testthat::expect_match(msg, "anonymous retry")
  testthat::expect_match(msg, "gitcreds_delete")
})
