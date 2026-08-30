# The pure-R backends push transmute() and filter() down to the Arrow table and
# collect last (#80 R1.10). Arrow falls back to R when it cannot translate an
# expression, silently changing the memory profile without changing the result,
# so these tests guard the translation itself rather than the output.

toy_table <- function(n = 1000L) {
  arrow::as_arrow_table(data.frame(
    LONGITUDE = as.character(c(rep(c(-38.5, -38.4), length.out = n - 2L), NA, -38.3)),
    LATITUDE = as.character(c(rep(c(-12.9, -12.8), length.out = n - 2L), -12.7, NA)),
    COD_ESPECIE = as.character(rep(c(1L, 7L, 9L, 2L), length.out = n)),
    PADDING = strrep("x", 20L),
    stringsAsFactors = FALSE
  ))
}

pipeline <- function(x, exclude7 = FALSE) {
  out <- x |>
    dplyr::transmute(
      LONGITUDE = as.numeric(.data$LONGITUDE),
      LATITUDE = as.numeric(.data$LATITUDE),
      COD_ESPECIE = as.integer(.data$COD_ESPECIE)
    ) |>
    dplyr::filter(
      !is.na(.data$LONGITUDE),
      !is.na(.data$LATITUDE),
      !is.na(.data$COD_ESPECIE),
      .data$COD_ESPECIE %in% 1L:8L
    )
  if (exclude7) out <- dplyr::filter(out, .data$COD_ESPECIE != 7L)
  out
}


testthat::test_that("Arrow translates the backend pipeline without falling back (#80 R1.10)", {
  testthat::skip_if_not_installed("arrow")
  tab <- toy_table()

  # A translation failure surfaces as a warning about pulling data into R.
  testthat::expect_no_warning(res <- dplyr::collect(pipeline(tab)))
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_true(all(res$COD_ESPECIE %in% 1L:8L))
  testthat::expect_false(anyNA(res$LONGITUDE))
})


testthat::test_that("pushing down gives the same rows as collecting first (#80 R1.10)", {
  testthat::skip_if_not_installed("arrow")
  tab <- toy_table()

  pushed <- dplyr::collect(pipeline(tab))
  eager <- pipeline(as.data.frame(tab))

  testthat::expect_identical(dim(pushed), dim(eager))
  testthat::expect_equal(sum(pushed$LONGITUDE), sum(eager$LONGITUDE))
  testthat::expect_identical(sort(unique(pushed$COD_ESPECIE)), sort(unique(eager$COD_ESPECIE)))
})


testthat::test_that("the compute_lumi variant also translates, including the type 7 filter (#80 R1.10)", {
  testthat::skip_if_not_installed("arrow")
  tab <- toy_table()

  testthat::expect_no_warning(res <- dplyr::collect(pipeline(tab, exclude7 = TRUE)))
  testthat::expect_false(7L %in% res$COD_ESPECIE)
  testthat::expect_true(all(res$COD_ESPECIE %in% c(1L, 2L)))
})


testthat::test_that("pushing down keeps only the selected columns (#80 R1.10)", {
  testthat::skip_if_not_installed("arrow")
  # The point of the change: PADDING never reaches R.
  res <- dplyr::collect(pipeline(toy_table()))
  testthat::expect_identical(names(res), c("LONGITUDE", "LATITUDE", "COD_ESPECIE"))
  testthat::expect_false("PADDING" %in% names(res))
})
