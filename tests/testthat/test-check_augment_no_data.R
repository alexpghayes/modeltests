context("test-check_augment_no_data")

fail_gracefully <- function(model, data = NULL, newdata = NULL) {
  stop("Must specify either `data` or `newdata` argument.", call. = FALSE)
}

fail_uninformatively <- function(model, data = NULL, newdata = NULL) {
  stop("A bad error message.", call. = FALSE)
}

missing_rows <- function(model, data = NULL, newdata = NULL) {
  as_tibble(head(iris))
}

missing_cols <- function(model, data = NULL, newdata = NULL) {
  as_tibble(iris)[, 1:4]
}

missing_rownames <- function(model, data = NULL, newdata = NULL) {
  as_tibble(iris, rownames = NULL)
}

correct <- function(model, data = NULL, newdata = NULL) {
  as_tibble(iris)
}

test_that("strict = TRUE", {

  expect_silent(
    check_augment_no_data(
      aug = correct,
      model = NULL,
      passed_data = iris,
      strict = TRUE
    )
  )

  expect_silent(
    check_augment_no_data(
      aug = fail_gracefully,
      model = NULL,
      passed_data = iris,
      strict = TRUE
    )
  )

  expect_error(
    check_augment_no_data(
      aug = fail_uninformatively,
      model = NULL,
      passed_data = iris,
      strict = TRUE
    ),
    paste0(
      "Augment failed but did not give an informative error message.\n",
      "Please use the following error message:\n",
      "  Must specify either `data` or `newdata` argument."
    )
  )
})
