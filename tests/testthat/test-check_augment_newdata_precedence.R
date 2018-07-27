context("test-check_augment_newdata_precedence")

library(tibble)

augment_always_data <- function(model, data = NULL, newdata = NULL) {
  as_tibble(data)
}

augment_correct <- function(model, data = NULL, newdata = NULL) {
  ret <- if (!is.null(newdata)) newdata else data
  as_tibble(ret)
}

test_that("strict = TRUE", {

  expect_error(
    check_augment_newdata_precedence(
      aug = augment_always_data,
      model = NULL,
      data = iris
    ),
    "Must specify either `data` or `newdata` argument."
  )

  expect_silent(
    check_augment_newdata_precedence(
      aug = augment_correct,
      model = NULL,
      data = iris
    )
  )
})
