context("test-check_augment_function")

base_aug <- function(data = NULL, newdata = NULL) {
  df <- if (!is.null(newdata)) newdata else data

  if (has_rownames(df))
    df$.rownames <- rownames(df)

  df$.fitted <- 1:nrow(df)
  df$.resid <- 1:nrow(df) + 1
  df
}

data_only_aug <- function(model, data) {
  as_tibble(base_aug(data))
}

consistent_aug <- function(model, data = NULL, newdata = NULL) {
  as_tibble(base_aug(data, newdata))
}

test_that("strict = FALSE", {

  expect_error(
    check_augment_function(
      aug = data_only_aug,
      model = NULL,
      data = NULL,
      newdata = NULL,
      strict = FALSE
    ),
    "Must pass `data` argument as augment method accepts data argument."
  )

  expect_error(
    check_augment_function(
      aug = consistent_aug,
      model = NULL,
      data = iris,
      newdata = NULL,
      strict = FALSE
    ),
    "Must pass `newdata` argument as augment method accepts newdata argument."
  )

  expect_silent(
    check_augment_function(
      aug = consistent_aug,
      model = NULL,
      data = iris,
      newdata = iris,
      strict = FALSE
    )
  )
})

test_that("strict = TRUE", {
  expect_silent(
    check_augment_function(
      aug = consistent_aug,
      model = NULL,
      data = iris,
      newdata = iris,
      strict = TRUE
    )
  )
})

