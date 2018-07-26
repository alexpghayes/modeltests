context("test-check_single_augment_output")

library(dplyr)

test_that("strict = FALSE", {

  orig <- iris
  au <- iris %>%
    mutate(
      .fitted = Sepal.Length,
      .resid = rnorm(Sepal.Length)
    ) %>%
    as_tibble()

  au_missing_col <- au %>%
    select(-Petal.Width)

  au_wrong_rows <- head(au)

  iris_rownames <- iris
  rownames(iris_rownames) <- paste0("obs", 1:nrow(iris))

  expect_silent(
    check_single_augment_output(au, iris, strict = FALSE)
  )

  expect_error(
    check_single_augment_output(au_missing_col, iris, strict = FALSE),
    "Original columns must be presented in augmented data."
  )

  expect_error(
    check_single_augment_output(au_wrong_rows, iris, strict = FALSE),
    "Augmented data must have same number of rows as original data."
  )

  expect_silent(
    check_single_augment_output(au, iris_rownames, strict = FALSE)
  )
})

test_that("strict = TRUE", {

  iris_rownames <- iris
  rownames(iris_rownames) <- paste0("obs", 1:nrow(iris))

  expect_error(
    check_single_augment_output(as_tibble(iris), iris_rownames, strict = TRUE),
    "A `.rownames` column must be present in augmented data when input"
  )
})
