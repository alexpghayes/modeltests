context("test-check_augment_data_specification")

base_aug <- function(data = NULL, newdata = NULL) {
  df <- if (!is.null(newdata)) newdata else data

  if (has_rownames(df))
    df$.rownames <- rownames(df)

  df$.fitted <- 1:nrow(df)
  df$.resid <- 1:nrow(df) + 1
  df
}

consistent_aug <- function(model, data = NULL, newdata = NULL) {
  as_tibble(base_aug(data, newdata))
}

# behaves differently for data frame and tibble input
inconsistent_tibble_aug <- function(model, data = NULL, newdata = NULL) {
  df <- base_aug(data, newdata)
  df[1, 1] <- if (is_tibble(df)) "pineapple" else "strawberry"
  as_tibble(df)
}

# behaves differently for data frames and dataframe with rownames
inconsistent_rowname_aug <- function(model, data = NULL, newdata = NULL) {
  df <- base_aug(data, newdata)
  df[1, 1] <- if (has_rownames(df)) "pineapple" else "strawberry"
  as_tibble(df)
}

test_that("add_missing = FALSE, test_newdata = FALSE", {

  expect_error(
    check_augment_data_specification(
      aug = inconsistent_tibble_aug,
      model = NULL,
      data = iris,
      add_missing = FALSE,
      test_newdata = FALSE
    ),
    "Augmented data must be the same for tibble and data frame input."
  )

  expect_error(
    check_augment_data_specification(
      aug = inconsistent_rowname_aug,
      model = NULL,
      data = iris,
      add_missing = FALSE,
      test_newdata = FALSE
    ),
    "Augmented data must be the same for dataframes with and without rownames."
  )

  expect_silent(
    check_augment_data_specification(
      aug = consistent_aug,
      model = NULL,
      data = iris,
      add_missing = FALSE,
      test_newdata = FALSE
    )
  )
})

test_that("add_missing = FALSE, test_newdata = TRUE", {

  expect_error(
    check_augment_data_specification(
      aug = inconsistent_tibble_aug,
      model = NULL,
      data = iris,
      add_missing = FALSE,
      test_newdata = TRUE
    ),
    "Augmented data must be the same for tibble and data frame input."
  )

  expect_error(
    check_augment_data_specification(
      aug = inconsistent_rowname_aug,
      model = NULL,
      data = iris,
      add_missing = FALSE,
      test_newdata = TRUE
    ),
    "Augmented data must be the same for dataframes with and without rownames."
  )

  expect_silent(
    check_augment_data_specification(
      aug = consistent_aug,
      model = NULL,
      data = iris,
      add_missing = FALSE,
      test_newdata = TRUE
    )
  )
})

test_that("add_missing = TRUE, test_newdata = TRUE", {

  expect_error(
    check_augment_data_specification(
      aug = inconsistent_tibble_aug,
      model = NULL,
      data = iris,
      add_missing = TRUE,
      test_newdata = TRUE
    ),
    "Augmented data must be the same for tibble and data frame input."
  )

  expect_error(
    check_augment_data_specification(
      aug = inconsistent_rowname_aug,
      model = NULL,
      data = iris,
      add_missing = TRUE,
      test_newdata = TRUE
    ),
    "Augmented data must be the same for dataframes with and without rownames."
  )

  expect_silent(
    check_augment_data_specification(
      aug = consistent_aug,
      model = NULL,
      data = iris,
      add_missing = TRUE,
      test_newdata = TRUE
    )
  )
})

rm(base_aug, consistent_aug, inconsistent_tibble_aug, inconsistent_rowname_aug)

