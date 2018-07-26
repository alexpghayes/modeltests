context("test-check_tibble")

library(tibble)

test_that("strict = FALSE", {

  expect_silent(check_tibble(tibble(), strict = FALSE))

  expect_error(check_tibble(data.frame(), strict = FALSE))
  expect_error(check_tibble(list(), strict = FALSE))
  expect_error(check_tibble(1L, strict = FALSE))
})

test_that("strict = TRUE", {

  df <- tibble(
    term = letters[1:5],
    estimate = rnorm(5),
    invalid_name = 1:5
  )

  expect_error(
    check_tibble(df, method = "tidy", strict = TRUE),
    "Output column names not in the column glossary: invalid_name"
  )

  expect_silent(
    check_tibble(
      df,
      method = "tidy",
      columns = c("term", "estimate"),
      strict = TRUE
    )
  )
})

