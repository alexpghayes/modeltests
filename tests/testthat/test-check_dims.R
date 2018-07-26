context("test-check_dims")

test_that("expected_rows argument works", {
  expect_silent(check_dims(iris, expected_rows = 150))
  expect_error(check_dims(iris, expected_rows = 10))
})

test_that("expected_cols argument works", {
  expect_silent(check_dims(iris, expected_cols = 5))
  expect_error(check_dims(iris, expected_cols = 1))
})
