context("test-check_glance_outputs")

library(tibble)

test_that("strict = FALSE", {

  gl <- tibble(AIC = 1, BIC = 2)
  gl2 <- tibble(AIC = 1, BIC = 3, logLik = 4)

  two_rows <- tibble(AIC = 1:2, BIC = 3:4)

  expect_silent(
    check_glance_outputs(gl, gl2, strict = FALSE)
  )

  expect_error(
    check_glance_outputs(two_rows, strict = FALSE),
    "Glance must return a tibble with exactly 1 row."
  )

  expect_error(
    check_glance_outputs(gl, gl2, two_rows, strict = FALSE),
    "Glance must return a tibble with exactly 1 row."
  )
})

test_that("strict = TRUE", {

  gl <- tibble(AIC = 1, BIC = 2)
  gl2 <- tibble(AIC = 1, BIC = 3, logLik = 4)
  gl3 <- tibble(BIC = 6, logLik = 5, AIC = 7)

  expect_error(
    check_glance_outputs(gl, gl2, strict = TRUE),
    "Glance column names and order must agree across all ouputs."
  )

  expect_error(
    check_glance_outputs(gl, gl2, strict = TRUE),
    "Glance column names and order must agree across all ouputs."
  )
})
