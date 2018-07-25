context("test-check_arguments")

tidy_missing_default <- function(x, conf.method, ...) 1
tidy_no_conf_inf <- function(x, conf.level = 0.95, ...) 1
tidy_wrong_conf_lvl <- function(x, conf.int = FALSE, conf.level = 0.8, ...) 1
tidy_wrong_conf_int <- function(x, conf.int = TRUE, conf.level = 0.95, ...) 1
tidy_not_in_glossary <- function(x, other_arg = NULL, ...) 1
tidy_correct <- function(x, conf.int = FALSE, conf.level = 0.95, conf.method = NULL, ...) 1

test_that("strict = FALSE", {
  expect_silent({
    check_arguments(tidy_missing_default, strict = FALSE)
    check_arguments(tidy_no_conf_inf, strict = FALSE)
    check_arguments(tidy_wrong_conf_lvl, strict = FALSE)
    check_arguments(tidy_wrong_conf_int, strict = FALSE)
    check_arguments(tidy_not_in_glossary, strict = FALSE)
    check_arguments(tidy_correct, strict = FALSE)
  })
})

test_that("strict = TRUE", {

  expect_error(
    check_arguments(tidy_missing_default, strict = TRUE),
    paste0(
      "Arguments conf.method to `tidy_missing_default` must have default values. ",
      "Use `NULL` to indicate missingness if necessary."
    )
  )

  expect_error(
    check_arguments(tidy_no_conf_inf, strict = TRUE),
    "Tidiers with `conf.level` argument must have `conf.int` argument."
  )

  expect_error(
    check_arguments(tidy_wrong_conf_lvl, strict = TRUE),
    "`conf.level` argument must default to `0.95`."
  )

  expect_error(
    check_arguments(tidy_wrong_conf_int, strict = TRUE),
    "`conf.int` argument must default to `FALSE`."
  )

  expect_error(
    check_arguments(tidy_not_in_glossary, strict = TRUE),
    "Arguments other_arg to `tidy_not_in_glossary` must be listed in the argument glossary."
  )

  expect_silent({
    check_arguments(tidy_correct, strict = TRUE)
  })
})
