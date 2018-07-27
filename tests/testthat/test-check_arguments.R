context("test-check_arguments")

tidy_missing_default <- function(x, conf.method, ...) 1
tidy_no_conf_int <- function(x, conf.level = 0.95, ...) 1
tidy_no_conf_lvl <- function(x, conf.int = FALSE, ...) 1
tidy_wrong_conf_lvl <- function(x, conf.int = FALSE, conf.level = 0.8, ...) 1
tidy_wrong_conf_int <- function(x, conf.int = TRUE, conf.level = 0.95, ...) 1
tidy_not_in_glossary <- function(x, other_arg = NULL, ...) 1
tidy_correct <- function(x, conf.int = FALSE, conf.level = 0.95, conf.method = NULL, ...) 1

augment_no_data <- function(x, ...) 1
augment_wrong_newdata <- function(x, data = NULL, newdata, ...) 1
augment_correct <-  function(x, data = NULL, newdata = NULL, ...) 1

test_that("strict = FALSE", {
  expect_silent({
    check_arguments(tidy_missing_default, strict = FALSE)
    check_arguments(tidy_no_conf_int, strict = FALSE)
    check_arguments(tidy_no_conf_lvl, strict = FALSE)
    check_arguments(tidy_wrong_conf_lvl, strict = FALSE)
    check_arguments(tidy_wrong_conf_int, strict = FALSE)
    check_arguments(tidy_not_in_glossary, strict = FALSE)
    check_arguments(tidy_correct, strict = FALSE)

    check_arguments(augment_no_data, strict = FALSE)
    check_arguments(augment_wrong_newdata, strict = FALSE)
    check_arguments(augment_correct, strict = FALSE)
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
    check_arguments(tidy_no_conf_int, strict = TRUE),
    "Tidiers with `conf.level` argument must have `conf.int` argument."
  )

  expect_error(
    check_arguments(tidy_no_conf_lvl, strict = TRUE),
    "Tidiers with `conf.int` argument must have `conf.level` argument."
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
    paste0(
      "Arguments other_arg to `tidy_not_in_glossary` must be listed in",
      " the argument glossary."
    )
  )

  expect_silent({
    check_arguments(tidy_correct, strict = TRUE)
  })

  expect_error(
    check_arguments(augment_no_data, strict = TRUE),
    "Augment methods must have a `data` argument."
  )

  expect_error(
    check_arguments(augment_wrong_newdata, strict = TRUE),
    "`newdata` argument must default to `NULL`."
  )

  expect_silent({
    check_arguments(augment_correct, strict = TRUE)
  })
})
