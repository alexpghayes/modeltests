context("test-check_predict_arguments")

no_object_arg <- function(newdata = NULL, type = "response", ...) 1
no_newdata_arg <- function(object, type = "response", ...) 1
extra_arg <- function(object, newdata = NULL, type = "response", extra = NA, ...) 1

newdata_not_null <- function(object, newdata = NA, type = "response", ...) 1

allowed_type_args <- function(
  object,
  newdata = NULL,
  type = c(
    "response",
    "class",
    "prob",
    "link",
    "conf_int",
    "pred_int",
    "raw",
    "param_pred"
  ),
  ...) 1

unallowed_type_arg <- function(object, newdata = NULL, type = "banana", ...) 1

missing_type_default <- function(object, newdata = NULL, type, ...) 1
missing_newdata_default <- function(object, newdata, type = "response", ...) 1

correct_args <- function(object, newdata = NULL, type = c("prob", "class"), ...) 1

test_that("no object argument", {
  expect_error(
    check_predict_arguments(no_object_arg),
    "Predict methods must accept models via an `object` argument."
  )
})

test_that("no newdata argument", {
  expect_error(
    check_predict_arguments(no_newdata_arg),
    "Predict methods must accept data via a `newdata` argument."
  )
})

test_that("extra arguments", {
  expect_error(
    check_predict_arguments(extra),
    "my_error"
  )
})

test_that("newdata default error is not null", {
  expect_error(
    check_predict_arguments(newdata_not_null),
    "my_error"
  )
})

test_that("newdata default error is not null", {
  expect_error(
    check_predict_arguments(newdata_not_null),
    "my_error"
  )
})

test_that("unallowed type arg", {
  expect_error(
    check_predict_arguments(unallowed_type_arg),
    "my_error"
  )
})

test_that("all allowed values for type pass check", {
  expect_silent(
    check_predict_arguments(allowed_type_args)
  )
})

test_that("type arg has default value", {
  expect_error(
    check_predict_arguments(missing_type_default),
    "my_error"
  )
})


test_that("newdata arg has default value", {
  expect_error(
    check_predict_arguments(missing_newdata_default),
    "my_error"
  )
})

test_that("correctly formed argument signature passes tests", {
  expect_silent(
    check_predict_arguments(correct_args)
  )
})

