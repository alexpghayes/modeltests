#' Check a predict method
#'
#' Driver function that calls lots of other tests
#' # should call twice: once with training data, once with test data
# test data should still have the outcome present in the data

#' @importFrom dplyr select mutate one_of
#' @export
check_predict <- function(
  predict_method,
  object,
  new_data = NULL,
  outcome = NULL,
  types = NULL) {

  new_data_passed <- !is.null(new_data)

  if (is.null(new_data))
    stop(
      "Must pass data to `check_predict_input()` via `new_data` argument.",
      call. = FALSE
    )

  n <- nrow(new_data)
  if (n < 5)
    stop("`new_data` must have at least five rows.", call. = FALSE)

  if (is.null(outcome))
    stop("Must specify model outcomes via `outcome` argument.", call. = FALSE)

  expect_error(
    predict_method(object),
    info = "Must specify data via `new_data` argument."
  )

  if (is.null(types)) {
    arglist <- as.list(formals(predict_method))
    types <- eval(arglist$type)
  }

  no_outcome <- select(new_data, -one_of(outcome))
  no_outcome <- select(new_data, -one_of(outcome))

  extra_cols <- mutate(
    new_data,
    .apples = rnorm(n),
    .oranges = rnorm(n),
    .bananas = rnorm(n),
    SnaKeCaseISbaDfoRtHeSOul = rnorm(n)
  )

  one_row <- new_data[1, ]

  expect_silent({
    ## outcome not required for new_data
    predict_method(object, no_outcome)

    ## but having the outcome and extra columns shouldn't break anything
    predict_method(object, extra_cols)

    ## and things should work with only one row
    predict_method(object, one_row)
  })

  ## same deal but explicitly specify type argument

  for (type in types) {

    expect_silent({
      no_outcome_preds <- predict_method(object, no_outcome, type = type)
    })

    check_predict_output(
      predictions = no_outcome_preds,
      passed_data = no_outcome,
      type = type
    )

    expect_silent({
      extra_cols_preds <- predict_method(object, extra_cols, type = type)
    })

    check_predict_output(
      predictions = extra_cols_preds,
      passed_data = extra_cols,
      type = type
    )

    expect_silent({
      one_row_preds <- predict_method(object, one_row, type = type)
    })

    check_predict_output(
      predictions = one_row_preds,
      passed_data = one_row,
      type = type
    )
  }

  ## TODO: check that se_fit does something
}
