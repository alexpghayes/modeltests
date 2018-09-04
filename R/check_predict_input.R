#' Check a predict method
#'
#' Driver function that calls lots of other tests
#'
check_predict <- function(
  predict_method,
  object,
  newdata = NULL,  # should call twice: once with training data, once with test data
                   # test data should still have the outcome present in the data
  outcome = NULL) {

  newdata_passed <- !is.null(newdata)


  if (is.null(newdata))
    stop(
      "Must pass data to `check_predict_input()` via `newdata` argument.",
      call. = FALSE
    )

  n <- nrow(newdata)
  if (n < 5)
    stop("`newdata` must have at least five rows.", call. = FALSE)

  if (is.null(outcome))
    stop("Must specify model outcomes via `outcome` argument.", call. = FALSE)

  expect_error(
    predict_method(object),
    info = "Must specify data via `newdata` argument."
  )

  ## TODO: test for each allowed `type`?

  arglist <- as.list(formals(predict_method))
  allowed_types <- arglist$type

  no_outcome <- select(newdata, -one_of(outcome))

  extra_cols <- mutate(
    newdata,
    .apples = rnorm(n),
    .oranges = rnorm(n),
    .bananas = rnorm(n),
    SnaKeCaseISbaDfoRtHeSOul = rnorm(n)
  )

  one_row <- slice(newdata, 1)

  expect_silent({
    ## outcome not required for newdata
    predict_method(object, no_outcome)

    ## but having the outcome and extra columns shouldn't break anything
    predict_method(object, extra_cols)

    ## and things should work with only one row
    predict_method(object, one_row)
  })

  ## same deal but explicitly specify type argument

  for (type in allowed_types) {

    ## TODO: abstract into a check_predict_on function

    expect_silent({
      predict_method(object, no_outcome, type = type)
      predict_method(object, extra_cols, type = type)
    })
  }

  ## having additio

}
