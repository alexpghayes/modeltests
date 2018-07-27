#' Check an augment method
#'
#' @template boilerplate
#'
#' @inheritParams check_augment_data_specification
#' @param newdata A dataset to use to check the `newdata` behavior, ideally distinct
#'   for the dataset used to check the `data` behavior.
#'
#' @export
#'
#' @description Test when `strict = FALSE`:
#'
#' - `aug(model, data = data)` passes [check_tibble()]
#' - `aug(model, newdata = newdata)` passes [check_tibble()]
#'
#' Additional tests when `strict = TRUE`:
#'
#' - `aug(model, data = data)` passes [check_augment_data_specification()].
#' - `aug(model, newdata = newdata)` passes [check_augment_data_specification()].
#' - `aug(model, newdata = newdata)` passes [check_augment_data_specification()]
#'   with `add_missing = TRUE`.
#' - If `aug` has a `newdata` argument, the `newdata` argument takes precedence
#'   over a `data` argument, i.e. calls [check_augment_newdata_precedence()].
#' - `aug` either gives an informative error or produces a reasonable tibble,
#'   i.e. calls [check_augment_no_data()].
#'
#' Note that it doesn't make sense to test that `aug(model, data = data)`
#' passes [check_augment_data_specification()] with `add_missing = TRUE`. This is
#' because the user is already guaranteeing that `data` is the original dataset
#' used to create `model`.
#'
check_augment_function <- function(
  aug,
  model,
  data = NULL,
  newdata = NULL,
  strict = TRUE) {

  args <- names(formals(aug))

  data_passed <- !is.null(data)
  newdata_passed <- !is.null(newdata)

  newdata_arg <- "newdata" %in% args

  if (!data_passed)
    stop("Must pass `data` argument as augment method accepts data argument.")

  if (newdata_arg && !newdata_passed)
    stop("Must pass `newdata` argument as augment method accepts newdata argument.")

  if (!strict) {

    au_data <- aug(model, data = data)
    check_tibble(au_data, method = "augment", strict = strict)

    if (newdata_arg) {
      au_newdata <- aug(model, newdata = newdata)
      check_tibble(au_newdata, method = "augment", strict = strict)
    }

    return(invisible())
  }

  check_augment_no_data(
    aug = aug,
    model = model,
    passed_data = data
  )

  # make sure data in data frame, dataframe with rows, and tibble
  # all give expected results

  check_augment_data_specification(
    aug = aug,
    model = model,
    data = data,
    add_missing = FALSE,
    test_newdata = FALSE
  )

  # we don't check add_missing = TRUE for the data argument because the
  # user is guaranteeing us that the data they give us is the same
  # they gave to the modelling function. also, the new row of NAs added
  # *should* cause things like influence calculates to break

  if (newdata_arg) {

    check_augment_data_specification(
      aug = aug,
      model = model,
      data = newdata,
      add_missing = FALSE,
      test_newdata = TRUE
    )

    check_augment_data_specification(
      aug = aug,
      model = model,
      data = newdata,
      add_missing = TRUE,
      test_newdata = TRUE
    )

    check_augment_newdata_precedence(
      aug = aug,
      model = model,
      data = data
    )
  }
}
