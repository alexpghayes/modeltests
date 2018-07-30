#' Check that newdata argument has higher precedence than data argument
#'
#' @template boilerplate
#' @inheritParams check_augment_data_specification
#' @export
#'
check_augment_newdata_precedence <- function(aug, model, data, strict = TRUE) {

  expect_true(TRUE)  # prevent skip message when strict

  if (!strict)
    return(invisible())

  if (nrow(data) < 10)
    stop(
      "Data for checking newdata predence must have at least 10 rows.",
      call. = FALSE
    )

  newdata <- tail(data, 5)

  au_data <- aug(model, data = data)
  au_newdata <- aug(model, newdata = newdata)
  au_data_newdata <- aug(model, data = data, newdata = newdata)

  expect_true(
    all.equal(au_newdata, au_data_newdata),
    info = "Must specify either `data` or `newdata` argument."
  )

  expect_false(
    all.equal(au_data, au_newdata),
    info = "Must specify either `data` or `newdata` argument."
  )

  expect_false(
    all.equal(au_data, au_data_newdata),
    info = "Must specify either `data` or `newdata` argument."
  )
}

