#' TODO: DOCUMENT!!!
#'
#' @param tidy_method A tidying method. For example: `glance.Arima`.
#' @template boilerplate
#'
#' @description Tests when `strict = FALSE`:
#'
#' - None
#'
#' Tests when `strict = TRUE`:
#'
#' - `tidy_method` has a `conf.int` argument if it has a `conf.level` argument.
#' - All arguments to `tidy_method` are listed in the [argument_glossary].
#'
#' @seealso [testthat], [testthat::expect_true()]
#' @export
#'
check_predict_arguments <- function(predict_method) {


  arglist <- as.list(formals(predict_method))
  args <- names(arglist)
  func_name <- as.character(substitute(predict_method))

  ### argument presence

  expect_true(
    "object" %in% args,
    info = "Predict methods must accept models via an `object` argument."
  )

  expect_true(
    "new_data" %in% args,
    info = "Predict methods must accept data via a `new_data` argument."
  )

  allowed_args <- c(
    "object",
    "new_data",
    "type",
    "se_fit",
    "level",
    "threshold",
    "..."
  )

  not_allowed <- setdiff(args, allowed_args)

  expect_true(
    length(not_allowed) == 0,
    info = paste0(
      "Arguments ", paste(not_allowed, collapse = ", "), " to `", func_name,
      "` are not allowed."
    )
  )

  ### default argument values

  allowed_type_values <- c(
    "response",
    "class",
    "prob",
    "link",
    "conf_int",
    "pred_int",
    "raw",
    "param_pred"
  )

  expect_true(
    # NOTE: ideally the `type = c("response", "raw", "link")` is the signature
    # i.e. a vector rather than a single value, and then the predict method
    # should use match.arg right after that
    all(eval(arglist$type) %in% allowed_type_values),
    info = paste0(
      "Default argument to `type` is not a subset of:",
      paste(allowed_type_values, sep = "\n")
    )
  )

  ### missing default values

  no_defaults <- names(arglist[purrr::map_lgl(arglist, is.name)])
  no_defaults <- setdiff(no_defaults, c("object", "new_data", "..."))

  expect_true(
    length(no_defaults) == 0,
    info = paste0(
      "Arguments ", paste(no_defaults, collapse = ", "), " to `", func_name,
      "` must have default values. Use `NULL` to indicate missingness if necessary."
    )
  )
}
