#' Check that tidying methods use allowed argument names
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
#' - `tidy_method` has a `conf.level` argument if it has a `conf.int` argument.
#' - `conf.int` defaults to `FALSE` when present.
#' - `conf.level` defaults to `0.95`` when present.
#' - `exponentiate` defaults to `FALSE` when present.
#' - All arguments to `tidy_method` are listed in the [argument_glossary].
#'
#' @seealso [testthat], [testthat::expect_true()]
#' @export
#'
check_arguments <- function(tidy_method, strict = TRUE) {

  if (!strict) {
    expect_true(TRUE)  # prevent skip message
    return(invisible())
  }

  arglist <- as.list(formals(tidy_method))
  args <- names(arglist)
  func_name <- as.character(substitute(tidy_method))

  # functions might be: tidy.irlba *or* tidy_irlba for list tidiers
  prefix <- gsub("[\\.|_].*","", func_name)
  allowed_args <- dplyr::filter(argument_glossary, method == !!prefix)$argument

  if (prefix %in% c("glance", "tidy") && "conf.level" %in% args) {
    expect_true(
      "conf.int" %in% args,
      info = "Tidiers with `conf.level` argument must have `conf.int` argument."
    )

    expect_equal(
      arglist$conf.level, 0.95,
      info = "`conf.level` argument must default to `0.95`."
    )
  }

  if ("conf.int" %in% args) {
    expect_false(
      arglist$conf.int,
      info = "`conf.int` argument must default to `FALSE`."
    )

    expect_true(
      "conf.level" %in% args,
      info = "Tidiers with `conf.int` argument must have `conf.level` argument."
    )
  }

  if ("exponentiate" %in% args) {
    expect_false(
      arglist$exponentiate,
      info = "Argument `exponentiate` must default to `FALSE`."
    )
  }

  if ("newdata" %in% args) {
    expect_null(
      arglist$newdata,
      info = "`newdata` argument must default to `NULL`."
    )
  }

  if (prefix == "augment") {
    expect_true(
      "data" %in% args,
      info = "Augment methods must have a `data` argument."
    )
  }

  no_defaults <- names(arglist[purrr::map_lgl(arglist, is.name)])
  no_defaults <- setdiff(no_defaults, c("x", "..."))

  expect_true(
    length(no_defaults) == 0,
    info = paste0(
      "Arguments ", paste(no_defaults, collapse = ", "), " to `", func_name,
      "` must have default values. Use `NULL` to indicate missingness if necessary."
    )
  )

  not_allowed <- setdiff(args, c(allowed_args, "x", "..."))

  expect_true(
    length(not_allowed) == 0,
    info = paste0(
      "Arguments ", paste(not_allowed, collapse = ", "), " to `", func_name,
      "` must be listed in the argument glossary."
    )
  )
}
