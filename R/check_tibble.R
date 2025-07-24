#' Check the output of a tidying method
#'
#' Do not call directly. Helper function used by [check_tidy_output()],
#' [check_glance_outputs()] and [check_augment_function()].
#'
#' @template boilerplate
#'
#' @param output Object returned from [broom::tidy()], [broom::augment()] or [broom::glance()].
#' @param method One of `"tidy"`, `"augment"` or `"glance"`. Determines
#'   which set of column name checks are applied.
#' @param columns The names of the columns in the output data frame. Defaults
#'   to the column names of `output`. Useful when checking [broom::augment()] when you only
#'   want to check the new columns in the data frame, as opposed to all
#'   columns.
#'
#' @description Tests when `strict = FALSE`:
#'
#' - `output` is a tibble.
#'
#' Additional tests when `strict = TRUE`:
#'
#' - `columns` are listed in the [column_glossary].
#'
#' @export
#'
check_tibble <- function(
  output,
  method,
  columns = colnames(output),
  strict = TRUE) {

  expect_s3_class(output, "tbl_df")

  if (!strict) {
    return(invisible())
  }

  acceptable_columns <- column_glossary %>%
    dplyr::filter(method == !!method) %>%
    dplyr::pull(column)

  unacceptable <- setdiff(columns, acceptable_columns)

  expect_true(
    length(unacceptable) == 0,
    info = paste0(
      "Output column names not in the column glossary: ",
      paste(unacceptable, collapse = ", ")
    )
  )
}
