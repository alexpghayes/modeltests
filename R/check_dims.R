#' Check that tibble has expected dimensions.
#'
#' @param data A tibble or data frame.
#' @param expected_rows Expected number of rows of tibble.
#' @param expected_cols Expected number of columns of tibble.
#'
#' @export
#'
#' @examples
#'
#' check_dims(iris, expected_rows = 150)
#'
check_dims <- function(data, expected_rows = NULL, expected_cols = NULL) {

  if (!is.null(expected_rows)) {
    expect_equal(nrow(data), expected_rows)
  }

  if (!is.null(expected_cols)) {
    expect_equal(ncol(data), expected_cols)
  }
}
