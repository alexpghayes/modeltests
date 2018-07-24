#' Check that all elements of a list are equal
#'
#' From StackOverflow: https://tinyurl.com/list-elems-equal-r
#'
#' @param x A list.
#'
#' @return Either `TRUE` or `FALSE`.
#' @noRd
all_equal_list <- function(x) {
  sum(duplicated.default(x)) == length(x) - 1L
}
