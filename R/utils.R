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

has_rownames <- function(df) {
  if (is_tibble(df))
    return(FALSE)

  any(rownames(df) != as.character(1:nrow(df)))
}
