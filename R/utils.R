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


#' Check whether or not a data-frame-like object has rownames
#'
#' @param df A data frame
#'
#' @return Logical indicating if `df` has rownames. If `df` is a tibble, returns `FALSE`.
#'   If `df` is a data.frame, return `FALSE` if the rownames are simply row numbers. If the
#'   rownames are anything other than the return row numbers, returns `TRUE`.
#' @export
has_rownames <- function(df) {
  if (tibble::is_tibble(df))
    return(FALSE)
  any(rownames(df) != as.character(1:nrow(df)))
}

all.equal.tbl_df <- function(target, current, ...) {
  df_target <- as.data.frame(target)
  df_current <- as.data.frame(current)
  isTRUE(all.equal(df_target, df_current))
}
