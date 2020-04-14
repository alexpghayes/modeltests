#' Get copies of a dataset with various rowname behaviors
#'
#' Helper function for [check_augment_data_specification()]. There should be no need
#' to ever use this directly in tests. Takes a dataset and returns a list
#' with three copies of the dataset. Optionally introduces `NA` values into
#' the dataset. Useful for checking that tibbles, data frames, and data frames with
#' rownames are treated equivalently.
#'
#' @param data A data set as a `data.frame` or `tibble`.
#' @param add_missing Whether or not to set some values in `data` to `NA`.
#'   When `TRUE` sets the diagonal elements of `data` to `NA` and adds a
#'   row of all `NA`s to the end of data. This ensures that every column
#'   has missing data. Defaults to `FALSE`.
#'
#' @return A list with three copies of `data`:
#' - **tibble**: the data in a [tibble::tibble()].
#' - **no_row**: the data in a [data.frame()] without row names.
#' - **row_nm**: the data in a `data.frame`, with row names.
#'
#' @seealso [.row_names_info()], [rownames()], [tibble::rownames_to_column()]
#' @keywords internal
#'
augment_data_helper <- function(data, add_missing) {

  if (add_missing) {

    for (i in 1:min(nrow(data), ncol(data))) {
      data[i, i] <- NA
    }

    data <- tibble::add_row(data)
  }

  tibble <- tibble::as_tibble(data)

  no_row <- as.data.frame(data)
  rownames(no_row) <- NULL

  row_nm <- no_row
  rownames(row_nm) <- paste0("obs", 1:nrow(data))

  list(tibble = tibble, no_row = no_row, row_nm = row_nm)
}
