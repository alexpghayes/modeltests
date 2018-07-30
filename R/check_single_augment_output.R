#' Check the output of an augment method
#'
#' @template boilerplate
#' @keywords internal
#'
#' @param au Output from a call to [augment()].
#' @param passed_data Whichever of `data` or `newdata` was passed to
#'   `augment`. Should be a data frame or tibble.
#'
#' @description Test when `strict = FALSE`:
#'
#' - `au` passes [check_tibble()].
#' - All column names present in `passed_data` are also present in `au`.
#'
#' Additional tests when `strict = TRUE`:
#'
#' - If `passed_data` has rownames other than simple row numbers (i.e. `paste(1:5)`),
#'   `au` contains a column called `.rownames`.
#'
check_single_augment_output <- function(au, passed_data, strict = TRUE) {

  orig_cols <- acceptable_augment_colnames(model, passed_data)
  aug_cols <- colnames(au)
  new_cols <- setdiff(aug_cols, orig_cols)

  check_tibble(au, method = "augment", columns = new_cols)

  expect_equal(nrow(au), nrow(passed_data),
    info = "Augmented data must have same number of rows as original data."
  )

  expect_true(
    all(orig_cols %in% aug_cols),
    info = "Original columns must be present in augmented data."
  )

  if (!strict) {
    return(invisible())
  }

  if (has_rownames(passed_data)) {
    row_nm <- rownames(passed_data)
    if (all(row_nm != seq_along(row_nm))) {
      expect_true(
        ".rownames" %in% aug_cols,
        info = paste(
          "A `.rownames` column must be present in augmented data when input\n",
          "data is a data.frame with rownames other than 1, 2, 3, ..."
        )
      )
    }
  }
}
