#' Check that augment behavior is consistent for dataframes and tibbles
#'
#' @template boilerplate
#' @keywords internal
#'
#' @description Uses [augment_data_helper()] to create copies of the same dataset as
#' a tibble, data frame and dataframe with rownames. When `add_missing = TRUE` these
#' datasets have missing values along the diagonal, and one row of entirely missing
#' values. Once the datasets have been generated, tests that:
#'
#' - `augment(fit, data = generated_dataset)` passes [check_tibble()] for each
#'   generated dataset.
#' - Output of `augment(fit, data = generated_dataset)` is the same for all three
#'   generated datasets, except the data frame with rownames should also generate
#'   a `.rownames` column that the tibble and nameless data frame do not.
#'
#' Additional tests when `test_newdata = TRUE`:
#'
#' - `head(aug(model, newdata = data))` equals `aug(head(model, newdata = data))`.
#'   This commutativity check catches issues where the output of `predict` changes
#'   for the same data point depending on the rest of the dataset.
#'
#' @param aug An augment method. For example, `augment.betareg`.
#' @param model A fit model object to call the augment method on.
#' @param data A data frame or tibble to use when testing `aug`.
#' @param add_missing Logical indicating whether or not missing data should be
#'   introduced into the datasets generated with [augment_data_helper()]. This
#'   missing data is only used to test the `newdata` argument, not the `data`
#'   argument.
#' @param test_newdata Logical indicating whether the `newdata` argument behavior
#'   should be tested instead of the `data` argument behavior.
#'
check_augment_data_specification <- function(
  aug,
  model,
  data,
  add_missing,
  test_newdata) {

  dl <- augment_data_helper(data, add_missing)
  new_dl <- dl

  if (test_newdata) {
    dl <- list()
    passed_data <- new_dl
  } else {
    new_dl <- list()
    passed_data <- dl
  }

  au_tibble <- aug(model, data = dl$tibble, newdata = new_dl$tibble)
  au_no_row <- aug(model, data = dl$no_row, newdata = new_dl$no_row)
  au_row_nm <- aug(model, data = dl$row_nm, newdata = new_dl$row_nm)

  au_list <- list(au_tibble, au_no_row, au_row_nm)
  purrr::walk2(au_list, passed_data, check_single_augment_output)

  expect_equal(au_tibble, au_no_row,
               info = "Augmented data must be the same for tibble and data frame input."
  )

  # au_row_nm should have a `.rownames` column not present in `au_tibble` or
  # `au_no_row`. presence is checked in `check_single_augment_output`,
  # here we just that that the results are the same after stripping this
  # column out.

  expect_equal(
    au_no_row,
    dplyr::select(au_row_nm, -.rownames),
    info = paste(
      "Augmented data must be the same for dataframes with and without",
      "rownames."
    )
  )

  # next up: tests that subsets of the newdata behave appropriately
  # i.e. we should have that results(head(newdata)) == head(results(newdata))

  if (test_newdata) {

    head_dl <- purrr::map(dl, head)
    head_new_dl <- purrr::map(new_dl, head)

    hd_tibble <- aug(model, data = head_dl$tibble, newdata = head_new_dl$tibble)
    hd_no_row <- aug(model, data = head_dl$no_row, newdata = head_new_dl$no_row)
    hd_row_nm <- aug(model, data = head_dl$row_nm, newdata = head_new_dl$row_nm)

    hd_list <- list(hd_tibble, hd_no_row, hd_row_nm)

    expect_equal(
      hd_list, purrr::map(au_list, head),
      info = paste0(
        "Subsetting data before vs after augmentation must not effect results.",
        "\ni.e. we must have results(head(newdata)) == head(results(newdata))"
      )
    )
  }
}
