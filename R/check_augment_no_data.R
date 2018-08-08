#' Check an augment method when no data or newdata is passed
#'
#' @template boilerplate
#'
#' @inheritParams check_augment_data_specification
#' @param passed_data The dataset that `model` was original fit on that
#'   `aug` should try to reconstruct when neither `data` nor `newdata` is
#'   specified.
#'
#' @description Test when `strict = FALSE`:
#'
#' - None
#'
#' Additional tests when `strict = TRUE`:
#'
#' - `aug(model)` either returns an informative error or produces output
#'   that passes [check_tibble()].
#' - If the output passes `check_tibble`, will issue warning when:
#'   - Augmented data is missing rows from original data.
#'   - Augmented data is missing columns from original data.
#'   - Original data has rownames but ugmented data is missing `.rownames`
#'      column.
#'
check_augment_no_data <- function(aug, model, passed_data, strict = TRUE) {

  expect_true(TRUE)  # prevent skip message when skip = FALSE

  if (!strict)
    return(invisible())

  safe_aug <- purrr::possibly(aug, NULL)
  au <- safe_aug(model)

  if (is.null(au)) {
    expect_error(
      aug(model),
      "Must specify either `data` or `newdata` argument.",
      info = paste0(
        "\n\n\nAugment failed but did not give an informative error message.\n",
        "Please use the following error message:\n",
        "  Must specify either `data` or `newdata` argument."
      )
    )
  } else {
    ok_cols <- acceptable_augment_colnames(model, passed_data)
    aug_cols <- colnames(au)
    new_cols <- setdiff(aug_cols, ok_cols)

    check_tibble(au, method = "augment", columns = new_cols)
  }
}
