#' Check the output of a predict method
#'
check_predict_output <- function(predictions, passed_data, interval = TRUE,
                                 std_error = TRUE, fancy = FALSE) {



  ## pass outcomes down to this?

  if (fancy) {

    .pred_col <- predictions$.pred
    exists <- !is.null(.pred_col)

    expect_true(
      exists,
      "A `.pred` column must be present in predictions when `fancy = TRUE`."
    )

    if (exists)
      expect_s3_class(.pred_col, "list")

    return(invisible())
  }

  pred_cols <- colnames(predictions)
  passed_cols <- colnames(passed_data)

  expect_false(
    any(passed_cols %in% pred_cols),
    info = "Columns from `newdata` must not appear in prediction tibble."
  )

  expect_equal(
    nrow(predictions), nrow(passed_data),
    info = "Prediction tibble must have same number of rows as `newdata`."
  )

  ## type checking

  if (".pred" %in% pred_cols) {

  }

  ## check that

  ## same number of rows

  ## name-column consistency
  # - univariate numeric predictions in .pred
  # - multivariate numeric predictions in .pred_{outcome name}
  # - class predictions (factor) in .pred_class
  # - class probabilities (numeric) in .pred_{class_level}
  #    - these should add up to one


  # how is uncertainty is class probabilities

  if (interval) {
    ## expect numeric columns .pred_lower and .pred_upper

    ## TODO: intervals / uncertainty for class probabilities
  }

  if (std_error) {
    ## expect numeric column .
  }
}
