#' Check the output of a glance method
#'
#' @template boilerplate
#'
#' @param ... Outputs returned from calls to (the same) [broom::glance()] method.
#'
#' @description Tests when `strict = FALSE`:
#'
#' - Each item passed to `...` passes [check_tibble()]
#' - Each item passed to `...` has exactly 1 row.
#'
#' Additional tests when `strict = TRUE`:
#'
#' - Column names and order agree across all elements of `...`.
#'
#' @export
#' @seealso [check_tibble()]
#'
check_glance_outputs <- function(..., strict = TRUE) {

  check_single_glance_output <- function(gl) {
    check_tibble(gl, method = "glance", strict = strict)
    expect_equal(
      nrow(gl), 1,
      info = "Glance must return a tibble with exactly 1 row."
    )
  }

  glances <- list(...)
  purrr::walk(glances, check_single_glance_output)

  if (!strict) {
    return(invisible())
  }

  expect_true(
    all_equal_list(purrr::map(glances, colnames)),
    info = "Glance column names and order must agree across all ouputs."
  )
}
