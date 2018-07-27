#' Check the output of a tidy method
#'
#' @template boilerplate
#'
#' @description A thin wrapper around [check_tibble()].
#'
#' @param td Output from a tidy method.
#' @inherit check_tibble params return
#'
#' @export
check_tidy_output <- function(td, strict = TRUE) {
  check_tibble(td, method = "tidy", strict = strict)
}
