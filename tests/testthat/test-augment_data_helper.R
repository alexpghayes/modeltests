context("test-augment_data_helper")

library(tibble)
library(dplyr)

check_rownames <- function(lst) {
  expect_equal(rownames(lst$row_nm), paste0("obs", 1:nrow(lst$row_nm)))
  expect_equal(rownames(lst$no_row), paste0(1:nrow(lst$no_row)))
}

check_datalist_classes <- function(lst) {
  expect_true(is_tibble(lst$tibble))

  expect_true(is.data.frame(lst$no_row))
  expect_true(is.data.frame(lst$row_nm))

  expect_false(is_tibble(lst$no_row))
  expect_false(is_tibble(lst$row_nm))
}

# assumes input to augment_data_helper is 5 rows by 3 columns
check_na <- function(lst) {

  check_one_na <- function(df) {
    if (is_tibble(df)) {
      expect_equal(pull(df[1, 1]), NA_real_)
      expect_equal(pull(df[2, 2]), NA_real_)
      expect_equal(pull(df[3, 3]), NA_real_)
      expect_equal(unlist(df[6, ], use.names = FALSE), rep(NA_real_, 3))
    } else {
      expect_equal(df[1, 1], NA_real_)
      expect_equal(df[2, 2], NA_real_)
      expect_equal(df[3, 3], NA_real_)
      expect_equivalent(df[6, ], rep(NA_real_, 3))
    }
  }

  purrr::walk(lst, check_one_na)
}

check_result <- function(lst, add_missing) {
  check_datalist_classes(lst)
  check_rownames(lst)
  if (add_missing)
    check_na(lst)
}

tbl <- tibble(x = 1:5, y = 6:10, z = 11:15)
df <- data.frame(x = 1:5, y = 6:10, z = 11:15)
df_row <- df
rownames(df_row) <- letters[1:5]

test_that("add_missing = FALSE", {
  res <- augment_data_helper(tbl, add_missing = FALSE)
  check_result(res, add_missing = FALSE)

  res2 <- augment_data_helper(df, add_missing = FALSE)
  check_result(res2, add_missing = FALSE)

  res3 <- augment_data_helper(df_row, add_missing = FALSE)
  check_result(res3, add_missing = FALSE)
})

test_that("add_missing = TRUE", {
  res <- augment_data_helper(tbl, add_missing = TRUE)
  check_result(res, add_missing = TRUE)

  res2 <- augment_data_helper(df, add_missing = TRUE)
  check_result(res2, add_missing = TRUE)

  res3 <- augment_data_helper(df_row, add_missing = TRUE)
  check_result(res3, add_missing = TRUE)
})

rm(tbl, df, df_row)
