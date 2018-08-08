context("test-check_augment_se_fit")

# don't get .se.fit column if
#  - no se_fit argument
#  - se_fit = FALSE

# do get .se.fit column if
#  - se_fit = TRUE

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
