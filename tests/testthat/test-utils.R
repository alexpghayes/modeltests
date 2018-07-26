context("test-utils")

test_that("has_rownames", {

  expect_false(has_rownames(as_tibble(iris)))
  expect_false(has_rownames(iris))

  iris2 <- iris
  rownames(iris2) <- paste0("obs", 1:nrow(iris2))

  expect_true(has_rownames(iris2))
})
