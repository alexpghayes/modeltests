context("test-utils")

test_that("has_rownames", {

  expect_false(has_rownames(as_tibble(iris)))
  expect_false(has_rownames(iris))

  iris2 <- iris
  rownames(iris2) <- paste0("obs", 1:nrow(iris2))

  expect_true(has_rownames(iris2))
})

test_that("acceptable_augment_colnames", {
  fit <- lm(hp ~ log(mpg), mtcars)
  ok_cols <- acceptable_augment_colnames(fit, mtcars)

  expect_true(all(colnames(mtcars) %in% ok_cols))
  expect_true("log.mpg." %in% ok_cols)

  # require that column names must be valid without quoting
  # this may turn out to be too strict
  expect_false("log(mpg)" %in% ok_cols)
})
