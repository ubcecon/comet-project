library(digest)
library(testthat)

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "30986e242495ba4a216a86700f7ece29")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4), "35246e848605e541fd7185fbb1f89f91")
  })
  print("Success!")
}