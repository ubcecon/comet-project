library(digest)
library(testthat)

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "fe44c01804a840826a3106dd8c7f5470")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "ca69835ab7247255f11f818dc5097271")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4), "e38aea1c1bd826f7861331e90aa659be")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_5), "dbc09cba9fe2583fb01d63c70e1555a8")
  })
  print("Success!")
}