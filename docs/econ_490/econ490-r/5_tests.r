library(digest)
library(testthat)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "fcade9672aeaaffa53564d1e42b6a075")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "38ed1b8c550b2a51c8ccb2534974dc7e")
  })
  print("Success!")
}