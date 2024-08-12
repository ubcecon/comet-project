library(digest)
library(testthat)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "94ab180ad7269576ff1d922cff1b676a")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "8dbde19b2dfe309acce40cc81b50558a")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "36854ded6834f187691a08d0f07b72c7")
  })
  print("Success!")
}