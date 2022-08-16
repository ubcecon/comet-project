library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "5f27e1ddc5abf47b89ca737ec8ca002c")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "d055f4e0f0ce06123d0210bced79d88a")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "a8509fa7ee5b900a9958cf0927e4b2e0")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4), "3a5505c06543876fe45598b5e5e5195d")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_5), "81949aed6f8e18b150efa97ff46a6fc3")
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_6), "b4f5e8891b19a71cd98e3e688e75ea02")
  })
  print("Success!")
}
