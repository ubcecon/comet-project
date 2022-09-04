library(digest)
library(testthat)

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "d38947288a6067c0290cd00d7f855a16")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4), "831526ad46f9e890a28eb44ec9f3afdc")
  })
  print("Success!")
}