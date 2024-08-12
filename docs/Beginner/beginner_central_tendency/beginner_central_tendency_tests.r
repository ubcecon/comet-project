library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "b71ae1fc63e62eeb285198e5b0fa9732")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "6dcbec622b786a4746ac48603c69f318")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "3a5505c06543876fe45598b5e5e5195d")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4), "dbe44290f77ce353f9b6dab6fce787b6")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_5), "861c36daa18d7a28798c8463a336717f")
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_6), "747e51aa7d6ab06826dd414e5c1ad1a3")
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_7), "f5873ad8cf04e4211f54d59469a6f162")
  })
  print("Success!")
}
