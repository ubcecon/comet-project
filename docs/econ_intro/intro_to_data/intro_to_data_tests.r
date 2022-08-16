library(testthat)
library(digest)

test_0 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer0), "03e6f9fdf05162a425d39f81512124a2")
  })
  print("Success!")
}

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer1), "70ee60bb4371259196e9c61e3851aac3")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer2), "dd531643bffc240879f11278d7a360c1")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer3), "e2d56c68572ad1cd17b74ef4aa424c88")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer4), "eb123648c8976514d74104f67d1200dc")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer5), "3b4dada5338837936059d916d01799cb")
  })
  print("Success!")
}
