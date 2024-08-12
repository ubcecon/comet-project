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
    expect_equal(digest(class(answer1$has_kids)), "317be6c8c60403220d0f4f9fa663a873")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer2$knows_english), "292b8d7e3f0f8819ba70ae7e22ec95e2")
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
    expect_equal(digest(answer4), "53ccbf0bbf5fc8810c3131aa9c68fc30")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer5), "6bb5401ed41412a0086eb53bcb1a0680")
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer6), "a66d819dbe9fb5ef876a0eeaeb822493")
  })
  print("Success!")
}
