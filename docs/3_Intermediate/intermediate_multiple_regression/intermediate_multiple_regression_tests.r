library(digest)
library(testthat)
library(tidyverse)



test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), 'f7b0db1c9bc01deadcdde41033af1311')
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg_LESS), '7ef0f8d46b652195aa7340bcf9340a61')
  })
  print("Success!")
}


test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg_HS), '4b9ba86e43b7a43ac4ae778c571266ae')
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg_NU), '6791390661fcbed87e3a3eea05337789')
  })
  print("Success!")
}


test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg_U), 'ced3ee3867deba3375a08212c8426ad6')
  })
  print("Success!")
}

test_8 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg2), '5da2b8e1d868eba551f80532d68e359e')
  })
  print("Success!")
}

test_14 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg3), '1fe712c3569f180890f84e0892e14268')
  })
  print("Success!")
}

test_17 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg4), '7711b9967c1bc23644f3dd0dca101772')
  })
  print("Success!")
}
test_20 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg5), '3daee8abecd1039e0d98a4ebb50e86a8')
  })
  print("Success!")
}