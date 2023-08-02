library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer1), "d110f00cfb1b248e835137025804a23b")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer2), "6e7a8c1c098e8817e3df3fd1b21149d1")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer3), "38ce1fe9e19a222505e693e8bdd8aeec")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer4), "234fd2733050ae61658f84b0144210b0")
  })
  print("Success!")
}


test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer5), "88d2821508a6625b093500c3e4d8d684")
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(answer6, digits = 2)), "90e5b5628ced60ec74b53a4d5527d4a1")
  })
  print("Success!")
}