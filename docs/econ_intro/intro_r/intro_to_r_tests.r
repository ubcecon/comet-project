library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer1), "709a7faa72cb6f3be79d683e234ccb25")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer2), "e5b57f323c7b3719bbaaf9f96b260d39")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer3), "9239c01218000016670da90e840448be")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer4), "956c89af90f9cdb78602f16f511bb248")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer5), "59637eb9ce16af715ecb172f2b5f25aa")
  })
  print("Success!")
}


test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer6), "88d2821508a6625b093500c3e4d8d684")
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(answer7, digits = 2)), "90e5b5628ced60ec74b53a4d5527d4a1")
  })
  print("Success!")
}