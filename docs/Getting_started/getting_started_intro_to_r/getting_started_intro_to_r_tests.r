library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    
    if(digest(answer1) == "709a7faa72cb6f3be79d683e234ccb25" | digest(answer1) == "8ae1ac7bdf62dca7c19b427a9153445c") {
        expect_equal("Got it", "Got it")
    }
    else {
      expect_equal("Got it?", "Not quite!")
    }
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer2), "11946e7a3ed5e1776e81c0f0ecd383d0")
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
    expect_equal(digest(answer5), "88d2821508a6625b093500c3e4d8d684")
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(answer6, digits = 2)), "88d2821508a6625b093500c3e4d8d684")
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer7), "fef8ee3e709a216af937131dec2cff3a")
  })
  print("Success!")
}