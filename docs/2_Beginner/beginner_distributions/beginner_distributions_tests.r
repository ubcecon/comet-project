library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "44b124c438681a6daead20469c4569e4")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "0590b0427c1b19a6eb612d19888aa52f")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "1a0468b92d718453727f0cca97824509")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4), "55ab2f5437a130b63d11dabc83b1d331")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_5), "71162853943c3ff45f1aa3dd8352f89e")
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_6), "908d1fd10b357ed0ceaaec823abf81bc")
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_7), "4940727701f3e0a12164baf5070f9a87")
  })
  print("Success!")
}

test_8 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_8), "db4b68351294e891168a5ecfc5f3009d")
  })
  print("Success!")
}

test_9 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(answer_9, 2)), "7524211463fd9c81f4124b5109054101")
  })
  print("Success!")
}

test_10 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_10), "9bf3774d8676f70583b66a822bdbdb88")
  })
  print("Success!")
}

test_11 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_11), "db4b68351294e891168a5ecfc5f3009d")
  })
  print("Success!")
}

test_12 <- function() {
  ans <- digest(answer_12)
  case_when(
    ans == "75f1160e72554f4270c809f041c7a776" ~
    "Try again! It is not equally likely that a person is 3 ft. or 6ft. ",
    ans == "3a5505c06543876fe45598b5e5e5195d" ~
    "Try again! It is not equally likely that a person is making $25,000 or $100,000",
    ans == "475bf9280aab63a82af60791302736f6" ~
    "Yes, this is correct! Approximately speaking, it is equally likely that someone is born on any given day of the year.",
    TRUE ~ "That is an invalid input. Recheck the formatting")
}

test_13 <- function() {
  ans <- digest(answer_13)
  case_when(
    ans == "75f1160e72554f4270c809f041c7a776" ~
    "Yes, this is correct! The height of students would be much more frequent around an average height (say 170 cm.) and then progressively less frequent on both ends.",
    ans == "3a5505c06543876fe45598b5e5e5195d" ~
    "Try again! Wages are bounded by the minimum wage and are typically right-skewed.",
    ans == "475bf9280aab63a82af60791302736f6" ~
    "Try again! It is equally likely that an individual is born on any of the days in a year.",
    TRUE ~ "That is an invalid input. Recheck the formatting")
}

test_14 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_14), "88d2821508a6625b093500c3e4d8d684")
  })
  print("Success!")
}

test_15 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_15), "03b3b7e9967823ca8ae75c138a3aa39c")
  })
  print("Success!")
}

test_16 <- function() {
  ans <- digest(answer_16)
  case_when(
    ans == "75f1160e72554f4270c809f041c7a776" ~
    "Yes, this is correct! ",
    ans == "3a5505c06543876fe45598b5e5e5195d" ~
    "Try again! This distribution approaches a normal distribution in large samples.",
    ans == "475bf9280aab63a82af60791302736f6" ~
    "Try again!",
    TRUE ~ "That is an invalid input. Recheck the formatting")
}