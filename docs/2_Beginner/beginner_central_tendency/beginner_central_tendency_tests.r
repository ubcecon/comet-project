library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round((answer_1),2)), "b71ae1fc63e62eeb285198e5b0fa9732")
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
  ans <- digest(answer_3)
  case_when(
    ans == "3a5505c06543876fe45598b5e5e5195d" ~
    "Correct!",
    ans == "75f1160e72554f4270c809f041c7a776" ~
    "This option is incorrect",
    ans == "475bf9280aab63a82af60791302736f6" ~
    "This option is incorrect",
    TRUE ~ "Review formatting" )
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4), "de6b5150ec99d04586f59b943587775d")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(answer_5,3)), "f7295e631ff9fead7bc632a470be8aa7")
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
