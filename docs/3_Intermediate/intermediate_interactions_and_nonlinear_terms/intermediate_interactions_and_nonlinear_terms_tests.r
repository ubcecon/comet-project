library(testthat)
library(digest)

test_2 <- function() {
  test_that("Solution is incorrect. Hint: remember the interaction variables?", {
    expect_equal(digest(reg0$coefficients["(Intercept)"]), '0f4ee01031ce9a493ad5c369d88f3af6')
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect. Hint: remember the interaction variables?", {
    expect_equal(digest(reg1$coefficients["(Intercept)"]), '72de9f7c67ad1f9929ee35190849c3cb')
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg2$coefficients["(Intercept)"]), 'be19ab73a3fb0ab21f8351d7c012063d')
  })
  print("Success!")
}
test_8 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg3A$coefficients["(Intercept)"]), '5277518dfdc0f4b416ec7cbf60f20916')
  })
  print("Success!")
}

test_10 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg3M$coefficients["(Intercept)"]), '093aa84e9899b2118c20e1c97ee54f0d')
  })
  print("Success!")
}

test_11 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg3F$coefficients["(Intercept)"]), '014fed05ac5840183495e98aed10956f')
  })
  print("Success!")
}

test_13 <- function() {
  test_that("Solution is incorrect. Hint: interactions?", {
    expect_equal(digest(reg4$coefficients["(Intercept)"]), '32dadbc917391ebb4c709dac12807cba')
  })
  print("Success!")
}


