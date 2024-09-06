library(testthat)
library(digest)

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg1$coefficients["(Intercept)"], 3)), "d21651ff4f1a03ee33f1e248fd1d497d")
  })
  print("Success!")
}


test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg2$coefficients["(Intercept)"], 3)), "a01f754ae07d1f545d7fa1e635f0d397")
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg3$coefficients["(Intercept)"], 3)), "04aedefae1f702c50c8bfa71eea389d7")
  })
  print("Success!")
}

test_11 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(SFS_data$resid[1], 3)), "221d88b706b11efb9707f3d239684288")
  })
  print("Success!")
}

test_12 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(WT$coefficients["Intercept"], 2)), "c35d2d8c7cfc23a36558abc01d9ed53a")
  })
  print("Success!")
}
