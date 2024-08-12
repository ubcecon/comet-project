print("You did it! Way to go!")

library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "a7fc3a942e87b2330addd44ed9b1fdf5")
  })
  print("Success!")
}


test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "dbc09cba9fe2583fb01d63c70e1555a8")
  })
  print("Success!")
}


test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "c2b1646c9d0d86b49379e998e940e772")
  })
  print("Success!")
}


test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4), "127a2ec00989b9f7faf671ed470be7f8")
  })
  print("Success!")
}
