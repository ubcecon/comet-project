library(testthat)
library(digest)

test_1.1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1.1), "3a5505c06543876fe45598b5e5e5195d")
  })
  print("Success!")
}

test_1.2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1.2), "941ce65c7d60753f1d8b410bb759d710")
  })
  print("Success!")
}