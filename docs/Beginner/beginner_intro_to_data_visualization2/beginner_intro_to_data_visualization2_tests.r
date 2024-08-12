library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "e3d67b6765997232a679dc5f1c65db58")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "b5a42577f646ae755d221ec99f16959b")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "76d6d92ec3ecdb4830a1739ec76c4e1a")
  })
  print("Success!")
}