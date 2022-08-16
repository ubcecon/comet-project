library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "7b83bd517162650f93329c224fa14e08")
  })
  print("Success!")
}