library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "1f20be7eca4672eff97b1528583c4865")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "b5a42577f646ae755d221ec99f16959b")
  })
  print("Success!")
}

test_3 <- function(){
  ans <- digest(answer_3)
  case_when(
    ans %in% c("a4bd702e0470547106e88677f805f64f", 
    "964fe8d5ca4794c660c56d271ba3e5c2") ~
    'Correct!',
    ans == "33739d0c70216a23c9056a8023b60829" ~
    'Incorrect, our second variable needs to be GDP per capita.',
    TRUE ~ "Solution is incorrect, review your answer and try again.")
}

test_4 <- function(){
  test_that("Solution is incorrect", {
    expect_equal(digest(gsub("[[:space:]]", "", answer_4)), "f5b0990c62216a4e9741c1206c1d6d61")
  })
  print("Success!")
}

test_5 <- function(){
  ans <- digest(answer_5)
  case_when(
    ans == "01a75cb73d67b0f895ff0e61449c7bf8" ~
    'Correct!',
    ans %in% c("75f1160e72554f4270c809f041c7a776",
    "3a5505c06543876fe45598b5e5e5195d",
    "475bf9280aab63a82af60791302736f6",
    "c1f86f7430df7ddb256980ea6a3b57a4",
    "f76b651ab8fcb8d470f79550bf2af53a") ~
    'Incorrect, review the confidence bands section of the notebook and try again.',
    TRUE ~ "Review formatting")
}
