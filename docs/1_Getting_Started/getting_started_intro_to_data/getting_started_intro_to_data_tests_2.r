library(testthat)
library(digest)
library(tidyverse)
library(haven)

test_1 <- function() {
    ans <- digest(answer_1)
    case_when(ans == "cd39dcca67adc330be5354e8ae699599" ~
               "Success!",
               ans %in% c("f01fe8a1197c7482caead6848bd43813", "e4572ca815eee12b211653fe788487f4", "eb5eb9d6241c4e49f8594a8b90b57032",  "5bffd9306c2e5426df1628792576075a") ~
               "Almost there.",
               TRUE ~ "Incorrect! Review your answer and try again.")
  }


test_2 <- function(){
  test_that("Solution is incorrect", {
  expect_equal(digest(answer_2), "01a75cb73d67b0f895ff0e61449c7bf8")
})
  print('Success! Impossible to tell without context')
}

test_3 <- function(){
  test_that("Solution is incorrect", {
  expect_equal(digest(answer_3), "edcb2fd5e0f2f1a635e405e924254681")
})
  print('Success!')
}

test_4 <- function(){
  test_that("Solution is incorrect", {
  expect_equal(digest(answer_4), "c1f86f7430df7ddb256980ea6a3b57a4")
})
  print('Success!')
}
