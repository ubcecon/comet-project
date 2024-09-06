library(testthat)
library(digest)
library(tidyverse)
library(haven)

test_1 <- function() {
    ans <- digest(answer_1)
    case_when(ans == "03e6f9fdf05162a425d39f81512124a2" ~
               "Success!",
               ans == "415cd2683ca7654bb55c69404e9e5854" ~
               "Success",
               TRUE ~ "Incorrect! Review your answer and try again.")
  }

test_2 <- function(){
  test_that("Solution is incorrect", {
  expect_equal(digest(answer_2), "2b6f4912f28a4340ed1aba4780cf3dbc")
})
  print('Success!')
}

test_3 <- function(){
  ans <- digest(answer_3)
  case_when(ans == "75f1160e72554f4270c809f041c7a776" ~
             "Almost there! Remember that mutate is the function to create a new variable, we're asking for the function that takes `grades` and 2 as inputs.",
             ans == "3a5505c06543876fe45598b5e5e5195d" ~
             "Not quite! Review the previous section and try again.",
             ans == "475bf9280aab63a82af60791302736f6" ~
             "Not quite! Review the previous section and try again.",
             ans == 'c1f86f7430df7ddb256980ea6a3b57a4' ~
             "Not quite! Review the previous section and try again.",
             ans == '01a75cb73d67b0f895ff0e61449c7bf8' ~
             "Success!",
             ans == 'f76b651ab8fcb8d470f79550bf2af53a' ~
             "Not quite! Review the previous section and try again.",
             TRUE ~ "Review the formatting of your answer.")
}

test_4 <- function() {
    ans <- digest(answer_4)
    case_when(ans == "317be6c8c60403220d0f4f9fa663a873" ~
               "Success!",
               TRUE ~ "Incorrect! Review your answer and try again.")
}

test_5 <- function() {
    ans <- digest(sum(as.numeric(answer_5$knows_english)))
    case_when(ans == "8a0f03861e6878a0abeefceef7d3a551" ~
               "Success!",
               TRUE ~ "Incorrect! Review your answer and try again.")
}