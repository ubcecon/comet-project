library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "7b83bd517162650f93329c224fa14e08")
  })
  print("Success!")
}

test_2 <- function(){
  ans <- digest(answer_2)
  case_when(
    ans == '75f1160e72554f4270c809f041c7a776' ~
    "Incorrect! We're missing the price level.",
     ans == '3a5505c06543876fe45598b5e5e5195d' ~
    "Success!",
     ans == '475bf9280aab63a82af60791302736f6' ~
    "Incorrect! We're missing the price level.",
     ans == 'c1f86f7430df7ddb256980ea6a3b57a4' ~
    "Incorrect! We're missing the price level and the population.",
    TRUE ~ "Review formatting"
  )
}

test_3 <- function(){
  ans <- digest(answer_3)
  case_when(
    ans == '75f1160e72554f4270c809f041c7a776' ~
    "Success!",
     ans == '3a5505c06543876fe45598b5e5e5195d' ~
    "Incorrect! We're looking at a cross-section of the data.",
     ans == '475bf9280aab63a82af60791302736f6' ~
    "Incorrect!",
     ans == 'c1f86f7430df7ddb256980ea6a3b57a4' ~
    "Incorrect! We're looking at the relationship between 2 continuous variables.",
    TRUE ~ "Review formatting"
  )
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4), "13d2424c1d75ec1b7e89ab318894a181")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_5), "d320af82dc12d3c7875b1efe19ad8d0c")
  })
  print("Success!")
}