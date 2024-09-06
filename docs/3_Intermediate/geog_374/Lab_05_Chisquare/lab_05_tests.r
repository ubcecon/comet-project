library(testthat)
library(digest)
library(dplyr)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "dd83be2a453a3f1e5bec256bdcc196bf")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "35a2b8437075729fa481ee652eacdae3")
  })
  print("Success!")
}

test_5 <- function() {
    answer_5 <- case_when(
        digest(answer_5) == "75f1160e72554f4270c809f041c7a776" ~ list(0, ""),
        digest(answer_5) == "127a2ec00989b9f7faf671ed470be7f8" ~ list(0, ""),
        digest(answer_5) == "ddf100612805359cd81fdc5ce3b9fbba" ~ list(0, "Hint: remember that the hypotheses differ between         the different chi-squared tests."),
        digest(answer_5) == "3a5505c06543876fe45598b5e5e5195d" ~ list(0, "Hint: remember that the hypotheses differ between         the different chi-squared tests."),
        digest(answer_5) == "475bf9280aab63a82af60791302736f6" ~ list(0, "Hint: remember that the hypotheses differ between         the different chi-squared tests."),
        digest(answer_5) == "6e7a8c1c098e8817e3df3fd1b21149d1" ~ list(0, "Hint: remember that the hypotheses differ between         the different chi-squared tests."),
        digest(answer_5) == "c1f86f7430df7ddb256980ea6a3b57a4" ~ list(1, ""),
        digest(answer_5) == "d110f00cfb1b248e835137025804a23b" ~ list(1, ""),
        TRUE ~ list(0, ""),
        )   
  test_that(paste("Solution is incorrect.", answer_5[2]),{
    expect_equal(answer_5[[1]], 1)
  })
  print("Success!")            
}

test_7 <- function() {  
    answer_7 <- case_when(
        digest(answer_7) == "811ba9368a7aa0fa41a17a68fba38f26" ~ 1,
        digest(answer_7) == "cc173ba78af0ca72e5288ce52641c243" ~ 1,
        digest(answer_7) == "0590b0427c1b19a6eb612d19888aa52f" ~ 1,
        digest(answer_7) == "863dfc36ab2bfe97404cc8fc074a5241" ~ 0,
        digest(answer_7) == "9fab63dd799a02c2bda25674a77ebdb8" ~ 0,
        digest(answer_7) == "1cd2d589088ce1df6ebdc5d9279a1f68" ~ 0,
        TRUE ~ 0
        )
  test_that("Solution is incorrect.", {
     expect_equal(answer_7, 1)
  })
  print("Success!")                 
}

test_10 <- function() { 
    answer_10 <- case_when(
        digest(answer_10) == "75f1160e72554f4270c809f041c7a776" ~ list(0, ""),
        digest(answer_10) == "127a2ec00989b9f7faf671ed470be7f8" ~ list(0, ""),
        digest(answer_10) == "ddf100612805359cd81fdc5ce3b9fbba" ~ list(0, ""),
        digest(answer_10) == "3a5505c06543876fe45598b5e5e5195d" ~ list(0, ""),
        digest(answer_10) == "475bf9280aab63a82af60791302736f6" ~ list(1, ""),
        digest(answer_10) == "6e7a8c1c098e8817e3df3fd1b21149d1" ~ list(1, ""),
        digest(answer_10) == "c1f86f7430df7ddb256980ea6a3b57a4" ~ list(0, ""),
        digest(answer_10) == "d110f00cfb1b248e835137025804a23b" ~ list(0, ""),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_10[2]),{
    expect_equal(answer_10[[1]], 1)
  })
  print("Success!")            
}

test_11 <- function() {  
    answer_11 <- case_when(
        digest(answer_11) == "811ba9368a7aa0fa41a17a68fba38f26" ~ 1,
        digest(answer_11) == "cc173ba78af0ca72e5288ce52641c243" ~ 1,
        digest(answer_11) == "0590b0427c1b19a6eb612d19888aa52f" ~ 1,
        digest(answer_11) == "863dfc36ab2bfe97404cc8fc074a5241" ~ 0,
        digest(answer_11) == "9fab63dd799a02c2bda25674a77ebdb8" ~ 0,
        digest(answer_11) == "1cd2d589088ce1df6ebdc5d9279a1f68" ~ 0,
        TRUE ~ 0
        )
  test_that("Solution is incorrect.", {
     expect_equal(answer_11, 1)
  })
  print("Success!")                 
}

test_13 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_13), "8fb622eba78dbcac3346b852b0cb5818")
  })
  print("Success!")
}