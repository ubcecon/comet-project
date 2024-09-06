library(testthat)
library(digest)
library(dplyr)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "7a78eddb66ba4d20a981b6d1d1b03699")
  })
  print("Success!")
}

test_2 <- function() {  
    answer_2 <- case_when(
        digest(answer_2) == "dbc09cba9fe2583fb01d63c70e1555a8" ~  list(1, ""),
        digest(answer_2) == "e5b57f323c7b3719bbaaf9f96b260d39" ~  list(0, "Hint: should OTHER be included as a category?"),
        TRUE ~ list(0, "")
        )
  test_that(paste("Solution is incorrect.", answer_2[2]),{
    expect_equal(answer_2[[1]], 1)
  })
  print("Success!")             
}       

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "0db1ebedeea22e3b2e8972367ab055b3")
  })
  print("Success!")
}

test_4.1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4.1), "da85254ab3eaf89d3289d9563bc60479")
  })
  print("Success!")
}

test_4.2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4.2), "59fc24511c91c6fbd3f600dad0cba238")
  })
  print("Success!")
}

test_5 <- function() {
    
    answer_5 <- case_when(
        digest(answer_5) == "811ba9368a7aa0fa41a17a68fba38f26" ~ 1,
        digest(answer_5) == "cc173ba78af0ca72e5288ce52641c243" ~ 1,
        digest(answer_5) == "0590b0427c1b19a6eb612d19888aa52f" ~ 1,
        digest(answer_5) == "863dfc36ab2bfe97404cc8fc074a5241" ~ 0,
        digest(answer_5) == "9fab63dd799a02c2bda25674a77ebdb8" ~ 0,
        digest(answer_5) == "1cd2d589088ce1df6ebdc5d9279a1f68" ~ 0,
        TRUE ~ 0
        ) 
  test_that("Solution is incorrect.", {
    expect_equal(answer_5, 1)
  })
  print("Success!")                  
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_6), "628956fe0b6e3204477d7c91a51551a0")
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_7), "d294a67aaf7bfcfc8313ff2f6afd2c1f")
  })
  print("Success!")
}

test_8 <- function() { 
    answer_8 <- case_when(
        digest(answer_8) == "811ba9368a7aa0fa41a17a68fba38f26" ~ 1,
        digest(answer_8) == "cc173ba78af0ca72e5288ce52641c243" ~ 1,
        digest(answer_8) == "0590b0427c1b19a6eb612d19888aa52f" ~ 1,
        digest(answer_8) == "863dfc36ab2bfe97404cc8fc074a5241" ~ 0,
        digest(answer_8) == "9fab63dd799a02c2bda25674a77ebdb8" ~ 0,
        digest(answer_8) == "1cd2d589088ce1df6ebdc5d9279a1f68" ~ 0,
        TRUE ~ 0
        )  
  test_that("Solution is incorrect. Review the required assumptions and consider which of them could be informed by the above plot.",{
    expect_equal(answer_8, 1)
  })
  print("Success!")                
}
    
test_9 <- function() {
    answer_9 <- case_when(
        digest(answer_9) == "75f1160e72554f4270c809f041c7a776" ~ list(0, ""),
        digest(answer_9) == "127a2ec00989b9f7faf671ed470be7f8" ~ list(0, ""),
        digest(answer_9) == "ddf100612805359cd81fdc5ce3b9fbba" ~ list(0, ""),
        digest(answer_9) == "3a5505c06543876fe45598b5e5e5195d" ~ list(0, ""),
        digest(answer_9) == "475bf9280aab63a82af60791302736f6" ~ list(1, ""),
        digest(answer_9) == "6e7a8c1c098e8817e3df3fd1b21149d1" ~ list(1, ""),
        digest(answer_9) == "c1f86f7430df7ddb256980ea6a3b57a4" ~ list(0, ""),
        digest(answer_9) == "d110f00cfb1b248e835137025804a23b" ~ list(0, ""),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_9[2]),{
    expect_equal(answer_9[[1]], 1)
  })
  print("Success!")             
}

test_10.1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_10.1), "db643dfa06a42412d2dce382464a83ac")
  })
  print("Success!")
}

test_10.2 <- function() { 
    answer_10.2 <- case_when(
        digest(answer_10.2) == "811ba9368a7aa0fa41a17a68fba38f26" ~ 1,
        digest(answer_10.2) == "cc173ba78af0ca72e5288ce52641c243" ~ 1,
        digest(answer_10.2) == "0590b0427c1b19a6eb612d19888aa52f" ~ 1,
        digest(answer_10.2) == "863dfc36ab2bfe97404cc8fc074a5241" ~ 0,
        digest(answer_10.2) == "9fab63dd799a02c2bda25674a77ebdb8" ~ 0,
        digest(answer_10.2) == "1cd2d589088ce1df6ebdc5d9279a1f68" ~ 0,
        TRUE ~ 0
        )
  test_that("Solution is incorrect.", {
     expect_equal(answer_10.2, 1)
  })
  print("Success!")                 
}

test_11 <- function() {
  test_that("Solution is incorrect. Hint: consider what the p adj means for this test.", {
    expect_equal(digest(answer_11), "db8e490a925a60e62212cefc7674ca02")
  })
  print("Success!")
}

test_12.1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_12.1), "75dc8b7b8724a54d1fba4cc109438cfb")
  })
  print("Success!")
}

test_12.2 <- function() {
  answer_12.2 <- case_when(
        digest(answer_12.2) == "811ba9368a7aa0fa41a17a68fba38f26" ~ 0,
        digest(answer_12.2) == "cc173ba78af0ca72e5288ce52641c243" ~ 0,
        digest(answer_12.2) == "0590b0427c1b19a6eb612d19888aa52f" ~ 0,
        digest(answer_12.2) == "863dfc36ab2bfe97404cc8fc074a5241" ~ 1,
        digest(answer_12.2) == "9fab63dd799a02c2bda25674a77ebdb8" ~ 1,
        digest(answer_12.2) == "1cd2d589088ce1df6ebdc5d9279a1f68" ~ 1,
        TRUE ~ 0
        )
  test_that("The solution is incorrect",{
    expect_equal(answer_12.2, 1)
  })
  print("Success!")                  
}