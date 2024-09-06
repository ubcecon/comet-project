library(testthat)
library(digest)
library(ggplot2)
library(dplyr)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "b95e0a840a3b3cc120b623dd768ce5b0")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "ff89cbcee32a4a47b69170dd367afdca")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "afa9f28cf9785ae6b0f7e35880f80cb5")
  })
  print("Success!")
}

test_4 <- function() { 
    answer_4 <- case_when(
        digest(answer_4) == "75f1160e72554f4270c809f041c7a776" ~ list(0, ""),
        digest(answer_4) == "127a2ec00989b9f7faf671ed470be7f8" ~ list(0, ""),
        digest(answer_4) == "ddf100612805359cd81fdc5ce3b9fbba" ~ list(1, ""),
        digest(answer_4) == "3a5505c06543876fe45598b5e5e5195d" ~ list(1, ""),
        digest(answer_4) == "475bf9280aab63a82af60791302736f6" ~ list(0, ""),
        digest(answer_4) == "6e7a8c1c098e8817e3df3fd1b21149d1" ~ list(0, ""),
        digest(answer_4) == "c1f86f7430df7ddb256980ea6a3b57a4" ~ list(0, ""),
        digest(answer_4) == "d110f00cfb1b248e835137025804a23b" ~ list(0, ""),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_4[2]),{
    expect_equal(answer_4[[1]], 1)
  })
  print("Success!")            
}

test_5 <- function() {
    answer_5 <- case_when(
        digest(answer_5) == "811ba9368a7aa0fa41a17a68fba38f26" ~ 0,
        digest(answer_5) == "cc173ba78af0ca72e5288ce52641c243" ~ 0,
        digest(answer_5) == "0590b0427c1b19a6eb612d19888aa52f" ~ 0,
        digest(answer_5) == "863dfc36ab2bfe97404cc8fc074a5241" ~ 1,
        digest(answer_5) == "9fab63dd799a02c2bda25674a77ebdb8" ~ 1,
        digest(answer_5) == "1cd2d589088ce1df6ebdc5d9279a1f68" ~ 1,
        TRUE ~ 0
        )
  test_that("Solution is incorrect.", {
     expect_equal(answer_5, 1)
  })
  print("Success!")                 
}

test_6 <- function() {
    answer_6 <- case_when(
        digest(answer_6) == "811ba9368a7aa0fa41a17a68fba38f26" ~ 0,
        digest(answer_6) == "cc173ba78af0ca72e5288ce52641c243" ~ 0,
        digest(answer_6) == "0590b0427c1b19a6eb612d19888aa52f" ~ 0,
        digest(answer_6) == "863dfc36ab2bfe97404cc8fc074a5241" ~ 1,
        digest(answer_6) == "9fab63dd799a02c2bda25674a77ebdb8" ~ 1,
        digest(answer_6) == "1cd2d589088ce1df6ebdc5d9279a1f68" ~ 1,
        TRUE ~ 0
        )
  test_that("Solution is incorrect.", {
     expect_equal(answer_6, 1)
  })
  print("Success!")                 
}

test_7 <- function() {
    answer_7 <- case_when(
        digest(answer_7) == "811ba9368a7aa0fa41a17a68fba38f26" ~ 0,
        digest(answer_7) == "cc173ba78af0ca72e5288ce52641c243" ~ 0,
        digest(answer_7) == "0590b0427c1b19a6eb612d19888aa52f" ~ 0,
        digest(answer_7) == "863dfc36ab2bfe97404cc8fc074a5241" ~ 1,
        digest(answer_7) == "9fab63dd799a02c2bda25674a77ebdb8" ~ 1,
        digest(answer_7) == "1cd2d589088ce1df6ebdc5d9279a1f68" ~ 1,
        TRUE ~ 0
        )
  test_that("Solution is incorrect.", {
     expect_equal(answer_7, 1)
  })
  print("Success!")                 
}

test_9 <- function() { 
    answer_9 <- case_when(
        digest(answer_9) == "75f1160e72554f4270c809f041c7a776" ~ list(0, ""),
        digest(answer_9) == "127a2ec00989b9f7faf671ed470be7f8" ~ list(0, ""),
        digest(answer_9) == "ddf100612805359cd81fdc5ce3b9fbba" ~ list(1, ""),
        digest(answer_9) == "3a5505c06543876fe45598b5e5e5195d" ~ list(1, ""),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_9[2]),{
    expect_equal(answer_9[[1]], 1)
  })
  print("Success!")            
}

test_10 <- function() { 
    answer_10 <- case_when(
        digest(answer_10) == "75f1160e72554f4270c809f041c7a776" ~ list(0, ""),
        digest(answer_10) == "127a2ec00989b9f7faf671ed470be7f8" ~ list(0, ""),
        digest(answer_10) == "ddf100612805359cd81fdc5ce3b9fbba" ~ list(0, ""),
        digest(answer_10) == "3a5505c06543876fe45598b5e5e5195d" ~ list(0, ""),
        digest(answer_10) == "475bf9280aab63a82af60791302736f6" ~ list(0, ""),
        digest(answer_10) == "6e7a8c1c098e8817e3df3fd1b21149d1" ~ list(0, ""),
        digest(answer_10) == "c1f86f7430df7ddb256980ea6a3b57a4" ~ list(1, ""),
        digest(answer_10) == "d110f00cfb1b248e835137025804a23b" ~ list(1, ""),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_10[2]),{
    expect_equal(answer_10[[1]], 1)
  })
  print("Success!")            
}

test_11 <- function() { 
    answer_11 <- case_when(
        digest(answer_11) == "75f1160e72554f4270c809f041c7a776" ~ list(0, ""),
        digest(answer_11) == "127a2ec00989b9f7faf671ed470be7f8" ~ list(0, ""),
        digest(answer_11) == "ddf100612805359cd81fdc5ce3b9fbba" ~ list(0, ""),
        digest(answer_11) == "3a5505c06543876fe45598b5e5e5195d" ~ list(0, ""),
        digest(answer_11) == "475bf9280aab63a82af60791302736f6" ~ list(1, ""),
        digest(answer_11) == "6e7a8c1c098e8817e3df3fd1b21149d1" ~ list(1, ""),
        digest(answer_11) == "c1f86f7430df7ddb256980ea6a3b57a4" ~ list(0, ""),
        digest(answer_11) == "d110f00cfb1b248e835137025804a23b" ~ list(0, ""),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_11[2]),{
    expect_equal(answer_11[[1]], 1)
  })
  print("Success!")            
}

test_12 <- function() {
    answer_12 <- case_when(
        digest(answer_12) == "811ba9368a7aa0fa41a17a68fba38f26" ~ 0,
        digest(answer_12) == "cc173ba78af0ca72e5288ce52641c243" ~ 0,
        digest(answer_12) == "0590b0427c1b19a6eb612d19888aa52f" ~ 0,
        digest(answer_12) == "863dfc36ab2bfe97404cc8fc074a5241" ~ 1,
        digest(answer_12) == "9fab63dd799a02c2bda25674a77ebdb8" ~ 1,
        digest(answer_12) == "1cd2d589088ce1df6ebdc5d9279a1f68" ~ 1,
        TRUE ~ 0
        )
  test_that("Solution is incorrect.", {
     expect_equal(answer_12, 1)
  })
  print("Success!")                 
}
