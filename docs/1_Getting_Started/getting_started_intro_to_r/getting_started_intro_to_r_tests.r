library(testthat)
library(digest)
library(tidyverse)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "709a7faa72cb6f3be79d683e234ccb25")
  })
  print("Success!")
}

test_2 <- function() {
    ans <- digest(answer_2)
    case_when(ans == "30ec4b133d2340c7555025de3207ab21" ~
               "Success!",
              ans == "dc70ed122666221cf418092423296ad1" ~
               "Incorrect! Make sure your answer is in lowercase.",
              ans == "3330c129d7b675cefa19f7eea0cb5373" ~
               "Incorrect! Make sure your answer is in lowercase.",
              TRUE ~ "Incorrect! Review your answer and try again.")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "fb75c0c3a4defdaffc11c03b912031b5")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4), "00e0cc22149839070c38fa9ac828826e")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_5), "daf9cffb26442e0f5253570444bb6f87")
  })
  print("Success!")
}

test_6 <- function() {
    ans <- digest(answer_6)
    case_when(ans == "9239c01218000016670da90e840448be" ~
               "Success!",
              ans == "228116ed07796c9a3ec47f98e543d82b" ~
               "Not quite! We want what is stored in the second object, not the object itself.",
              TRUE ~ "Incorrect! Review your answer and try again.")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_7), "af9e5c24af013c970922362b8850b060")
  })
  print("Success!")
}

test_8 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_8), "6f6167522f32e131f999273c788e4930")
  })
  print("Success!")
}

test_9 <- function() {
    ans <- digest(answer_9)
    case_when(ans == "88d2821508a6625b093500c3e4d8d684" ~
               "Success!",
              ans == "7b7f5dcec7ebe086fa5140a717aab222" ~
               "Not quite! Remember to add parenthesis to indicate the order of the operations.",
              TRUE ~ "Incorrect! Review your answer and try again.")
}

test_10 <- function() {
    ans <- digest(answer_10)
    case_when(ans == "dfaa6288fea053164975b4db847aa059" ~
               "Success!",
              ans == "90f9b1492c568ffbdf646ecc7a931132" ~
               "Not quite! The order of the elements of the vector needs to be v1, v2, v3.",
              ans == "1956070e59dcb224faee8589432a4506" ~
               "Not quite! The order of the elements of the vector needs to be v1, v2, v3.",
              ans == "6a7ee562edf6b5894abb55dd06a292b3" ~
               "Not quite! The order of the elements of the vector needs to be v1, v2, v3.",
              ans == "2060047c17f8d796e3e55f8337c15b3b" ~
               "Not quite! The order of the elements of the vector needs to be v1, v2, v3.",
              ans == "aa0d59b2e128cda135e6ab55fb7c71b8" ~
               "Not quite! The order of the elements of the vector needs to be v1, v2, v3.",
              TRUE ~ "Incorrect! Review your answer and try again.")
}

test_11 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_11), "fef8ee3e709a216af937131dec2cff3a")
  })
  print("Success!")
}

test_12 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_12), "53e26a46ad7948eb2ae453b79d572589")
  })
  print("Success!")
}

test_13 <- function() {
    ans <- digest(answer_13)
    case_when(ans == "03fd1d869a1a7fb2c286fa4568b080fe" ~
               "Success!",
              ans == "0b1837623d42bb5f94c21355d552e5dc" ~
               "Not quite! You provided the State name stored in State, not the value of Murder corresponding to the row of Arizona.",
               ans == "de065e54564148e66a8937997d5b848b" ~
               "Almost there! You provided the entire column Murder; we just need the value of Murder for Arizona (third element).",
              TRUE ~ "Incorrect! Review your answer and try again.")
}

test_14 <- function() {
  test_that("Solution is incorrect, try again!", {
    expect_equal(digest(answer_14), "4d44d9f5b98947cb393336056ffb473b")
  })
  print("Success!")
}

test_15 <- function() {
    ans <- digest(answer_15)
    case_when(ans == "3da398a2eeb1d472aba89c9db1deeae9" ~
               "Success!",
              ans == "76e19b9c2639a5a0be0fbe3e7dbf1e80" ~
               "Not quite! The order of the elements of the vector needs to be murder, assault, rape.",
              ans == "c65290b90c91d88549cffb0e1e2075e9" ~
               "Not quite! The order of the elements of the vector needs to be murder, assault, rape.",
              ans == "222f42f1816f14adfda882320473c30d" ~
               "Not quite! The order of the elements of the vector needs to be murder, assault, rape.",
              ans == "4e6fcc40d1603366ec78ec3653ce643e" ~
               "Not quite! The order of the elements of the vector needs to be murder, assault, rape.",
              ans == "dca23948d81ffa952c302e01884f69a3" ~
               "Not quite! The order of the elements of the vector needs to be murder, assault, rape.",
              TRUE ~ "Incorrect! Review your answer and try again.")
}
