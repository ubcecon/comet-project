library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "44b124c438681a6daead20469c4569e4")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "0590b0427c1b19a6eb612d19888aa52f")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "1a0468b92d718453727f0cca97824509")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4), "53f106f0810996a7abcddcb89a0d659d")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_5), "be34be9198b2baebe0ec1ff4811e81a3")
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_6), "4e257636509cd70b313df2efbe98daa6")
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_7), "811e292ea672045e386a53e15cdfbe04")
  })
  print("Success!")
}

test_8 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_8), "195ee15273c197b63b82de2955b11d83")
  })
  print("Success!")
}

test_9 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_9), "97c884a6879f4b25f46b7fb774c49a37")
  })
  print("Success!")
}

test_10 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_10), "195ee15273c197b63b82de2955b11d83")
  })
  print("Success!")
}

test_11 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_11), "071251066305956df7540d2bb4f7c891")
  })
  print("Success!")
}

test_12 <- function(answer_12) {
    if (answer_12 == "A" | answer_12 == "B" | answer_12 == "C") 
        case_when(answer_12 == "A" ~ "Try again! It is not equally likely that a person is 3 ft. or 6ft. "
                ,answer_12 == "B" ~ "Try again! It is not equally likely that a person is making $25,000 or $100,000"
                ,answer_12 == "C" ~ "Yes, this is correct! Approximately speaking, it is equally likely that someone is born on any given day of the year.")
    else 
        print("That is an invalid input. Recheck the formatting")
}

test_13 <- function(answer_13) {
    if (answer_13 == "A" | answer_13 == "B" | answer_13 == "C") 
        case_when(answer_13 == "A" ~ "Yes, this is correct! The height of students would be much more frequent around an average height (say 170 cm.) and then progressively less frequent on both ends."
                ,answer_13 == "B" ~ "Try again! Grades would be more frequent towards the higher marks than the lower marks. Additionally, its not possible to get higher than the maximum mark. The distribution would be heavily skewed to the right."
                ,answer_13 == "C" ~ "Try again! It is equally likely that an individual is born on any of the days in a year.")
    else 
        print("That is an invalid input. Recheck the formatting")
}

test_14 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_14), "88d2821508a6625b093500c3e4d8d684")
  })
  print("Success!")
}

test_15 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_15), "03b3b7e9967823ca8ae75c138a3aa39c")
  })
  print("Success!")
}

test_16 <- function(answer_16) {
    if (answer_16 == "A" | answer_16 == "B" | answer_16 == "C") 
        case_when(answer_16 == "A" ~ "Yes. this is correct! The t-distribution becomes to look very similar to the normal distribution when the degrees of freedom parameter is large."
                ,answer_16 == "B" ~ "Try again!"
                ,answer_16 == "C" ~ "Try again!")
    else 
        print("That is an invalid input. Recheck the formatting")
}





        
                  
                  







