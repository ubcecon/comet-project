library(testthat)
library(digest)
library(tidyverse)

test_1 <- function(){
    test_that("Solution is incorrect", {
        expect_equal(digest(answer_1), "63702b9352d8b5ee18462d2b81d086d5")
    })
    print("Success!")
}

test_2 <- function(){
    test_that("Solution is incorrect", {
        expect_equal(digest(answer_2), "de9f545774cc92a7c55ccd2acf6420e3")
    })
    print("Success!")
}

test_3 <- function(){
    ans <- digest(answer_3)
    case_when(
        ans == "75f1160e72554f4270c809f041c7a776" ~
        "Incorrect",
        ans == "3a5505c06543876fe45598b5e5e5195d" ~
        "Incorrect",
        ans == "475bf9280aab63a82af60791302736f6" ~
        "No, this is the function to create a simple feature column.",
        ans == "c1f86f7430df7ddb256980ea6a3b57a4" ~
        "Correct!",
        TRUE ~ "Review format of your answer"
    )
}

test_4 <- function(){
    ans <- digest(answer_4)
    case_when(
        ans == "75f1160e72554f4270c809f041c7a776" ~
        "Almost there! Remember which of long/lat index the veritically/horizontally?",
        ans == "3a5505c06543876fe45598b5e5e5195d" ~
        "Correct!",
        ans == "475bf9280aab63a82af60791302736f6" ~
        "No, geographic CRS use spherical coordinates.",
        ans == "c1f86f7430df7ddb256980ea6a3b57a4" ~
        "No, geographic CRS use spherical coordinates.",
        TRUE ~ "Review format of your answer"

    )
}
