library(testthat)
library(digest)

test_1 <- function(){
    test_that("Solution is incorrect", {
        expect_equal(digest(answer_1), "a420f759ea489b49af44ee7c32e12918")
    })
    print("Success!")
}

test_2 <- function(){
    test_that("Solution is incorrect", {
        expect_equal(digest(answer_2), "7895c416419db3a37c859f11077bf52d")
    })
    print("Success!")
}

test_3 <- function(){
    test_that("Solution is incorrect", {
        expect_equal(digest(answer_3), "c96fee598641122a0732f0d1a62212a3")
    })
    print("Success!")
}

test_4 <- function(){
    test_that("Solution is incorrect", {
        expect_equal(digest(answer_4), "92a8d8b9c8b468aa3fa0cb7823bd84ff")
    })
    print("Success!")
}

test_5 <- function(){
    test_that("Solution is incorrect", {
        expect_equal(digest(answer_5), "48b51c5b63c000fdba363980bcc3f900")
    })
    print("Success!")
}

test_6 <- function(){
    test_that("Solution is incorrect", {
        expect_equal(digest(answer_6), "75f1160e72554f4270c809f041c7a776")
    })
    print("Success!")
}
