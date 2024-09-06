library(testthat)
library(digest)
library(tidyverse)

test_1 <- function() {
    ans <- digest(answer_1)
    case_when(ans == "75f1160e72554f4270c809f041c7a776" ~
              "That is incorrect. The amount of rainfall likely affects supply without affecting demand.",
              ans == "3a5505c06543876fe45598b5e5e5195d" ~ 
              "Correct!",
              ans == "475bf9280aab63a82af60791302736f6" ~
              "That is incorrect. The amount of GDP per capita likely affects both supply and demand.",
              TRUE ~ "That is an invalid input. Recheck the formatting of your answer.")
}

test_2 <- function() {
    ans <- digest(answer_2)
    case_when(ans == "75f1160e72554f4270c809f041c7a776" ~
              "Correct!",
              ans == "3a5505c06543876fe45598b5e5e5195d" ~ 
              "That is incorrect. Income taxes likely affect demand without affecting supply.",
              ans == "475bf9280aab63a82af60791302736f6" ~
              "That is incorrect. The marginal product of labor likely affects both supply and demand.",
              TRUE ~ "That is an invalid input. Recheck the formatting of your answer.")
}

test_3 <- function() {
    ans <- digest(answer_3)
    case_when(ans == "75f1160e72554f4270c809f041c7a776" ~
              "This is not the correct answer, try again!",
              ans == "3a5505c06543876fe45598b5e5e5195d" ~ 
              "This is not the correct answer, try again!",
              ans == "475bf9280aab63a82af60791302736f6" ~
              "This is not the correct answer, try again!",
              ans == "c1f86f7430df7ddb256980ea6a3b57a4" ~
              "Correct! All of the options describe the issue we're trying to fix.",
              TRUE ~ "That is an invalid input. Recheck the formatting of your answer.")
}

test_4 <- function() {
    ans <- digest(answer_4)
    case_when(ans == "6c9a729ae30ccee9205c4e1f9b58d4a0" ~
              "Almost there, try again!",
              ans == "eb5eb9d6241c4e49f8594a8b90b57032" ~
              "This is not the correct answer. We need an instrument for the price of cigarettes, not the quantity.",
              ans == "e4572ca815eee12b211653fe788487f4" ~
              "Almost there, try again!",
              ans == "d57400f585fce361f54c917698aad75c" ~
              "Correct!",
              TRUE ~ "That is not correct. Hint: the answer string has exactly three letters.")
}

test_5 <- function() {
    ans <- digest(answer_5)
    case_when(ans == "75f1160e72554f4270c809f041c7a776" ~
              "That is incorrect! Remember that our treatment is the price of cigarettes.",
              ans == "3a5505c06543876fe45598b5e5e5195d" ~ 
              "Correct!",
              ans == "475bf9280aab63a82af60791302736f6" ~
              "This is not the correct answer, try again!",
              ans == "c1f86f7430df7ddb256980ea6a3b57a4" ~
              "This is not the correct answer, try again!",
              TRUE ~ "That is an invalid input. Recheck the formatting of your answer.")
}