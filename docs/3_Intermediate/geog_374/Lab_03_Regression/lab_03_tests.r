library(testthat)
library(digest)
library(dplyr)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "17a201f80ef144d9c8fa5ec961d382af")
  })
  print("Success!")
}

test_2 <- function() {
    answer_2 <- case_when(
        digest(answer_2) == "75f1160e72554f4270c809f041c7a776" ~ list(0, ""),
        digest(answer_2) == "127a2ec00989b9f7faf671ed470be7f8" ~ list(0, ""),
        digest(answer_2) == "ddf100612805359cd81fdc5ce3b9fbba" ~ list(1, ""),
        digest(answer_2) == "3a5505c06543876fe45598b5e5e5195d" ~ list(1, ""),
        digest(answer_2) == "475bf9280aab63a82af60791302736f6" ~ list(0, ""),
        digest(answer_2) == "6e7a8c1c098e8817e3df3fd1b21149d1" ~ list(0, ""),
        digest(answer_2) == "c1f86f7430df7ddb256980ea6a3b57a4" ~ list(0, ""),
        digest(answer_2) == "d110f00cfb1b248e835137025804a23b" ~ list(0, ""),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_2[2]),{
    expect_equal(answer_2[[1]], 1)
  })
  print("Success!")            
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "a23be9b590f396a4181738a1bbf92fa6")
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

test_6 <- function() {
   answer_6 <- case_when(
       digest(answer_6) == "e0295f090469c6a5da77b1ed43ffcb76" ~ list(0,"Hint: recall the relationship between r-squared and the correlation value you have just computed."),
       digest(answer_6) == "6ef59f8b12efd87fd6432ca937e3a206" ~ list(1,""),
      TRUE ~ list(0, "")
       )
  test_that(paste("Solution is incorrect.", answer_6[2]),{
    expect_equal(answer_6[[1]], 1)
  })
  print("Success!")            
}     

test_10 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_10), "d7c8493c8c376162106bd3bd85b2ee58")
  })
  print("Success!")
}