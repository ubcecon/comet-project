library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "c1d8cc6293c3b3aac106136380fb6692")
  })
  print("Success!")
}


test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "493dcce701d8b08fdd7f1de99e091d8f")
  })
  print("Success!")
}


test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "9cff45aea79578643ef29bf8e0492192")
  })
  print("Success!")
}



test_4 <- function(answer_4) {
    if (answer_4 == "A" | answer_4 == "B" | answer_4 == "C" | answer_4 == "D")
        case_when(answer_4 == "A" ~ "Remember the standard error is the standard deviation of the sampling distribution, not the population!"
                ,answer_4 == "B" ~ "This is correct. The sampling distribution shows the frequency of the various sample estimates that we calculated from each of our samples that we drew. The standard error shows the average variation in these. In practice, the standard error is used to indicate how likely it is that our sample estimate lies close to our population parameter. Obviously, we want to aim for low standard errors so that this likelihood increases."
                ,answer_4 == "C" ~ "No our sample estimate cant be exactly correct. The standard error gives a hint as to how likely it is that our sample estimate is close to our population parameter. This is not the same as probaility."
                ,answer_4 == "D" ~ "Try again")
    else 
        print("That is an invalid input. Recheck the formatting")
    }



test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_5), "23c38ac5843c8733a51d610daff60bb1")
  })
  print("Success!")
}
