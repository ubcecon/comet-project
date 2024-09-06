library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "48c265e4d3c821bd60faf71f7c2cd201")
  })
  print("Success! Since we don't know the standard deviation of the population and the sample size is small, we need to use the t-distribution.")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(answer_2)), "f968a5690f764ccf4f20e81c5f8972a2")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(answer_3)), "0b7be151f6f816b71bb9e81a7074dde7")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(answer_4,3)), "a69bc5e1f19ba909c48b471bc5c4bb61")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(answer_5,3)), "1953d3eb24659a68fee5238832e649e9")
  })
  print("Success!")
}


test_6 <- function() {
  ans <- digest(answer_6)
  case_when(
    ans == "5e17165d7dd92f0ebda92658595f4f6b" ~
    "Not quite! Remember our second condition for confidence intervals of proportions!",
    ans == "853277e1983a1a23f029c6571ce5abe9" ~
    "Success!",
    TRUE ~ "Solution is incorrect")
}


test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(answer_7)), "5a71a6ff5671a0228b183b6aca435d5a")
  })
  print("Success!")
}

test_8 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(answer_8)), "d68de7e2bc035c7d0d9d361cc5c710e1")
  })
  print("Success!")
}


test_9 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_9), "8366fd21d5e6f366649db967d3b1489b")
  })
  print("Success!")
}

test_10 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_10), "bf4fc9d764bc70e07616102cb04f5f94")
  })
  print("Success!")
}


test_11 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_11), "475bf9280aab63a82af60791302736f6")
  })
  print("Success!")
}
