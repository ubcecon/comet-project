library(testthat)
library(digest)

test_1 <- function() {
    answer_1 <- case_when(
        digest(answer_1) == "05ca18b596514af73f6880309a21b5dd" ~ list(1, "A trial will always have an outcome, so the probabilty of an outcome occuring (any outcome at all!) will be 1"),
        digest(answer_1) == "96c24a598c808db5ff9c1aa505c6aa15" ~ list(1, "A trial will always have an outcome, so the probabilty of an outcome occuring (any outcome at all!) will be 1"),
        digest(answer_1) == "7fdf825afde995ae29c2b06466f5da93" ~ list(1, "A trial will always have an outcome, so the probabilty of an outcome occuring (any outcome at all!) will be 1"),
        digest(answer_1) == "81949aed6f8e18b150efa97ff46a6fc3" ~ list(0, "Think about it what it would mean if the probability is less than or greater than one. Would that imply that the chances that the trial has an outcome (any outcome) is somehow greater than or less than 100%?"),
        digest(answer_1) == "d2a90307aac5ae8d0ef58e2fe730d38b" ~ list(0, "Think about it what it would mean if the probability is less than or greater than one. Would that imply that the chances that the trial has an outcome (any outcome) is somehow greater than or less than 100%?"),
        digest(answer_1) == "5fdfdbc473eddbfb612e2b77189ce77a" ~ list(0, "Think about it what it would mean if the probability is less than or greater than one. Would that imply that the chances that the trial has an outcome (any outcome) is somehow greater than or less than 100%?"),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_1[2]),{
    expect_equal(answer_1[[1]], 1)
  })
  print("Success!")             
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "6a8e65e0821e8011c0f04d886dce9323")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect. Hint: what outcomes are in the event 'catching a salmon you can take home'?", {
    expect_equal(digest(answer_3), "3c3b9d75cc0e8cfcc29f40abd17afe8a")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect. Hint: what outcomes are in the event 'catching a salmon you cannot take home'?", {
    expect_equal(digest(answer_4), "af04a6f39588915a4dcac626c46434de")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_5), "6a8e65e0821e8011c0f04d886dce9323")
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect. Hint: are these events mutually exclusive or not? What equation should you use?", {
    expect_equal(digest(answer_6), "88d2821508a6625b093500c3e4d8d684")
  })
  print("Success!")
}

test_6.1 <- function() {
    answer_6.1 <- case_when(
        digest(answer_6.1) == "def2fbc829828dbfb34b6812623cae7d" ~ list(1, ""),
        digest(answer_6.1) == "2022775b58600cf7bb16c462b7c418b7" ~ list(0, "This would be the probability of catching a coho given it's a wild salmon."),
        digest(answer_6.1) == "522dbf08f17812fee06f0991cf0481af" ~ list(0, "This would be the probability of catching a wild salmon, given it's a sockeye. Remember that order matters."),
        digest(answer_6.1) == "6a8e65e0821e8011c0f04d886dce9323" ~ list(0, "This would be the probability of catching a salmon that is both wild and coho, from the total population. Remember, we've placed a condition that we're drawing from the sample of wild salmon only."),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_6.1[2]),{
    expect_equal(answer_6.1[[1]], 1)
  })
  print("Success!")             
}

test_6.2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_6.2), "d8427c27197a76311f498e200e8fd982")
  })
  print("Success!")
}

test_7 <- function() {
    answer_7 <- case_when(
        digest(answer_7) == "4332568af35d6a56121bcaca08bf25a3" ~ list(1, ""),
        digest(answer_7) == "c1357071979ffea410a8e0a047474381" ~ list(0, "Remember, in this case, having a sockeye salmon at the first level does not preclude having one at another level. The salmon types can repeat!"),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_7[2]),{
    expect_equal(answer_7[[1]], 1)
  })
  print("Success!")             
}

test_8 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_8), "c1357071979ffea410a8e0a047474381")
  })
  print("Success!")
}

test_9 <- function() {
    answer_9 <- case_when(
        digest(answer_9) == "4bdb128c943f718f5b8f347bb4b7641b" ~ list(1, ""),
        digest(answer_9) == "0aee9b78301d7ec8998971363be87c03" ~ list(0, "This is 3 factorical, which is what we would get if we wanted a permutation of three which had only three possibile elements. Try again."),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect. Hint: what would happen if you took the above example for a permutation of two salmon, and added another level to make is a permutation of three salmon?", answer_9[2]),{
    expect_equal(answer_9[[1]], 1)
  })
  print("Success!")             
}

test_10 <- function() {
  test_that("Solution is incorrect. Hint n = the total number of elements in the equation, not accounting for the repetitions. a and b are where you will account for the number of repetitions for different elements of the given list.", {
    expect_equal(digest(answer_10), "7710de4fa4cd6b09b8acf709568abdaf")
  })
  print("Success!")
}

test_11 <- function() {
  test_that("Solution is incorrect. Remember that the total number of possible elements (n) includes all salmon species, and the number of elements being taken (r) is the number of elements in the combination.", {
    expect_equal(digest(answer_11), "2522027d230e3dfe02d8b6eba1fd73e1")
  })
  print("Success!")
}


test_12 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_12), "5f3cce822c12054ce3a5b6b540c532fa")
  })
  print("Success!")
}


test_13 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_13), "e1f4ee07574db81a0bccbb017c304408")
  })
  print("Success!")
}

test_14 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_14), "756b2fb61ce2c2a915beba9d8a3e12c7")
  })
  print("Success!")
}

test_15 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_15), "105e39daecbfa9d3cf679cadbc5c1813")
  })
  print("Success!")
}

test_16 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_16), "55c7264469cb597ed8d928e6d42bcbbf")
  })
  print("Success!")
}

test_17 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_17), "55c7264469cb597ed8d928e6d42bcbbf")
  })
  print("Success!")
}

test_18 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_18), "425aea52f7e75538ab6eb7c80e9cf364")
  })
  print("Success!")
}

test_18 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_18), "425aea52f7e75538ab6eb7c80e9cf364")
  })
  print("Success!")
}

test_19 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_19), "f5c1f0a8793b3f7a19c2032e4fa71647")
  })
  print("Success!")
}

test_20 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_20), "af566de2eb7f0768e23af3965a803f2c")
  })
  print("Success!")
}

test_21 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_21), "21684aabad4e79d93546f3b31c41144a")
  })
  print("Success!")
}
