library(testthat)
library(digest)

test_0 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_0), "db8e490a925a60e62212cefc7674ca02")
  })
  print("Success!")
}



test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "863dfc36ab2bfe97404cc8fc074a5241")
  })
  print("Success!")
}


test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "100740baf9d4d8a4c27550e75a231b31")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "0590b0427c1b19a6eb612d19888aa52f")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_4), "90989770ce41a5c7c0fef8eec808df7c")
  })
  print("Success!")
}

# Aux Functions


# Plot function (for demonstrating rejection region plot)

draw_cr <- function(type = "two-tailed", df, cv, lowerx = -5, upperx = 5) {
  x <- seq(lowerx, upperx, len = 1000)
  dx <- dt(x, df = df)
  plot(x, dx, xlab = "test value", ylab = "density", type = "l")
  abline(h = 0)

  if(type == "left-tailed") {
    cvx <- seq(lowerx, cv, len = 100)
    cvx2 <- c(cvx, rev(cvx))
    dcvx <- dt(cvx, df = df)
    dcvx2 <- c(dcvx, rep(0, length(cvx)))
    polygon(cvx2, dcvx2, col = "red")
  }else if (type == "right-tailed") {
    cvx <- seq(cv, upperx, len = 100)
    cvx2 <- c(cvx, rev(cvx))
    dcvx <- dt(cvx, df = df)
    dcvx2 <- c(dcvx, rep(0, length(cvx)))
    polygon(cvx2, dcvx2, col = "red")
  }else {
    cvx <- seq(lowerx, -abs(cv), len = 100)
    cvx2 <- c(cvx, rev(cvx))
    dcvx <- dt(cvx, df = df)
    dcvx2 <- c(dcvx, rep(0, length(cvx)))
    polygon(cvx2, dcvx2, col = "red")
    cvx <- seq(abs(cv), upperx, len = 100)
    cvx2 <- c(cvx, rev(cvx))
    dcvx <- dt(cvx, df = df)
    dcvx2 <- c(dcvx, rep(0, length(cvx)))
    polygon(cvx2, dcvx2, col = "red")
  }
  
  legend("topright", c("Rejection Region", "Non-Rejection Region"), fill = c("red", "white"))
}