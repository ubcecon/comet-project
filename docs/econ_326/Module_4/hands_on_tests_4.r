library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg1$coefficients[1], 3)), "5492ea03b542f2865d0889675db7c138")
  })
  print("Success!")
}


test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg2$coefficients[1], 3)), "0b9cf3c70995c397636b67404f48be2e")
  })
  print("Success!")
}


test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg3$coefficients[1], 3)), "582b427814fd3bda6d2c546f0ba305b3")
  })
  print("Success!")
}


test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg4$coefficients[1], 3)), "f02741b40f9113aeb5a5e87fb9b2ab23")
  })
  print("Success!")
}




test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(SFS_data$resid[1], 3)), "d3234e252def10a1229e290c73a683fb")
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(WT$coefficients[1], 2)), "dc02857c346f47ec045e73775b22ca2c")
  })
  print("Success!")
}


vif <- function(model,x_j,y){
  #s_2=RMSE(model)^2
  #var=var(x_j)
  R_2j =cor(x_j,y)
  v=1/(1-R_2j)
  return(v)
}


RMSE <-function(model){
RSS <- c(crossprod(model$residuals))
MSE <- RSS / length(model$residuals)
RMSE <- sqrt(MSE)
return(RMSE) }




















