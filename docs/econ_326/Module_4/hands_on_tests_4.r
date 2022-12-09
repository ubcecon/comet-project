library(testthat)
library(digest)


clean_up_data <- function() {

    SFS_data <- rename(SFS_data,age= pagemieg)
SFS_data <- filter(SFS_data, !is.na(SFS_data$pefmtinc))  
SFS_data <- rename(SFS_data, income_before_tax = pefmtinc) 
SFS_data <- rename(SFS_data, income_after_tax = pefatinc)
SFS_data <- rename(SFS_data, wealth = pwnetwpg)
SFS_data <- rename(SFS_data, gender = pgdrmie)
SFS_data <- rename(SFS_data, education = peducmie)
SFS_data<-SFS_data[!(SFS_data$education=="9"),]
SFS_data$education <- as.numeric(SFS_data$education)
SFS_data <- SFS_data[order(SFS_data$education),]
SFS_data$education <- as.character(SFS_data$education)
SFS_data$education[SFS_data$education == "1"] <- "Less than high school" 
SFS_data$education[SFS_data$education == "2"] <- "High school"
SFS_data$education[SFS_data$education == "3"] <- "Non-university post-secondary"
SFS_data$education[SFS_data$education == "4"] <- "University"
SFS_data$gender <- as_factor(SFS_data$gender) 
SFS_data$education <- as_factor(SFS_data$education)
    
    SFS_data <- SFS_data %>% #some data cleaning
               mutate(ln_income_before_tax = log(income_before_tax))
SFS_data <- filter(SFS_data, !is.na(SFS_data$income_before_tax))
SFS_data <- filter(SFS_data, !is.na(SFS_data$income_after_tax))

return(SFS_data)
}


test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg1$coefficients[1], 3)), "29770426ed080d4f4efbdf64d1a28661")
  })
  print("Success!")
}


test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg2$coefficients[1], 3)), "fd5035ac6b5c7e9db9e3d62c8519f977")
  })
  print("Success!")
}


test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg3$coefficients[1], 3)), "7fd37b08034ac4cc6fd8101fd458d4e0")
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
    expect_equal(digest(round(SFS_data$resid[1], 3)), "942178c59c538708463f950c4fbbc053")
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(WT$coefficients[1], 2)), "47162f98701a4bb9421e1574a3b5bd95")
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




















