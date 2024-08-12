library(testthat)
library(digest)


checker <- function(ans_given,hash,response,haser=TRUE){
  if(digest(ans_given)==hash && haser){
    print(response)
  }
  if(ans_given==hash && haser==FALSE){
    print(response)
  }
}

count1=0
test_1 <- function() {
  count1 <<- count1 + 1
  print(count1)
  checker(ans1,'927ba249f1c5afcf283b8d24174a8570',"Success!")
  checker(ans1,'908d1fd10b357ed0ceaaec823abf81bc',"No that's the p-value")
  checker(ans1,'1a3af4cddebd90c0e9d1a54816bee00a',"No that's the F-value")
  if (count1>5){
    print("It's either the r-squared or the adjusted r-squared")
  }
}

count2=0
test_2 <- function() {
    count2<<- count2+1
    checker(abs(answer2), "f3481c09ab4181d4cd15f79869589215","Success!")
    checker(abs(answer2), '4a2a6f0787c1f8efb37a6041b28f6857',"That is the Intercept")
    checker(count2, 5,"How would you interpret the slope here since there are only two points and what is the slope?",haser=FALSE)
}

test_2.5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer2.5), "3a5505c06543876fe45598b5e5e5195d")
  })
  print("Success!")
}
count3=0
test_3 <- function(regm) {
    count3<<-count3+1
    if (count3==1){
      print("You want both the r-squared and the coefficients to be correct")
    }
    checker(round(regm$coefficients,2), "7e5d8b0c20511e8a98fbd77bb169a6be","The regression Coefficients are correct")
    checker(round(regm$r.squared,2), '908d1fd10b357ed0ceaaec823abf81bc',"The R-squared is correct")
    checker(count3,3,"Perhaps you have entered the wrong data",haser=FALSE)
}
count4=0
test_4 <- function(regf) {
    count4<<-count4+1
    if (count3==1){
      print("You want both the r-squared and the coefficients to be correct")
    }
    checker(round(regf$coefficients,2), "336d7e5b4c1c9ae028fd1267791477e5","The regression Coefficients are correct")
    checker(round(regf$r.squared,2), '908d1fd10b357ed0ceaaec823abf81bc',"The R-squared is correct")
    checker(count3,3,"Perhaps you have entered the wrong data",haser=FALSE)
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(lnreg$coefficients,2)), "0d6321ce24f7863ac3aacff0855b328f")
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(lnregf), "aa8ca5ff5de0776fb598f5945315ed51")
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg2A$coefficients,2)), "e2c2f7f8174ef76ed82c27f77636b62e")
  })
  print("Success!")
}

test_8 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg2B$coefficients[1],2)), "dc866355624fa98af7f3af50cec3c7ff")
  })
  print("Success!")
}


test_10 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg_race$coefficients,2)), "e4c85e58f1688fc79bc7f0f8e329318f")
  })
  print("Success!")
}

test_11 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg4), "b725a136eb98459cf499ef0e3f245c3c")
  })
  print("Success!")
}

test_12 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg5_20$coefficients,2)), "e5dbfaaca5da98db52ab38de583975af")
  })
  print("Success!")
}


test_16 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg5_50$coefficients,2)), "eca7a47a1caf78bd2fc48dd00168330c")
  })
  print("Success!")
}

test_20 <- function(answer20) {
    if (answer20 == "A" | answer20 == "B" | answer20 == "C" | answer20 == "D")
        case_when(answer20 == "A" ~ "Congratulations! this is the correct answer"
                ,answer20 == "B" ~ "Close, try again!"
                ,answer20 == "C" ~ "Nope thats the value of the immstatimmigrants (beta_1)"
                ,answer20 == "D" ~ "You should definitely worry about this!")
    else 
        print("That is an invalid input. Recheck the formatting")
    }

test_21 <- function(answer21) {
    if (answer21 == "A" | answer21 == "B" | answer21 == "C")
        case_when(answer21 == "A" ~ "Incorrect"
                ,answer21 == "B" ~ "Correct!"
                ,answer21 == "C" ~ "Incorrect")
    else 
        print("That is an invalid input. Recheck the formatting")
    }

test_22 <- function(answer22) {
    if (answer22 == "A" | answer22 == "B" | answer22 == "C")
        case_when(answer22 == "A" ~ "Incorrect"
                ,answer22 == "B" ~ "Correct!"
                ,answer22 == "C" ~ "Incorrect")
    else 
        print("That is an invalid input. Recheck the formatting")
    }

test_23 <- function(answer23) {
    if (answer23 == "A" | answer23 == "B" | answer23 == "C")
        case_when(answer23 == "A" ~ "Incorrect!"
                ,answer23 == "B" ~ "Correct!"
                ,answer23 == "C" ~ "Incorrect!")
    else 
        print("That is an invalid input. Recheck the formatting")
    }

test_24 <- function(answer24) {
    if (answer24 == "A" | answer24 == "B" | answer24 == "C")
        case_when(answer24 == "A" ~ "Correct!"
                ,answer24 == "B" ~ "No we are no longer talking about the immigrant wage gap!"
                ,answer24 == "C" ~ "We haven't added controls to this regression model")
    else 
        print("That is an invalid input. Recheck the formatting")
    }

test_25 <- function(answer25) {
    if (answer25 == "A" | answer25 == "B" | answer25 == "C" | answer25 == "D")
        case_when(answer25 == "A" ~ "We should add controls!"
                ,answer25 == "B" ~ "Correct! We should experiment with other controls because other factors could help explain this wage gap"
                ,answer25 == "C" ~ "We shouldn't say that education is the only missing control. Other factors could also be a contributing factor"
                ,answer25 == "D" ~ "We shouldn't say that immigrant status is the only missing control. Other factors could also be a contributing factor")
    else 
        print("That is an invalid input. Recheck the formatting")
    }


test_26 <- function(answer26) {
    if (answer26 == "A" | answer26 == "B" | answer26 == "C" | answer26 == "D")
        case_when(answer26 == "A" ~ "Correct"
                ,answer26 == "B" ~ "Incorrect! Remember we are log transforming both the independent and dependent variable"
                ,answer26 == "C" ~ "Incorrect! Remember we are log transforming both the independent and dependent variable"
                ,answer26 == "D" ~ "Incorrect! Remember we are log transforming both the independent and dependent variable")
    else 
        print("That is an invalid input. Recheck the formatting")
    }


