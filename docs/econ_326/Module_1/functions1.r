library(testthat)
library(digest)
library(dplyr)




dictionary <- function(var) {
r <- c("Age of the major income earner in the family unit.", "PAGEMIEG")
r <- rbind(r,c("price of property", "PASRCST"))
r <- rbind(r,c("interest rate", "PASRINTG"))
r <- rbind(r,c("What is the total credit limit on all credit card(s) that you (and your family) own?", "PATTCRLM"))
r <- rbind(r,c("expected change in total income", "PATTSTIN"))
r <- rbind(r,c("have a business", "PBUSIND"))
r <- rbind(r,c("Highest level of education of the major income earner.", "PEDUCMIE"))
r <- rbind(r,c("after-tax-income", "PEFATINC"))
r <- rbind(r,c("government transfers", "PEFGTR"))
r <- rbind(r,c("Major source of income for the family unit.", "PEFMJSIF"))
r <- rbind(r,c("market income. Also, income before taxes and government transfers.", "PEFMTINC"))
r <- rbind(r,c("Amount of mortgage payments", "PEXMG1A"))
r <- rbind(r,c("Frequency of mortgage payment", "PEXMG1F"))
r <- rbind(r,c("Number of members in the family unit, all ages", "PFSZ"))
r <- rbind(r,c("Presence of persons in the family of age 65 and up", "PFSZ65UP"))
r <- rbind(r,c("Principal residence ownership status", "PFTENUR"))
r <- rbind(r,c("Total value of inheritances received in 2016 constant dollars", "PINHERT"))
r <- rbind(r,c("Major income earner in the family usually works 30 hours or more per week", "PLFCHRME"))
r <- rbind(r,c("Major income earner in the family worked either full time or part-time (in 2018)", "PLFFPTME"))
r <- rbind(r,c("Major income earner in the family is either a paid worker or self employed", "PLFPDMEG"))
r <- rbind(r,c("Number of earners aged 15 or over in the family unit", "PNBEARG"))
r <- rbind(r,c("Province of residence for the family unit", "PPVRES"))
r <- rbind(r,c("Region of residence of the family unit", "PREGION"))
r <- rbind(r,c("Indicates if the major income earner has ever retired", "PRETIRME"))
r <- rbind(r,c("Gender of the major income earner in the family unit", "PGDRMIE"))
r <- rbind(r,c("Other retirement funds", "PWAOTPEN"))
r <- rbind(r,c("Value of the principal residence", "PWAPRVAL"))
r <- rbind(r,c("Value of all employer pension plans", "PWARPPG"))
r <- rbind(r,c("Registered retirement income funds (RRIFs)", "PWARRIF"))
r <- rbind(r,c("RRSP investments including locked in RRSP’s", "PWARRSPL"))
r <- rbind(r,c("Bonds, non-registered", "PWASTBND"))
r <- rbind(r,c("Money in banks, non-registered", "PWASTDEP"))
r <- rbind(r,c("Mutual funds, other investment, Income trusts, non-registered", "PWASTMUI"))    
r <- rbind(r,c("Other investments or financial assets", "PWASTOIN"))     
r <- rbind(r,c("Other non-financial assets", "PWASTONF"))     
r <- rbind(r,c("Real estate other than principal residence", "PWASTRST"))     
r <- rbind(r,c("Stocks and shares, non registered", "PWASTSTK"))     
r <- rbind(r,c("Total of vehicles (cars, trucks and vans and other vehicles)", "PWASTVHE"))
r <- rbind(r,c("Tax Free Saving Accounts (TFSA)", "PWATFS"))
r <- rbind(r,c("Total assets including employer pension plans - On going concern basis", "PWATOTPG"))
r <- rbind(r,c("Equity value of businesses operated by the family unit", "PWBUSEQ"))
r <- rbind(r,c("Mortgage on principal residence, final value", "PWDPRMOR"))
r <- rbind(r,c("Debt value of student loans", "PWDSLOAN"))
r <- rbind(r,c("Credit card and installment debt", "PWDSTCRD"))
r <- rbind(r,c("What is the total credit limit on all line(s) of credit that you (and your family) own?", "PATTLMLC"))
r <- rbind(r,c("Line-of-credit debt (home and other line of credit)", "PWDSTLOC"))    
r <- rbind(r,c("Total of other debt (other loans from financial instituitions and other money owed)", "PWDSTODB"))     
r <- rbind(r,c("Mortgages on ’other real estate’ in and outside of Canada", "PWDSTOMR"))       
r <- rbind(r,c("Debt on vehicles", "PWDSTVHN"))
r <- rbind(r,c("Total of all debts for the family", "PWDTOTAL"))
r <- rbind(r,c("Networth of the family unit. (On going concern basis)", "PWNETWPG"))
r = as.data.frame(r)
r= r %>% filter_all(any_vars(. %in% c(toupper(var))))

return(r[1,1])
}

test_00 <- function() {
      test_that("Solution is incorrect, Hint: there are 3 commands to view data", {
    expect_equal(digest(answer00), 'ffb497cd8da1b4a3f983803a079ef5ab')
  })
        
      
  print("Success!")
}

test_0 <- function() {
  test_that("Solution is incorrect, Hint: what is the jargon for education?", {
    expect_equal(digest(answer0),'f401db5dd95a0d99eecaa13b19c3735a')
  })
  print("Success!")
}

test_1 <- function() {
  test_that("Solution is incorrect. Hint: what do you compare income to?", {
    expect_equal(digest(answer1), '22f225ee36612a03a5ee660519194dd2')
  })
  print("Success!")
}
test_1_5 <- function() {
  test_that("Solution is incorrect. Hint: what variables are we comparing?", {
    expect_equal(digest(answer1_5), 'd30807db015302ff6b212d9f1f38c5b3')
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect. Hint: have we defined all variables?", {
    expect_equal(digest(answer2), 'a23fc01ed1d2f26bcba3195907c9a6a6')
  })
  print("Success!")
}
test_2_5 <- function() {
  test_that("Solution is incorrect. Hint: what variables are we comparing?", {
    expect_equal(digest(answer2_5), '2350a19744da5e1a68551053d62860ba')
  })
  print("Success!")
}

test_3_3 <- function() {
  test_that("Solution is incorrect. Hint: what variables are we comparing?", {
    expect_equal(digest(answer3_3), 'fe5c801a09f3d2b8946001948a622a61')
  })
  print("Success!")
}
test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer3), 'c9a464017bb4a89722b0738995703a95')
  })
  print("Success!")
}

test_3_7 <- function() {
  test_that("Solution is incorrect. Hint: what variables are we comparing?", {
    expect_equal(digest(answer3_7), 'c0f56d971341cd25d063c90671c9fcc8')
  })
  print("Success!")
}
test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer4), '17cc3055ce3a012e6bcc9a640911f7b9')
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(retHS_ans), '7d8a2f0744cdf35882e769acf64be8b7')
  })
  print("Success!")
}

test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(retHSF_ans), 'afc3e94dec8db21fa7f29156059ede00')
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(retU_ans), 'bdf551b33ddd329a701e9b81aaba378c')
  })
  print("Success!")
}

test_8 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(retUF_ans), '62e32d54ef7575db96c559ea8dc18924')
  })
  print("Success!")
}

test_9 <- function() {
  test_that("Solution is incorrect. Hint: what variables are we comparing?", {
    expect_equal(digest(tab_income3), 'c4c7f928c6fc74f44464d3719cc2bf45')
  })
  print("Success!")
}