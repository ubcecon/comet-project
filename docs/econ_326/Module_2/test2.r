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



test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer1), '0f419358af6bf4563aac92df78b9c239')
  })
  print("Success!")
}


'8ddb40a1f799071bad5eba025a24ccdf'


test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer2),'8ddb40a1f799071bad5eba025a24ccdf')
  })
  print("Success!")
}





test_2.5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer2.5),'3a5505c06543876fe45598b5e5e5195d')
  })
  print("Success!")
}


test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(regm$coefficients,2)), "c0e47c5eec3817a5e7bdf2b4cca29162")
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(regf$coefficients,2)), '6aa65201ee0e0ee526865b6be3b32520')
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(lnreg$coefficients,2)), '52308ed21f0b1e637ffb6a5a825c4e9e')
  })
  print("Success!")
}


test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg2A$coefficients,2)), '1a2d224b3da69ace473379ce54546bb4')
  })
  print("Success!")
}

test_8 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg2B$coefficients[1],2)), "683d5cec07eaf7aad55da0e92dd9bc77")
  })
  print("Success!")
}

test_12 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(rege2$coefficients,2)), "5691989989603579fcee1a6c8d90ae15")
  })
  print("Success!")
}


test_16 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(rege4$coefficients,2)), "b9bbe179e78e372ac2de843cc951fa77")
  })
  print("Success!")
}





