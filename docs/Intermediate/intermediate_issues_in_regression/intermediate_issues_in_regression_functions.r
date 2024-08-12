library(testthat)
library(digest)

dictionary <- function(var) {
  r <- tribble(
    ~description, ~code,
    "Age of the major income earner in the family unit.", "PAGEMIEG",
    "price of property", "PASRCST",
    "interest rate", "PASRINTG",
    "What is the total credit limit on all credit card(s) that you (and your family) own?", "PATTCRLM",
    "expected change in total income", "PATTSTIN",
    "have a business", "PBUSIND",
    "Highest level of education of the major income earner.", "PEDUCMIE",
    "after-tax-income", "PEFATINC",
    "government transfers", "PEFGTR",
    "Major source of income for the family unit.", "PEFMJSIF",
    "market income. Also, income before taxes and government transfers.", "PEFMTINC",
    "Amount of mortgage payments", "PEXMG1A",
    "Frequency of mortgage payment", "PEXMG1F",
    "Number of members in the family unit, all ages", "PFSZ",
    "Presence of persons in the family of age 65 and up", "PFSZ65UP",
    "Principal residence ownership status", "PFTENUR",
    "Total value of inheritances received in 2016 constant dollars", "PINHERT",
    "Major income earner in the family usually works 30 hours or more per week", "PLFCHRME",
    "Major income earner in the family worked either full time or part-time (in 2018)", "PLFFPTME",
    "Major income earner in the family is either a paid worker or self employed", "PLFPDMEG",
    "Number of earners aged 15 or over in the family unit", "PNBEARG",
    "Province of residence for the family unit", "PPVRES",
    "Region of residence of the family unit", "PREGION",
    "Indicates if the major income earner has ever retired", "PRETIRME",
    "Gender of the major income earner in the family unit", "PGDRMIE",
    "Other retirement funds", "PWAOTPEN",
    "Value of the principal residence", "PWAPRVAL",
    "Value of all employer pension plans", "PWARPPG",
    "Registered retirement income funds (RRIFs)", "PWARRIF",
    "RRSP investments including locked in RRSP’s", "PWARRSPL",
    "Bonds, non-registered", "PWASTBND",
    "Money in banks, non-registered", "PWASTDEP",
    "Mutual funds, other investment, Income trusts, non-registered", "PWASTMUI",
    "Other investments or financial assets", "PWASTOIN",
    "Other non-financial assets", "PWASTONF",
    "Real estate other than principal residence", "PWASTRST",
    "Stocks and shares, non registered", "PWASTSTK",
    "Total of vehicles (cars, trucks and vans and other vehicles)", "PWASTVHE",
    "Tax Free Saving Accounts (TFSA)", "PWATFS",
    "Total assets including employer pension plans - On going concern basis", "PWATOTPG",
    "Equity value of businesses operated by the family unit", "PWBUSEQ",
    "Mortgage on principal residence, final value", "PWDPRMOR",
    "Debt value of student loans", "PWDSLOAN",
    "Credit card and installment debt", "PWDSTCRD",
    "What is the total credit limit on all line(s) of credit that you (and your family) own?", "PATTLMLC",
    "Line-of-credit debt (home and other line of credit)", "PWDSTLOC",
    "Total of other debt (other loans from financial instituitions and other money owed)", "PWDSTODB",
    "Mortgages on ’other real estate’ in and outside of Canada", "PWDSTOMR",
    "Debt on vehicles", "PWDSTVHN",
    "Total of all debts for the family", "PWDTOTAL",
    "Networth of the family unit. (On going concern basis)", "PWNETWPG",
    "Year property was purchased", "PASRBUYG"
  )
  
  r <- r %>% filter_all(any_vars(toupper(.) %in% toupper(var)))
  
  return(r[1, "description"])
}

clean_up_data <- function(SFS_data) {
  SFS_data <- SFS_data %>%
    filter(!is.na(pefmtinc)) %>%
    rename(
      income_before_tax = pefmtinc,
      income_after_tax = pefatinc,
      wealth = pwnetwpg,
      gender = pgdrmie,
      education = peducmie,
      business = pbusind,
      province = ppvres,
      credit_limit = pattlmlc,
      age = pagemieg,
      employment = plffptme
    ) %>%
    mutate(
      education = case_when(
        education == "1" ~ "Less than high school",
        education == "2" ~ "High school",
        education == "3" ~ "Non-university post-secondary",
        education == "4" ~ "University",
        TRUE ~ as.character(education)
      ),
      province = case_when(
        province == "10" ~ "Newfoundland and Labrador",
        province == "11" ~ "Prince Edward Island",
        province == "12" ~ "Nova Scotia",
        province == "13" ~ "New Brunswick",
        province == "24" ~ "Quebec",
        province == "35" ~ "Ontario",
        province == "46" ~ "Manitoba",
        province == "47" ~ "Saskatchewan",
        province == "48" ~ "Alberta",
        province == "59" ~ "British Columbia",
        TRUE ~ as.character(province)
      ),
      employment = case_when(
        employment == "1" ~ "Full-time",
        employment == "2" ~ "Part-time",
        employment == "3" ~ "Did not work",
        employment %in% c("6", "9") ~ NA,
        TRUE ~ as.character(employment)
      ),
      pasrbuyg = as.factor(ifelse(pasrbuyg == 96, NA, pasrbuyg)),
      gender = as.factor(gender),
      education = as.factor(education),
      business = as.factor(business),
      province = as.factor(province),
      age = as.factor(age),
      employment = as.factor(employment)
    ) %>%
    mutate(
      financial_asset = pwastbnd + pwastdep + pwastmui + pwastoin + pwaststk,
      risk_proxy = pwaststk / financial_asset
    ) %>%
    rename(
      stock = pwaststk,
      bond = pwastbnd,
      bank_deposits = pwastdep,
      mutual_funds = pwastmui,
      other_investments = pwastoin
    )
  
  return(SFS_data)
}

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg1$coefficients[1], 3)), "d21651ff4f1a03ee33f1e248fd1d497d")
  })
  print("Success!")
}


test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg2$coefficients[1], 3)), "a01f754ae07d1f545d7fa1e635f0d397")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg3$coefficients[1], 3)), "04aedefae1f702c50c8bfa71eea389d7")
  })
  print("Success!")
}


test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg4$coefficients[1], 3)), "f02741b40f9113aeb5a5e87fb9b2ab23")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(x), "047869e1cafdfce7faa4f07419604bc0")
  })
  print("Success!")
}


test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(SFS_data$resid[1], 3)), "221d88b706b11efb9707f3d239684288")
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(WT$coefficients[1], 2)), "1191bdb37060071fb219fa1c2a78c4d4")
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



















