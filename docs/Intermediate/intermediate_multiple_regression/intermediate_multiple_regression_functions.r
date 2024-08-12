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

clean_up_sfs <- function(SFS_data) {
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
      )
    ) %>%
    mutate(
      srbuyg = ifelse(pasrbuyg == 96, NA, pasrbuyg),
      financial_asset = pwastbnd + pwastdep + pwastmui + pwastoin + pwaststk,
      risk_proxy = pwaststk / financial_asset
    ) %>%
    mutate_at(vars(pasrbuyg, gender, education, business, province, age, employment), as.factor
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



### Self-tests

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer1), 'f7b0db1c9bc01deadcdde41033af1311')
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg_LESS), 'cf1829ebfea06a34d670471c16367bc7')
  })
  print("Success!")
}


test_2.5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg_HS), 'ca0ef80393a482fe6a299b69ab9374f8')
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg_NU), 'dfadec6bab303513637fdf70e319f5a1')
  })
  print("Success!")
}


test_3.5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg_U), '3d2023189710af9b7b6679f7ded2e880')
  })
  print("Success!")
}

test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg2), '35884f5f525ea0144acc39d32ca88217')
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg3), '217dd5c6670df0e3e21e143a181aa52d')
  })
  print("Success!")
}

test_5.5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg4), 'f779a169528a7262d60e11a4c212369a')
  })
  print("Success!")
}
test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(reg5), '6b5828b36a805070e35646458c120410')
  })
  print("Success!")
}