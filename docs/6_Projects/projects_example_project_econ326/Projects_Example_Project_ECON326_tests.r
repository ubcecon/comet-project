library(testthat)
library(digest)

test_1 <- function() {
    answer_1 <- case_when(
        digest(answer_1) == "75f1160e72554f4270c809f041c7a776" ~ list(0, "Think about what role multicollinearity plays in a model's explanatory power."),
        digest(answer_1) == "127a2ec00989b9f7faf671ed470be7f8" ~ list(0, "Think about what role multicollinearity plays in a model's explanatory power."),
        digest(answer_1) == "ddf100612805359cd81fdc5ce3b9fbba" ~ list(1, ""),
        digest(answer_1) == "3a5505c06543876fe45598b5e5e5195d" ~ list(1, ""),
        digest(answer_1) == "475bf9280aab63a82af60791302736f6" ~ list(0, "Is this whole statement true? What uses could a simple regression have?"),
        digest(answer_1) == "6e7a8c1c098e8817e3df3fd1b21149d1" ~ list(0, "Is this whole statement true? What uses could a simple regression have?"),
        digest(answer_1) == "c1f86f7430df7ddb256980ea6a3b57a4" ~ list(0, "Be careful while interpreting coefficients of multiple regression models, and remember that a multiple regression model does not always contain an interaction term."),
        digest(answer_1) == "d110f00cfb1b248e835137025804a23b" ~ list(0, "Be careful while interpreting coefficients of multiple regression models, and remember that a multiple regression model does not always contain an interaction term."),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_1[2]),{
    expect_equal(answer_1[[1]], 1)
  })
  print("Success!")
}

test_2 <- function() {
    answer_2 <- case_when(
        digest(answer_2) == "75f1160e72554f4270c809f041c7a776" ~ list(0, "In cases where it makes economic sense, log transformations are a great method of adjusting the data to combat heteroskedasticity."),
        digest(answer_2) == "127a2ec00989b9f7faf671ed470be7f8" ~ list(0, "In cases where it makes economic sense, log transformations are a great method of adjusting the data to combat heteroskedasticity."),
        digest(answer_2) == "ddf100612805359cd81fdc5ce3b9fbba" ~ list(0, "Recall that HC1 errors are also called 'robust standard errors', which are an acceptable way of accounting for heteroskedasticity in a model."),
        digest(answer_2) == "3a5505c06543876fe45598b5e5e5195d" ~ list(0, "Recall that HC1 errors are also called 'robust standard errors', which are an acceptable way of accounting for heteroskedasticity in a model."),
        digest(answer_2) == "475bf9280aab63a82af60791302736f6" ~ list(0, "Remember that hetroskedasticity is a quality of the data themselves. While this approach shouldn't be your first solution, it could resolve the heteroskedasticity."),
        digest(answer_2) == "6e7a8c1c098e8817e3df3fd1b21149d1" ~ list(0, "Remember that hetroskedasticity is a quality of the data themselves. While this approach shouldn't be your first solution, it could resolve the heteroskedasticity"),
        digest(answer_2) == "c1f86f7430df7ddb256980ea6a3b57a4" ~ list(1, ""),
        digest(answer_2) == "d110f00cfb1b248e835137025804a23b" ~ list(1, ""),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_2[2]),{
    expect_equal(answer_2[[1]], 1)
  })
  print("Success!")
}

test_3 <- function() {
    answer_3 <- case_when(
        digest(answer_3) == "75f1160e72554f4270c809f041c7a776" ~ list(0, "Not quite. Heteroskedasticity in the data must be detected in other ways."),
        digest(answer_3) == "127a2ec00989b9f7faf671ed470be7f8" ~ list(0, "Not quite. Heteroskedasticity in the data must be detected in other ways."),
        digest(answer_3) == "ddf100612805359cd81fdc5ce3b9fbba" ~ list(1, ""),
        digest(answer_3) == "3a5505c06543876fe45598b5e5e5195d" ~ list(1, ""),
        digest(answer_3) == "475bf9280aab63a82af60791302736f6" ~ list(0, "sort of... but is there a 'hard and fast' rule about determining multicollinearity from VIF?"),
        digest(answer_3) == "6e7a8c1c098e8817e3df3fd1b21149d1" ~ list(0, "sort of... but is there a 'hard and fast' rule about determining multicollinearity from VIF?"),
        digest(answer_3) == "c1f86f7430df7ddb256980ea6a3b57a4" ~ list(0, ""),
        digest(answer_3) == "d110f00cfb1b248e835137025804a23b" ~ list(0, ""),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_3[2]),{
    expect_equal(answer_3[[1]], 1)
  })
  print("Success!")
}


test_4 <- function() {
    answer_4 <- case_when(
        digest(answer_4) == "75f1160e72554f4270c809f041c7a776" ~ list(1, ""),
        digest(answer_4) == "127a2ec00989b9f7faf671ed470be7f8" ~ list(1, ""),
        digest(answer_4) == "ddf100612805359cd81fdc5ce3b9fbba" ~ list(0, "Is it consistent with economic theory to assume that interactions only occur between continuous variables?"),
        digest(answer_4) == "3a5505c06543876fe45598b5e5e5195d" ~ list(0, "Is it consistent with economic theory to assume that interactions only occur between continuous variables?"),
        digest(answer_4) == "475bf9280aab63a82af60791302736f6" ~ list(0, "Think about what an interaction can tell us. Would it be particularly useful if your model displayed multicollinearity?"),
        digest(answer_4) == "6e7a8c1c098e8817e3df3fd1b21149d1" ~ list(0, "Think about what an interaction can tell us. Would it be particularly useful if your model displayed multicollinearity?"),
        digest(answer_4) == "c1f86f7430df7ddb256980ea6a3b57a4" ~ list(0, "Fruitful data exploration is often driven by interest, so there's no problem in wanting to tease out interesting relationships from the data! Just remember that a 'boring' model is not necessarily a bad one."),
        digest(answer_4) == "d110f00cfb1b248e835137025804a23b" ~ list(0, "Fruitful data exploration is often driven by interest, so there's no problem in wanting to tease out interesting relationships from the data! Just remember that a 'boring' model is not necessarily a bad one."),
        TRUE ~ list(0, ""),
        )
  test_that(paste("Solution is incorrect.", answer_4[2]),{
    expect_equal(answer_4[[1]], 1)
  })
  print("Success!")
}
