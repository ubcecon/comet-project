library(dplyr)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(testthat)
library(digest)


simulate_data2 <- function(center1, center2) {
    x1 <- center1[1]
    x2 <- center2[1]
    y1 <- center1[2]
    y2 <- center2[2]

    dataset <- rbind(
            data.frame(x = rnorm(50, mean = x1, sd = 0.2),
                    y = rnorm(50, mean = y1, sd = 0.3),
                    color = "red", cluster = 1),
            data.frame(x = rnorm(50, mean = x2, sd = 0.2),
                    y = rnorm(50, mean = y2, sd = 0.3),
                    color = "blue", cluster = 2)
            )

    return(dataset)
}

simulate_data3 <- function(center1, center2, center3) {
    x1 <- center1[1]
    x2 <- center2[1]
    y1 <- center1[2]
    y2 <- center2[2]
    x3 <- center3[1]
    y3 <- center3[2]

    dataset <- rbind(
                    data.frame(x = rnorm(50, mean = x1, sd = 0.2),
                               y = rnorm(50, mean = y1, sd = 0.3),
                               color = "red"),
                    data.frame(x = rnorm(50, mean = x2, sd = 0.2),
                               y = rnorm(50, mean = y2, sd = 0.3),
                               color = "green"),
                    data.frame(x = rnorm(50, mean = x3, sd = 0.2),
                               y = rnorm(50, mean = y3, sd = 0.3),
                               color = "blue"))

    return(dataset)
}

elbow_plot <- function( ) {

    # Getting our within-cluster sum of squares for k values of 1, 2, .... 9, 10
    tot_withinss <- sapply(1:10, function(k){
        km_mod <- kmeans(x = dataset3, centers = k)
        km_mod$tot.withinss
    })

    # Storing our within-cluster sum of squares in a dataframe
    tot_withinss_df <- data.frame(k = 1:10, tot_withinss = tot_withinss)

    # Creating our plot
    f <- ggplot(tot_withinss_df, aes(x = k, y = tot_withinss)) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = 1:10) + 
        ylab('Within Sum of Squares') + 
        xlab('k = number of clusters')
        
    return(f)

}


clean_up_data <- function(raw_data) {


    raw_data <- raw_data %>% select(age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
                        days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
            filter(days_b_screening_arrest <= 30) %>%
            filter(days_b_screening_arrest >= -30) %>%
            filter(is_recid != -1) %>%
            filter(c_charge_degree != "O") %>%
            filter(score_text != 'N/A') %>%
            mutate(race = as.factor(race)) %>%
            within(race <- relevel(race, ref = 3)) %>%
            mutate(age_cat = as.factor(age_cat)) %>%
            mutate(sex = as.factor(sex)) %>%
            mutate(c_charge_degree = as.factor(c_charge_degree)) %>%
            mutate(score_text = as.factor(score_text)) %>%
            within(sex <- relevel(sex, ref = 2))

    raw_data$length_of_stay <- as.numeric(as.Date(raw_data$c_jail_out) - as.Date(raw_data$c_jail_in))
    return(raw_data)
}

make_dummies <- function(data) {

            data <- data %>%
                mutate(c_charge_degree = c_charge_degree == "F") %>%
                mutate(sex = sex == "Male")

        return(data)


}

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_1), "75f1160e72554f4270c809f041c7a776")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_2), "475bf9280aab63a82af60791302736f6")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer_3), "3a5505c06543876fe45598b5e5e5195d")
  })
  print("Success!")
}