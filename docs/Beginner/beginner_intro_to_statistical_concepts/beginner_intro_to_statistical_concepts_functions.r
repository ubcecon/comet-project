library(tidyverse)
library(plotly)
library(dplyr)
library(RColorBrewer)


#Data
population_data <- read.csv("datasets/salmon_populations.csv")

salmon_data <- read.csv("datasets/salmon_data.csv")

sample_data <- read.csv("datasets/sample_proportions.csv")

# Central Tendency - Median


variable_1 <- c(1.1, 0.8, 1.6, 2.1, 2.0, 2.2, 1.7, 1.8, 2.1)
variable_2 <- c(1.8, 1.8, 1.7, 1.7, 1.5, 2.6, 2.4, 3.3, 2.3)
df2 <- data.frame(variable_1, variable_2)


#Regression

slr <- lm(salmon_data$mass ~ salmon_data$length)

#Inferential Statistics

sample_estimates <- c(0.44, 0.47, 0.47, 0.39, 0.45)
sample_no <-  c(1,2,3,4,5)
df1 <- data.frame(sample_no, sample_estimates)

histogram_5_samples <- ggplot(data = df1, aes(x = sample_estimates)) +
  geom_histogram(fill = "dodgerblue3", color = "lightgrey", bins = 5) +
  labs(x = "Sample proportions", y = "Count") +
  theme(text = element_text(size = 12))



hist1 <- ggplot(data = df2, aes(x = variable_1)) +
  geom_histogram(fill = "darkseagreen3", color = "darkseagreen4", bins = 8) +
  labs(x = "Variable 1", y = "Frequency") + xlim(0.5, 3.5) +
  theme(text = element_text(size = 12))

hist2 <- ggplot(data = df2, aes(x = variable_2)) +
  geom_histogram(fill = "darkslategray3", color = "darkslategray4", bins = 7) +
  labs(x = "Variable 2", y = "Frequency") + xlim(0.5, 3.5) +
  theme(text = element_text(size = 12))

