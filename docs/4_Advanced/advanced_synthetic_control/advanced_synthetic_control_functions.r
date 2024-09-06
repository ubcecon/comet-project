library(foreign)
library(Synth)
library(haven)
library(tidyverse)
library(SCtools)
library(skimr)

#very minor data cleaning
unique(oecd_data$index)

# re-code index values s.t. 12 == 11, 14 == 12, 16 == 13, 18 == 14, 19 == 15, 20 == 16, 21 == 17, 22 == 18

oecd_data$index <- recode(
  oecd_data$index,
  `12` = 11,
  `14` = 12, 
  `16` = 13, 
  `18` = 14, 
  `19` = 15, 
  `20` = 16, 
  `21` = 17, 
  `22` = 18
  )