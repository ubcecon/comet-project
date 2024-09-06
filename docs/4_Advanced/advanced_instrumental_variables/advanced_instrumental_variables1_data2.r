library(tidyverse)
data<-as.data.frame(read_csv("datasets/card.csv"))

feduc_vec <- data %>%
            drop_na(fatheduc)%>%
            select(fatheduc)%>%
            summarise(avg_feduc = (mean(fatheduc)))

avg_feduc <- as.integer(feduc_vec[1])
avg_feduc


cdist_data <- data%>%
            select(nearc4, educ, age, fatheduc, momdad14,
#            reg661, reg662, reg663, reg664, reg665, reg666, reg667, reg668, reg669, 
            south, smsa, black, wage, lwage, exper)%>%
            rowwise()%>%
            mutate(fatheduc = if_else(is.na(fatheduc) == TRUE, avg_feduc, fatheduc),
            urban=smsa, distance=nearc4)%>%
            select(-lwage, -smsa,-nearc4)

summary(cdist_data)
nrow(cdist_data)
