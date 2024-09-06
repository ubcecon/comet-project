library(tidyverse)
set.seed(123)

lottery_winners <- data.frame(lot_win = sample(x=c(1,0), 1000, replace=TRUE),
                                income = rlnorm(1000)*100) %>%
                    cbind("insurance_status"= NA)%>%
                    cbind("health_outcome"= NA)

for (row in 1:nrow(lottery_winners)){
    lot_status <- lottery_winners[row, "lot_win"]
    income_status <- lottery_winners[row, "income"]

    if(lot_status == 1){
    lottery_winners[row, "insurance_status"] <- min(round(income_status/100),1)*rbinom(1,1,prob=0.9)
    } else {
     lottery_winners[row, "insurance_status"] <-  0
    }

}

for (row in 1:nrow(lottery_winners)){
    insurance_status <- lottery_winners[row, "insurance_status"]
    income_status <- lottery_winners[row, "income"]

    if(insurance_status == 1){
    lottery_winners[row, "health_outcome"] <- log(income_status)*1.05
    } else {
     lottery_winners[row, "health_outcome"] <- log(income_status)
    }

}
print(as_tibble(lottery_winners), n=100)

simulated_health_data <- as.data.frame((lottery_winners))%>%
                         select(-income)
simulated_health_data

simulated_health_data_extended <- as.data.frame((lottery_winners))
