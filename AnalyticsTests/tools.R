#Function to anualize holding period return----
annualize_R <- function(h_p_return, months) {
  ((1 + h_p_return)^(12 / months)) - 1 # Annualize hp
}

#Function to get holding period returns from annualized returns----
holdingperiod_R <- function(annualized_return, period) {
  ((1 + annualized_return)^(1 / (364 / period))) - 1 # Add one to anualized to convert to hp
}


library(dplyr)

df2 <- read.table("clipboard", sep = "\t", header = T)
write.csv(df2,"182DayTbill.csv")


Tbill_182_dayperf <- df2 %>% group_by(TransactionDate) %>% mutate(perf = holdingperiod_R((Value/100 + 0.02) ,1))
write.csv(Tbill_182_dayperf,"182DayTbillPerfApril.csv")

View(Tbill_182_dayperf)

