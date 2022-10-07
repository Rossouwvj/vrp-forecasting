# Main 


library(pacman)
p_load(xts,timeDate,RQuantLib,anytime,fBasics,lubridate,kableExtra)
p_unload(purrr, readxl, dplyr) # These packages are being unloaded if they are loaded previously, since they cause issues in the moment_clean_all function

source("./moment_clean_all.R")
source("./diagnostic.R")

all_pairs <- fxpairs_list5m_jan
names(all_pairs) <- c("AUDUSD","EURUSD","GBPUSD","NZDUSD","USDAUD","USDBRL","USDCAD","USDCHF","USDCNH","USDCNY",
                      "USDEUR","USDGBP","USDHKD","USDINR","USDJPY","USDKRW","USDMXN","USDNOK","USDNZD","USDPLN",
                      "USDRUB","USDSEK","USDSGD","USDTRY","USDZAR")



output <- moment_clean_all(all_pairs, zero_tolerance = 0.95, outlier_wins = TRUE, weekend_strip = TRUE, 
                           holidays = "UnitedStates/NYSE", diagnose = TRUE)


saveRDS(output, file = "./output2.rds")
#saveRDS(output$bid_ask, file = "./bid_ask.rds")


# plot scatter divided by ATM vol
# If you have VIX square and de-annualize

#The variance risk premium is defined as the difference between the risk-neutral and objective expectations of realized variance, 
#where the risk-neutral expectation of variance is measured as the end-of-month VIX-squared de-annualized (VIX^2/12) and the realized variance is the sum of squared 5-minute log returns of the S&P 500 index over the month. Both variance measures are of monthly basis in percentage-squared and are available in real time at the end of observation month. Expected variance is a statistical forecast of realized variance by researcher's own choice: the one using simple lag realized variance is suitable for predictability exercise, as there is no modeling assumption involved and all the information is available at time-t.


# Risk measures to xts
library(readxl)
risk_measures <- read_excel("Copy of Risk_measures.xlsx", 
                                    col_types = c("date", "numeric", "numeric"))
View(risk_measures)
risk_measures_xts <- as.xts(risk_measures[,-1], order.by = risk_measures$Date)

risk_measures2 <- read_excel("Copy of Risk_measures.xlsx", 
                            sheet = "other controls", col_types = c("date", 
                                                                    "text", "text", "text", "text"))
View(risk_measures2)
colnames(risk_measures2) <- c("dates",risk_measures2[3,2:5])
risk_measures2 <- risk_measures2[7:nrow(risk_measures2),]

risk_measures2[,2:5] <- lapply(risk_measures2[,2:5], function(x) as.numeric(x))

risk_measures2_xts <- as.xts(risk_measures2[1:5439,-1], order.by = risk_measures2$dates[1:5439])
risk_xts <- merge(risk_measures_xts, risk_measures2_xts, join="left")

saveRDS(risk_xts, file = "./risk_xts.rds")


head(which(output$rmoments$USDCNY$rvar_scaled != 0 ))

 par(mfrow = c(1,2))
 plot(output$rmoments$USDCNH$rvar_scaled, main = "USDCNH")
 plot(output$rmoments$USDCNY$rvar_scaled, main = "USDCNY")

 plot((output$rmoments$USDCNY$rvar_scaled - output$rmoments$USDCNH$rvar_scaled), main = "USDCNY - USDCNH")

 diff <- na.omit((output$rmoments$USDCNY$rvol_scaled[index(output$rmoments$USDCNH$rvol_scaled)] - output$rmoments$USDCNH$rvol_scaled))
mean(diff) 
plot(diff, main = "diff realized vol") 
par(mfrow = c(1,2))
plot(output$rmoments$USDCNH$rvol_scaled, main = "USDCNH")
plot(output$rmoments$USDCNY$rvol_scaled, main = "USDCNY")
