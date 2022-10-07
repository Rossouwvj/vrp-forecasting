library(pacman)
p_load(xts, tidyverse, xtable,rbenchmark,data.table, foreach, doParallel)
p_load(car)
source('./loading_and_formatting.R')
source('./report_loading_and_formatting.R')

# Function testing parameters
period_conversion <- 5
path <- "../Data/Jan"
write_to <- "../Data/Results/original/5min"


fxpairs_list <- load_and_format(path = path, period_conversion = period_conversion, write_to = write_to)

format(object.size(fxpairs_list), units = "auto")
saveRDS(fxpairs_list, file = "./fxpairs_list5m_jan.rds")




# Create and save the report
report <- report_loading_and_formatting(end_date, start_date, periods)
xtable(report) # get latex table of the report.
write.table(xtable(report), "./report_loading_and_formatting.txt") # Save it into text file if for some reason some one wants to see that. 
saveRDS(report, file = "./report.rds")

rm(list=ls())


data_read_report <- cbind(report, rep(FALSE, nrow(report)))

colnames(data_read_report)[4] <- "Reading data in errors" # 

data_read_report_old <- cbind(report_old,rep(FALSE, nrow(report_old)))
colnames(data_read_report_old)[4] <- "Reading data in errors"





# Data checks for start and end


benchmark("fread" = { USDZAR_data_table <- fread(file_names[1])},
          "read_csv" = { USDZAR_csv1 <- read_csv(file_names[1], col_types = cols(`Date-Time` = col_character()))},
          "read.csv" = { USDZAR_csv2 <- read.csv(file_names[1], stringsAsFactors = FALSE)},
          replications = 1)



plot(diff((fxpairs_list$USDCNY$Close.Bid["2016"]-fxpairs_list$USDCNY$Close.Ask["2016"])/2))

plot(diff((fxpairs_list$USDAUD$Close.Bid["2013"]-fxpairs_list$USDAUD$Close.Ask["2013"])/2))

fxpairs_list$USDAUD$Close.Ask





USDAUD_data_table <- fread(file_names[1])


# Issue files 
# Errors at 11,
# USDJPY

