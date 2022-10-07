# Calculation of VRP


library(pacman)
p_load(quantmod,Rblpapi,readxl,xts,timeDate,quantreg,plotly,ggplot2,stargazer,tidyr,data.table,dplyr,reshape2)

IV <- read_excel("C:/Users/User-pc/Dropbox/Functions external/VRP/FX CLEANING/Cleaned code/Data/IV.xlsx", 
                 col_types = c("date", "numeric", "date", 
                               "numeric", "date", "numeric", "date", 
                               "numeric", "date", "numeric", "date", 
                               "numeric", "date", "numeric", "date", 
                               "numeric", "date", "numeric", "date", 
                               "numeric", "date", "numeric", "date", 
                               "numeric", "date", "numeric", "date", 
                               "numeric", "date", "numeric", "date", 
                               "numeric", "date", "numeric"))


col_counter <- 1
report_IV_data_read <- as.data.frame(matrix(data = NA, nrow = (ncol(IV)/2), ncol = 3))
for (i in 1:(ncol(IV)/2)) {
  
  IV_temp <- IV[,c(col_counter,(col_counter+1))]
  #report_IV_data_read[i,3] <- sum(is.na(IV_temp[,2]))
  IV_temp <- na.omit(IV_temp)
  report_IV_data_read[i,3] <- nrow(IV_temp)
  
  
  rownames(report_IV_data_read)[i] <- colnames(IV)[col_counter+1] 
  
  report_IV_data_read[i,1] <- as.character(IV_temp[1,1, drop=TRUE])
  report_IV_data_read[i,2] <- as.character(IV_temp[nrow(IV_temp),1, drop=TRUE])
  
  col_counter <- col_counter+2
  if(col_counter>(ncol(IV)/2)*2){break}
  #print(col_counter)
}

report_IV_data_read <- cbind(report_IV_data_read,rep(FALSE,nrow(report_IV_data_read)))
colnames(report_IV_data_read) <- c("Start date", "End date", "Nr of observations", "Data read errors") 

#----------------------------------------------------------------------------------
# Calculate HAR model
rvar_har <- moments$ZARUSD$rvar_scaled
# We will find the first day in this year 
T_0 <- as.Date("2009-01-02") # Start of estimation sample
TT <- as.Date("2019-11-19") # end of estimation sample

# Check if the selected date is in sample
all_dates <- index(rvar_har)
if((T_0 %in% as.Date(all_dates))){
  print("Selected T_0 date is in sample")
}else{print("Selected T_0 date not in sample!")}

if((TT %in% as.Date(all_dates))){
  print("Selected TT date is in sample")
}else{print("Selected TT date not in sample!")}

in_sample <- paste(T_0,TT,sep = "/")

# Variable definitions for HAR model

rvar_har <- na.locf(rvar_har, fromLast = T)
rvar_har$seq <- seq(1,nrow(rvar_har)) # To help with indexing

# Find numeric indicies of selected sub sample
T_0_index <- as.numeric(rvar_har$seq[T_0])
TT_index <- as.numeric(rvar_har$seq[TT])
T_max_index <- as.numeric(max(rvar_har$seq))

# Construct 5 and 22 period rolling average 
rvar_har$rvar_har_av5 <- runSum(rvar_har$rvar_scaled[in_sample], n = 5)/5
rvar_har$rvar_har_av22 <- runSum(rvar_har$rvar_scaled[in_sample], n=22)/22

# Adding Daans IV calcs 
rvar_har$IV_BB <- (BB_moments_xts$USDZARV1M)^2
rvar_har$IV_BB <- na.locf(rvar_har$IV_BB, fromLast = T)

# Convert to dataframe
rvar_har <- as.data.frame(rvar_har)
rvar_har <- rvar_har[1:T_max_index,]
# Estimate HAR model 

# Decide if you want to use standard HAR or option augmented HAR
AHAR <- TRUE
#AHAR <- FALSE


if(AHAR == TRUE){
  HAR_lm <- lm(rvar_har$rvar_scaled ~ 1 + lag(rvar_har$rvar_scaled) + lag(rvar_har$rvar_har_av5) 
               + lag(rvar_har$rvar_har_av22) + lag(rvar_har$IV_BB))
}  else{
  HAR_lm <- lm(rvar_har$rvar_scaled ~ 1 + lag(rvar_har$rvar_scaled) + lag(rvar_har$rvar_har_av5) 
               + lag(rvar_har$rvar_har_av22))
}
summary(HAR_lm)


# Static forecast 
# Initialize first values in the loop
# Create empty col for forecast
rvar_har$rvar_F <- rvar_har$rvar_scaled

rvar_har$rvar_F[(TT_index+1):T_max_index] <- NA 

for(h in 1:(T_max_index-TT_index)){
  
  if(AHAR == F){   
    point_forecast <- HAR_lm[["coefficients"]][1] + 
      HAR_lm[["coefficients"]][2]*rvar_har$rvar_scaled[(TT_index+h-1)] +
      HAR_lm[["coefficients"]][3]*rvar_har$rvar_har_av5[(TT_index+h-1)] +
      HAR_lm[["coefficients"]][4]*rvar_har$rvar_har_av22[(TT_index+h-1)]
  } else {
    point_forecast <- HAR_lm[["coefficients"]][1] + 
      HAR_lm[["coefficients"]][2]*rvar_har$rvar_scaled[(TT_index+h-1)] +
      HAR_lm[["coefficients"]][3]*rvar_har$rvar_har_av5[(TT_index+h-1)] +
      HAR_lm[["coefficients"]][4]*rvar_har$rvar_har_av22[(TT_index+h-1)] +
      HAR_lm[["coefficients"]][5]*rvar_har$IV_BB[(TT_index+h-1)]
  } 
  rvar_har$rvar_F[TT_index+h] <- point_forecast 
  
  rvar_har$rvar_har_av5[(TT_index+h)] <- sum(rvar_har$rvar_scaled[(TT_index+h-5+1):(TT_index+h)])/5
  rvar_har$rvar_har_av22[(TT_index+h)] <- sum(rvar_har$rvar_scaled[(TT_index+h-22+1):(TT_index+h)])/22
}# end of FOR

# Convert dataframe to xts
rvar_har_xts <- as.xts(rvar_har, order.by = as.Date(row.names(rvar_har), format = "%Y-%m-%d"))

IV <- ((BB_moments_xts$USDZARV1M)^2)/12
cond_var <- runSum(rvar_har_xts$rvar_F, n=22)/260
VRP <- IV - cond_var
plot(VRP["2007/2019"], main = "1M VRP", cex = 1)
# Save VRP series
#saveRDS(VRP, file = "./VRP.rds")
plot(IV)
lines(cond_var, col = "red")




