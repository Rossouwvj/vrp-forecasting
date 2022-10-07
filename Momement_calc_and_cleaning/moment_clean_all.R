# First function will call all of the values 


# Notes---
# 1) Need to see if using RQuantLib provides better holiday functionality 



moment_clean_all <- function(all_pairs, zero_tolerance, outlier_wins = TRUE, weekend_strip, holidays, diagnose = TRUE){
  # Loop through all the fx pairs in the list
  # define all elements to which objects will be stored in the loop
  source("./diagnostic.R")
  
  r_moments_list <- list()
  obs_removed <- as.data.frame(matrix(data = NA, ncol = 2, nrow = 5)) # Data frame that will store all the obs 
  rownames(obs_removed) <- c("Holidays","Zero tolerance","Weekends","winsorization","Total obs removed")
  colnames(obs_removed) <- c("Total obs","Approx. nr of days")
  
  days_removed_df <- as.data.frame(matrix(data = NA, nrow = 4, ncol = (2022-1996+1)))
  colnames(days_removed_df) <- 1996:2022
  rownames(days_removed_df) <- c("Holidays","Zero tolerance","Winsorization","Weekends")
  
  obs_removed_list <- list()
  diagnose_list <- list()
  
  dates_modified_list <- rep(list(list(holidays = list(), zeros = list(), outliers = list())), length(all_pairs))
  days_removed_py <- list()
  
  liquid <- list()
  
  bid_ask_list <- list()
  
  for(i in 1:length(all_pairs)){
    
    
    
    # Read in temporary dataset
    data_temp <- all_pairs[[i]]
    
    print(paste(as.character(i)," Cleaning pair: ",  names(all_pairs)[i], sep = ""))
    
    clean_logret <- data_temp_xts$logret
    
    
 # Make sure first day in dataset starts at 09:05pm and last day ends at 09:00pm   
    period <- as.numeric(unlist(periodicity(clean_logret)[1]))
    index_first9pm <- as.numeric(which(.indexhour(clean_logret)==21 & .indexmin(clean_logret) == 5)[1])
    if(index_first9pm>1){
      clean_logret<- clean_logret[(index_first9pm):nrow(clean_logret),]}
    
    index_last9pm <- as.numeric(which(.indexhour(clean_logret)==21 & .indexmin(clean_logret) == 0))
    index_last9pm <- index_last9pm[length(index_last9pm)] 
    clean_logret <- clean_logret[1:(index_last9pm),] 
    
# Holidays
#-------------------------------------------------------------------------------------------------------------------
    # Take out US, JAP, UK, SWiss, and GER
    remove_counter <- nrow(clean_logret)
    
   
    #holidays_2 <- "SouthAfrica"
    
    holiday_str_1 <-  holidayList(calendar=holidays, from=anydate(first(index(clean_logret))), to=anydate(last(index(clean_logret))))
    #holiday_str_2 <-  holidayList(calendar= "Japan", from=anydate(first(index(clean_logret))), to=anydate(last(index(clean_logret))))
    #holiday_str_3 <-  holidayList(calendar= "UnitedKingdom", from=anydate(first(index(clean_logret))), to=anydate(last(index(clean_logret))))
    #holiday_str_4 <-  holidayList(calendar= "Switzerland", from=anydate(first(index(clean_logret))), to=anydate(last(index(clean_logret))))
    #holiday_str_5 <-  holidayList(calendar= "Germany/Eurex", from=anydate(first(index(clean_logret))), to=anydate(last(index(clean_logret))))
    holiday_all <- unique(holiday_str_1)
    
    
    #holiday_all <- unique(c(holiday_str_1,holiday_str_2,holiday_str_3,holiday_str_4,holiday_str_5))
    
    
    cut <- vector()
    
    for(j in 1:length(holiday_all)){
      cut[j] <- paste(holiday_all[j]-1, " 21:05", "/", holiday_all[j], " 21:00", sep = "")
          }    
      rmrow <- clean_logret[cut, which.i = TRUE]
      clean_logret <- clean_logret[-rmrow, ]
    
      if (diagnose==TRUE){
        assign("clean_logret", clean_logret, environment(diagnostic))
        diag_holidays <- diagnostic(clean_logret)
      } 
      
      obs_removed[1,1] <- remove_counter - nrow(clean_logret)
      obs_removed[1,2] <- obs_removed[1,1]/288

      
      
      # Check nr of days per year and read to dates modified list
      
      dates_modified_list[[i]][["holidays"]] <- holiday_all
      
        years <- as.factor(year(holiday_all))
        years <- summary(years)
        if(length(years) < ncol(days_removed_df)){ years <- c(rep(NA,(ncol(days_removed_df) - length(years))), years) }
        days_removed_df[1,] <- years
        
      
       
      
# Remove weekends
#--------------------------------------------------------------------------------------------------------------------
      remove_counter <- nrow(clean_logret)
      
      if(weekend_strip==TRUE){
        # Exclude Friday 21:05 to Sunday 21:00, inclusive
        # (see https://stackoverflow.com/questions/28922337/excluding-hours-and-days-in-xts)
        wday <- .indexwday(clean_logret)             # Weekday for each observation
        hh <- .indexhour(clean_logret)               # Hour for each observation
        mm <- .indexmin(clean_logret)                # Minute for each observation
        week.subset <-
          !((wday == 5 & hh >= 22) |           # Remove Fridays from 22:00
              (wday == 6) |                      # Remove Saturdays all day
              (wday == 0 & hh < 21) |            # Remove Sundays until 20:55
              (wday == 5 & hh == 21 & mm > 00) | # Remove Fridays 21:05--21:55
              (wday == 0 & hh == 21 & mm == 00)) # Remove Sundays 21:00
        clean_logret <- clean_logret[week.subset,]
      }
      
      
      
      if (diagnose==TRUE){
        assign("clean_logret", clean_logret, environment(diagnostic))
        diag_weekends <- diagnostic(clean_logret)
      } 
      
      obs_removed[3,1] <- remove_counter - nrow(clean_logret)
      obs_removed[3,2] <- obs_removed[3,1]/288      
      
      
    
# Remove zeros 
#-------------------------------------------------------------------------------------------------------------------
    period <- as.numeric(unlist(periodicity(clean_logret)[1]))
    endpts <- seq(0, nrow(clean_logret),by=((24*60)/period))
    
    ep <- endpts
    sp <- (ep + 1)[-length(ep)]
    ep <- ep[-1]
    clean_logret_split <- lapply(1:length(ep), function(X) clean_logret[sp[X]:ep[X]])
    
    intervals_per_day <- (24*60)/unlist(periodicity(clean_logret)[1])
    # Apply the rule that will remove a trading day if there are more 0's than a certain threshold
    index_zeros <- which(lapply(clean_logret_split, function(x){length(which(x[,"logret"]==0))}) > intervals_per_day*zero_tolerance)
    
    # Check nr of days per year and read to dates modified list
      # Get all the dates that are modified
      if(length(index_zeros) != 0){  
      date_zero_vec <- vector()
          for(j in 1:length(index_zeros)){
            temp_zero <- clean_logret_split[[index_zeros[j]]]
            last_zero <- index(last(temp_zero))
            date_zero_vec[j] <- format(as.Date(last_zero))
          }
        dates_modified_list[[i]][["zeros"]] <- date_zero_vec
        
        # Get days removed per year  
        years <- as.factor(year(as.Date(date_zero_vec)))
        years <- summary(years)
        if(length(years) < ncol(days_removed_df)){ years <- c(rep(NA,(ncol(days_removed_df) - length(years))), years) }
        days_removed_df[2,] <- years
        
       } else{dates_modified_list[[i]][["zeros"]] <- vector()}
      
        
        
    
    #clean_logret <- do.call(rbind, clean_logret_split[-index_zeros])
    # Note: Zero days are not removed. All observations in r_moment is filled with NA on zero days so that they can be na.locf
        
        
    if (diagnose==TRUE){
      assign("clean_logret", clean_logret, environment(diagnostic))
      diag_consec_zeros <- diagnostic(clean_logret)
    }
    
    
# Remove outliers
#-------------------------------------------------------------------------------------------------------------------
    # Perform 99% wins
    # Roughly remove 
    
    if (outlier_wins == TRUE){
    remove_counter <- nrow(clean_logret)
    
    per_99 <- quantile(clean_logret, probs = c(0.9999,0.0001), na.rm = TRUE)
    
    dates_modified_list[[i]][["outliers"]] <- index(clean_logret[which(abs(clean_logret$logret)> per_99)])
    years <- as.factor(year(index(clean_logret[which(abs(clean_logret$logret)> per_99)])))
    years <- summary(years)
    if(length(years) < ncol(days_removed_df)){ years <- c(rep(NA,(ncol(days_removed_df) - length(years))), years) }
    days_removed_df[3,] <- years
    
    clean_logret[clean_logret$logret > per_99] <- per_99
    clean_logret[clean_logret$logret < -per_99] <- -per_99
    
    obs_removed[4,1] <- nrow(clean_logret[abs(clean_logret$logret)> per_99])  
    obs_removed[4,2] <- NA
    
    } else{dates_modified_list[[i]][["outliers"]] <- list()}    

# Spliced USDNZD and USDAUD
#-------------------------------------------------------------------------------------------------------------------  
  
    # Check 
        
        if (names(all_pairs)[i] %in% c("NZDUSD","AUDUSD")){
          invert_bid <- 1/data_temp$Close.Bid
          invert_ask <- 1/data_temp$Close.Ask
          
          invert_midp <- (invert_bid+invert_ask)/as.numeric(2)
          
          invert_logret <- diff(log(invert_midp))
          
          chk_index <- index(clean_logret)
          
          invert_logret <- invert_logret[chk_index]
          
            if(identical(index(invert_logret$Close.Bid),index(clean_logret$logret)) == FALSE ){print("Spliced index matching FAILED")}
          
          clean_logret$logret <- invert_logret$Close.Bid
          
        }
        

    saveRDS(clean_logret, "./5minrZARlogret.rds")
    
# Calculate moments
#-------------------------------------------------------------------------------------------------------------------  

    period <- as.numeric(unlist(periodicity(clean_logret)[1]))
    endpts <- seq(0, nrow(clean_logret),by=((24*60)/period))
    N_id <- diff(endpts)
    
    # Calculate realized variance
    rmoment <- period.apply(clean_logret^2,INDEX = endpts,FUN=sum)  
    colnames(rmoment) <- "rvar"
    rmoment$rvar[rmoment$rvar==0] <- NA # get rid of 0 rvar with NA
    
    # Need to scale rvar and realized volatility
    rmoment$rvar_scaled <- 10000*rmoment$rvar*260 # Annualized variance units
    rmoment$rvol_scaled <- sqrt(rmoment$rvar_scaled) # Annualized percent
    
    # Compute realised skewness
    tmp1 <- period.apply(clean_logret$logret^3,INDEX=endpts,FUN=sum)
    tmp2 <- sqrt(N_id)/rmoment$rvar^(3/2)
    rmoment$rskew <- tmp1*tmp2
    rm(tmp1,tmp2)
    
    # Compute realised kurtosis
    tmp1 <- period.apply(clean_logret$logret^4,INDEX=endpts,FUN=sum)
    tmp2 <- N_id/rmoment$rvar^2
    rmoment$rkurt <- tmp1*tmp2
    rm(tmp1,tmp2)
    
    # Convert returns to daily
    daily_returns <- period.apply(clean_logret$logret,INDEX=endpts,FUN=mean)
    rmoment$logret <- daily_returns
    
    # Over-write days with too many zeros 
    rmoment[index_zeros,] <- NA
    remove_counter <- nrow(rmoment)
    obs_removed[2,1] <- length(which(is.na(rmoment)))*288
    obs_removed[2,2] <- obs_removed[2,1]/288
    
    # Liquidity
    liquid_temp <- (data_temp$Close.Ask-data_temp$Close.Bid)/data_temp$midp
    period_l <- as.numeric(unlist(periodicity(liquid_temp)[1]))
    endpts_l <- seq(0, nrow(liquid_temp),by=((24*60)/period_l))
    liquid[[i]] <- period.apply(liquid_temp,INDEX=endpts_l,FUN=mean)
    
    # Add bids and asks for all the pairs
    bid_ask <- data_temp$Close.Bid - data_temp$Close.Ask
    colnames(bid_ask) <- "spread"
    
    period_ba <- as.numeric(unlist(periodicity(bid_ask)[1]))
    endpts_ba <- seq(0, nrow(bid_ask),by=((24*60)/period_ba))
    
    bid_ask <- period.apply(bid_ask$spread,INDEX=endpts_ba,FUN=mean)
    bid_ask$bid <- period.apply(data_temp$Close.Bid,INDEX=endpts_ba,FUN=mean)
    bid_ask$ask <- period.apply(data_temp$Close.Ask,INDEX=endpts_ba,FUN=mean) 
    
    bid_ask <- bid_ask[index(rmoment)]
    bid_ask_list[[i]] <- bid_ask
    
    if(outlier_wins==TRUE){
    for (j in 1:ncol(bid_ask)) {
      per_99_ba <- quantile(bid_ask[,j], probs = c(0.9999,0.0001), na.rm = TRUE)
      bid_ask[(bid_ask[,j] > per_99_ba[1]), j] <- per_99_ba[1]
      bid_ask[(bid_ask[,j] < per_99_ba[2]), j] <- per_99_ba[2]
      }  
      bid_ask <- bid_ask[index(rmoment)]
      bid_ask_list[[i]] <- bid_ask
    }
    
# Write the moments to a list and store them
#--------------------------------------------------------------------------------------------------------------------
    if(diagnose==TRUE){
    diag_all <- cbind(diag_holidays, diag_consec_zeros, diag_weekends)
    colnames(diag_all) <- c("Holidays clean","Zero tol clean","Weekend clean")} else {diag_all <- list()}
    
    # Calculate total obs removed
    obs_removed[5,1] <- obs_removed[1,1]+obs_removed[2,1]+obs_removed[3,1]
    obs_removed[5,2] <- obs_removed[5,1]/288
    
    # Add calculated moments, diagnostics, and removed stats to a list
    r_moments_list[[i]] <- rmoment
    obs_removed_list[[i]] <- obs_removed
    diagnose_list[[i]] <- diag_all
    
    # Write days removed per year to list
    days_removed_py[[i]] <- days_removed_df
        
    
# Assign names to all the objects inside the lists
#--------------------------------------------------------------------------------------------------------------------
    
    name_temp <- names(all_pairs)[i]
    
    # Rename the shorter USDNZD and USDAUD series. 
    if (names(all_pairs)[i] == "NZDUSD"){ name_temp <- "USDNZD"}
    if (names(all_pairs)[i] == "USDNZD"){ name_temp <- "USDNZD_short"}
    if (names(all_pairs)[i] == "AUDUSD"){ name_temp <- "USDAUD"}
    if (names(all_pairs)[i] == "USDAUD"){ name_temp <- "USDAUD_short"}  
    
    names(obs_removed_list)[i] <- name_temp
    names(diagnose_list)[i] <- name_temp
    names(r_moments_list)[i] <-name_temp 
    
    names(liquid)[i] <- name_temp
    
    names(bid_ask_list)[i] <- name_temp
    #!!!! ADD NAMES FOR DAYS REMOVED 2 LISTS
    names(days_removed_py)[i] <- name_temp
    names(dates_modified_list)[i] <- name_temp
    
    # Remove objects that do not need to be stored
    rm(data_temp)
    
    
  }# end for loop all_pairs
  #names(all_pairs)
  
  output <- list(r_moments_list,
                 obs_removed_list,
                 diagnose_list,
                 days_removed_py,
                 dates_modified_list,
                 liquid,
                 bid_ask_list)
  names(output) <- c("rmoments","nrobs_changed","diagnostic","days_changed_py","dates_modified",
                     "liquidity", "bid_ask")
  
  return(output)
   
}


