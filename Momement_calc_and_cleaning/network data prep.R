backup <- r_moments_network



library(pacman)
#p_load(quantmod,Rblpapi,readxl,xts,timeDate,quantreg,plotly,ggplot2,stargazer,data.table,dplyr,reshape2,stringr,purrr)
p_load(purrr, readxl, dplyr)

# Add RV from new output

r_moments_list <- output$rmoments
#============================================================================================
# Create USDNZD version that is pushed back further
#============================================================================================



#============================================================================================
# Load and format IV & RV
#============================================================================================

IV <- as.data.frame(read_excel("../IVestimation_code/IV_dataset.xlsx"))


IV[ ,2:ncol(IV)] <- apply(IV[,2:ncol(IV)], 2, function(x) as.numeric(as.character(x)))
colnames(IV)[1] <- "date"

col_counter <- 5
report_IV_data_read <- as.data.frame(matrix(data = NA, nrow = (ncol(IV)/2), ncol = 3))
IV_list <- list()

for (i in 1:20) {
  
  IV_temp <- IV[,c(1,(col_counter-3):col_counter)]
  #report_IV_data_read[i,3] <- sum(is.na(IV_temp[,2]))
  IV_temp <- na.omit(IV_temp)
  pair_name <- colnames(IV_temp)[2]
  colnames(IV_temp)[2:5] <- c("mean",	"st_dev",	"skew",	"kurt")
  
  
  
  
  report_IV_data_read[i,3] <- nrow(IV_temp)
  
  
  rownames(report_IV_data_read)[i] <- pair_name 
  
  report_IV_data_read[i,1] <- as.character(IV_temp[1,1])
  report_IV_data_read[i,2] <- as.character(IV_temp[nrow(IV_temp),1])
  
  IV_list[[i]] <- xts(IV_temp[,-1], order.by = IV_temp[,1])
  names(IV_list)[i] <- pair_name
  
  col_counter <- col_counter+4
  #print(col_counter)
}
colnames(report_IV_data_read) <- c("Start date", "End date", "Nr of observations")



 # RV code-----------------------------------------------------------------------------------------------
  # Create a list with merged dates for all the moments-----------------
    r_moments_list <- pre_network_jan 

    curr_pairs <- names(r_moments_list) 
    curr_pairs <- curr_pairs[-c(2,3,5,9,19)]


  dates_list_rv <- list()
  #dates_list_iv <- list()
  list_count <- 1
  for(i in curr_pairs){
    date_temp_1 <-as.data.frame(as.Date(index(r_moments_list[[i]]))) 
  #  date_temp_2 <-as.data.frame(as.Date(index(IV_list[[i]]))) 
    colnames(date_temp_1) <- "date"
  #  colnames(date_temp_2) <- "date"
    
    dates_list_rv[[list_count]] <- date_temp_1
  # dates_list_iv[[list_count]] <- date_temp_2
    
    
    list_count <- list_count+1
  }

  dates_rv <- reduce(dates_list_rv, inner_join, by = "date")
  #dates_iv <- reduce(dates_list_iv, inner_join, by = "date") # Technically dont need these dates
  
  # Check that the dates match for all the moments
  for (i in curr_pairs){
    chk <-  all.equal(as.data.frame(as.Date(index(r_moments_list[[i]]))) ,  dates_rv)
    print(paste(i, chk))
  }
  
    dates_rv_xts <- as.xts(dates_rv[,-1], order.by = dates_rv$date)

    #IV_network <- list()  
    #for(i in 1:length(IV_list)){
    #  IV_temp <- IV_list[[i]]
    #  IV_network[[i]] <- merge.xts(as.xts(dates_rv[,-1], order.by = dates_rv[,1]), IV_temp,  join="left")
    #  names(IV_network)[i] <- names(IV_list)[i]
    #}
    
    
    
    # Check if it worked
    
    
    #for (i in 1:length(IV_network)){
    #print(which(index(IV_network[[i]])!= dates_rv$date))
    #}
    
    # Merge the moments-------
    
    r_moments_network <- list()
    list_count <- 1
    for (i in curr_pairs){
        moment_temp <-  r_moments_list[[i]]
        index(moment_temp) <- as.Date(index(moment_temp))
        moment_temp <- merge.xts(as.xts(dates_rv[,-1], order.by = dates_rv[,1]), 
                                 moment_temp,  join="left")
        
        r_moments_network[[list_count]] <- moment_temp
        names(r_moments_network)[list_count] <- i
        list_count <- list_count+1
        
    }
    
     
    # Check if the moments were merged correctly on the dates
    for(i in 1:length(r_moments_network)){
      print(which(index(r_moments_network[[i]]) != dates_rv$date))
    }
    
  

    # Check for na in r_moments
    for(i in curr_pairs){
      print(i)
      print(colSums(is.na(r_moments_network[[i]])))
    }
    
    
    # Check if IV and RV have same dates
    #for(i in curr_pairs){
    #  print(which(index(r_moments_network[[i]]) != index(IV_network[[i]])))
    #}
    
    # Check the number of NA per years ------------------
    days_per_year_na <- as.data.frame(matrix(data=0,nrow = length(r_moments_network), ncol = (2022-1996+1)))
    colnames(days_per_year_na) <- 1996:2022
    # Check nr of days per year
    for (i in 1:length(r_moments_network)){
     moment_temp <- r_moments_network[[i]]
     na_index <- which(is.na(moment_temp$logret)) 
     years <- as.factor(year(moment_temp[na_index,]))
     years <- summary(years)
     days_per_year_na[i, names(years)] <- years
     rownames(days_per_year_na)[i] <- names(r_moments_network)[i]
     }
    
    
    
    # Check the number of obs per years ------------------
    days_per_year_network <- as.data.frame(matrix(data=NA,nrow = length(r_moments_network), ncol = (2022-1996+1)))
    colnames(days_per_year_network) <- 1996:2022
    # Check nr of days per year
    for (i in 1:length(r_moments_network)){
      years <- as.factor(year(r_moments_network[[i]]))
      years <- summary(years)
      days_per_year_network[i, names(years)] <- years
      rownames(days_per_year_network)[i] <- names(r_moments_network)[i]
      
    }
    
    # Fill in NA's in r_moments_network with na.locf
    r_moments_network_noNA <- lapply(r_moments_network, na.locf)
      # Check if that worked
    for (i in 1:length(r_moments_network_noNA)){
     print(which(is.na(r_moments_network_noNA[[i]])))
    }
         #There is a specific issue where USDCNY starts one year later than the rest and has NA Values up to that point
        # Shift the years for both RV and IV
            r_moments_network_noNA$USDCNY[384] # Check the last NA in USDCNY which is 289
            r_moments_network_noNA <- lapply(r_moments_network_noNA, function(x) x["2006-07-12/"])
            #r_moments_network_noNA <- lapply(r_moments_network_noNA , na.locf)
    
    
    # R moments currently have more dates than IV
      # Remove NA's at end
       # IV_network <- lapply(IV_network, na.omit)
      # Make sure RV end at same date (cheack what the last IV date is)
       # r_moments_network_noNA <- lapply(r_moments_network_noNA, function(x) x["/2020-07-31"])
    
    
    
    # Check for obs with 0 rvar and remove
    
  #  for (i in 1:length(IV_network)){
  #  temp_iv <- IV_network[[i]]
    #print(which(chk$st_dev <= 1E-8))
    
  #  rmrow <- temp_iv[temp_iv$st_dev <= 1E-8, which.i = TRUE]
  #  temp_iv[rmrow,] <- NA
    
  #  IV_network[[i]] <- temp_iv
  #  }
    
  #  IV_network <- lapply(IV_network, na.locf)
    
    
  #  RV_wins <- IV_network
    RV_wins <- r_moments_network_noNA
    
    #names(IV_wins)
    names(RV_wins)
    
  #  IV_wins <- IV_wins[names(RV_wins)]
      
  #  saveRDS(IV_wins, file = "../Network_data/IV_wins.rds")
    saveRDS(RV_nowins, file = "../Network_data/RV_nowins_dec.rds")
    
    
    saveRDS(RV_wins, file = "../Network_data/RV_wins_jan.rds")
    
    
    saveRDS(r_moments_list, file = "../Network_data/RV_nowins_full.rds")
    
    
    IV_nowins_chk <- IV_nowins[names(RV_chk)] 
  
    
    
    
  