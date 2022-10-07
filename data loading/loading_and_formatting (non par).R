# Notes
#------------------------------------------------------------------------------------------------------
# The function loads in all datasets that are in a specific folder 

# Some basic cleaning operations are performed:
  # 1) x.RIC, Domain and Type are fields that are not needed and removed
  # 2) The date field is converted to POSIXct, GMT
  # 3) Each dataframe is converted to xts 

# All the xts objects for each currecncy pair is stored in a named list with the name corresponding to the 
# currency pairs.

# Function parameters
#------------------------------------------------------------------------------------------------------
# 1) path is the directory in which the data is stored.
# 2) period_conversion is one of 1, 5 or 30. This converts the periodicity of the dataset. 





# Properties of the function that needs to be added
#------------------------------------------------------------------------------------------------------
# Need to keep track of the following:


#------------------------------------------------------------------------------------------------------
# Issues??
# 
#------------------------------------------------------------------------------------------------------

require("pacman")
p_load(xts, tidyverse, data.table, doParallel, foreach)
#path = "E:/Dropbox/Data/Dec/"
path = "../Data/Jan"

load_and_format <- function(path, period_conversion, write_to){
  # Obtain the file names of all the files inside the specific folder
  file_names <- list.files(path = path, pattern = "*.csv.gz",
                           full.names = T) 
  
  data_temp <- read_csv("D:/Dropbox/Functions external/VRP/Data/Jan/USDZAR_Jan.csv.gz", col_types = cols(`Date-Time` = col_character()))# Read in all the files from the folder. Each file will be stored with as a seperate object
  fxpairs_list <- list() # empty list that the fx data will be stored into 
  periods <- list() # stores the original periodicity of each data 
  start_date <- list() # stores the start date of each dataset
  end_date <- list() # stores the end date of each dataset
  
  
  
  
  for(i in 1:length(file_names)){


  period_conversion <- 5 
  
  data_temp <- fread("D:/Dropbox/Functions external/VRP/Data/Jan/USDZAR_Jan.csv.gz", stringsAsFactors = FALSE)
   data_temp <- data_temp[, !(c("#RIC", "Domain","Type"))]
   data_temp$`Date-Time` <- sub("T"," ", data_temp$`Date-Time`)
   data_temp$`Date-Time` <- sub("Z", "", data_temp$`Date-Time`)
   
   # Format date to be rownames in dataframe and remove date column
      #date <- as.POSIXct(data_temp$`Date-Time`, "GMT")
      # warning if gmt offset is not 0 
      #rownames(data_temp) <- date
      data_temp$`Date-Time` <- as.POSIXct(data_temp$`Date-Time`, "GMT")
      
      # Check which cols is not numeric and convert them 
      #data_temp[, lapply(data_temp, is.numeric) == FALSE, with = FALSE]
      num_col <- names(which(unlist(lapply(data_temp, is.numeric))==FALSE))
      num_col <- num_col[2:length(num_col)] # To remove date as a column that is not numeric
      
      data_temp[ , (num_col) := lapply(.SD, as.numeric), .SDcols = num_col]
      
      data_temp_xts <- as.xts(data_temp)
    
    # Capture the periodicity of each dataset and convert to appropriate periodicity---------------    
    periods[[1]] <- unlist(periodicity(data_temp_xts)[1]) # Store the original period
  
      if ((unlist(periodicity(data_temp_xts)[1]) != period_conversion) & (period_conversion==5)){
        data_temp_xts_mean  <- period.apply(data_temp_xts[,c("GMT Offset","Open","Last","Open Bid","High Bid","Low Bid","Close Bid","High Ask",
                                                             "Low Ask","Open Ask","Close Ask")] , endpoints(data_temp_xts, "mins", k=5), mean)
        data_temp_xts_sum  <- period.apply(data_temp_xts[,c("Volume","No. Trades","No. Bids","No. Asks")] , endpoints(data_temp_xts, "mins", k=5), colSums, na.rm=FALSE)
        
        data_temp_xts <- cbind(data_temp_xts_mean,data_temp_xts_sum)
        data_temp_xts <- align.time(data_temp_xts,5*60)
       
        rm(data_temp_xts_mean,data_temp_xts_sum )
      }
      
      if((unlist(periodicity(data_temp_xts)[1]) != period_conversion) & (period_conversion==30)){
        data_temp_xts  <- period.apply(data_temp_xts , endpoints(data_temp_xts, "mins", k=30), mean)
        data_temp_xts <- align.time(data_temp_xts,30*60)
       # colnames(data_temp_xts) <- colnames(data_temp)[-c(1,2)] # Restore column names. For some weird reason
        # to.minutes5 changes the column names
      }
      
      if((unlist(periodicity(data_temp_xts)[1]) != period_conversion) & (period_conversion==1)){
        print(paste(substr(file_names[i], start = 18, stop = (nchar(file_names[i])-7)), "does not have a
                    1 min interval. If period_conversion=1 was chosen and you see this message it means
                    the series read in series has a higher than 1min periodicity and converting to 1min
                    is problematic."))
      }
    
    # Check if there are NA terms in the first rows and remove them--------------------------------
    #  while(anyNA(data_temp_xts[1,c(2,3,4,5)])==TRUE){
     #   data_temp_xts=data_temp_xts[-1,]
    #  }
      
    # Find the start and end date of each dataset and store it-------------------------------------
      start_date[[i]] <- index(data_temp_xts[1,])  
      end_date[[i]] <- index(data_temp_xts[nrow(data_temp_xts),])
      
    # Calculate returns----------------------------------------------------------------------------
      # Do the calculation from 9 to 9
      index_first9pm <- as.numeric(which(.indexhour(data_temp_xts)==21)[1])
      if(index_first9pm>1){
        data_temp_xts <- data_temp_xts[(index_first9pm+1):nrow(data_temp_xts),]}
     
      index_last9pm <- as.numeric(which(.indexhour(data_temp_xts)==21))
      index_last9pm <- index_last9pm[length(index_last9pm)] 
      data_temp_xts <- data_temp_xts[1:(index_last9pm-(60/period_conversion)+1),]
      
      data_temp_xts$midp <- (data_temp_xts$Close.Bid+data_temp_xts$Close.Ask)/as.numeric(2)
      data_temp_xts$logret <- diff(log(data_temp_xts$midp))
      
      
    # Write the xts dataframes to a list----------------------------------------------------------------   
      fxpairs_list[[i]] <- data_temp_xts
    
    # Rename the lists-----------------------------------------------------------------------------
      #fxpair_name <- substr(file_names[i], start = 14, stop = (nchar(file_names[i])-7))
      fxpair_name <- strsplit(strsplit(file_names[i], split = "/")[[1]][4], split = "_")[[1]][1]
      print(c("Reading in pair: ",fxpair_name))
      
      names(fxpairs_list)[i] <- fxpair_name
      names(periods)[i] <- fxpair_name 
      names(start_date)[i] <- fxpair_name
      names(end_date)[i] <- fxpair_name
      
      
    # What this naming does: It renames the list element to contain a sensible name.
    # The start in the substr command will cut out the 18 first characters of the file_name[i],
    # there are 18 characters in "../Data/testzips/", and the stop is set to the length of the 
    # file_names[i] -7 since there are 7 characters in ".csv.gz"
    # Remove temporary objects
      
    # Write each dataset to csv and rds-----------------------------------------------------------------
     #write.csv(as.data.frame(data_temp_xts), file = paste(write_to,"/csv/", fxpair_name, ".csv", sep = "")) 
     #saveRDS(data_temp_xts, file = paste(write_to,"/rds/", fxpair_name, ".rds", sep = ""))
      
      rm(data_temp, data_temp_xts)
  } # end for loop (i in 1:length(file_names)
   


  periods <<- periods
  start_date <<- start_date
  end_date <<- end_date 
  
  return(fxpairs_list)
}# end of function


for (i in 1:length(file_names)){
  print(strsplit(strsplit(file_names[i], split = "/")[[1]][4], split = "_")[[1]][1])
}


