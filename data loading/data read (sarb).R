library(pacman)
p_load(xts,ggplot2,zoo,lubridate,readxl,stringr,dplyr,DescTools,reshape2,forecast,ggforce)

# Functions for data work----------------------------------------

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# Data read-------------------------------------------------------

  # VRP data---
    imoments_list <- readRDS("./Data/imoments_list.rds")
    RV_nowins <- readRDS("./Data/RV_nowins.rds")
    vrp_list_val <- readRDS("./Data/vrp_list_val.rds")
    
  # Alternative IV's---
    ZAR_IV_returns <- read_excel("Data/ZAR_IV_returns.xlsx", 
                                 sheet = "1m", col_types = c("date", "numeric", 
                                                             "numeric", "numeric", 
                                                             "numeric"))
    
  # SARB data---
    sarb_data <- read_excel_allsheets("./Data/Daan exchange rate.xlsx") 
    
  # SARB historical---
    sarb_hist <- read_excel("./Data/EXDOLLD vintage 2000q1 to 2013q3 .xlsx", 
                                      col_types = c("text", rep("numeric",80)))
  # Consensus---
    consensus <- read_excel("./Data/Consensus.xlsx", 
                            sheet = "Sheet3", col_types = c("numeric", 
                                                            "numeric"))
    
  # Actual rate---
    actual <- read_excel("Data/Daan exchange rate.xlsx", 
                                     col_types = c("text", "numeric", "numeric", 
                                                   "text", "text", "text"))
    

# Data cleaning--------------------------------------------------
    
    # Clean SARB historical---
    sarb_hist_f <- as.data.frame(matrix(data = NA,nrow = nrow(sarb_hist),
                                         ncol = 2))
      for ( i in 2:ncol(sarb_hist)){
        curr_d <- strsplit(colnames(sarb_hist)[i], " ")[[1]][2]
        ind <- which(sarb_hist$Quarterly == curr_d)
        
        sarb_hist_f[i,] <- c(curr_d,sarb_hist[ind,i])
      }
    sarb_hist_f <- na.omit(sarb_hist_f)
   
    # Fix cases where there are the same nr for more than one quarter
      sarb_hist_fn <- as.data.frame(matrix(data = NA, nrow = (length(sarb_hist_f)), 
                                    ncol = 2))
      sarb_hist_fn <- na.omit(sarb_hist_fn)
      colnames(sarb_hist_f) <- c("Date","USDZAR_f")
      
      for (i in 2:nrow(sarb_hist_f)){
        prev_d <- sarb_hist_f$Date[(i-1)]
        curr_d <- sarb_hist_f$Date[i]
      
        prev_v <- sarb_hist_f$USDZAR_f[(i-1)]
        curr_v <- sarb_hist_f$USDZAR_f[i]
      
        if (prev_d==curr_d){
          sarb_hist_fn[i,] <- c(curr_d, ((prev_v+curr_v)/2))
          sarb_hist_fn[(i-1),] <- c(NA,NA)
        } else {sarb_hist_fn[i,] <- c(curr_d,curr_v)}
      }
      sarb_hist_fn[1,] <- sarb_hist_f[1,]
      sarb_hist_fn <- na.omit(sarb_hist_fn)
      colnames(sarb_hist_fn) <- c("Date", "SARB_f")
    
    
    # SARB data---
      sarb_data <- lapply(sarb_data, FUN = function(x) x <- x[9:nrow(x), c(1,2)])
      sarb_data <- lapply(sarb_data, setNames, c("Date", "USDZAR"))
    
      # Get vintages out for each quarter
     
      sarb_f <- as.data.frame(matrix(data = NA, nrow = (length(sarb_data)+10), 
                                     ncol = 2))
      colnames(sarb_f) <- c("Date", "USDZAR_f")
      
      for(i in 1:length(sarb_data)){
        month <- word(names(sarb_data)[i],1)
        if (month %in% c("Jan", "Feb", "Mar")){quarter <- "Q1"}
        if (month %in% c("Apr", "April", "May", "Jun")){quarter <- "Q2"}
        if (month %in% c("Jul","July", "Aug", "Sep")){quarter <- "Q3"}
        if (month %in% c("Oct", "Nov", "Dec")){quarter <- "Q4"}
        
        year <- word(names(sarb_data)[i],-1)
        
        Date <- paste(year,quarter,sep="")
        
        curr <- Date
        ind <- which(sarb_data[[i]]$Date == Date)
          value <- as.numeric(sarb_data[[i]]$USDZAR[ind])
        
        sarb_f[i,] <- c(Date, value)
      } # end for i 
       # Fix cases where there are two numbers for the same quarter
         sarb_fn <- as.data.frame(matrix(data = NA, nrow = (length(sarb_data)), 
                                     ncol = 2))
          sarb_f <- na.omit(sarb_f)
          sarb_f$USDZAR_f <- as.numeric(sarb_f$USDZAR_f) 
          
          for (i in 2:nrow(sarb_f)){
            prev_d <- sarb_f$Date[(i-1)]
            curr_d <- sarb_f$Date[i]
            
            prev_v <- sarb_f$USDZAR_f[(i-1)]
            curr_v <- sarb_f$USDZAR_f[i]
            
            if (prev_d==curr_d){
              sarb_fn[i,] <- c(curr_d, ((prev_v+curr_v)/2))
              sarb_fn[(i-1),] <- c(NA,NA)
            } else {sarb_fn[i,] <- c(curr_d,curr_v)}
          }
          
          sarb_fn <- na.omit(sarb_fn[seq(dim(sarb_fn)[1],1),]) 
          colnames(sarb_fn) <- c("Date", "SARB_f")
          
    # Consensus---
      consensus$Date <- seq(as.Date("2006-01-01"), as.Date("2021-09-01"), "month")
      # Convert to quarterly
     consensus <- consensus %>%
        group_by(year = paste(lubridate::year(Date),quarters(Date))) %>%
        summarise(USDZAR = mean(USDZAR)) 
     
     consensus$year <- lubridate::yq(consensus$year)
     colnames(consensus)[1] <- "Date"
    
    # Actual rate---
      actual <- actual[9:213,]
      actual <- actual[,c(1,2)]
      colnames(actual) <- c("Date","USDZAR")
      
      
  # Compile a main DF to store all values in-------------------------------
      main_df <- sarb_hist_fn
      main_df <- rbind(main_df,sarb_fn[c(16:(nrow(sarb_fn))-1),])
      colnames(main_df) <- c("Date", "SARB_f")
      main_df$USDZAR_a <- actual$USDZAR[121:205]
      main_df$Date <- lubridate::yq(main_df$Date)
      
      main_df <- merge(main_df,consensus,by="Date",all = TRUE)
      colnames(main_df)[4] <- "USDZAR_c"
      
      
# Get forecasts further out--------------------
      
      
 # 2 Q ahead---------------------------------------------------------------------
      # Grab the historicals
      sarb_hist_f2 <- as.data.frame(matrix(data = NA,nrow = nrow(sarb_hist),
                                          ncol = 2))
      for ( i in 2:ncol(sarb_hist)){
        curr_d <- strsplit(colnames(sarb_hist)[i], " ")[[1]][2]
        ind <- which(sarb_hist$Quarterly == curr_d)
        
        sarb_hist_f2[i,] <- c(curr_d,sarb_hist[(ind+1),i])
      }
      sarb_hist_f2 <- na.omit(sarb_hist_f2)
      
      # Fix cases where there are the same nr for more than one quarter
      sarb_hist_fn2 <- as.data.frame(matrix(data = NA, nrow = (length(sarb_hist_f2)), 
                                           ncol = 2))
      sarb_hist_fn2 <- na.omit(sarb_hist_fn2)
      colnames(sarb_hist_f2) <- c("Date","USDZAR_f")
      
      for (i in 2:nrow(sarb_hist_f2)){
        prev_d <- sarb_hist_f2$Date[(i-1)]
        curr_d <- sarb_hist_f2$Date[i]
        
        prev_v <- sarb_hist_f2$USDZAR_f[(i-1)]
        curr_v <- sarb_hist_f2$USDZAR_f[i]
        
        if (prev_d==curr_d){
          sarb_hist_fn2[i,] <- c(curr_d, ((prev_v+curr_v)/2))
          sarb_hist_fn2[(i-1),] <- c(NA,NA)
        } else {sarb_hist_fn2[i,] <- c(curr_d,curr_v)}
      }
      sarb_hist_fn2[1,] <- sarb_hist_f2[1,]
      sarb_hist_fn2 <- na.omit(sarb_hist_fn2)
      colnames(sarb_hist_fn2) <- c("Date", "SARB_f")
      
      # 2010 file
      sarb_f2 <- as.data.frame(matrix(data = NA, nrow = (length(sarb_data)+10), 
                                     ncol = 2))
      colnames(sarb_f2) <- c("Date", "USDZAR_f")
      
      for(i in 1:length(sarb_data)){
        month <- word(names(sarb_data)[i],1)
        if (month %in% c("Jan", "Feb", "Mar")){quarter <- "Q1"}
        if (month %in% c("Apr", "April", "May", "Jun")){quarter <- "Q2"}
        if (month %in% c("Jul","July", "Aug", "Sep")){quarter <- "Q3"}
        if (month %in% c("Oct", "Nov", "Dec")){quarter <- "Q4"}
        
        year <- word(names(sarb_data)[i],-1)
        
        Date <- paste(year,quarter,sep="")
        
        curr <- Date
        ind <- which(sarb_data[[i]]$Date == Date)
        value <- as.numeric(sarb_data[[i]]$USDZAR[ind+1])
        
        sarb_f2[i,] <- c(Date, value)
      } # end for i 
      # Fix cases where there are two numbers for the same quarter
      sarb_fn2 <- as.data.frame(matrix(data = NA, nrow = (length(sarb_data)), 
                                      ncol = 2))
      sarb_f2 <- na.omit(sarb_f2)
      sarb_f2$USDZAR_f <- as.numeric(sarb_f2$USDZAR_f) 
      
      for (i in 2:nrow(sarb_f2)){
        prev_d <- sarb_f2$Date[(i-1)]
        curr_d <- sarb_f2$Date[i]
        
        prev_v <- sarb_f2$USDZAR_f[(i-1)]
        curr_v <- sarb_f2$USDZAR_f[i]
        
        if (prev_d==curr_d){
          sarb_fn2[i,] <- c(curr_d, ((prev_v+curr_v)/2))
          sarb_fn2[(i-1),] <- c(NA,NA)
        } else {sarb_fn2[i,] <- c(curr_d,curr_v)}
      }
      
      sarb_fn2 <- na.omit(sarb_fn2[seq(dim(sarb_fn2)[1],1),]) 
      colnames(sarb_fn2) <- c("Date", "SARB_f")      
      
      # Merge the historical and current together
      
      
      for_q2 <- rbind(sarb_hist_fn2[1:39,],sarb_fn2)
      for_q2$Date <- as.Date(as.yearqtr(for_q2$Date)+1/4)      

# Q 3 -------------------------------------------------------------
      # Grab the historicals
      sarb_hist_f3 <- as.data.frame(matrix(data = NA,nrow = nrow(sarb_hist),
                                           ncol = 2))
      for ( i in 2:ncol(sarb_hist)){
        curr_d <- strsplit(colnames(sarb_hist)[i], " ")[[1]][2]
        ind <- which(sarb_hist$Quarterly == curr_d)
        
        sarb_hist_f3[i,] <- c(curr_d,sarb_hist[(ind+2),i])
      }
      sarb_hist_f3 <- na.omit(sarb_hist_f3)
      
      # Fix cases where there are the same nr for more than one quarter
      sarb_hist_fn3 <- as.data.frame(matrix(data = NA, nrow = (length(sarb_hist_f3)), 
                                            ncol = 2))
      sarb_hist_fn3 <- na.omit(sarb_hist_fn3)
      colnames(sarb_hist_f3) <- c("Date","USDZAR_f")
      
      for (i in 2:nrow(sarb_hist_f3)){
        prev_d <- sarb_hist_f3$Date[(i-1)]
        curr_d <- sarb_hist_f3$Date[i]
        
        prev_v <- sarb_hist_f3$USDZAR_f[(i-1)]
        curr_v <- sarb_hist_f3$USDZAR_f[i]
        
        if (prev_d==curr_d){
          sarb_hist_fn3[i,] <- c(curr_d, ((prev_v+curr_v)/2))
          sarb_hist_fn3[(i-1),] <- c(NA,NA)
        } else {sarb_hist_fn3[i,] <- c(curr_d,curr_v)}
      }
      sarb_hist_fn3[1,] <- sarb_hist_f3[1,]
      sarb_hist_fn3 <- na.omit(sarb_hist_fn3)
      colnames(sarb_hist_fn3) <- c("Date", "SARB_f")
      
      # 2010 file
      sarb_f3 <- as.data.frame(matrix(data = NA, nrow = (length(sarb_data)+10), 
                                      ncol = 2))
      colnames(sarb_f3) <- c("Date", "USDZAR_f")
      
      for(i in 1:length(sarb_data)){
        month <- word(names(sarb_data)[i],1)
        if (month %in% c("Jan", "Feb", "Mar")){quarter <- "Q1"}
        if (month %in% c("Apr", "April", "May", "Jun")){quarter <- "Q2"}
        if (month %in% c("Jul","July", "Aug", "Sep")){quarter <- "Q3"}
        if (month %in% c("Oct", "Nov", "Dec")){quarter <- "Q4"}
        
        year <- word(names(sarb_data)[i],-1)
        
        Date <- paste(year,quarter,sep="")
        
        curr <- Date
        ind <- which(sarb_data[[i]]$Date == Date)
        value <- as.numeric(sarb_data[[i]]$USDZAR[ind+2])
        
        sarb_f3[i,] <- c(Date, value)
      } # end for i 
      # Fix cases where there are two numbers for the same quarter
      sarb_fn3 <- as.data.frame(matrix(data = NA, nrow = (length(sarb_data)), 
                                       ncol = 2))
      sarb_f3 <- na.omit(sarb_f3)
      sarb_f3$USDZAR_f <- as.numeric(sarb_f3$USDZAR_f) 
      
      for (i in 2:nrow(sarb_f3)){
        prev_d <- sarb_f3$Date[(i-1)]
        curr_d <- sarb_f3$Date[i]
        
        prev_v <- sarb_f3$USDZAR_f[(i-1)]
        curr_v <- sarb_f3$USDZAR_f[i]
        
        if (prev_d==curr_d){
          sarb_fn3[i,] <- c(curr_d, ((prev_v+curr_v)/2))
          sarb_fn3[(i-1),] <- c(NA,NA)
        } else {sarb_fn3[i,] <- c(curr_d,curr_v)}
      }
      
      sarb_fn3 <- na.omit(sarb_fn3[seq(dim(sarb_fn3)[1],1),]) 
      colnames(sarb_fn3) <- c("Date", "SARB_f")      
      
      # Merge the historical and current together
      
      
      for_q3 <- rbind(sarb_hist_fn3[1:39,],sarb_fn3)
      for_q3$Date <- as.Date(as.yearqtr(for_q3$Date)+2/4)  
      
# 4 Quarters ahead-------------------------------------------------------
      
      # Grab the historicals
      sarb_hist_f4 <- as.data.frame(matrix(data = NA,nrow = nrow(sarb_hist),
                                           ncol = 2))
      for ( i in 2:ncol(sarb_hist)){
        curr_d <- strsplit(colnames(sarb_hist)[i], " ")[[1]][2]
        ind <- which(sarb_hist$Quarterly == curr_d)
        
        sarb_hist_f4[i,] <- c(curr_d,sarb_hist[(ind+3),i])
      }
      sarb_hist_f4 <- na.omit(sarb_hist_f4)
      
      # Fix cases where there are the same nr for more than one quarter
      sarb_hist_fn4 <- as.data.frame(matrix(data = NA, nrow = (length(sarb_hist_f4)), 
                                            ncol = 2))
      sarb_hist_fn4 <- na.omit(sarb_hist_fn4)
      colnames(sarb_hist_f4) <- c("Date","USDZAR_f")
      
      for (i in 2:nrow(sarb_hist_f4)){
        prev_d <- sarb_hist_f4$Date[(i-1)]
        curr_d <- sarb_hist_f4$Date[i]
        
        prev_v <- sarb_hist_f4$USDZAR_f[(i-1)]
        curr_v <- sarb_hist_f4$USDZAR_f[i]
        
        if (prev_d==curr_d){
          sarb_hist_fn4[i,] <- c(curr_d, ((prev_v+curr_v)/2))
          sarb_hist_fn4[(i-1),] <- c(NA,NA)
        } else {sarb_hist_fn4[i,] <- c(curr_d,curr_v)}
      }
      sarb_hist_fn4[1,] <- sarb_hist_f4[1,]
      sarb_hist_fn4 <- na.omit(sarb_hist_fn4)
      colnames(sarb_hist_fn4) <- c("Date", "SARB_f")
      
      # 2010 file
      sarb_f4 <- as.data.frame(matrix(data = NA, nrow = (length(sarb_data)+10), 
                                      ncol = 2))
      colnames(sarb_f4) <- c("Date", "USDZAR_f")
      
      for(i in 1:length(sarb_data)){
        month <- word(names(sarb_data)[i],1)
        if (month %in% c("Jan", "Feb", "Mar")){quarter <- "Q1"}
        if (month %in% c("Apr", "April", "May", "Jun")){quarter <- "Q2"}
        if (month %in% c("Jul","July", "Aug", "Sep")){quarter <- "Q3"}
        if (month %in% c("Oct", "Nov", "Dec")){quarter <- "Q4"}
        
        year <- word(names(sarb_data)[i],-1)
        
        Date <- paste(year,quarter,sep="")
        
        curr <- Date
        ind <- which(sarb_data[[i]]$Date == Date)
        value <- as.numeric(sarb_data[[i]]$USDZAR[ind+3])
        
        sarb_f4[i,] <- c(Date, value)
      } # end for i 
      # Fix cases where there are two numbers for the same quarter
      sarb_fn4 <- as.data.frame(matrix(data = NA, nrow = (length(sarb_data)), 
                                       ncol = 2))
      sarb_f4 <- na.omit(sarb_f4)
      sarb_f4$USDZAR_f <- as.numeric(sarb_f4$USDZAR_f) 
      
      for (i in 2:nrow(sarb_f4)){
        prev_d <- sarb_f4$Date[(i-1)]
        curr_d <- sarb_f4$Date[i]
        
        prev_v <- sarb_f4$USDZAR_f[(i-1)]
        curr_v <- sarb_f4$USDZAR_f[i]
        
        if (prev_d==curr_d){
          sarb_fn4[i,] <- c(curr_d, ((prev_v+curr_v)/2))
          sarb_fn4[(i-1),] <- c(NA,NA)
        } else {sarb_fn4[i,] <- c(curr_d,curr_v)}
      }
      
      sarb_fn4 <- na.omit(sarb_fn4[seq(dim(sarb_fn4)[1],1),]) 
      colnames(sarb_fn4) <- c("Date", "SARB_f")      
      
      # Merge the historical and current together
      
      for_q4 <- rbind(sarb_hist_fn4[1:39,],sarb_fn4)
      for_q4$Date <- as.Date(as.yearqtr(for_q4$Date)+3/4)  
      
      

      
      
      
          