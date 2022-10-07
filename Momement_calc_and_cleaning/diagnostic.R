# Check that every day starts at 905 and ends at 9 and has 288 obs

diagnostic <- function(input){
  input <- clean_logret
  
  period <- as.numeric(unlist(periodicity(input)[1]))
  endpts <- seq(0, nrow(input),by=((24*60)/period))
  
  ep <- endpts
  sp <- (ep + 1)[-length(ep)]
  ep <- ep[-1]
  input_split <- lapply(1:length(ep), function(X) input[sp[X]:ep[X]])
  
  first_val <- lapply(1:length(input_split), function(x) {index(input_split[[x]][1])})
  check_first_val <- lapply(1:length(first_val), function(x) {(as.numeric(format(first_val[[x]], "%H")) == 21) * (as.numeric(format(first_val[[x]], "%M")) ==5)})
  
  last_val <- lapply(1:length(input_split), function(x) {index(last(input_split[[x]]))})
  check_last_val <- lapply(1:length(last_val), function(x) {(as.numeric(format(last_val[[x]], "%H")) == 21) * (as.numeric(format(last_val[[x]], "%M")) == 0)})
  
  check_row_num <- lapply(1:length(input_split),function(x) { dim(input_split[[x]])[1]})
  
  check_all_first <- as.logical(min(unlist(check_first_val)))
  check_all_last <- as.logical(min(unlist(check_last_val)))
  
  if(min(unlist(check_row_num)) == 288 & max(unlist(check_row_num)) == 288){check_all_rows <- TRUE} else{check_all_rows <- FALSE}
  
  diag_mat <- rbind(check_all_first,check_all_last,check_all_rows)
  rownames(diag_mat) <- c("All days start 9:05pm", "All days end 9:00pm", "All days have 288 obs")
  return(diag_mat)
  
  
}
