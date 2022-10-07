


for (i  in 1:length(pre_network_jan)) {
  
  pair_name <- names(pre_network_jan)[i]
  
  if(pair_name=="USDPLD"){
    pair_name <- "USDPLN"
    names(RV_nowins_1)[i] <- "USDPLN"
  }
  
  print(sum(abs(na.omit(pre_network_jan[[pair_name]]$rvar - pre_network_dec[[pair_name]]$rvar))))
}


for (i  in 1:length(RV_nowins_jan)) {
  
  pair_name <- names(RV_nowins_jan)[i]
  
  if(pair_name=="USDPLD"){
    pair_name <- "USDPLN"
    names(RV_nowins_1)[i] <- "USDPLN"
  }
  
  print(sum(abs(na.omit(RV_nowins_jan[[pair_name]]$logret - RV_nowins_dec[[pair_name]]$logret))))
}
