#______________________________________________________________________________#
###Field Significance Test###


###INPUT
#1. Data Field (Annual Block Maximum)
#2. Level of Significance


###OUTPUT
#1. Null hypothesis (p-value)


#______________________________________________________________________________#
get_field_significance <- function(Data_Matrix, p_thresh){
  
  #Load the Packages
  library(trend)
  
  
  #MK-Test on the Data
  p_data <- list()
  for(i in 1:ncol(Data_Matrix)){
    test <- mk.test(Data_Matrix[,i], alternative = "two.sided")
    
    if(test$p.value < p_thresh){
      p_data[[i]] <- 1
    } else {
      p_data[[i]] <- 0
    }
  }
  t_sample <- sum(unlist(p_data))
  
  
  #Block Bootstrap
  N_boot <- 1000
  p_boot <- list()
  
  for(jk in 1:N_boot){
    
    #Create the Bootstrap
    nsam <- sample(1:nrow(Data_Matrix),nrow(Data_Matrix), replace = TRUE)
    data_boot <- Data_Matrix[nsam,]
    
    #MK-Test on the Data
    p_temp <- list()
    for(i in 1:ncol(data_boot)){
      test <- mk.test(data_boot[,i], alternative = "two.sided")
      
      if(test$p.value < p_thresh){
        p_temp[[i]] <- 1
      } else {
        p_temp[[i]] <- 0
      }
    }
    p_boot[[jk]] <- sum(unlist(p_temp))
  }
  t_bootstrap <- unlist(p_boot)
  
  
  ###Field Significance Test
  T_star = quantile(t_bootstrap, 1-p_thresh)
  
  if(T_star > t_sample){
    d <- " (Spatial Correlation)"
  } else {
    d <- " (Trends Significant)"
  }
  
  return(d)
}