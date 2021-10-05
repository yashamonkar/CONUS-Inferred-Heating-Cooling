#______________________________________________________________________________#
#Function to find the # of days between specified date and list of dates

###INPUT
#1. Data_Matrix
#2. Reference Dates
#3. Type (For self reference)


###Output
#1. Data_Matrix of number of days. 


get_num_of_days <- function(Data_Matrix, Ref_Date, Type){
  
  
  #Set-up Storage
  Data_Dates <- matrix(NA, ncol = ncol(Data_Matrix),
                       nrow = nrow(Data_Matrix))
  
  for(j in 1:ncol(Data_Matrix)){
    
    for(i in 1:nrow(Data_Matrix)){
      
      #Convert current values to date and get the year
      td <- as.POSIXct(Data_Matrix[i,j])
      yr <- as.numeric(format(td, format="%Y"))
      mn <- as.numeric(format(td, format="%m"))
      
      #Adjust Reference Year for HDD
      if(Type == "HDD"){
        if(mn < 7){
        yr = yr-1 
        } else {
          yr = yr
        }
      }
      
      #Adding the year to the reference date
      ref <- as.POSIXct(paste0(Ref_Date,"-",yr, "00:00"),
                        format = "%m-%d-%Y %H:%M")
      
      #Compute the difference in days
      
      Data_Dates[i,j] <- difftime(td,ref,units ="days")
      
    }
    
  }
  
  return(Data_Dates)
}