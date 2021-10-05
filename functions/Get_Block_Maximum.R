#______________________________________________________________________________#
#####CODE TO GET BLOCK ANNUAL MAXIMA#####

###INPUT
#1. Time Series of Daily CDD/HDD (Single Site)
#2. Start_Date
#3. Block Size (1 day, 3 day or 7 days)
#4. Time_Step (Hourly or Daily)


###OUTPUT
#1. Time Series of Annual Block Maxima Values and Dates


get_block_maxima <- function(t_series, st_date, block_size, Time_Step){
  
  #Get the Dates
  if(Time_Step == "Hourly"){
    start_date <- as.POSIXlt(st_date, format="%m-%d-%Y %H:%M")
    time_stamps <- seq(start_date, by = "hour", length.out = length(t_series))
  } else {
    start_date <- as.POSIXlt(st_date, format="%m-%d-%Y")
    time_stamps <- seq(start_date, by = "day", length.out = length(t_series))
  }
  yrs_stamps <- as.numeric(format(time_stamps,"%Y"))
  
  #Compute the moving average
  df <- data.frame(Metric = t_series, Year = yrs_stamps, Dates = time_stamps)
  df$Block <- rollsum(df$Metric, block_size, align = "center", fill = NA)
  df$Metric <- NULL
  df <- df[complete.cases(df), ]
  
  #Compute the Annual Maximum
  df_ann_max <- df %>% group_by(Year) %>% slice_max(Block, n = 1, 
                                                    with_ties = FALSE)
  
  return(df_ann_max)

  
}




     