#______________________________________________________________________________#
####Objective :- Code to read the ERA-5 Shape Files and get the block maxima

###This is done since reading and opening the ERA-5 data is a long process.

###Output Needed
#1. Annual Average. Matrix. Size - 71 x Num of Grid Cells. 
#2. Site-level Block Maxima. Size - 71 x Num of Grid Cells x 6
#3. Site-level Block Maxima Dates. Size - 71 x Num of Grid Cells x 6


#Note: 2-3 and 4-5 are stored as RData and 1 via Dataframe.

#______________________________________________________________________________#
#Set-up Directory
setwd("~/GitHub/CONUS-Inferred-Heating-Cooling")


#______________________________________________________________________________#
###Load Data and Dependencies###

#Load Dependencies
library(ggplot2)
library(usmap)
library(maps)
library(dplyr)
library(trend)
library(zoo)
library(ncdf4) 
library(doParallel)
library(foreach)
library(broom)
library(rgdal)


#Load the CONUS Locations
all_grids <- get(load("data/NERC_Regions_lat_lon_index_key.RData"))
grid_locs <- bind_rows(lapply(all_grids,data.frame))



#______________________________________________________________________________#
#Hyper-Parameters
yrs <- 1951:2021
thresh_temp <- 291.5 #65 Fahrenheit #
block_sizes <- c(6,12,24,72, 168, 336) #hours


#Plotting the grid points
world <- map_data("world")
us <- map_data("state")


#______________________________________________________________________________#
#Set-up storage 


#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
###Objective 1:- Annual Average. Matrix. Size - 71 x 3285.


###Method
#1. Read the .nc files
#2. Compute the output
#3. Save output as .txt file

###Output Characteristics
#1. Saved as a .txt file
#2. Units of (mean) degree-hours/day


#Storage
Mean_HDD <- matrix(NA, nrow = length(yrs), ncol = nrow(grid_locs))


pb = txtProgressBar(min = 1, max = length(yrs), initial = 1) 
for(y in 1:length(yrs)){
  
  setTxtProgressBar(pb,y)  #Progress Bar
  
  #Current year
  yr <- yrs[y]
  
  #Get the number of hours from Jan-01 to June 30
  st_date <- as.POSIXct(paste0("01-01-",yr," 00:00"), format="%m-%d-%Y %H:%M")
  yr_end <- as.POSIXct(paste0("06-30-",yr," 23:00"), format="%m-%d-%Y %H:%M")
  yr_start <- as.POSIXct(paste0("07-01-",yr-1," 00:00"), format="%m-%d-%Y %H:%M")
  hrs_ahead <- difftime(yr_end,st_date, units = "hours")
  hrs_behind <- difftime(st_date,yr_start, units = "hours")
  
  #-------------------------------------------------------#
  #Read a single ERA-5 download file for previous year
  name <- paste0("data/raw_data/era_5/",yr-1,".nc")
  nc_data <- nc_open(name)
  
  #Get the attributes
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  t2m <- ncvar_get(nc_data, "t2m") 
  t2m <- t2m[,,(dim(t2m)[3]-hrs_behind):dim(t2m)[3]]
  
  #Subset to needed values
  t2m_lag <- matrix(ncol = nrow(grid_locs),
                    nrow = dim(t2m)[3])
  
  for(j in 1:ncol(t2m_lag)){
    lon_gr <- which(lon == grid_locs$Longitude[j])
    lat_gr <- which(lat == grid_locs$Latitude[j])
    
    t2m_lag[,j] <- t2m[lon_gr,lat_gr,]
  }
  
  #Clear the .nc file
  t2m <- NULL
  nc_close(nc_data) #Closing the Netcdf file. 
  
  
  #-------------------------------------------------------#
  #Read a single ERA-5 download file
  name <- paste0("data/raw_data/era_5/",yr,".nc")
  nc_data <- nc_open(name)
  
  #Get the attributes
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  t2m <- ncvar_get(nc_data, "t2m") 
  t2m <- t2m[,,1:hrs_ahead]
  
  #Subset to needed values
  t2m_front <- matrix(ncol = nrow(grid_locs),
                      nrow = dim(t2m)[3])
  
  for(j in 1:ncol(t2m_front)){
    lon_gr <- which(lon == grid_locs$Longitude[j])
    lat_gr <- which(lat == grid_locs$Latitude[j])
    
    t2m_front[,j] <- t2m[lon_gr,lat_gr,]
  }
  
  #Clear the .nc file
  t2m <- NULL
  nc_close(nc_data) #Closing the Netcdf file. 
  
  #Combining the values
  t2m_land <- rbind(t2m_lag,t2m_front)
  t2m_lag <- t2m_front <- NULL
  
  
  #----------------------------------------------------------------------------#
  ###Compute the Annual Average
  HDD <-  thresh_temp - t2m_land
  HDD[HDD<0] <- 0
  HDD_Mean <- colMeans(HDD)
  
  #Storing the data
  Mean_HDD[y,] <- HDD_Mean
}

#Consistency Check
sum(is.na(Mean_HDD))

#Plotting the results - Consistency Check
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -60))+
  scale_y_continuous(name = " ", limits = c(20, 55)) +
  geom_tile(data = grid_locs, aes(x=Longitude, y = Latitude, 
                                  fill = colMeans(Mean_HDD))) +
  scale_fill_gradient2(midpoint=median(colMeans(Mean_HDD)),
                       low="blue", mid="white",high="red",
                       name = "HDD") +
  ggtitle(paste0("Mean CDD - Consistency Check"))
print(p)


#Clean-up for next objectives
HDD <- nc_data <- t2m_land <- NULL





#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
###Objective 2:- Site-level Block Maxima. Size - 71 x 3285 x 6
###Objective 3:- Site-level Block Maxima. Size - 71 x 3285 x 6


###Output Characteristics
#1. Saved as a .RData file
#2. Units of degree-hours/event-duration



###Output Format
#Single List of Lists. 
#[[1]] - Contains all the Values
#[[2]] - Contains all the Dates

#[[1]][[1]] - Data Frame [71 x Num Grid Cells] with ann-max values.



get_hdd <- function(yr, block_size, Grids, thresh_temp){
  
  #Load the Packages
  library(ncdf4)
  library(zoo)
  library(dplyr)
  
  #Source functions
  source("functions/Get_Block_Maximum.R")
  
  #Current year
  
  #Get the number of hours from Jan-01 to June 30
  st_date <- as.POSIXct(paste0("01-01-",yr," 00:00"), format="%m-%d-%Y %H:%M")
  yr_end <- as.POSIXct(paste0("06-30-",yr," 23:00"), format="%m-%d-%Y %H:%M")
  yr_start <- as.POSIXct(paste0("07-01-",yr-1," 00:00"), format="%m-%d-%Y %H:%M")
  hrs_ahead <- difftime(yr_end,st_date, units = "hours")
  hrs_behind <- difftime(st_date,yr_start, units = "hours")
  
  #-------------------------------------------------------#
  #Read a single ERA-5 download file for previous year
  name <- paste0("data/raw_data/era_5/",yr-1,".nc")
  nc_data <- nc_open(name)
  
  #Get the attributes
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  t2m <- ncvar_get(nc_data, "t2m") 
  t2m <- t2m[,,(dim(t2m)[3]-hrs_behind):dim(t2m)[3]]
  
  #Subset to needed values
  t2m_lag <- matrix(ncol = nrow(Grids),
                    nrow = dim(t2m)[3])
  
  for(j in 1:ncol(t2m_lag)){
    lon_gr <- which(lon == Grids$Longitude[j])
    lat_gr <- which(lat == Grids$Latitude[j])
    
    t2m_lag[,j] <- t2m[lon_gr,lat_gr,]
  }
  
  #Clear the .nc file
  t2m <- NULL
  nc_close(nc_data) #Closing the Netcdf file. 
  
  
  #-------------------------------------------------------#
  #Read a single ERA-5 download file
  name <- paste0("data/raw_data/era_5/",yr,".nc")
  nc_data <- nc_open(name)
  
  #Get the attributes
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  t2m <- ncvar_get(nc_data, "t2m") 
  t2m <- t2m[,,1:hrs_ahead]
  
  #Subset to needed values
  t2m_front <- matrix(ncol = nrow(Grids),
                      nrow = dim(t2m)[3])
  
  for(j in 1:ncol(t2m_front)){
    lon_gr <- which(lon == Grids$Longitude[j])
    lat_gr <- which(lat == Grids$Latitude[j])
    
    t2m_front[,j] <- t2m[lon_gr,lat_gr,]
  }
  
  #Clear the .nc file
  t2m <- NULL
  nc_close(nc_data) #Closing the Netcdf file. 
  
  #Combining the values
  t2m_land <- rbind(t2m_lag,t2m_front)
  t2m_lag <- t2m_front <- NULL
  
  
  #----------------------------------------------------------------------------#
  ###Compute the Ann-Max HDD for the year
  HDD <-   thresh_temp - t2m_land
  HDD[HDD<0] <- 0
  all_vals <- all_dates <- list()
  
  for(k in 1:length(block_size)) {
    
    #Setup current block  
    cur_block <- block_size[k]
    
    #Setup storage
    hdd_vals <- hdd_dates <- list()
    
    for(j in 1:ncol(HDD)){
      hdd_temp <- get_block_maxima(t_series = HDD[,j],
                                   st_date = paste0("01-01-", yr, " 00:00"),
                                   block_size = cur_block,
                                   Time_Step = "Hourly")
      hdd_vals[[j]] <- hdd_temp$Block
      hdd_dates[[j]] <- as.character(hdd_temp$Dates - hrs_behind)
    }
    
    #Store the results
    all_vals[[k]] <- unlist(hdd_vals)
    all_dates[[k]] <- unlist(hdd_dates)
  }
  
  #Sorting the Block-Sizes
  HDD <- NULL
  
  out = list(HDD_Values=all_vals,
             HDD_Dates=all_dates)
}


#------------------------------------------------------------------------------#
###Results all block years#######

nsim <- length(yrs)

cores=detectCores()-4
registerDoParallel(cores)
start.time <- Sys.time()
ynew_results <- foreach(m = 1:nsim, .verbose = TRUE) %dopar% {
  
  
  yr <- m+1950
  get_hdd(yr = yr, block_size = block_sizes,
          Grids = grid_locs, 
          thresh_temp = thresh_temp)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
stopImplicitCluster()


###Converting the results to individual dataframes.

###Save the Values
ann_max_dates <- list()
ann_max_values <- list()

for(j in 1:length(block_sizes)){
  
  #Saving the Annual Maximum Values
  temp <- matrix(NA, ncol = nrow(grid_locs),
                 nrow = length(yrs))
  for(i in 1:nsim){
    temp[i,] <- ynew_results[[i]][[1]][[j]]
  }
  ann_max_values[[j]] <- temp
  
  #Saving the Annual Maximum Dates
  temp <- matrix(NA, ncol = nrow(grid_locs),
                 nrow = length(yrs))
  for(i in 1:nsim){
    temp[i,] <- ynew_results[[i]][[2]][[j]]
  }
  ann_max_dates[[j]] <- temp
  
}


#Creating a Single List
HDD_Site_Level <- list()
HDD_Site_Level[[1]] <- ann_max_values
HDD_Site_Level[[2]] <- ann_max_dates


##Consistency Check 
#Plotting the results - Consistency Check
pt_dt <- HDD_Site_Level[[1]][[1]]
pt_dt <- colMeans(pt_dt)

p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -60))+
  scale_y_continuous(name = " ", limits = c(20, 55)) +
  geom_tile(data = grid_locs, aes(x=Longitude, y = Latitude, 
                                  fill = pt_dt/block_sizes[1])) +
  scale_fill_gradient2(midpoint=median(pt_dt/block_sizes[1]),
                       low="blue", mid="white",high="red",
                       name = "HDD") +
  ggtitle("Ann_Max HDD - Block Size 6 Hours - Code Consistency Check")
print(p)

#------------------------------------------------------------------------------#

CONUS_HDD_Site <- list(Mean = Mean_HDD,
                       Site_Level = HDD_Site_Level)







#______________________________________________________________________________#
####Saving the results
save(CONUS_HDD_Site, file = paste0("data/processed_data/HDD_Site_Level.RData"))

