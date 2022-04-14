#______________________________________________________________________________#
####Objective :- Code to read the ERA-5 Shape Files and get the block maxima

###This is done since reading and opening the ERA-5 data is a long process.

###Output Needed
#1. Annual Average. Matrix. Size - 71 x 3285. 
#2. Site-level Block Maxima. Size - 71 x 3285 x 6
#3. Site-level Block Maxima Dates. Size - 71 x 3285 x 6
#4. Grid-level Block Maxima. Size - 71 x (3285) x 6
#5. Grid-level Block Maxima Dates. Size - 71 x 1 x 6
#6. Site-level distribution of Block Maxima. 71 x 3285 x 6

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

#Source functions
source("functions/Get_Block_Maximum.R")
source("functions/Get_Day_Difference.R")

#Load the CONUS Locations
load("data/NERC_Regions_lat_lon_index_key.RData")

#Load Population and Temperature Grid Cell Data
load("data/NERC_Regions_Temp_Population.RData")
load("data/Population.RData") #Temporary


#NERC Shapefiles
nerc_sf <- readOGR(dsn= paste0("data/sf/NERC_Regions-shp"),
                   layer="NERC_Regions_EIA")


###MAKE A Selection
nerc_sf$NERC_Label
sel_rto <- 2
grid_locs <- grid_nerc[[sel_rto]]
nerc_cur <- tidy(nerc_sf[sel_rto,])
nerc_label <- nerc_sf$NERC_Label[sel_rto]



#______________________________________________________________________________#
#Hyper-Parameters
thresh_temp <- 291.5 #65 Fahrenheit #

#Plotting the grid points
world <- map_data("world")
us <- map_data("state")


#Grid Points
ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -60))+
  scale_y_continuous(name = " ", limits = c(20, 55)) +
  geom_point(data = grid_locs, aes(x=Longitude, y = Latitude),
             size = 0.5, color = 'red') +
  geom_polygon(data = nerc_cur, mapping = aes( x = long, y = lat, group = group), 
               fill = NA, color = 'black', size = 1.2) +
  ggtitle(paste0("CDD - Consistency Check - ", nerc_label))


#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
###Objective 1:- Annual Average. Matrix. Size - 72 x No of Grid Cells in RTO.


###Method
#1. Read the .nc files
#2. Compute the output
#3. Save output as .txt file

###Output Characteristics
#1. Saved as a .txt file
#2. Units of (mean) degree-hours/day


#Hyper-Parameters
yrs <- 1950:2021
Mean_CDD <- matrix(NA, nrow = length(yrs), ncol = nrow(grid_locs))


pb = txtProgressBar(min = 1, max = length(yrs), initial = 1) 
for(y in 1:length(yrs)){
  
  setTxtProgressBar(pb,y)  #Progress Bar
  
  #Current year
  yr <- yrs[y]
  
  #Read a single ERA-5 download file
  name <- paste0("data/raw_data/era_5/",yr,".nc")
  nc_data <- nc_open(name)
  
  #Get the attributes
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  t2m <- ncvar_get(nc_data, "t2m") 
  
  #Subset to needed values
  t2m_land <- matrix(ncol = nrow(grid_locs),
                     nrow = dim(t2m)[3])
  
  for(j in 1:ncol(t2m_land)){
    lon_gr <- which(lon == grid_locs$Longitude[j])
    lat_gr <- which(lat == grid_locs$Latitude[j])
    
    t2m_land[,j] <- t2m[lon_gr,lat_gr,]
  }
  
  #Clear the .nc file
  t2m <- NULL
  nc_close(nc_data) #Closing the Netcdf file. 
  
  
  #----------------------------------------------------------------------------#
  ###Compute the Annual Average
  CDD <-   t2m_land - thresh_temp
  CDD[CDD<0] <- 0
  CDD_Mean <- colMeans(CDD)
  
  #Storing the data
  Mean_CDD[y,] <- CDD_Mean
}

#Consistency Check
sum(is.na(Mean_CDD))

#Plotting the results - Consistency Check
ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -60))+
  scale_y_continuous(name = " ", limits = c(20, 55)) +
  geom_tile(data = grid_locs, aes(x=Longitude, y = Latitude, 
                                  fill = colMeans(Mean_CDD))) +
  scale_fill_gradient2(midpoint=median(colMeans(Mean_CDD)),
                       low="blue", mid="white",high="red",
                       name = "CDD") +
  geom_polygon(data = nerc_cur, mapping = aes( x = long, y = lat, group = group), 
               fill = NA, color = 'black', size = 1.2) +
  ggtitle(paste0("Mean CDD - Consistency Check - ", nerc_label))


#Clean-up for next objectives
CDD <- nc_data <- t2m_land <- NULL




#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
###Objective 2:- Site-level Block Maxima. Size - 72 x Num Grid Cells x 6
###Objective 3:- Site-level Block Maxima Date. Size - 72 x Num Grid Cells x 6


###Output Characteristics
#1. Saved as a .RData file
#2. Units of degree-hours/event-duration



###Output Format
#Single List of Lists. 
#[[1]] - Contains all the Values
#[[2]] - Contains all the Dates

#[[1]][[1]] - Data Frame [72 x 3285] with ann-max values.


#Hyper-parameters
yrs <- 1950:2021
block_sizes <- c(6,12,24,72, 168, 336) #hours


get_cdd <- function(yr, block_size, Grids, thresh_temp){
  
  #Load the Packages
  library(ncdf4)
  library(zoo)
  library(dplyr)
  
  #Source functions
  source("functions/Get_Block_Maximum.R")
  
  #Read a single ERA-5 download file
  name <- paste0("data/raw_data/era_5/",yr,".nc")
  nc_data <- nc_open(name)
  
  #Get the attributes
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  t2m <- ncvar_get(nc_data, "t2m") 
  
  #Subset to needed values
  t2m_land <- matrix(ncol = nrow(Grids),
                     nrow = dim(t2m)[3])
  
  for(j in 1:ncol(t2m_land)){
    lon_gr <- which(lon == Grids$Longitude[j])
    lat_gr <- which(lat == Grids$Latitude[j])
    
    t2m_land[,j] <- t2m[lon_gr,lat_gr,]
  }
  
  #Clear the .nc file
  t2m <- NULL
  nc_close(nc_data) #Closing the Netcdf file. 
  
  
  #----------------------------------------------------------------------------#
  ###Compute the Ann-Max CDD for the year
  CDD <-   t2m_land - thresh_temp
  CDD[CDD<0] <- 0
  
  
  all_vals <- all_dates <- list()
  
  for(k in 1:length(block_size)) {
    
    #Setup current block  
    cur_block <- block_size[k]
    
    #Setup storage
    cdd_vals <- cdd_dates <- list()
  
    for(j in 1:ncol(CDD)){
      cdd_temp <- get_block_maxima(t_series = CDD[,j],
                                   st_date = paste0("01-01-", yr, " 00:00"),
                                   block_size = cur_block,
                                   Time_Step = "Hourly")
      cdd_vals[[j]] <- cdd_temp$Block
      cdd_dates[[j]] <- as.character(cdd_temp$Dates)
    }
    
    #Store the results
    all_vals[[k]] <- unlist(cdd_vals)
    all_dates[[k]] <- unlist(cdd_dates)
  }
  
  #Sorting the Block-Sizes
  CDD <- NULL
  
  out = list(CDD_Values=all_vals,
             CDD_Dates=all_dates)
}



#------------------------------------------------------------------------------#
###Results all block years#######

nsim <- length(yrs)

cores=detectCores()
registerDoParallel(cores)
start.time <- Sys.time()
ynew_results <- foreach(m = 1:nsim, .verbose = TRUE) %dopar% {
  
  
  yr <- m+1949
  get_cdd(yr = yr, block_size = block_sizes,
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
CONUS_CDD_Site_Level <- list()
CONUS_CDD_Site_Level[[1]] <- ann_max_values
CONUS_CDD_Site_Level[[2]] <- ann_max_dates



##Consistency Check 
#Plotting the results - Consistency Check
pt_dt <- CONUS_CDD_Site_Level[[1]][[1]]
pt_dt <- colMeans(pt_dt)

ggplot() +
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
                       name = "CDD") +
  ggtitle("Ann_Max CDD - Block Size 6 Hours - Code Consistency Check")


#Second Plot
pt_dt <- CONUS_CDD_Site_Level[[1]][[4]]
pt_dt <- colMeans(pt_dt)


ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -60))+
  scale_y_continuous(name = " ", limits = c(20, 55)) +
  geom_tile(data = grid_locs, aes(x=Longitude, y = Latitude, 
                                  fill = pt_dt/block_sizes[4])) +
  scale_fill_gradient2(midpoint=median(pt_dt/block_sizes[4]),
                       low="blue", mid="white",high="red",
                       name = "CDD") +
  ggtitle("Ann Max CDD - Block Size 3 Days - Code Consistency Check")

#Third Plot
pt_dt <- CONUS_CDD_Site_Level[[1]][[6]]
pt_dt <- colMeans(pt_dt)

ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -60))+
  scale_y_continuous(name = " ", limits = c(20, 55)) +
  geom_tile(data = grid_locs, aes(x=Longitude, y = Latitude, 
                                  fill = pt_dt/block_sizes[6])) +
  scale_fill_gradient2(midpoint=median(pt_dt/block_sizes[6]),
                       low="blue", mid="white",high="red",
                       name = "CDD") +
  ggtitle("Ann Max CDD - Block Size 14 Days - Code Consistency Check")









#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
###Objective 4:- Grid-level Block Maxima. Size - 72 x 6
###Objective 5:- Grid-level Block Maxima Dates. Size - 72 x 6
###Objective 6:- Site-level CDD Contribution. Size - 72 x 3285 x 6

###Output Characteristics
#1. Saved as a .RData file
#2. Units of degree-hours/event-duration


###Output Format
#Single List of Lists. 
#[[1]] - Contains all the Values
#[[2]] - Contains all the Dates [Single Time Series]


#Hyper-parameters
yrs <- 1950:2021
block_sizes <- c(6,12,24,72, 168, 336) #hours

#Set-up Storage
CDD_Grid_Values <- CDD_Dates <- CDD_Site_Values <- list()

  
pb = txtProgressBar(min = 1, max = length(yrs), initial = 1) 
for(y in 1:length(yrs)){
  
  #Progress
  setTxtProgressBar(pb,y)
  
  #Current year
  yr <- yrs[y]
  
  #Load the Packages
  library(ncdf4)
  library(zoo)
  library(dplyr)
  
  #Source functions
  source("functions/Get_Block_Maximum.R")
  
  #Read a single ERA-5 download file
  name <- paste0("data/raw_data/era_5/",yr,".nc")
  nc_data <- nc_open(name)
  
  #Get the attributes
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  t2m <- ncvar_get(nc_data, "t2m") 
  
  #Subset to needed values
  t2m_land <- matrix(ncol = nrow(grid_locs),
                     nrow = dim(t2m)[3])
  
  for(j in 1:ncol(t2m_land)){
    lon_gr <- which(lon == grid_locs$Longitude[j])
    lat_gr <- which(lat == grid_locs$Latitude[j])
    
    t2m_land[,j] <- t2m[lon_gr,lat_gr,]
  }
  
  #Clear the .nc file
  t2m <- NULL
  nc_close(nc_data) #Closing the Netcdf file. 
  
  
  #----------------------------------------------------------------------------#
  ###Compute the Ann-Max CDD for the year
  CDD <-   t2m_land - thresh_temp
  CDD[CDD<0] <- 0
  
  #Multiply by Population Density Weights
  CDD <- sweep(CDD, MARGIN=2, grid_locs$Pop_Wts , `*`)
  
  CDD_Agg <- rowSums(CDD)
  
  max_val <- max_dates <- max_site_vals <-  list()
  
  for(k in 1:length(block_sizes)) {
    
    #Setup current block  
    cur_block <- block_sizes[k]
    
    #Dates
    st_date <- paste0("01-01-",yr, " 00:00")
    start_date <- as.POSIXlt(st_date, format="%m-%d-%Y %H:%M")
    time_stamps <- seq(start_date, by = "hour", length.out = length(CDD_Agg))
    
    #Compute the moving average
    df <- data.frame(Metric = CDD_Agg, 
                     Dates = time_stamps, 
                     Index = 1:length(CDD_Agg))
    df$Block <- rollsum(df$Metric, cur_block, align = "center", fill = NA)
    df$Metric <- NULL
    df[is.na(df)] <- 0
    max_index <- which(df$Block==max(df$Block))
    
    #Get the mean values across the grid
    max_dates[[k]] <- df$Dates[max_index]
    max_val[[k]] <- df$Block[max_index]
  
    #Get the Site values
    #max_site_vals[[k]] <- CDD[max_index,] #Just the middle hour
    low_index <- max_index - cur_block*0.5 + 1
    upper_index <- max_index + cur_block*0.5
    temp_sites <- CDD[low_index:upper_index,]
    max_site_vals[[k]] <- colSums(temp_sites)
    
    }
  CDD <- NULL
  
  CDD_Grid_Values[[y]] <- max_val
  CDD_Dates[[y]] <- max_dates
  CDD_Site_Values[[y]] <-  max_site_vals
}



###Changing to Output Format
Grid_Values <- Grid_Dates <- Site_Values <- list()
for(j in 1:length(block_sizes)) {
  
  #Getting the Grid Values
  temp <- list()
  for(i in 1:length(yrs)){
    temp[[i]] <- CDD_Grid_Values[[i]][[j]]
  }
  Grid_Values[[j]] <- unlist(temp)
  
  #Getting the Grid Dates
  temp <- seq(1:length(yrs))
  for(i in 1:length(yrs)){
    temp[i] <- as.character(CDD_Dates[[i]][[j]])
  }
  Grid_Dates[[j]] <- temp
  
  #Getting the Site Values
  temp <- matrix(NA, ncol = nrow(grid_locs), nrow = length(yrs))
  for(i in 1:length(yrs)){
    temp[i,] <- CDD_Site_Values[[i]][[j]]
  }
  Site_Values[[j]] <- temp
}


#Creating a Single List
CONUS_CDD_Grid_Level <- list()
CONUS_CDD_Grid_Level[[1]] <- Grid_Values
CONUS_CDD_Grid_Level[[2]] <- Grid_Dates
CONUS_CDD_Grid_Level[[3]] <- Site_Values

#save(CONUS_CDD_Grid_Level, 
#     file = paste0("data/processed_data/ERCOT_CDD_Grid_Level.RData"))


#Consistency Checks

#Plot - 6 Hours
plot(1950:2021, CONUS_CDD_Grid_Level[[1]][[1]], type='l',
     xlab = "Year", ylab = "Degree-Hours/Ann Max Event", 
     main = " Annual Maximum CDD across CONUS for 6 hr events")


#Plot - 6 Hours
pt_dt <- CONUS_CDD_Grid_Level[[3]][[1]]
pt_dt <- colMeans(pt_dt)

ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -60))+
  scale_y_continuous(name = " ", limits = c(20, 55)) +
  geom_tile(data = grid_locs, aes(x=Longitude, y = Latitude, 
                                  fill = pt_dt)) +
  scale_fill_gradient2(midpoint=median(pt_dt),
                       low="blue", mid="white",high="red",
                       name = "CDD") +
  ggtitle("Grid Maximum  - Block Size 6 Hours - Code Consistency Check")


#Plot - 3 Days
pt_dt <- CONUS_CDD_Grid_Level[[3]][[4]]
pt_dt <- colMeans(pt_dt)

ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -60))+
  scale_y_continuous(name = " ", limits = c(20, 55)) +
  geom_tile(data = grid_locs, aes(x=Longitude, y = Latitude, 
                                  fill = pt_dt)) +
  scale_fill_gradient2(midpoint=median(pt_dt),
                       low="blue", mid="white",high="red",
                       name = "CDD") +
  ggtitle("Grid Maximum  - Block Size 3 Days - Code Consistency Check")


#______________________________________________________________________________#
####Saving the results

MISO <- list(Mean = Mean_CDD, 
             Site_Level = CONUS_CDD_Site_Level,
             Grid_Level = CONUS_CDD_Grid_Level)

save(MISO, file = paste0("data/processed_data/MISO_CDD.RData"))