#______________________________________________________________________________#
####Objective :- To generate AM HDD Time Series (and Save) for each Sub-Region####


###This is done since reading and opening the ERA-5 data is a long process.


###Input Needed
#1. ERA-5 Temperature Gridded Data. -- Data
#2. NERC Shape-Files -- Indexing
#3. Population & Temperature Grid Points -- Indexing

###Output Needed
#@Each Sub-Region
#@Each Population Year
#1. Sub-Region Block Maxima. Size - 72 x 1 x 6
#2. Sub-Region Block Maxima Dates. Size - 71 x 1 x 6
#3. Site-level distribution of Block Maxima. 71 x Num of Grid Cells x 6


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
library(broom)
library(rgdal)
library(doParallel)
library(foreach)

#Source functions
source("functions/Get_Block_Maximum.R")
source("functions/Get_Day_Difference.R")

#Plotting the grid points
world <- map_data("world")
us <- map_data("state")

#Load the CONUS Locations
load("data/NERC_Regions_lat_lon_index_key.RData")

#Load Population and Temperature Grid Cell Data
load("data/NERC_Regions_Temp_Population.RData")
load("data/Population.RData") #Temporary


#NERC Shapefiles
nerc_sf <- readOGR(dsn= paste0("data/sf/NERC_Regions-shp"),
                   layer="NERC_Regions_EIA")


#______________________________________________________________________________#
###---Global-Hyperparameters---###
all_grids <- nerc_pop_temp$Temperature
population <- sub_region_pop_grids  #nerc_pop_temp$Population[[sel_rto]]

thresh_temp <- 291.5 #-----65 Fahrenheit 
yrs <- 1951:2021
block_sizes <- c(6,12,24,72, 168, 336) #hours
n_regions <- length(nerc_sf$FID)
nerc_labels <- nerc_sf$NERC_Label


###---Select Population Year---### 
#Based on Column in Population -- 3-2000, 4-2005, 5-2010, 6-2015, 7-2020
scenario <- 7


#______________________________________________________________________________#
###----Code to get regional block Maxima----###
get_hdd_maxima <- function(Population, Temp_grids, 
                           Years, thresh_temp, 
                           block_sizes, Scenario){
  
  #Library
  library(zoo)
  library(ncdf4) 
  
  #Population Weights
  pop_cur <- Population[[rto]]
  population_wts <- pop_cur[,Scenario]/sum(pop_cur[,Scenario])
  
  #Temperature Grids
  grid_locs <- Temp_grids[[rto]]
  
  #Set-up Storage
  HDD_Grid_Values <- HDD_Dates <- HDD_Site_Values <- list()
  
  pb = txtProgressBar(min = 1, max = length(Years), initial = 1) 
  for(y in 1:length(Years)){
    
    setTxtProgressBar(pb,y)  #Progress
    
    #Current year
    yr <- Years[y]
    
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
    
    
    #--------------------------------------------------------------------------#
    ###Compute the Ann-Max HDD for the year
    HDD <-  thresh_temp - t2m_land
    HDD[HDD<0] <- 0
    
    #Multiply by Population Density Weights
    HDD <- sweep(HDD, MARGIN=2, population_wts , `*`)
    
    HDD_Agg <- rowSums(HDD)
    max_val <- max_dates <- max_site_vals <-  list()
    
    for(k in 1:length(block_sizes)) {
      
      #Setup current block  
      cur_block <- block_sizes[k]
      
      #Dates
      st_date <- paste0("01-01-",yr, " 00:00")
      start_date <- as.POSIXlt(st_date, format="%m-%d-%Y %H:%M")
      time_stamps <- seq(start_date, by = "hour", length.out = length(HDD_Agg))
      
      #Compute the moving average
      df <- data.frame(Metric = HDD_Agg, 
                       Dates = time_stamps, 
                       Index = 1:length(HDD_Agg))
      df$Block <- rollsum(df$Metric, cur_block, align = "center", fill = NA)
      df$Metric <- NULL
      df[is.na(df)] <- 0
      max_index <- which(df$Block==max(df$Block))
      
      #Get the mean values across the grid
      max_dates[[k]] <- df$Dates[max_index]- hrs_behind
      max_val[[k]] <- df$Block[max_index]
      
      #Get the Site values
      #max_site_vals[[k]] <- HDD[max_index,] #Just the middle hour
      low_index <- max_index - cur_block*0.5 + 1
      upper_index <- max_index + cur_block*0.5
      temp_sites <- HDD[low_index:upper_index,]
      max_site_vals[[k]] <- colSums(temp_sites)
      
    }
    HDD <- NULL
    
    HDD_Grid_Values[[y]] <- max_val
    HDD_Dates[[y]] <- max_dates
    HDD_Site_Values[[y]] <-  max_site_vals
    
  }
  
  
  ###----Changing to a useful output format---###
  Grid_Values <- Grid_Dates <- Site_Values <- list()
  for(j in 1:length(block_sizes)) {
    
    #Getting the Grid Values
    temp <- list()
    for(i in 1:length(yrs)){
      temp[[i]] <- HDD_Grid_Values[[i]][[j]]
    }
    Grid_Values[[j]] <- unlist(temp)
    
    #Getting the Grid Dates
    temp <- seq(1:length(yrs))
    for(i in 1:length(yrs)){
      temp[i] <- as.character(HDD_Dates[[i]][[j]])
    }
    Grid_Dates[[j]] <- temp
    
    #Getting the Site Values
    temp <- matrix(NA, ncol = nrow(grid_locs), nrow = length(yrs))
    for(i in 1:length(yrs)){
      temp[i,] <- HDD_Site_Values[[i]][[j]]
    }
    Site_Values[[j]] <- temp
  }
  
  
  #Creating a Single List
  HDD_Regional <- list()
  HDD_Regional[[1]] <- Grid_Values
  HDD_Regional[[2]] <- Grid_Dates
  HDD_Regional[[3]] <- Site_Values
  
  ###Saving 
  return(HDD_Regional)
  
  
}

#------------------------------------------------------------------------------#
#Running the function
cores=detectCores()
registerDoParallel(cores)
start.time <- Sys.time()
NERC_HDD_Region <- foreach(rto = 1:n_regions, .verbose = TRUE) %dopar% {
  
  
  get_hdd_maxima(Population = population, 
                 Temp_grids = all_grids,
                 Years = yrs,
                 thresh_temp = thresh_temp,
                 block_sizes = block_sizes, 
                 Scenario=scenario)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
stopImplicitCluster()


#______________________________________________________________________________#
##Saving the results
save(NERC_HDD_Region, file = paste0("data/processed_data/HDD_Regional_2020.RData"))



#------------------------------------------------------------------------------#
#Plots - Consistency Check
for(i in 1:length(NERC_HDD_Region)){
  
  #Plots - Consistency Checks
  plot(yrs, NERC_HDD_Region[[i]][[1]][[1]], type='l',
       xlab = "Year", ylab = "Degree-Hours/Ann Max Event", 
       main = " Annual Maximum HDD across CONUS for 6 hr events")
  mtext(paste0(nerc_labels[i]), side = 3,
        cex = 1.15)
  
  #Spatial Plots
  sub_region <- nerc_sf[i,]
  pt_dt <- NERC_HDD_Region[[i]][[3]][[1]]
  pt_dt <- colMeans(pt_dt)
  
  p1 <- ggplot() +
    geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    scale_x_continuous(name = " ", limits = c(-125, -60))+
    scale_y_continuous(name = " ", limits = c(20, 55)) +
    geom_polygon(data = sub_region, mapping = aes( x = long, y = lat, group = group), 
                 fill = NA, color = 'black', size = 1.2) +
    geom_tile(data = all_grids[[i]], aes(x=Longitude, y = Latitude, 
                                         fill = pt_dt)) +
    scale_fill_gradient2(midpoint=median(pt_dt),
                         low="blue", mid="white",high="red",
                         name = "HDD") +
    ggtitle("Grid Maximum  - Block Size 6 Hours - Code Consistency Check")
  print(p1)
}


