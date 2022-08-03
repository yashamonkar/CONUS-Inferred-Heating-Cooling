#______________________________________________________________________________#
####Objective :- To generate AM CDD Time Series (and Save) for each Sub-Region####


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

#Source functions
source("functions/Get_Block_Maximum.R")
source("functions/Get_Day_Difference.R")
source("~/GitHub/CONUS-Inferred-Heating-Cooling/functions/Get_Conus_Regions.R")


#Plotting the grid points
world <- map_data("world")
us <- map_data("state")

#Load the CONUS Locations
load("data/NERC_Regions_lat_lon_index_key.RData")

#Load Population and Temperature Grid Cell Data
load("data/NERC_Regions_Temp_Population.RData")


#NERC Shapefiles
egrids <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/egrid2020_subregions"),
                  layer="eGRID2020_subregions")
nerc_sf <- get_egrids(egrids_sf = egrids)
nerc_labels <- nerc_sf$Labels
n_regions <- length(nerc_sf$Labels)



#______________________________________________________________________________#
###---Global-Hyperparameters---###
all_grids <- nerc_pop_temp$Temperature
population <- nerc_pop_temp$Population

thresh_temp <- 291.5 #-----65 Fahrenheit 
yrs <- 1950:2021
block_sizes <- c(6,12,24,72, 168, 336) #hours



###---Select Population Year---### 
#Based on Column in Population -- 3-2000, 4-2005, 5-2010, 6-2015, 7-2020
scenario <- 7


#______________________________________________________________________________#
###----Code to get regional block Maxima----###
NERC_CDD_Region <- list()

for(rto in 1:n_regions){
  
  #Population Weights
  pop_cur <- population[[rto]]
  population_wts <- pop_cur[,scenario]/sum(pop_cur[,scenario])
  
  #Temperature Grids
  grid_locs <- all_grids[[rto]]
  
  #Set-up Storage
  CDD_Grid_Values <- CDD_Dates <- list()
  CDD_Mean <- list()
  
  pb = txtProgressBar(min = 1, max = length(yrs), initial = 1) 
  for(y in 1:length(yrs)){
    
    setTxtProgressBar(pb,y)  #Progress
    
    #Current year
    yr <- yrs[y]
    
    #----------------------------------------------------------------------------#
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
    CDD <- sweep(CDD, MARGIN=2, population_wts , `*`)
    
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
      
    }
    CDD <- NULL
    
    CDD_Mean[[y]] <- mean(CDD_Agg)
    CDD_Grid_Values[[y]] <- max_val
    CDD_Dates[[y]] <- max_dates
    
  }
  
  
  ###----Changing to a useful output format---###
  Grid_Values <- Grid_Dates <- list()
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
    
  }
  
  #Creating a Single List
  CDD_Regional <- list()
  CDD_Regional[[1]] <- Grid_Values
  CDD_Regional[[2]] <- Grid_Dates
  CDD_Regional[[3]] <- unlist(CDD_Mean)
  
  #Plots - Consistency Checks
  plot(yrs, CDD_Regional[[1]][[1]], type='l',
       xlab = "Year", ylab = "Degree-Hours/Ann Max Event", 
       main = " Annual Maximum CDD across CONUS for 6 hr events")
  mtext(paste0(nerc_labels[rto]), side = 3,
        cex = 1.15)
  
  ###Saving 
  NERC_CDD_Region[[rto]] <- CDD_Regional
  
  
  
}


#______________________________________________________________________________#
##Saving the results
save(NERC_CDD_Region, file = paste0("data/processed_data/CDD_Regional.RData"))
