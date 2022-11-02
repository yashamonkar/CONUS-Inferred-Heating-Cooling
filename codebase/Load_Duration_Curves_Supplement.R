#______________________________________________________________________________#
####Objective :- To generate AM Thermal Heat Load Time Series (and Save) for each Sub-Region####


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
library(patchwork)

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
yrs <- 1951:2021
block_sizes <- c(6,12,24,72, 168, 336) #hours


nerc_labels <- c("Arizona/New Mexico", "CAISO", "ERCOT", "Florida", 
                 "Wisconsin (Rural)", "Midwest (MISO)", "ISO New England", 
                 "Northwest", "NYISO", "PJM (East)", "Michigan", "PJM (West)", 
                 "Colorado", "Kansas", "Oklahoma", "Arkansas/Louisiana" , 
                 "Missouri" ,"Southeast", "Tennesse Valley", "Carolinas")


###---Select Population Year---### 
#Based on Column in Population -- 3-2000, 4-2005, 5-2010, 6-2015, 7-2020
scenario <- 7


#______________________________________________________________________________#
###----Code to get regional block Maxima----###
get_tl_maxima <- function(Population, Temp_grids, 
                          Years, thresh_temp, 
                          block_sizes, Scenario,rto, Labels){
  
  #Library
  library(zoo)
  library(ncdf4) 
  library(patchwork)
  
  #Population Weights
  pop_cur <- Population[[rto]]
  population_wts <- pop_cur[,Scenario]/sum(pop_cur[,Scenario])
  
  #Temperature Grids
  grid_locs <- Temp_grids[[rto]]
  
  #Set-up Storage
  Thermal_Load_Mean <- list()
  
  pb = txtProgressBar(min = 1, max = length(Years), initial = 1) 
  for(y in 1:length(Years)){
    
    setTxtProgressBar(pb,y)  #Progress
    
    #Current year
    yr <- Years[y]
    
    #Get the number of hours from Jan-01 to June 30
    st_date <- as.POSIXct(paste0("01-01-",yr," 00:00"), format="%m-%d-%Y %H:%M")
    yr_end <- as.POSIXct(paste0("10-31-",yr," 23:00"), format="%m-%d-%Y %H:%M")
    yr_start <- as.POSIXct(paste0("11-01-",yr-1," 00:00"), format="%m-%d-%Y %H:%M")
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
    #HDD
    HDD <-  thresh_temp - t2m_land
    HDD[HDD<0] <- 0
    
    #CDD
    CDD <-  t2m_land - thresh_temp
    CDD[CDD<0] <- 0
    
    #Thermal Load
    Therm_Load <- CDD+HDD
    
    
    #Multiply by Population Density Weights
    Therm_Load <- sweep(Therm_Load, MARGIN=2, population_wts , `*`)
    Therm_Load_Agg <- rowSums(Therm_Load)
    
    Thermal_Load_Mean[[y]] <- Therm_Load_Agg
    
  }
  
  
  #Select the two periods
  tl1 <- unlist(Thermal_Load_Mean[1:10])
  tl2 <- unlist(Thermal_Load_Mean[62:71])
  
  #Compute the densities
  ds1 <- density(tl1)
  ds2 <- density(tl2)

  #Plotting Dataset
  plt_dataset <- data.frame(x = c(ds1$x,ds2$x),
                            y = c(ds1$y,ds2$y),
                            Type = c(rep("1951-1960", length(ds1$x)),
                                     rep("2012-2021", length(ds1$x))))
  
  group.colors <- c("1951-1960" ="#af8dc3",  "2012-2021" = "#7fbf7b")
  
  p1 <- ggplot(plt_dataset) +
    geom_line(aes(x=x,y=y, color = Type), size = 1.25) +
    ggtitle("Density Plot") +
    geom_vline(xintercept = mean(tl1), color = '#af8dc3', linetype = "dashed") +
    geom_vline(xintercept = mean(tl2), color = '#7fbf7b', linetype = "dashed") +
    ylab("Density") +
    xlab("Thermal Load (oC)") +
    scale_color_manual(values=group.colors) +
    theme_bw() +
    theme(axis.text=element_text(size=5),
          axis.title=element_text(size=7),
          plot.title = element_text(size=10),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position="none")
  
  #Compute the CDFs
  ds1 <- ecdf(tl1)
  ds2 <- ecdf(tl2)
  r1 <- seq(0,ceiling(max(tl1,tl2)), length.out = 200)
  
  
  #Plotting Dataset
  plt_dataset <- data.frame(x = c(r1,r1),
                            y = c(100-ds1(r1)*100, 100-ds2(r1)*100),
                            Type = c(rep("1951-1960", length(r1)),
                                     rep("2012-2021", length(r1))))
  
  p2 <- ggplot(plt_dataset) +
    geom_line(aes(x=y,y=x, color = Type), size = 1.25) +
    ggtitle(paste0(Labels[rto])) +
    geom_hline(yintercept = mean(tl1), color = '#af8dc3', 
               linetype = "dashed", size = 1.25) +
    geom_hline(yintercept = mean(tl2), color = '#7fbf7b', 
               linetype = "dashed", size = 1.25) +
    scale_color_manual(values=group.colors) +
    ylab("Total Thermal Load (oC)") +
    xlab("Capacity Utilization [Percent of Time (%)]") +
    theme_bw() +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          plot.title = element_text(size=18),
          legend.position="none")
  
  
  
  p3 <- p2 + inset_element(p1, left = 0.5, bottom = 0.4, right = 0.95, top = 0.95)
  
  ###Saving 
  return(p3)
  
  
}

#------------------------------------------------------------------------------#
#Running the function
cores=detectCores()
registerDoParallel(cores)
start.time <- Sys.time()
NERC_TL_Region <- foreach(rto = 1:n_regions, .verbose = TRUE) %dopar% {
  
  library(ggplot2)
  get_tl_maxima(Population = population, 
                Temp_grids = all_grids,
                Years = yrs,
                thresh_temp = thresh_temp,
                block_sizes = block_sizes, 
                Scenario=scenario,
                rto=rto,
                Labels = nerc_labels)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
stopImplicitCluster()




pdf("figures/Load_Duration_Curves_Supplement.pdf", height=5550/300, width=7000/300)

#------------------------------------------------------------------------------#
#Plots - Consistency Check

plot_grid(NERC_TL_Region[[1]],
          NERC_TL_Region[[2]],
          NERC_TL_Region[[3]],
          NERC_TL_Region[[5]],
          NERC_TL_Region[[7]],
          NERC_TL_Region[[8]],
          nrow = 3,
          labels = c('A', 'B', 'C', 'D', 'E', 'F'), 
          label_size = 16)


plot_grid(NERC_TL_Region[[9]],
          NERC_TL_Region[[10]],
          NERC_TL_Region[[11]],
          NERC_TL_Region[[12]],
          NERC_TL_Region[[13]],
          NERC_TL_Region[[14]],
          nrow = 3,
          labels = c('G', 'H', 'I', 'J', 'K', 'L'), 
          label_size = 16)



plot_grid(NERC_TL_Region[[15]],
          NERC_TL_Region[[16]],
          NERC_TL_Region[[17]],
          NERC_TL_Region[[18]],
          NERC_TL_Region[[19]],
          NERC_TL_Region[[20]],
          nrow = 3,
          labels = c('M', 'N', 'O', 'P', 'Q', 'R'), 
          label_size = 16)




dev.off()
