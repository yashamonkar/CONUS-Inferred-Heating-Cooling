#______________________________________________________________________________#
####SCRIPT TO PROCESS THE POPULATION COUNT and DENSITY DATASET################
###OUTPUT ON THE NERC LEVEL#####

#Population Dataset:- Gridded Population of the World (GPW), v4.
#Source:- https://sedac.ciesin.columbia.edu/data/collection/gpw-v4
#Institution - SEDAC, CIESIN, NASA


###INPUT
#1. Temperature Grid Cells within each NERC Region.
#2. Population Data


###OUTPUT
#1. Lat-Lon Population Grid Cells for each sub-region.
#2. Closest temperature grid cell for that sub-region. 


#______________________________________________________________________________#
###Load Dependencies and Data

#Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Packages
library(ncdf4)
library(ggplot2)
library(dplyr)
library(rgdal)
library(geosphere)
library(doParallel)
library(foreach)

#Functions
source("~/GitHub/CONUS-Inferred-Heating-Cooling/functions/Get_Conus_Regions.R")


#Plotting the grid points
world <- map_data("world")
us <- map_data("state")


#NERC Shapefiles
egrids <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/egrid2020_subregions"),
                   layer="eGRID2020_subregions")
nerc_sf <- get_egrids(egrids_sf = egrids)
nerc_labels <- nerc_sf$Labels
nerc_regions <- length(nerc_sf$Labels)


#------------------------------------------------------------------------------#
###Population Count

#Read a population data
name <- c("gpw_v4_population_count_rev11_15_min.nc")
nc_data <- nc_open(name)
print(nc_data) 

#Get the points
lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude")
pop_nc <- ncvar_get(nc_data, "Population Count, v4.11 (2000, 2005, 2010, 2015, 2020): 15 arc-minutes")
pop_nc <- pop_nc[,,1:5] #2020 Count
nc_close(nc_data) #Closing the Netcdf file. 

#Make a lat-lon box
all_grids <- data.frame(Longitude = rep(lon, length(lat)),
                        Latitude = rep(lat, each = length(lon)))


#Plot the results to just check
#Grid Points
#ggplot() +
#  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
#           fill = "#D3D3D3", color = "#000000", size = 0.15) +
#  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
#           fill = "#D3D3D3", color = "#000000", size = 0.15) +
#  scale_x_continuous(name = " ", limits = c(-125, -66))+
#  scale_y_continuous(name = " ", limits = c(24, 50)) +
#  geom_polygon(data = egrids, mapping = aes( x = long, y = lat, group = group), 
#               fill = NA, color = 'black', size = 1.2) +
#  geom_point(data = all_grids, aes(x=Longitude, y = Latitude), 
#             color = 'blue', size = 0.5) +
#  ggtitle("Popluation and Temperature Grid Points - Visual Check")



#______________________________________________________________________________#
###Find all Grid Cells (Population Data) within each shapefile###
sub_region_pop_grids <- list()



for(n in 1:nerc_regions) {
  
  #Current Sub-Region
  sub_region <- nerc_sf$Shapefiles[[n]]
  
  #Convert the grids to a spatial dataset
  dat <- all_grids
  coordinates(dat) <- ~ Longitude + Latitude # Assignment modified according
  proj4string(dat) <- proj4string(sub_region) # Set the projection of the SpatialPointsDataFrame
  
  
  #Find the locations inside the United States
  loc_sub <- over(dat,sub_region)
  grids_rto <- all_grids
  grids_rto$LOC <- loc_sub[,1]
  grids_rto <- grids_rto[complete.cases(grids_rto), ]
  
  #Find the population for each grid
  grids_rto$LOC <- NULL
  grids_rto$pop_2020 <- grids_rto$pop_2015 <- grids_rto$pop_2010 <- grids_rto$pop_2005 <- grids_rto$pop_2000 <- NA
  for(j in 1:nrow(grids_rto)){
    lon_cur <- which(grids_rto$Longitude[[j]] == lon)
    lat_cur <- which(grids_rto$Latitude[[j]] == lat)
    grids_rto[j,3:7] <- pop_nc[lon_cur,lat_cur,]
  }
  
  #Plotting the results
  p1 <- ggplot() +
    geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    scale_x_continuous(name = " ", limits = c(-125, -66))+
    scale_y_continuous(name = " ", limits = c(24, 50)) +
    geom_polygon(data = sub_region, mapping = aes( x = long, y = lat, group = group), 
                 fill = NA, color = 'black', size = 1.2) +
    geom_tile(data = grids_rto, aes(x=Longitude, y = Latitude, fill = pop_2020)) +
    ggtitle(paste0(nerc_labels[n]))
  print(p1)
  
  #Save the grid cells within the RTO  
  sub_region_pop_grids[[n]] <- grids_rto
  
}

#Clean-Up
dat <- grids_rto <- all_grids <- loc_sub <- nc_data <- sub_region <- NULL


#______________________________________________________________________________#
###Find closet temp grid for each population within each sub-region###

#Read a single ERA-5 download file
name <- c("~/GitHub/CONUS-Inferred-Heating-Cooling/data/raw_data/era_5/2001.nc")
nc_data <- nc_open(name)
print(nc_data) 

lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude")
nc_close(nc_data) #Closing the Netcdf file. 

#Set up the lat-lon box
temp_grids <- data.frame(Longitude = rep(lon, length(lat)),
                    Latitude = rep(lat, each = length(lon)))

#Setup storage
sub_region_temp_grids <- list()



#------------------------------------------------------------------------------#
###INPUT
#1. NERC Shape Files
#2. Population Grids with Sub-Regions (list)
#3. All Temperature Grid Cells (Data-Frame)

###OUTPUT
#1. Temperature grid cells associated (nearest) the population grid cells

get_temp_grids <- function(Shapefiles, Population_Grids, Temperature_Grids,Iter){
  
  library(geosphere)
  
  #Current Sub-Region
  sub_region <- Shapefiles[[Iter]]
  
  #Get the current population Grids
  pop_grids <- Population_Grids[[Iter]]
  
  #Set-Up Temporary Storage
  tx_grids <- pop_grids
  
  
  pb = txtProgressBar(min = 1, max = nrow(pop_grids), initial = 1)
  for(i in 1:nrow(pop_grids)){
    
    #Progress-Bar
    setTxtProgressBar(pb,i)
    
    #Compute distances
    distances_km <- rep(NA, nrow(Temperature_Grids))
    for(j in 1:nrow(Temperature_Grids)){
      distances_km[j] <- distHaversine(c(pop_grids$Longitude[i], pop_grids$Latitude[i]),
                                       c(Temperature_Grids$Longitude[j], Temperature_Grids$Latitude[j]),
                                       r=6378137)
    }
    
    tx_grids[i,] <- Temperature_Grids[which.min(distances_km),]
  }
  
  #Returning the results
  return(tx_grids)
  
}


#-----------------------------------------------------------------------------#
###Running the function
cores=detectCores()-2
registerDoParallel(cores)
start.time <- Sys.time()
sub_region_temp_grids <- foreach(m = 1:nerc_regions, .verbose = TRUE) %dopar% {
  get_temp_grids(Shapefiles = nerc_sf$Shapefiles,
                 Population_Grids = sub_region_pop_grids, 
                 Temperature_Grids = temp_grids,
                 Iter = m)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
stopImplicitCluster()


#______________________________________________________________________________#
###Saving the results###
nerc_pop_temp <- list(Population = sub_region_pop_grids,
                      Temperature = sub_region_temp_grids)

save(nerc_pop_temp, file = paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/NERC_Regions_Temp_Population.RData"))



#Plotting the results
for(i in 1:nerc_regions){
  
  #Current Sub-Region
  sub_region <- nerc_sf$Shapefiles[[i]]

  plot(sub_region, xlim = c(-125, -66),
      ylim = c(25,50),
      main = paste0(nerc_labels[i]))
  points(sub_region_pop_grids[[i]]$Longitude, sub_region_pop_grids[[i]]$Latitude, size =0.01, col='blue')
  points(sub_region_temp_grids[[i]]$Longitude, sub_region_temp_grids[[i]]$Latitude, size =0.05, col='red', pch = 19)
  


}




