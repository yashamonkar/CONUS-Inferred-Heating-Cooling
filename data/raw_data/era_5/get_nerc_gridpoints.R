#______________________________________________________________________________#
####CODE TO GET GRID POINTS INSIDE THE each nerc region################

###INPUT
#1. GRID BOX USED TO DOWNLOAD THE ERA-5 data. 
#2. NERC Shape Files. 

#______________________________________________________________________________#
###Load Dependencies and Data

#Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Packages
library(rgdal)
library(sf)
library(ncdf4) 

#Load the shape-file data 
nerc_regs <- readOGR("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/NERC_Regions-shp/NERC_Regions_EIA.shp")

#Plotting the data
plot(nerc_regs, xlim = c(-125, -66),
     ylim = c(25,50),
     main = "NERC Regions")

#______________________________________________________________________________#
###ERA-5 Gridbox

#Read a single ERA-5 download file
name <- c("2001.nc")
nc_data <- nc_open(name)
print(nc_data) 

lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude")
t <- ncvar_get(nc_data, "time")
nc_close(nc_data) #Closing the Netcdf file. 

#Set up the lat-lon box
grids <- data.frame(Longitude = rep(lon, length(lat)),
                    Latitude = rep(lat, each = length(lon)))


#______________________________________________________________________________#
###ERA-5 Gridbox

n_regions <- length(nerc_regs$FID)
grids_usa <- list()

for(i in 1:n_regions){
        
        #Current Region
        states <- nerc_regs[i,]
        
        #Convert the grids to a spatial dataset
        dat <- grids
        coordinates(dat) <- ~ Longitude + Latitude # Assignment modified according
        proj4string(dat) <- proj4string(states) # Set the projection of the SpatialPointsDataFrame
        
        #Find the locations inside the United States
        loc_usa <- over(dat,states)
        grids_rto <- grids
        grids_rto$LOC <- loc_usa[,1]
        grids_rto <- grids_rto[complete.cases(grids_rto), ]
        
        #Plotting the results
        plot(states, xlim = c(-125, -66),
             ylim = c(25,50),
             main = "Continental United States")
        points(grids$Longitude, grids$Latitude, size =0.01)
        points(grids_rto$Longitude, grids_rto$Latitude, 
               pch = 19, size =0.01, col='red')
        
        grids_rto$LOC <- NULL
        grids_usa[[i]] <- grids_rto 
}


#______________________________________________________________________________#
###Plot_Check
plot(nerc_regs, xlim = c(-125, -66),
     ylim = c(25,50),
     main = "NERC Regions")
for(i in 1:n_regions){
        points(grids_usa[[i]]$Longitude, grids_usa[[i]]$Latitude, 
               pch = 19, size =0.01, col=i)
}

grid_nerc <- grids_usa

#Saving the results
save(grid_nerc, 
     file = paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/NERC_Regions_lat_lon_index_key.RData"))
