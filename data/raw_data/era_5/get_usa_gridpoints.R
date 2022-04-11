#______________________________________________________________________________#
####CODE TO GET GRID POINTS INSIDE THE CONTINENTAL UNITED STATES################

###INPUT
#1. GRID BOX USED TO DOWNLOAD THE ERA-5 data. 
#2. USA SHAPE FILE. 

#______________________________________________________________________________#
###Load Dependencies and Data

#Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Packages
library(rgdal)
library(sf)
library(ncdf4) 

#Load the shape-file data 
states <- readOGR("usa_shapefile/cb_2014_us_nation_20m.shp")

#Plotting the data
plot(states, xlim = c(-125, -66),
     ylim = c(25,50),
     main = "Continental United States")

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

#Convert the grids to a spatial dataset
dat <- grids
coordinates(dat) <- ~ Longitude + Latitude # Assignment modified according
proj4string(dat) <- proj4string(states) # Set the projection of the SpatialPointsDataFrame
 
#Find the locations inside the United States
loc_usa <- over(dat,states)
grids_usa <- grids
grids_usa$LOC <- loc_usa$NAME
grids_usa <- grids_usa[complete.cases(grids_usa), ]

#Plotting the results
plot(states, xlim = c(-125, -66),
     ylim = c(25,50),
     main = "Continental United States")
points(grids$Longitude, grids$Latitude, size =0.01)
points(grids_usa$Longitude, grids_usa$Latitude, pch = 19, size =0.01, col='red')


#Saving the results
grids_usa$LOC <- NULL
write.table(grids_usa, "~/GitHub/CONUS-Inferred-Heating-Cooling/data/CONUS_0_5_deg_lat_lon_index_key.txt", sep =" ")
