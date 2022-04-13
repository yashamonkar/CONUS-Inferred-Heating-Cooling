#______________________________________________________________________________#
####Objective :- To generate AM CDD Time Series (and Save) for each Sub-Region####


###This is done since reading and opening the ERA-5 data is a long process.


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


