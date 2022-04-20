#______________________________________________________________________________#
###---Script to understand population effects---###

##Code can be used for either CDD or HDD

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
library(rgdal)
library(ggpattern)

#Load Functions
source("functions/Get_Day_Difference.R")
source("~/GitHub/CONUS-Inferred-Heating-Cooling/functions/Get_Conus_Regions.R")


#NERC Shapefiles
egrids <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/egrid2020_subregions"),
                  layer="eGRID2020_subregions")
nerc_sf <- get_egrids(egrids_sf = egrids)
nerc_labels <- nerc_sf$Labels
n_regions <- length(nerc_sf$Labels)



#______________________________________________________________________________#
###Hyper-Parameters###
Data_Type <- "HDD"



yrs <- 1951:2021
block_sizes <- c(6,12,24,72, 168, 336) #hours


#ggplot shape files
world <- map_data("world")
us <- map_data("state")


#______________________________________________________________________________#
##Reading the data###
j <- 3
par(mfrow = c(3,3))

for(i in 1:n_regions){


#2020 Data
DD_Regional <- get(load("data/processed_data/HDD_Regional_2020.RData"))
ts_2020 <- DD_Regional[[i]][[1]][[j]]/block_sizes[j]
tds <- density(ts_2020)

#2010 Data
DD_Regional <- get(load("data/processed_data/HDD_Regional_2010.RData"))
ts_2010 <- DD_Regional[[i]][[1]][[j]]/block_sizes[j]
tdsx <- density(ts_2010)

#2000 Data
DD_Regional <- get(load("data/processed_data/HDD_Regional_2000.RData"))
ts_2000 <- DD_Regional[[i]][[1]][[j]]/block_sizes[j]

plot(density(ts_2000), main = nerc_labels[i], xlab = "Temperature")
lines(tds$x, tds$y, col='red')
lines(tdsx$x, tdsx$y, col='blue')


}
