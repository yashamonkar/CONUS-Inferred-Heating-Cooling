#______________________________________________________________________________#
###---Script to generate CONUS wide plots---###

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

#Load Population and Temperature Grid Cell Data
DD_Regional <- get(load("data/processed_data/CDD_Regional_2020.RData"))



#NERC Shapefiles
egrids <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/egrid2020_subregions"),
                  layer="eGRID2020_subregions")
nerc_sf <- get_egrids(egrids_sf = egrids)
nerc_labels <- nerc_sf$Labels
n_regions <- length(nerc_sf$Labels)



#______________________________________________________________________________#
###Hyper-Parameters###
Data_Type <- "CDD"



yrs <- 1950:2021
block_sizes <- c(6,12,24,72, 168, 336) #hours


#ggplot shape files
world <- map_data("world")
us <- map_data("state")


#______________________________________________________________________________#
###---Trends Analysis on Values---###
pdf("CDD_Values.pdf")



#Select the Block Size
j <- 6


#Plotting the results
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -60))+
  scale_y_continuous(name = " ", limits = c(20, 55)) +
  ggtitle(paste0("Peak per-capita Inferred ",Data_Type,
                 " Demand \n Block Size - ", block_sizes[j] ," Hours"))


p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- DD_Regional[[i]][[1]][[j]]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[[i]] = 0.1}
  
  #Sens_Slop
  p_col <- ts_ss$estimates*length(ts_agg)/mean(ts_agg)
  
  #Get the Direction
  if(p_col > 0) {
  colfunc <- colorRamp(c("#FFFFFF", "#FF0000" ))
  fill_col <- rgb(colfunc(p_col), maxColorValue = 255)  
  } else {
    p_col <- -p_col
    colfunc <- colorRamp(c("#FFFFFF", "#0000FF"))
    fill_col <- rgb(colfunc(p_col), maxColorValue = 255)  
  }
   
  
  #Subset to RTO
  sub_region <- nerc_sf$Shapefiles[[i]]
  
  #Add color to the sub-region
  #p <- p +
  #  geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
  #               fill = p_col[[i]], color = 'black', size = 1.2, pattern_fill = 'black')
  
  p <- p +  geom_polygon_pattern(
    data = sub_region, 
    mapping = aes(x = long, y = lat, group = group), 
    fill            = fill_col, 
    colour          = 'black',
    pattern_spacing = 0.05, 
    pattern_density = sig[[i]], 
    pattern_fill    = 'black', 
    pattern_colour  = '#002366',
    pattern_angle   = 45) 
  
}

#Add state boundaries
p <- p + 
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#964B00", size = 0.15) +
  geom_polygon(data = egrids, mapping = aes(x = long, y = lat, group = group), 
               fill = NA, color = 'black', size = 1.33)

print(p)




dev.off()



#______________________________________________________________________________#
###---Trends Analysis on Dates---###
pdf("HDD_Dates.pdf")

#Select the Block Size
j <- 6


#Plotting the results
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -60))+
  scale_y_continuous(name = " ", limits = c(20, 55)) +
  geom_polygon(data = nerc_sf, mapping = aes(x = long, y = lat, group = group), 
               fill = NA, color = 'black', size = 1.33) +
  ggtitle(paste0("Peak per-capita Inferred ",Data_Type,
                 " Demand -- Dates/Occurrence \n Block Size - ", block_sizes[j] ," Hours"))


p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- DD_Regional[[i]][[2]][[j]]
  
  if(Data_Type == "HDD") {
  ts_agg <- get_num_of_days(Data_Matrix = as.matrix(ts_agg,ncol=1),
                            Ref_Date = "07-01",
                            Type="HDD")
  } else {
    ts_agg <- get_num_of_days(Data_Matrix = as.matrix(ts_agg,ncol=1),
                              Ref_Date = "01-01",
                              Type="CDD")
  }
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[[i]] = 0.1}
  
  #Sens_Slop
  p_col <- ts_ss$estimates*length(ts_agg)/mean(ts_agg)
  
  #Get the Direction
  if(p_col > 0) {
    colfunc <- colorRamp(c("#FFFFFF", "#FF0000" ))
    fill_col <- rgb(colfunc(p_col), maxColorValue = 255)  
  } else {
    p_col <- -p_col
    colfunc <- colorRamp(c("#FFFFFF", "#0000FF"))
    fill_col <- rgb(colfunc(p_col), maxColorValue = 255)  
  }
  
  
  #Subset to RTO
  sub_region <- nerc_sf$Shapefiles[[i]]
  
  #Add color to the sub-region
  #p <- p +
  #  geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
  #               fill = p_col[[i]], color = 'black', size = 1.2, pattern_fill = 'black')
  
  p <- p +  geom_polygon_pattern(
    data = sub_region, 
    mapping = aes(x = long, y = lat, group = group), 
    fill            = fill_col, 
    colour          = 'black', 
    pattern_spacing = 0.05, 
    pattern_density = sig[[i]], 
    pattern_fill    = 'black', 
    pattern_colour  = '#002366',
    pattern_angle   = 45) 
  
}

print(p)




dev.off()
