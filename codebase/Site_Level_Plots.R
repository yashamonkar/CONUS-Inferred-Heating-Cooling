#______________________________________________________________________________#
###---Script to generate CONUS wide site-level plots---###

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
DD_Regional <- get(load("data/processed_data/HDD_Site_Level.RData"))

#Load the CONUS Locations
all_grids <- get(load("data/NERC_Regions_lat_lon_index_key.RData"))
grid_locs <- bind_rows(lapply(all_grids,data.frame))


#NERC Shapefiles
egrids <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/egrid2020_subregions"),
                  layer="eGRID2020_subregions")



#______________________________________________________________________________#
###Hyper-Parameters###
Data_Type <- "HDD"
yrs <- 1951:2021
block_sizes <- c(6,12,24,72, 168, 336) #hours


#ggplot shape files
world <- map_data("world")
us <- map_data("state")


#Select the Block Size
j <- 4

#______________________________________________________________________________#
###---Trends Analysis on Values---###
pdf("figures/HDD_Sites.pdf")


fill_col <- slope <- list()
sig <- rep(0, nrow(grid_locs))

pb = txtProgressBar(min = 1, max = nrow(grid_locs), initial = 1) 
for(i in 1:nrow(grid_locs)){
  
  setTxtProgressBar(pb,i)  #Progress Bar
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- DD_Regional$Site_Level[[1]][[j]][,i]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[i] = 0.1}
  
  #Sens_Slop
  p_col <- ts_ss$estimates*length(ts_agg)/mean(ts_agg)
  
  #Set Absolute Values
  if(abs(p_col) > 1){ p_col = 1*sign(p_col)}
  slope[[i]] <- p_col
  
  #Get the Direction
  if(p_col > 0) {
  colfunc <- colorRamp(c("#FFFFFF", "#FF0000" ))
  fill_col[[i]] <- rgb(colfunc(p_col), maxColorValue = 255)  
  } else {
    p_col <- -p_col
    colfunc <- colorRamp(c("#FFFFFF", "#0000FF"))
    fill_col[[i]] <- rgb(colfunc(p_col), maxColorValue = 255)  
  }
  
}

#------------------------------------------------------------------------------#
###Plotting the values
grid_locs$Significance <- sig
grid_locs$color <- unlist(fill_col)
grid_locs$Change <- unlist(slope)

sig_points <- grid_locs %>% filter(Significance > 0)


#Plotting the results
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = Change)) +
  geom_point(sig_points, mapping = aes(x = Longitude, y = Latitude), size = 0.1) +
  scale_x_continuous(name = " ", limits = c(-125, -60))+
  scale_y_continuous(name = " ", limits = c(20, 55)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(-1, 1)) +
  geom_polygon(data = egrids, mapping = aes( x = long, y = lat, group = group), 
               fill = NA, color = 'black', size = 0.75) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "black", size = 0.15) +
  ggtitle(paste0("Peak site level ",Data_Type,
                 " Demand \n Block Size - ", block_sizes[j] ," Hours"))

print(p)




#______________________________________________________________________________#
###---Trends Analysis on Dates---###

fill_col <- slope <- list()
sig <- rep(0, nrow(grid_locs))

pb = txtProgressBar(min = 1, max = nrow(grid_locs), initial = 1) 
for(i in 1:nrow(grid_locs)){
  
  setTxtProgressBar(pb,i)  #Progress Bar
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- DD_Regional$Site_Level[[2]][[j]][,i]
  
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
    sig[i] = 0.1}
  
  #Sens_Slop
  p_col <- ts_ss$estimates*length(ts_agg)/mean(ts_agg)
  
  #Set Absolute Values
  if(abs(p_col) > 1){ p_col = 1*sign(p_col)}
  slope[[i]] <- p_col
  
  #Get the Direction
  if(p_col > 0) {
    colfunc <- colorRamp(c("#FFFFFF", "#FF0000" ))
    fill_col[[i]] <- rgb(colfunc(p_col), maxColorValue = 255)  
  } else {
    p_col <- -p_col
    colfunc <- colorRamp(c("#FFFFFF", "#0000FF"))
    fill_col[[i]] <- rgb(colfunc(p_col), maxColorValue = 255)  
  }
  
}

#------------------------------------------------------------------------------#
###Plotting the values
grid_locs$Significance <- sig
grid_locs$color <- unlist(fill_col)
grid_locs$Change <- unlist(slope)

sig_points <- grid_locs %>% filter(Significance > 0)


#Plotting the results
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = Change)) +
  geom_point(sig_points, mapping = aes(x = Longitude, y = Latitude), size = 0.1) +
  scale_x_continuous(name = " ", limits = c(-125, -60))+
  scale_y_continuous(name = " ", limits = c(20, 55)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(-1, 1)) +
  geom_polygon(data = egrids, mapping = aes( x = long, y = lat, group = group), 
               fill = NA, color = 'black', size = 0.75) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "black", size = 0.15) +
  ggtitle(paste0("Peak site level ",Data_Type,
                 " Dates \n Block Size - ", block_sizes[j] ," Hours"))

print(p)





dev.off()
