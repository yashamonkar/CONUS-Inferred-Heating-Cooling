#______________________________________________________________________________#
###---Analysis of Thermal Loads at the Regional Level---###
###1. at the ISO/Interconnect level.


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
library(gridExtra)
library(zoo)
library(cowplot)

#Load Functions
source("functions/Get_Day_Difference.R")
source("~/GitHub/CONUS-Inferred-Heating-Cooling/functions/Get_Conus_Regions.R")


#NERC Shapefiles
egrids <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/egrid2020_subregions"),
                  layer="eGRID2020_subregions")
nerc_sf <- get_egrids(egrids_sf = egrids) #Convert to needed regions
nerc_labels <- nerc_sf$Labels
n_regions <- length(nerc_sf$Labels)

nerc_labels <- c("Arizona/New Mexico", "CAISO", "ERCOT", "Florida", 
                 "Wisconsin (Rural)", "Midwest (MISO)", "ISO New England", 
                 "Northwest", "NYISO", "PJM (East)", "Michigan", "PJM (West)", 
                 "Colorado", "Kansas", "Oklahoma", "Arkansas/Louisiana" , 
                 "Missouri" ,"Southeast", "Tennesse Valley", "Carolinas")

#Population Shape-Files
pop_regions <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/USA_Urban_Areas"),
                       layer="USA_Urban_Areas")


#Load Temperature Grid Cell Data
load("data/processed_data/Thermal_Load_Regional.RData") 


pdf("figures/ISO_Plots.pdf", height=3700/300, width=5000/300)


#______________________________________________________________________________#
###Hyper-Parameters###
for(jk in 1:n_regions){
  
  print(jk)
  
  sel_rto <- jk
  RTO_Label <- nerc_labels[sel_rto]
  sel_block <- 4
  
  block_sizes <- c(6,12,24,72, 168, 336) #hours
  
  
  #______________________________________________________________________________#
  ###----Plot One: Map of the ISO-----###
  
  #ggplot shape files
  world <- map_data("world")
  us <- map_data("state")
  
  
  
  #Subset to RTO
  sub_region <- nerc_sf$Shapefiles[[sel_rto]]
  
  
  #Get Lat-Lon Extend
  lat_lon <- sub_region %>% fortify() %>% select(long,lat)
  lat_min <- min(lat_lon$lat)-0.5
  lat_max <- max(lat_lon$lat)+0.5
  lon_min <- min(lat_lon$long)-0.5
  lon_max <- max(lat_lon$long)+0.5
  
  p1 <-  ggplot() +
    geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
             fill = NA, color = "#000000", size = 0.15) +
    geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = NA) +
    geom_polygon(data = sub_region, mapping = aes( x = long, y = lat, group = group), 
                 fill = "#FFFFFF", color = 'black', size = 1) + 
    geom_polygon(data = pop_regions, mapping = aes( x = long, y = lat, group = group), 
                 fill = "black", color = NA, alpha = 0.2) +
    geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = NA, color = "#000000", size = 0.15) +
    scale_x_continuous(name = " ", limits = c(lon_min, lon_max)) +
    scale_y_continuous(name = " ", limits = c(lat_min, lat_max)) +
    ggtitle(paste0("  ",RTO_Label)) +
    theme_bw() +
    theme(axis.text=element_text(size=0),
          axis.title=element_text(size=0),
          axis.ticks = element_blank(),
          plot.title = element_text(size=20))
  
  
  
  #______________________________________________________________________________#
  ###----Plot Four: Map of the Aggregated Peak HDD and CDD-----###
  
  #Load Population and Temperature Grid Cell Data
  CDD_Regional <- get(load("data/processed_data/CDD_Regional.RData"))
  cdd_agg <- CDD_Regional[[sel_rto]][[1]][[sel_block]]/(block_sizes[sel_block]/24)
  
  if(jk == 5){cdd_agg = cdd_agg[-65]}  #Temporary - Ties
  
  
  #Load Population and Temperature Grid Cell Data
  HDD_Regional <- get(load("data/processed_data/HDD_Regional.RData"))
  hdd_agg <- HDD_Regional[[sel_rto]][[1]][[sel_block]]/(block_sizes[sel_block]/24)
  
  
  #Plotting Dataset
  Plt_Dt_HDD <- data.frame(Years = 1951:2021,
                           HDD = hdd_agg)
  
  Plt_Dt_CDD <- data.frame(Years = 1950:2021,
                           CDD = cdd_agg)
  
  
  p2 <-  ggplot() +
    geom_line(Plt_Dt_HDD, mapping = aes(x=Years, y = HDD), col ='blue') +
    geom_line(Plt_Dt_HDD, mapping = aes(x= Years, 
                                        y=rollmean(HDD, 10, na.pad=TRUE)), 
              color = 'blue', size = 1.33, linetype = "longdash") +
    geom_line(Plt_Dt_CDD, mapping = aes(x=Years, y = CDD), col='red') +
    geom_line(Plt_Dt_CDD, mapping = aes(x= Years, 
                                        y=rollmean(CDD, 10, na.pad=TRUE)), 
              color = 'red', size = 1.33, linetype = "longdash") +
    ggtitle(paste0("Peak Inferred Demand")) +
    ylab(paste0("Population Adjusted Daily Degree Days \n (Averaged over ",
                block_sizes[sel_block], " hours)")) +
    theme_bw() +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          plot.title = element_text(size=18))
  
  
  #______________________________________________________________________________#
  ###----Plot Four: Map of the Mean Peak HDD and CDD-----###
  
  #Load Population and Temperature Grid Cell Data
  cdd_mean <- CDD_Regional[[sel_rto]][[3]]
  
  #Load Population and Temperature Grid Cell Data
  hdd_mean <- HDD_Regional[[sel_rto]][[3]]
  
  
  #Plotting Dataset
  Plt_Dt_HDD <- data.frame(Years = 1951:2021,
                           HDD = hdd_mean)
  
  Plt_Dt_CDD <- data.frame(Years = 1950:2021,
                           CDD = cdd_mean)
  
  
  p3 <-  ggplot() +
    geom_line(Plt_Dt_HDD, mapping = aes(x=Years, y = HDD), col ='blue') +
    geom_line(Plt_Dt_HDD, mapping = aes(x= Years, 
                                        y=rollmean(HDD, 10, na.pad=TRUE)), 
              color = 'blue', size = 1.33, linetype = "longdash") +
    geom_line(Plt_Dt_CDD, mapping = aes(x=Years, y = CDD), col='red') +
    geom_line(Plt_Dt_CDD, mapping = aes(x= Years, 
                                        y=rollmean(CDD, 10, na.pad=TRUE)), 
              color = 'red', size = 1.33, linetype = "longdash") +
    ggtitle(paste0("Mean Inferred Demand")) +
    ylab("Daily Aggregated Degree Hours") +
    theme_bw() +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          plot.title = element_text(size=18))
  
  
  #______________________________________________________________________________#
  ###----Plot Three: Map of the Thermal Load Heat Factor-----###
  
  #Load Population and Temperature Grid Cell Data
  Thermal_Load <- block_sizes[sel_block]*NERC_TL_Region[[sel_rto]][[3]]/NERC_TL_Region[[sel_rto]][[1]][[4]]
  
  #Plotting Dataset
  Plt_Dt <- data.frame(Years = 1951:2021,
                       TL = Thermal_Load)
  
  
  p4 <-  ggplot(Plt_Dt) +
    geom_line(mapping = aes(x=Years, y = TL)) +
    geom_line(aes(x= Years, y=rollmean(TL, 10, na.pad=TRUE)), 
              color = 'black', size = 1.33, linetype = "dashed") +
    ggtitle(paste0("Load Factors \n Total Thermal Load")) +
    ylab("Load Factor") +
    theme_bw() +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=12),
          plot.title = element_text(size=18))
  
  
  
  
  
  
  ###---------------Combine the plots------------------------------------------###
  
  
  p_total <- plot_grid(p1,p4,p2,p3,
                       nrow =2,
                       labels = c('A', 'B', 'C', 'D'), 
                       label_size = 14)
  
  print(p_total)
  
}


dev.off()
