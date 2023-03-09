#______________________________________________________________________________#
###---Script to generate CONUS wide site-level plots CDD, HDD and Thermal Load---###
###1. Plot the average heating, cooling and total thermal load profiles 
###2. at the the grid cell level. 

###Input
#1. CDD AM Data
#2. HDD AM Data

###Output
#1. Plots of trends in CDD and HDD


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
library(cowplot)

#Load Functions
source("functions/Get_Day_Difference.R")

#Load Temperature Grid Cell Data
load("data/processed_data/HDD_Site_Level.RData")  
load("data/processed_data/CDD_Site_Level.RData") 
load("data/processed_data/Thermal_Load_Site_Level.RData") 


#Load the CONUS Locations
all_grids <- get(load("data/NERC_Regions_lat_lon_index_key.RData"))
grid_locs <- bind_rows(lapply(all_grids,data.frame))


#NERC Shapefiles
egrids <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/egrid2020_subregions"),
                  layer="eGRID2020_subregions")


#ggplot shape files
world <- map_data("world")
us <- map_data("state")


#Save the plots
pdf("figures/Sites_CONUS_Supplement_Average_Profile.pdf", height=3700/300, width=5000/300)


#______________________________________________________________________________#


###Compute Mean Values
mean_cdd <- colMeans(CONUS_CDD_Site$Mean)
mean_hdd <- colMeans(CONUS_HDD_Site$Mean)
mean_ttl <- colMeans(CONUS_Thermal_Load_Site$Mean)

tx_max <- max(mean_ttl, mean_cdd, mean_hdd)


###---------------------------Plot the CDD Values---------------------------###
grid_locs$CDD <- mean_cdd

p1 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = CDD)) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(0, tx_max), name = "Inferred Demand (deg F) ") +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.15) +
  ggtitle(" Mean Cooling Demand") +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28),
        legend.key.width = unit(2, "cm")) 


###---------------------------Plot the HDD Values---------------------------###
grid_locs$HDD <- mean_hdd

p2 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = HDD)) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(0, tx_max), name = "HDD ") +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.15) +
  ggtitle(" Mean Heating Demand") +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28),
        legend.key.width = unit(2, "cm")) 


###----------------------Plot the Total Thermal Values----------------------###
grid_locs$TL <- mean_ttl

p3 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = TL)) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(0, tx_max), name = "TL") +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.15) +
  ggtitle(" Mean Total Thermal Demand") +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28),
        legend.key.width = unit(2, "cm")) 




###---------------Combine the plots------------------------------------------###


p_total <- plot_grid(p1 + theme(legend.position="none"),
                     p2 + theme(legend.position="none"),
                     p3 + theme(legend.position="none"),
                     nrow =2,
                     labels = c('A', 'B', 'C'), 
                     label_size = 20)

legend_b <- get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1, override.aes = list(size=2))) +
    theme(legend.position = "bottom")
)



# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
#plot_grid(p_total, legend, rel_widths = c(1, .2))
pf <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .2))

print(pf)

dev.off()
















