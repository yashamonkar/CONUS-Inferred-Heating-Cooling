#______________________________________________________________________________#
###---Script to generate Spatial Boundaries for ERA-5 and Grid Sub-Regions---###



###Output
#1. Plot of ERA-5 grid points
#2. Plot of Grid Sub-regions


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
source("~/GitHub/CONUS-Inferred-Heating-Cooling/functions/Get_Conus_Regions.R")


#Load the ERA-5 CONUS Locations
all_grids <- get(load("data/NERC_Regions_lat_lon_index_key.RData"))
grid_locs <- bind_rows(lapply(all_grids,data.frame))


#NERC Shapefiles
egrids <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/egrid2020_subregions"),
                  layer="eGRID2020_subregions")
nerc_sf <- get_egrids(egrids_sf = egrids)
nerc_labels <- nerc_sf$Labels
n_regions <- length(nerc_sf$Labels)


#ggplot shape files
world <- map_data("world")
us <- map_data("state")


#Save the plots
#pdf("figures/Sites_CONUS.pdf", height=3700/300, width=5000/300)


#______________________________________________________________________________#
###---------------------Plot-1  ERA-5 CONUS Grid Points----------------------###

p1 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#FFFFFF", color = "black", size = 0.5) +
  geom_point(grid_locs, mapping = aes(x = Longitude, y = Latitude), col = 'red', size = 0.5) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  ggtitle("    ERA-5 Temperature Grid Points") +
  theme_bw() +
  theme(axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=20)) 


#______________________________________________________________________________#
###------------------------Plot-2  Grid Sub-Regions--------------------------###

#Generate Initial Plot
p2 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.5) +
  ggtitle("    Grid Sub-Regions")

n_cols <- c("#ff6666", "#3377ff", "#4dff4d", "#ffff4d", "#660066")


for(i in 1:n_regions){
  
  #Subset to RTO
  sub_region <- nerc_sf$Shapefiles[[i]]
  
  #Sample a color
  cur_col <- sample(n_cols, 1)
  
  #Add color to the sub-region
  p2 <- p2 +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group),
                 fill = cur_col, size = 0.75, color = 'black', alpha = 0.25) 
  
}

#Combine the Centroids
cent <- matrix(NA, ncol = 3, nrow = n_regions)

for(i in 1:n_regions){
  
  #Subset to RTO
  sub_region <- nerc_sf$Shapefiles[[i]]
  
  #Extract Centroid
  ct_temp <- gCentroid(sub_region)
  cent[i,1:2] <- ct_temp@coords[1:2]
  cent[i,3] <- 64+i
  
  #COnvert to dataframe
  ct_temp <- data.frame(matrix(cent[i,], nrow = 1))
  
  p2 <- p2 +
    geom_point(data = ct_temp, mapping = aes(x = X1, y = X2), shape = ct_temp$X3,
               size = 5, color = 'white') 
  
}

p2 <- p2 + 
  theme(axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=20)) 


#______________________________________________________________________________#
###---------------Combine the plots------------------------------------------###
#Save the plots
pdf("figures/Spatial_Supplement.pdf", height=1850/300, width=5000/300)

p_total <- plot_grid(p1,p2,
                     nrow =1,
                     labels = c('A', 'B'), 
                     label_size = 14)



print(p_total)
dev.off()
