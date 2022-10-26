#______________________________________________________________________________#
###---Script to generate CONUS wide plots for ISO Regions---###
###1. Estimate the trend in population adjusted peak HDD/CDD/Thermal for values and Dates
###2. at the ISO/Interconnect level.

#One plot at the CONUS level, with the RTO boundaries and shading for slope and significance. 


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
source("~/GitHub/CONUS-Inferred-Heating-Cooling/functions/Get_Conus_Regions.R")

#Load Temperature Grid Cell Data
load("data/processed_data/HDD_Regional.RData") #NERC_HDD_Region
load("data/processed_data/CDD_Regional.RData") #NERC_CDD_Region
load("data/processed_data/Thermal_Load_Regional.RData") 

#Load the Grid Cells Data
load("data/NERC_Regions_lat_lon_index_key.RData")


#NERC Shapefiles
egrids <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/egrid2020_subregions"),
                  layer="eGRID2020_subregions")
nerc_sf <- get_egrids(egrids_sf = egrids)
nerc_labels <- nerc_sf$Labels
n_regions <- length(nerc_sf$Labels)


#Load the Legend
load("simulations/Legend.RData")


#______________________________________________________________________________#
###Hyper-Parameters###
block_sizes <- c(6,12,24,72, 168, 336) #hours
j <- 4

#ggplot shape files
world <- map_data("world")
us <- map_data("state")


#Save the plots
pdf("figures/Regional_CONUS.pdf", height=3700/300, width=5000/300)



#______________________________________________________________________________#
###----------------------------Trends Analysis on Means---------------------###


###--------------------------------CDD---------------------------------------###

#Generate Initial Plot
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  ggtitle("CDD - Mean")

p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- NERC_CDD_Region[[i]][[3]]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[[i]] = 1}
  
  #Sens_Slop
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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
  p <- p +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
                 fill = fill_col, color = 'black', size = 0.75)
  
  #Add points for significance
  if(sig[[i]] == 1) {
    plt_grids <- grid_nerc[[i]]
    p <- p + geom_point(data = plt_grids, mapping = aes(x = Longitude, y = Latitude), size = 0.05, alpha = 0.5)
  }
  
}

#Add state boundaries
p1 <- p + 
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#964B00", size = 0.1) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=15),
        legend.key.width = unit(2, "cm")) 



###--------------------------------HDD---------------------------------------###

#Generate Initial Plot
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  ggtitle("HDD - Mean")


p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- NERC_HDD_Region[[i]][[3]]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[[i]] = 1}
  
  #Sens_Slop
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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
  p <- p +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
                 fill = fill_col, color = 'black', size = 0.75)
  
  #Add points for significance
  if(sig[[i]] == 1) {
    plt_grids <- grid_nerc[[i]]
    p <- p + geom_point(data = plt_grids, mapping = aes(x = Longitude, y = Latitude), size = 0.05, alpha = 0.5)
  }
  
  
}

#Add state boundaries
p2 <- p + 
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#964B00", size = 0.1) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=15),
        legend.key.width = unit(2, "cm")) 


###--------------------------Thermal Load------------------------------------###

#Generate Initial Plot
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  ggtitle("Thermal Load - Mean")


p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- NERC_TL_Region[[i]][[3]]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[[i]] = 1}
  
  #Sens_Slop
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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
  p <- p +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
                 fill = fill_col, color = 'black', size = 0.75)
  
  #Add points for significance
  if(sig[[i]] == 1) {
    plt_grids <- grid_nerc[[i]]
    p <- p + geom_point(data = plt_grids, mapping = aes(x = Longitude, y = Latitude), size = 0.05, alpha = 0.5)
  }
  
  
}

#Add state boundaries
p3 <- p + 
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#964B00", size = 0.1) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=15),
        legend.key.width = unit(2, "cm")) 



###---------------Combine the plots------------------------------------------###


p_total <- plot_grid(p1,
                     p2,
                     p3,
                     nrow =2,
                     labels = c('A', 'B', 'C'), 
                     label_size = 12)


# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
#plot_grid(p_total, legend, rel_widths = c(1, .2))
plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .2))



#______________________________________________________________________________#
###----------------------------Trends Analysis on Peaks----------------------###

###--------------------------------CDD---------------------------------------###

#Generate Initial Plot
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  ggtitle("CDD - Peak")

p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- NERC_CDD_Region[[i]][[1]][[j]]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[[i]] = 1}
  
  #Sens_Slop
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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
  p <- p +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
                 fill = fill_col, color = 'black', size = 0.75)
  
  #Add points for significance
  if(sig[[i]] == 1) {
    plt_grids <- grid_nerc[[i]]
    p <- p + geom_point(data = plt_grids, mapping = aes(x = Longitude, y = Latitude), size = 0.05, alpha = 0.5)
  }
  
}

#Add state boundaries
p1 <- p + 
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#964B00", size = 0.1) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=15),
        legend.key.width = unit(2, "cm")) 



###--------------------------------HDD---------------------------------------###

#Generate Initial Plot
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  ggtitle("HDD - Peak")


p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- NERC_HDD_Region[[i]][[1]][[j]]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[[i]] = 1}
  
  #Sens_Slop
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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
  p <- p +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
                 fill = fill_col, color = 'black', size = 0.75)
  
  #Add points for significance
  if(sig[[i]] == 1) {
    plt_grids <- grid_nerc[[i]]
    p <- p + geom_point(data = plt_grids, mapping = aes(x = Longitude, y = Latitude), size = 0.05, alpha = 0.5)
  }
  
  
}

#Add state boundaries
p2 <- p + 
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#964B00", size = 0.1) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=15),
        legend.key.width = unit(2, "cm")) 


###--------------------------Thermal Load------------------------------------###

#Generate Initial Plot
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  ggtitle("Thermal Load - Peak")


p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- NERC_TL_Region[[i]][[1]][[j]]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[[i]] = 1}
  
  #Sens_Slop
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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
  p <- p +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
                 fill = fill_col, color = 'black', size = 0.75)
  
  #Add points for significance
  if(sig[[i]] == 1) {
    plt_grids <- grid_nerc[[i]]
    p <- p + geom_point(data = plt_grids, mapping = aes(x = Longitude, y = Latitude), size = 0.05, alpha = 0.5)
  }
  
  
}

#Add state boundaries
p3 <- p + 
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#964B00", size = 0.1) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=15),
        legend.key.width = unit(2, "cm")) 



###---------------Combine the plots------------------------------------------###


p_total <- plot_grid(p1,
                     p2,
                     p3,
                     nrow =2,
                     labels = c('A', 'B', 'C'), 
                     label_size = 12)


# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
#plot_grid(p_total, legend, rel_widths = c(1, .2))
plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .2))



#______________________________________________________________________________#
###----------------------------Trends Analysis on Load Factor----------------------###

###--------------------------------CDD---------------------------------------###

#Generate Initial Plot
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  ggtitle("CDD - Load Factor")

p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- NERC_CDD_Region[[i]][[3]]/NERC_CDD_Region[[i]][[1]][[j]]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[[i]] = 1}
  
  #Sens_Slop
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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
  p <- p +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
                 fill = fill_col, color = 'black', size = 0.75)
  
  #Add points for significance
  if(sig[[i]] == 1) {
    plt_grids <- grid_nerc[[i]]
    p <- p + geom_point(data = plt_grids, mapping = aes(x = Longitude, y = Latitude), size = 0.05, alpha = 0.5)
  }
  
}

#Add state boundaries
p1 <- p + 
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#964B00", size = 0.1) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=15),
        legend.key.width = unit(2, "cm")) 



###--------------------------------HDD---------------------------------------###

#Generate Initial Plot
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  ggtitle("HDD - Load Factor")


p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- NERC_HDD_Region[[i]][[3]]/NERC_HDD_Region[[i]][[1]][[j]]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[[i]] = 1}
  
  #Sens_Slop
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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
  p <- p +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
                 fill = fill_col, color = 'black', size = 0.75)
  
  #Add points for significance
  if(sig[[i]] == 1) {
    plt_grids <- grid_nerc[[i]]
    p <- p + geom_point(data = plt_grids, mapping = aes(x = Longitude, y = Latitude), size = 0.05, alpha = 0.5)
  }
  
  
}

#Add state boundaries
p2 <- p + 
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#964B00", size = 0.1) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=15),
        legend.key.width = unit(2, "cm")) 


###--------------------------Thermal Load------------------------------------###

#Generate Initial Plot
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  ggtitle("Thermal Load - Load Factor")


p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- NERC_TL_Region[[i]][[3]]/NERC_TL_Region[[i]][[1]][[j]]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[[i]] = 1}
  
  #Sens_Slop
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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
  p <- p +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
                 fill = fill_col, color = 'black', size = 0.75)
  
  #Add points for significance
  if(sig[[i]] == 1) {
    plt_grids <- grid_nerc[[i]]
    p <- p + geom_point(data = plt_grids, mapping = aes(x = Longitude, y = Latitude), size = 0.05, alpha = 0.5)
  }
  
  
}

#Add state boundaries
p3 <- p + 
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#964B00", size = 0.1) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=15),
        legend.key.width = unit(2, "cm")) 



###---------------Combine the plots------------------------------------------###


p_total <- plot_grid(p1,
                     p2,
                     p3,
                     nrow =2,
                     labels = c('A', 'B', 'C'), 
                     label_size = 12)


# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
#plot_grid(p_total, legend, rel_widths = c(1, .2))
plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .2))


dev.off()

















#______________________________________________________________________________#
###----------------------------Trends Analysis on Dates---------------------###


###--------------------------------CDD---------------------------------------###

#Generate Initial Plot
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) 

Data_Type <- "CDD"
p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- NERC_CDD_Region[[i]][[2]][[j]]
  
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
    sig[[i]] = 1}
  
  #Sens_Slop
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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
  p <- p +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
                 fill = fill_col, color = 'black', size = 0.75)
  
  #Add points for significance
  if(sig[[i]] == 1) {
    plt_grids <- grid_nerc[[i]]
    p <- p + geom_point(data = plt_grids, mapping = aes(x = Longitude, y = Latitude), size = 0.05, alpha = 0.5)
  }
  
}

#Add state boundaries
p3 <- p + 
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#964B00", size = 0.1) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=15),
        legend.key.width = unit(2, "cm")) 



###--------------------------------HDD---------------------------------------###

#Generate Initial Plot
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) 

Data_Type <- "HDD"
p_col <- list()
sig <- rep(0, n_regions)

for(i in 1:n_regions){
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- NERC_HDD_Region[[i]][[2]][[j]]
  
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
    sig[[i]] = 1}
  
  #Sens_Slop
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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
  p <- p +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
                 fill = fill_col, color = 'black', size = 0.75)
  
  #Add points for significance
  if(sig[[i]] == 1) {
    plt_grids <- grid_nerc[[i]]
    p <- p + geom_point(data = plt_grids, mapping = aes(x = Longitude, y = Latitude), size = 0.05, alpha = 0.5)
  }
  
  
}

#Add state boundaries
p4 <- p + 
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#964B00", size = 0.1) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=15),
        legend.key.width = unit(2, "cm")) 



###---------------Combine the plots------------------------------------------###


p_total <- plot_grid(p3,
                     p4,
                     nrow =1,
                     labels = c('A', 'B'), 
                     label_size = 12)


# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
#plot_grid(p_total, legend, rel_widths = c(1, .2))
plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .2))

dev.off()