#______________________________________________________________________________#
###---Script to generate CONUS wide site-level plots CDD, HDD and Thermal Load---###
###1. Estimate the trend in population adjusted peak HDD/CDD for values and Dates
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
source("functions/Get_Field_Significance.R")

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
pdf("figures/Sites_CONUS.pdf", height=3700/300, width=5000/300)


#______________________________________________________________________________#
###---Trend in the mean---###


###--------------------------------CDD---------------------------------------###
fill_col <- slope <- list()
sig <- rep(0, nrow(grid_locs))

pb = txtProgressBar(min = 1, max = nrow(grid_locs), initial = 1) 
for(i in 1:nrow(grid_locs)){
  
  setTxtProgressBar(pb,i)  #Progress Bar
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- CONUS_CDD_Site$Mean[,i]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[i] = 0.01}
  
  #Sens_Slop -- 
  p_col <- 100*ts_ss$estimates/mean(ts_agg) 
  
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


grid_locs$Significance <- sig
grid_locs$color <- unlist(fill_col)
grid_locs$Change <- unlist(slope)

sig_points <- grid_locs %>% filter(Significance > 0)


#Plotting the results
p1 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = Change)) +
  geom_point(sig_points, mapping = aes(x = Longitude, y = Latitude), size = 0.1, alpha = 0.5) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(-1, 1), name = "Scaled Slope (%/yr)  ") +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.15) +
  ggtitle(" Cooling Demand - Annual Mean") +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=24),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28),
        legend.key.width = unit(2, "cm")) 


###--------------------------------HDD---------------------------------------###
fill_col <- slope <- list()
sig <- rep(0, nrow(grid_locs))

pb = txtProgressBar(min = 1, max = nrow(grid_locs), initial = 1) 
for(i in 1:nrow(grid_locs)){
  
  setTxtProgressBar(pb,i)  #Progress Bar
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- CONUS_HDD_Site$Mean[,i]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[i] = 0.01}
  
  #Sens_Slop -- 
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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


grid_locs$Significance <- sig
grid_locs$color <- unlist(fill_col)
grid_locs$Change <- unlist(slope)

sig_points <- grid_locs %>% filter(Significance > 0)


#Plotting the results
p2 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = Change)) +
  geom_point(sig_points, mapping = aes(x = Longitude, y = Latitude), size = 0.1, alpha = 0.5) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(-1, 1)) +
  #geom_polygon(data = egrids, mapping = aes( x = long, y = lat, group = group), 
  #             fill = NA, color = 'black', size = 0.25) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.15) +
  ggtitle(" Heating Demand - Annual Mean") +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=24),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28),
        legend.key.width  = unit(2, "cm"))




###-----------------------------Thermal Load---------------------------------###
fill_col <- slope <- list()
sig <- rep(0, nrow(grid_locs))

pb = txtProgressBar(min = 1, max = nrow(grid_locs), initial = 1) 
for(i in 1:nrow(grid_locs)){
  
  setTxtProgressBar(pb,i)  #Progress Bar
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- CONUS_Thermal_Load_Site$Mean[,i]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[i] = 0.01}
  
  #Sens_Slop -- 
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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


grid_locs$Significance <- sig
grid_locs$color <- unlist(fill_col)
grid_locs$Change <- unlist(slope)

sig_points <- grid_locs %>% filter(Significance > 0)


#Plotting the results
p3 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = Change)) +
  geom_point(sig_points, mapping = aes(x = Longitude, y = Latitude), size = 0.1, alpha = 0.5) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(-1, 1)) +
  #geom_polygon(data = egrids, mapping = aes( x = long, y = lat, group = group), 
  #             fill = NA, color = 'black', size = 0.25) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.15) +
  ggtitle(" Total Thermal Demand - Annual Mean") +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=24),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28),
        legend.key.width  = unit(2, "cm"))




###---------------Combine the plots------------------------------------------###


p_total <- plot_grid(p1 + theme(legend.position="none"),
                     p2 + theme(legend.position="none"),
                     p3 + theme(legend.position="none"),
                     nrow =2,
                     labels = c('a', 'b', 'c'), 
                     label_size = 20)

legend_b <- get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1, override.aes = list(size=2))) +
    theme(legend.position = "bottom")
)

#Save the Legend
save(legend_b, file = "simulations/Legend.RData")

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
#plot_grid(p_total, legend, rel_widths = c(1, .2))
plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .2))




#______________________________________________________________________________#
##----------------------Trends Analysis on Extreme Value---------------------###
#Block
block_sizes <- c(6,12,24,72, 168, 336) #hours


#Current Block Size
j <- 4


###--------------------------------CDD---------------------------------------###
fill_col <- slope <- list()
sig <- rep(0, nrow(grid_locs))

pb = txtProgressBar(min = 1, max = nrow(grid_locs), initial = 1) 
for(i in 1:nrow(grid_locs)){
  
  setTxtProgressBar(pb,i)  #Progress Bar
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- CONUS_CDD_Site$Site_Level[[1]][[j]][,i]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[i] = 0.01}
  
  #Sens_Slop -- 
  p_col <- 100*ts_ss$estimates/mean(ts_agg) 
  
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


grid_locs$Significance <- sig
grid_locs$color <- unlist(fill_col)
grid_locs$Change <- unlist(slope)

sig_points <- grid_locs %>% filter(Significance > 0)


#Plotting the results
p1 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = Change)) +
  geom_point(sig_points, mapping = aes(x = Longitude, y = Latitude), size = 0.1, alpha = 0.5) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(-1, 1), name = "Scaled Slope (%/yr)  ") +
  #geom_polygon(data = egrids, mapping = aes( x = long, y = lat, group = group), 
  #             fill = NA, color = 'black', size = 0.25) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.15) +
  ggtitle(paste0(" Cooling Demand - Annual Peak")) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=24),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28),
        legend.key.width = unit(2, "cm")) 

###--------------------------------HDD---------------------------------------###
fill_col <- slope <- list()
sig <- rep(0, nrow(grid_locs))

pb = txtProgressBar(min = 1, max = nrow(grid_locs), initial = 1) 
for(i in 1:nrow(grid_locs)){
  
  setTxtProgressBar(pb,i)  #Progress Bar
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- CONUS_HDD_Site$Site_Level[[1]][[j]][,i]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[i] = 0.01}
  
  #Sens_Slop -- 
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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


grid_locs$Significance <- sig
grid_locs$color <- unlist(fill_col)
grid_locs$Change <- unlist(slope)

sig_points <- grid_locs %>% filter(Significance > 0)


#Plotting the results
p2 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = Change)) +
  geom_point(sig_points, mapping = aes(x = Longitude, y = Latitude), size = 0.1, alpha = 0.5) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(-1, 1)) +
  #geom_polygon(data = egrids, mapping = aes( x = long, y = lat, group = group), 
  #             fill = NA, color = 'black', size = 0.25) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.15) +
  ggtitle(paste0(" Heating Demand - Annual Peak")) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=24),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28),
        legend.key.width  = unit(2, "cm"))


###---------------------------Thermal Load-----------------------------------###
fill_col <- slope <- list()
sig <- rep(0, nrow(grid_locs))

pb = txtProgressBar(min = 1, max = nrow(grid_locs), initial = 1) 
for(i in 1:nrow(grid_locs)){
  
  setTxtProgressBar(pb,i)  #Progress Bar
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- CONUS_Thermal_Load_Site$Site_Level[[1]][[j]][,i]
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[i] = 0.01}
  
  #Sens_Slop -- 
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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


grid_locs$Significance <- sig
grid_locs$color <- unlist(fill_col)
grid_locs$Change <- unlist(slope)

sig_points <- grid_locs %>% filter(Significance > 0)


#Plotting the results
p3 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = Change)) +
  geom_point(sig_points, mapping = aes(x = Longitude, y = Latitude), size = 0.1, alpha = 0.5) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(-1, 1)) +
  #geom_polygon(data = egrids, mapping = aes( x = long, y = lat, group = group), 
  #             fill = NA, color = 'black', size = 0.25) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.15) +
  ggtitle(paste0(" Total Thermal Demand - Annual Peak")) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=24),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28),
        legend.key.width  = unit(2, "cm"))


###---------------Combine the plots------------------------------------------###


p_total <- plot_grid(p1 + theme(legend.position="none"),
                     p2 + theme(legend.position="none"),
                     p3 + theme(legend.position="none"),
                     nrow =2,
                     labels = c('a', 'b', 'c'), 
                     label_size = 20)

legend_b <- get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1, override.aes = list(size=2))) +
    theme(legend.position = "bottom")
)

#Save the Legend
save(legend_b, file = "simulations/Legend.RData")

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
#plot_grid(p_total, legend, rel_widths = c(1, .2))
pf <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .2))

print(pf)



#______________________________________________________________________________#
##----------------------Trends Analysis on Load Factor-----------------------###


#Current Block Size
j <- 4


###--------------------------------CDD---------------------------------------###
fill_col <- slope <- list()
sig <- rep(0, nrow(grid_locs))

pb = txtProgressBar(min = 1, max = nrow(grid_locs), initial = 1) 
for(i in 1:nrow(grid_locs)){
  
  setTxtProgressBar(pb,i)  #Progress Bar
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- CONUS_CDD_Site$Mean[,i]/CONUS_CDD_Site$Site_Level[[1]][[j]][,i]
  ts_agg <- na.omit(ts_agg)
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[i] = 0.01}
  
  #Sens_Slop -- 
  p_col <- 100*ts_ss$estimates/mean(ts_agg) 
  
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


grid_locs$Significance <- sig
grid_locs$color <- unlist(fill_col)
grid_locs$Change <- unlist(slope)

sig_points <- grid_locs %>% filter(Significance > 0)


#Plotting the results
p1 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = Change)) +
  geom_point(sig_points, mapping = aes(x = Longitude, y = Latitude), size = 0.1, alpha = 0.5) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(-1, 1), name = "Scaled Slope (%/yr)  ") +
  #geom_polygon(data = egrids, mapping = aes( x = long, y = lat, group = group), 
  #             fill = NA, color = 'black', size = 0.25) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.15) +
  ggtitle(paste0(" Cooling Demand - Thermal Load Factor")) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28),
        legend.key.width = unit(2, "cm")) 

###--------------------------------HDD---------------------------------------###
fill_col <- slope <- list()
sig <- rep(0, nrow(grid_locs))

pb = txtProgressBar(min = 1, max = nrow(grid_locs), initial = 1) 
for(i in 1:nrow(grid_locs)){
  
  setTxtProgressBar(pb,i)  #Progress Bar
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- CONUS_HDD_Site$Mean[,i]/CONUS_HDD_Site$Site_Level[[1]][[j]][,i]
  ts_agg <- na.omit(ts_agg)
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[i] = 0.01}
  
  #Sens_Slop -- 
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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


grid_locs$Significance <- sig
grid_locs$color <- unlist(fill_col)
grid_locs$Change <- unlist(slope)

sig_points <- grid_locs %>% filter(Significance > 0)


#Plotting the results
p2 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = Change)) +
  geom_point(sig_points, mapping = aes(x = Longitude, y = Latitude), size = 0.1, alpha = 0.5) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(-1, 1)) +
  #geom_polygon(data = egrids, mapping = aes( x = long, y = lat, group = group), 
  #             fill = NA, color = 'black', size = 0.25) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.15) +
  ggtitle(paste0(" Heating Demand - Thermal Load Factor")) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28),
        legend.key.width  = unit(2, "cm"))


###---------------------------Thermal Load-----------------------------------###
fill_col <- slope <- list()
sig <- rep(0, nrow(grid_locs))

pb = txtProgressBar(min = 1, max = nrow(grid_locs), initial = 1) 
for(i in 1:nrow(grid_locs)){
  
  setTxtProgressBar(pb,i)  #Progress Bar
  
  #Get Per-Captia Aggregated Degree Day Time Series
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- CONUS_Thermal_Load_Site$Mean[,i]/CONUS_Thermal_Load_Site$Site_Level[[1]][[j]][,i]
  ts_agg <- na.omit(ts_agg)
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[i] = 0.01}
  
  #Sens_Slop -- 
  p_col <- 100*ts_ss$estimates/mean(ts_agg)
  
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


grid_locs$Significance <- sig
grid_locs$color <- unlist(fill_col)
grid_locs$Change <- unlist(slope)

sig_points <- grid_locs %>% filter(Significance > 0)


#Plotting the results
p3 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_tile(grid_locs, mapping = aes(x = Longitude, y = Latitude, fill = Change)) +
  geom_point(sig_points, mapping = aes(x = Longitude, y = Latitude), size = 0.1, alpha = 0.5) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red", 
                       limits = c(-1, 1)) +
  #geom_polygon(data = egrids, mapping = aes( x = long, y = lat, group = group), 
  #             fill = NA, color = 'black', size = 0.25) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 0.15) +
  ggtitle(paste0(" Thermal Load Factor")) +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28),
        legend.key.width  = unit(2, "cm"))


###---------------Combine the plots------------------------------------------###


p_total <- plot_grid(p1 + theme(legend.position="none"),
                     p2 + theme(legend.position="none"),
                     p3 + theme(legend.position="none"),
                     nrow =2,
                     labels = c('a', 'b', 'c'), 
                     label_size = 20)

legend_b <- get_legend(
  p1 + 
    guides(color = guide_legend(nrow = 1, override.aes = list(size=2))) +
    theme(legend.position = "bottom")
)

#Save the Legend
save(legend_b, file = "simulations/Legend.RData")

# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
#plot_grid(p_total, legend, rel_widths = c(1, .2))
pf <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .2))

print(pf)

dev.off()


#------------------------------------------------------------------------------#
#Additional Plots

#Save the plots
pdf("figures/Sites_CONUS_Load_Factors.pdf", height=1850/300, width=2500/300)

p_total <- plot_grid(p3 + theme(legend.position="none"),
                     nrow =1)

pf <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .2))

print(pf)

dev.off()




#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#

#Cooling Demand --- Mean ---- Trends Significant
start.time <- Sys.time()
get_field_significance(Data_Matrix = CONUS_CDD_Site$Mean,
                       p_thresh = 0.05)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)


#Heating Demand --- Mean --- Trends Significant
start.time <- Sys.time()
get_field_significance(Data_Matrix = CONUS_HDD_Site$Mean,
                       p_thresh = 0.05)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)


#Thermal Demand --- Mean --- Trends Significant
start.time <- Sys.time()
get_field_significance(Data_Matrix = CONUS_Thermal_Load_Site$Mean,
                       p_thresh = 0.05)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)



#Cooling Demand --- Peak ---- Trends Significant
start.time <- Sys.time()
get_field_significance(Data_Matrix = CONUS_CDD_Site$Site_Level[[1]][[4]],
                       p_thresh = 0.05)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)



#Heating Demand --- Peak ---- Trends Significant
start.time <- Sys.time()
get_field_significance(Data_Matrix = CONUS_HDD_Site$Site_Level[[1]][[4]],
                       p_thresh = 0.05)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)




#Thermal Demand --- Mean ---- Trends Significant 
start.time <- Sys.time()
get_field_significance(Data_Matrix = CONUS_Thermal_Load_Site$Site_Level[[1]][[4]],
                       p_thresh = 0.05)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)



#Thermal --- Load Factors ---- Spatial Correlation. 

Load_Factors <- matrix(NA, ncol = ncol(CONUS_Thermal_Load_Site$Mean),
                       nrow = nrow(CONUS_Thermal_Load_Site$Mean))

for(i in 1:ncol(CONUS_Thermal_Load_Site$Mean)) {
  Load_Factors[,i] <-  CONUS_Thermal_Load_Site$Mean[,i]/CONUS_Thermal_Load_Site$Site_Level[[1]][[4]][,i]
}



start.time <- Sys.time()
get_field_significance(Data_Matrix = Load_Factors,
                       p_thresh = 0.1)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
