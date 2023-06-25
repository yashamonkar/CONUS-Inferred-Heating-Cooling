#______________________________________________________________________________#
###---Script to generate CONUS wide site-level plots CDD, HDD and Thermal Load---###
###-----------Response to Revierwer 2------------------------------------------#
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
pdf("figures/Sites_CONUS_Magnitude.pdf", height=3700/300, width=5000/300)


#______________________________________________________________________________#
###------------------------Trend in the mean---------------------------------###

#####---------HARDCODING TO GET THE LEGEND LIMITS----------------------------###


##CDD
slope_cdd <- list()
for(i in 1:nrow(grid_locs)){
  ts_agg <- CONUS_CDD_Site$Mean[,i]
  ts_ss <- sens.slope(ts_agg)
  p_col <- ts_ss$estimates
  slope_cdd[[i]] <- p_col
}


##HDD
slope_hdd <- list()
for(i in 1:nrow(grid_locs)){
  ts_agg <- CONUS_HDD_Site$Mean[,i]
  ts_ss <- sens.slope(ts_agg)
  p_col <- ts_ss$estimates
  slope_hdd[[i]] <- p_col
}


##Total Thermal Demand
slope_tldd <- list()
for(i in 1:nrow(grid_locs)){
  ts_agg <- CONUS_Thermal_Load_Site$Mean[,i]
  ts_ss <- sens.slope(ts_agg)
  p_col <- ts_ss$estimates
  slope_tldd[[i]] <- p_col
}

min_leg <- min(unlist(slope_cdd), unlist(slope_hdd), unlist(slope_tldd))
max_leg <- max(unlist(slope_cdd), unlist(slope_hdd), unlist(slope_tldd))

#Set to a symmetric scale
legend_limit <- max(abs(min_leg), abs(max_leg))
min_leg <- -legend_limit*1.5
max_leg <- legend_limit*1.5

###--------------------------------CDD---------------------------------------###
slope <- list()
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
  p_col <- ts_ss$estimates
  slope[[i]] <- p_col
  
}


grid_locs$Significance <- sig
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
                       limits = c(min_leg, max_leg), 
                       name = "Estimated Slope in terms of \n (Inferred Demand/yr)") +
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
slope <- list()
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
  p_col <- ts_ss$estimates
  slope[[i]] <- p_col
  
}


grid_locs$Significance <- sig
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
                       limits = c(min_leg, max_leg), 
                       name = "Estimated Slope in terms of \n (Inferred Demand/yr)") +
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
slope <- list()
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
  p_col <- ts_ss$estimates
  slope[[i]] <- p_col
  
}


grid_locs$Significance <- sig
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
                       limits = c(min_leg, max_leg), 
                       name = "Estimated Slope in terms of \n (Inferred Demand/yr)") +
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
plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .2))




#______________________________________________________________________________#
##----------------------Trends Analysis on Extreme Value---------------------###
#Block
block_sizes <- c(6,12,24,72, 168, 336) #hours


#Current Block Size
j <- 4

#####---------HARDCODING TO GET THE LEGEND LIMITS----------------------------###


##CDD
slope_cdd <- list()
for(i in 1:nrow(grid_locs)){
  ts_agg <- CONUS_CDD_Site$Site_Level[[1]][[j]][,i]
  ts_ss <- sens.slope(ts_agg)
  p_col <- ts_ss$estimates
  slope_cdd[[i]] <- p_col
}


##HDD
slope_hdd <- list()
for(i in 1:nrow(grid_locs)){
  ts_agg <- CONUS_HDD_Site$Site_Level[[1]][[j]][,i]
  ts_ss <- sens.slope(ts_agg)
  p_col <- ts_ss$estimates
  slope_hdd[[i]] <- p_col
}


##Total Thermal Demand
slope_tldd <- list()
for(i in 1:nrow(grid_locs)){
  ts_agg <- CONUS_Thermal_Load_Site$Site_Level[[1]][[j]][,i]
  ts_ss <- sens.slope(ts_agg)
  p_col <- ts_ss$estimates
  slope_tldd[[i]] <- p_col
}

min_leg <- min(unlist(slope_cdd), unlist(slope_hdd), unlist(slope_tldd))
max_leg <- max(unlist(slope_cdd), unlist(slope_hdd), unlist(slope_tldd))

#Set to a symmetric scale
legend_limit <- max(abs(min_leg), abs(max_leg))
min_leg <- -legend_limit*1.5
max_leg <- legend_limit*1.5

###--------------------------------CDD---------------------------------------###
slope <- list()
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
  p_col <- ts_ss$estimates
  slope[[i]] <- p_col
  
}


grid_locs$Significance <- sig
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
                       limits = c(min_leg, max_leg), 
                       name = "Estimated Slope in terms of \n (Inferred Demand/yr)") +
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
slope <- list()
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
  p_col <- ts_ss$estimates
  slope[[i]] <- p_col
  
}


grid_locs$Significance <- sig
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
                       limits = c(min_leg, max_leg), 
                       name = "Estimated Slope in terms of \n (Inferred Demand/yr)") +
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
slope <- list()
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
  p_col <- ts_ss$estimates
  slope[[i]] <- p_col
  
  
}


grid_locs$Significance <- sig
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
                       limits = c(min_leg, max_leg), 
                       name = "Estimated Slope in terms of \n (Inferred Demand/yr)") +
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


#______________________________________________________________________________#
##----------------------Trends Analysis on Load Factor-----------------------###

#Current Block Size
j <- 4

#####---------HARDCODING TO GET THE LEGEND LIMITS----------------------------###


##CDD
slope_cdd <- list()
for(i in 1:nrow(grid_locs)){
  ts_agg <- CONUS_CDD_Site$Mean[,i]/CONUS_CDD_Site$Site_Level[[1]][[j]][,i]
  ts_agg <- na.omit(ts_agg)
  ts_ss <- sens.slope(ts_agg)
  p_col <- ts_ss$estimates
  slope_cdd[[i]] <- p_col
}


##HDD
slope_hdd <- list()
for(i in 1:nrow(grid_locs)){
  ts_agg <- CONUS_HDD_Site$Mean[,i]/CONUS_HDD_Site$Site_Level[[1]][[j]][,i]
  ts_agg <- na.omit(ts_agg)
  ts_ss <- sens.slope(ts_agg)
  p_col <- ts_ss$estimates
  slope_hdd[[i]] <- p_col
}


##Total Thermal Demand
slope_tldd <- list()
for(i in 1:nrow(grid_locs)){
  ts_agg <- CONUS_Thermal_Load_Site$Mean[,i]/CONUS_Thermal_Load_Site$Site_Level[[1]][[j]][,i]
  ts_agg <- na.omit(ts_agg)
  ts_ss <- sens.slope(ts_agg)
  p_col <- ts_ss$estimates
  slope_cdd[[i]] <- p_col
}

min_leg <- min(unlist(slope_cdd), unlist(slope_hdd), unlist(slope_tldd))
max_leg <- max(unlist(slope_cdd), unlist(slope_hdd), unlist(slope_tldd))


#Set to a symmetric scale
legend_limit <- max(abs(min_leg), abs(max_leg))
min_leg <- -legend_limit*1.5
max_leg <- legend_limit*1.5

###--------------------------------CDD---------------------------------------###
slope <- list()
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
  p_col <- ts_ss$estimates
  slope[[i]] <- p_col
  
}


grid_locs$Significance <- sig
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
                       limits = c(min_leg, max_leg), 
                       name = "Slope") +
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
slope <- list()
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
  p_col <- ts_ss$estimates
  slope[[i]] <- p_col
  
}


grid_locs$Significance <- sig
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
                       limits = c(min_leg, max_leg), 
                       name = "Slope") +
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
slope <- list()
sig <- rep(0, nrow(grid_locs))

pb = txtProgressBar(min = 1, max = nrow(grid_locs), initial = 1) 
for(i in 1:nrow(grid_locs)){
  
  setTxtProgressBar(pb,i)  #Progress Bar
  
  #Get Per-Captia Aggregated Degree Day Time Series
  ts_agg <- CONUS_Thermal_Load_Site$Mean[,i]/CONUS_Thermal_Load_Site$Site_Level[[1]][[j]][,i]
  ts_agg <- na.omit(ts_agg)
  
  #Sens_Slope
  ts_ss <- sens.slope(ts_agg)
  
  #Significance
  if(ts_ss$p.value < 0.05) {
    sig[i] = 0.01}
  
  #Sens_Slop -- 
  p_col <- ts_ss$estimates
  slope[[i]] <- p_col

}


grid_locs$Significance <- sig
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
                       limits = c(min_leg, max_leg), 
                       name = "Slope") +
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



#------------------------------------------------------------------------------#
#Additional Plots

#Save the plots
pdf("figures/Sites_CONUS_Load_Factors_Mag.pdf", height=1850/300, width=2500/300)

p_total <- plot_grid(p3 + theme(legend.position="none"),
                     nrow =1)

pf <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .2))

print(pf)

dev.off()