#______________________________________________________________________________#
###---Script to understand role of Extreme Winter Event----------------------###

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
library(gridExtra)
library(cowplot)


#Load Functions
source("~/GitHub/CONUS-Inferred-Heating-Cooling/functions/Get_Conus_Regions.R")

#NERC Shapefiles
egrids <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/egrid2020_subregions"),
                  layer="eGRID2020_subregions")
nerc_sf <- get_egrids(egrids_sf = egrids)
nerc_labels <- nerc_sf$Labels
n_regions <- length(nerc_sf$Labels)


#Plotting the grid points
world <- map_data("world")
us <- map_data("state")


#Load the Legend
load("simulations/Legend_Population.RData")


#______________________________________________________________________________#
yr <- c(1951, 1962, 1984, 1990, 2021)
yrs <- 1951:2021
j <- 4

DD_Regional <- get(load("data/processed_data/HDD_Regional_2020.RData"))

pop_color <- list()

for(i in 1:length(nerc_labels)){
  
  ts_2020 <- DD_Regional[[i]][[1]][[j]]/24
  avg <- mean(ts_2020)
  peak <- mean(ts_2020[yrs %in% yr])
  
  pop_color[[i]] <- (peak-avg)/avg
  
  print(paste0(i, " - ", nerc_labels[[i]], " is ", round(100*(peak-avg)/avg, 2)))
  
}



fill_col <- list()

for(i in 1:length(nerc_labels)) {
  
  if(pop_color[[i]] > 0) {
    colfunc <- colorRamp(c("#FFFFFF", "#FF0000" ))
    fill_col[[i]] <- rgb(colfunc(pop_color[[i]]), maxColorValue = 255)  
  } else {
    pop_color[[i]] <- -pop_color[[i]]
    colfunc <- colorRamp(c("#FFFFFF", "#0000FF"))
    fill_col[[i]] <- rgb(colfunc(pop_color[[i]]), maxColorValue = 255)  
  }
  
}



p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -65))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "brown", size = 1)

for(i in 1:n_regions){
  
  #Subset to RTO
  sub_region <- nerc_sf$Shapefiles[[i]]
  
  #Add color to the sub-region
  p <- p +
    geom_polygon(data = sub_region, mapping = aes(x = long, y = lat, group = group), 
                 fill = fill_col[[i]], color = 'black', size = 0.5, alpha = 0.9)
  
  
}


p <- p + 
  ggtitle("Extreme Winter Events") +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=20),
        legend.key.width = unit(2, "cm"))


  
pdf("figures/Extreme_Winter_Events.pdf")
plot_grid(p, legend_b, ncol = 1, rel_heights = c(1, .2)) #--- Add Legend
dev.off()
