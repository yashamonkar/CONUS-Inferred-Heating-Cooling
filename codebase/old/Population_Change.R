#------------------------------------------------------------------------------#
#Subroutine to plot the population growth across the regions. 


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
library(zoo)
library(ncdf4) 
library(broom)
library(rgdal)

#Plotting the grid points
world <- map_data("world")
us <- map_data("state")


#Source functions
source("~/GitHub/CONUS-Inferred-Heating-Cooling/functions/Get_Conus_Regions.R")


#Load Population and Temperature Grid Cell Data
load("data/NERC_Regions_Temp_Population.RData")


#NERC Shapefiles
egrids <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/egrid2020_subregions"),
                  layer="eGRID2020_subregions")
nerc_sf <- get_egrids(egrids_sf = egrids)
nerc_labels <- nerc_sf$Labels
n_regions <- length(nerc_sf$Labels)


#Load the Legend
load("simulations/Legend_Population.RData")


#______________________________________________________________________________#
###Compute the population increases and generate the color.###
pop_inc <- list()

for(i in 1:length(nerc_labels)){
  inc <- colSums(nerc_pop_temp$Population[[i]])[7]/colSums(nerc_pop_temp$Population[[i]])[3]-1
  pop_inc[[i]] <- round(inc,2)
  print(paste0(i, "- The population change for ", nerc_labels[i], " is ", 100*pop_inc[[i]], " %"))
  
}


fill_col <- list()

for(i in 1:length(nerc_labels)) {

if(pop_inc[[i]] > 0) {
  colfunc <- colorRamp(c("#FFFFFF", "#FF0000" ))
  fill_col[[i]] <- rgb(colfunc(pop_inc[[i]]), maxColorValue = 255)  
} else {
  pop_inc[[i]] <- -pop_inc[[i]]
  colfunc <- colorRamp(c("#FFFFFF", "#0000FF"))
  fill_col[[i]] <- rgb(colfunc(pop_inc[[i]]), maxColorValue = 255)  
}

}



#______________________________________________________________________________#
###Plotting the results.###


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
                 fill = fill_col[[i]], color = 'black', size = 0.5, alpha = 0.85)
  
  
}


p <- p + 
  ggtitle("Population Changes (2000-2020)") +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=20),
        legend.key.width = unit(2, "cm")) 




#______________________________________________________________________________#
###Save the plot

pdf("figures/Population_Changes.pdf")
plot_grid(p, legend_b, ncol = 1, rel_heights = c(1, .2)) #--- Add Legend
dev.off()



