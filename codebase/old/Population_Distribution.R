#______________________________________________________________________________#
###---Script to understand population effects---###

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
source("functions/Get_Day_Difference.R")
source("~/GitHub/CONUS-Inferred-Heating-Cooling/functions/Get_Conus_Regions.R")


#NERC Shapefiles
egrids <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/egrid2020_subregions"),
                  layer="eGRID2020_subregions")
nerc_sf <- get_egrids(egrids_sf = egrids)
nerc_labels <- nerc_sf$Labels
n_regions <- length(nerc_sf$Labels)


#Population Shape-Files
pop_regions <- readOGR(dsn= paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/sf/USA_Urban_Areas"),
                       layer="USA_Urban_Areas")

#Load Population and Temperature Grid Cell Data
load("data/NERC_Regions_Temp_Population.RData")

#______________________________________________________________________________#
###Hyper-Parameters###
Data_Type <- "CDD"



yrs <- 1950:2021
block_sizes <- c(6,12,24,72, 168, 336) #hours


#ggplot shape files
world <- map_data("world")
us <- map_data("state")


#______________________________________________________________________________#
##Reading the data###
j <- 4
par(mfrow = c(4,3))

for(i in 1:n_regions){


#2020 Data
DD_Regional <- get(load("data/processed_data/CDD_Regional_2020.RData"))
ts_2020 <- DD_Regional[[i]][[1]][[j]]/block_sizes[j]
tds <- density(ts_2020)

#2010 Data
DD_Regional <- get(load("data/processed_data/CDD_Regional_2010.RData"))
ts_2010 <- DD_Regional[[i]][[1]][[j]]/block_sizes[j]
tdsx <- density(ts_2010)

#2000 Data
DD_Regional <- get(load("data/processed_data/CDD_Regional_2000.RData"))
ts_2000 <- DD_Regional[[i]][[1]][[j]]/block_sizes[j]

plot(density(ts_2000), main = nerc_labels[i], xlab = "Temperature",
     ylab = NA, yaxt = "n")
lines(tds$x, tds$y, col='red')
lines(tdsx$x, tdsx$y, col='blue')


}



#______________________________________________________________________________#
##Arizona New-Mexico###

#ggplot shape files
world <- map_data("world")
us <- map_data("state")


pop_changes <- nerc_pop_temp$Population[[1]]
pop_changes$Change <- pop_changes$pop_2020 - pop_changes$pop_2000
pop_changes$Change[is.nan(pop_changes$Change)] <- 0

p1 <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-120, -100))+
  scale_y_continuous(name = " ", limits = c(30, 40)) +
  geom_tile(data = pop_changes, aes(x= Longitude, y = Latitude, fill = Change)) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color ='black') +
  geom_polygon(data = nerc_sf$Shapefiles[[1]], mapping = aes(x = long, y = lat, group = group), 
               fill = NA, color = 'black', size = 1.33) +
  geom_polygon(data = pop_regions, mapping = aes( x = long, y = lat, group = group), 
               fill = "black", color = NA, alpha = 0.25) +
  ggtitle("Population Changes - Arizona/New Mexico/Las Vegas Region") +
  scale_fill_gradient2(midpoint= 0,
                       low="blue", high="red") +
  theme_bw() +
  theme(legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=15),
        legend.key.height = unit(2, "cm")) 



#------------------------------------------------------------------------------#
#2020 Data
DD_Regional <- get(load("data/processed_data/CDD_Regional_2020.RData"))
ts_2020 <- DD_Regional[[1]][[1]][[j]]/24

#2000 Data
DD_Regional <- get(load("data/processed_data/CDD_Regional_2000.RData"))
ts_2000 <- DD_Regional[[1]][[1]][[j]]/24



plt_dataset <- data.frame(Years = rep(1950:2021, 2),
                          Scenario = as.factor(rep(c(2000,2020), each = 72)),
                          DD = c(ts_2000, ts_2020))

p2 <- ggplot(plt_dataset, aes(x=Years, y = DD, color = Scenario)) +
  geom_line(size = 1.15) +
  ggtitle("Peak Inferred Degree Days") +
  xlab("Years") +
  ylab("Population Adjusted Degree Days") +
  theme_bw() +
  theme(legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        axis.text=element_text(size=10),
        axis.title=element_text(size=15),
        axis.ticks = element_blank(),
        plot.title = element_text(size=20),
        legend.key.height = unit(2, "cm")) 


pdf("figures/Arizona_New_Mexico.pdf", height=1850/300, width=5000/300)
plot_grid(p1,
          p2,
          nrow =1,
          labels = c('A', 'B'), 
          label_size = 12)

dev.off()
