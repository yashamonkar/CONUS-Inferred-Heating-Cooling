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


#Load the Processed Data
CDD_Regional <- get(load("data/processed_data/CDD_Regional.RData"))
HDD_Regional <- get(load("data/processed_data/HDD_Regional.RData"))
TL_Regional <- get(load("data/processed_data/Thermal_Load_Regional.RData"))



#______________________________________________________________________________#
#####--------------Plot 1 - Map + Mean-----------------------------------------#

###Hyper-Parameters###
block_sizes <- c(6,12,24,72, 168, 336) #hours
sel_block <- 4 #--- 72 Hours

#ggplot shape files
world <- map_data("world")
us <- map_data("state")

#______________________________________________________________________________#
###----Plot One: Map of the ISO --- Florida -----###
sel_rto <- 4
RTO_Label <- nerc_labels[sel_rto]

#Subset to RTO
sub_region <- nerc_sf$Shapefiles[[sel_rto]]

#Get Lat-Lon Extend
lat_lon <- sub_region %>% fortify() %>% select(long,lat)
lat_min <- min(lat_lon$lat)-0.5
lat_max <- max(lat_lon$lat)+0.5
lon_min <- min(lat_lon$long)-2
lon_max <- max(lat_lon$long)+1.75

p1 <-  ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = NA) +
  geom_polygon(data = sub_region, mapping = aes( x = long, y = lat, group = group), 
               fill = "#FFFFFF", color = 'black', size = 1) + 
  #geom_polygon(data = pop_regions, mapping = aes( x = long, y = lat, group = group), 
  #            fill = "black", color = NA, alpha = 0.2) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(lon_min, lon_max)) +
  scale_y_continuous(name = " ", limits = c(lat_min, lat_max)) +
  ggtitle(paste0("  ",RTO_Label)) +
  theme_bw() +
  theme(axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28))


#______________________________________________________________________________#
###----Mean HDD and CDD -- Florida-----###


#Load Population and Temperature Grid Cell Data
cdd_mean <- CDD_Regional[[sel_rto]][[3]]
hdd_mean <- HDD_Regional[[sel_rto]][[3]]
tl_mean <- TL_Regional[[sel_rto]][[3]]


#Plotting Dataset
Plt_Dt_HDD <- data.frame(Years = 1951:2021,
                         DD = hdd_mean,
                         Type = "Heating Demand")

Plt_Dt_CDD <- data.frame(Years = 1950:2021,
                         DD = cdd_mean,
                         Type = "Cooling Demand")

Plt_Dt_TL <- data.frame(Years = 1951:2021,
                        DD = tl_mean,
                        Type = "Total Thermal Demand")

#Combine the Datasets
Plt_Dataset <- rbind(Plt_Dt_CDD, Plt_Dt_HDD, Plt_Dt_TL)

#group.colors <- c(Heating_Demand ="#ff0000",
#                  Cooling_Demand = "#0000FF",
#                  Total_Thermal_Demand = "#000000")


p2 <-  ggplot() +
  geom_line(Plt_Dataset, mapping = aes(x=Years, y = DD, color = Type)) +
  geom_line(Plt_Dt_HDD, mapping = aes(x= Years, 
                                      y=rollmean(DD, 10, na.pad=TRUE)), 
            color = "#ff0000", size = 1.33, linetype = "longdash") +
  geom_line(Plt_Dt_CDD, mapping = aes(x= Years, 
                                      y=rollmean(DD, 10, na.pad=TRUE)), 
            color = "#0000FF", size = 1.33, linetype = "longdash") +
  geom_line(Plt_Dt_TL, mapping = aes(x= Years, 
                                     y=rollmean(DD, 10, na.pad=TRUE)), 
            color = "#000000", size = 1.33, linetype = "longdash") +
  ylim(0, 14) +
  ggtitle(paste0("Annual Mean Demand \n ", RTO_Label)) +
  ylab("Inferred Demand (deg F)") +
  theme_bw() +
  theme(legend.text=element_text(size=24),
        legend.title=element_text(size=24),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18),
        plot.title = element_text(size=24)) +
  scale_color_manual(values=c("#0000FF", "#ff0000", "#000000"))



#______________________________________________________________________________#
###----Plot Two: Map of the ISO --- MISO -----###
sel_rto <- 6
RTO_Label <- nerc_labels[sel_rto]

#Subset to RTO
sub_region <- nerc_sf$Shapefiles[[sel_rto]]

#Get Lat-Lon Extend
lat_lon <- sub_region %>% fortify() %>% select(long,lat)
lat_min <- min(lat_lon$lat)-0.5
lat_max <- max(lat_lon$lat)+0.5
lon_min <- min(lat_lon$long)-0.5
lon_max <- max(lat_lon$long)+0.5

p3 <-  ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = NA) +
  geom_polygon(data = sub_region, mapping = aes( x = long, y = lat, group = group), 
               fill = "#FFFFFF", color = 'black', size = 1) + 
  #geom_polygon(data = pop_regions, mapping = aes( x = long, y = lat, group = group), 
  #             fill = "black", color = NA, alpha = 0.2) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = NA, color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(lon_min, lon_max)) +
  scale_y_continuous(name = " ", limits = c(lat_min, lat_max)) +
  ggtitle(paste0("  ",RTO_Label)) +
  theme_bw() +
  theme(axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=28))


#______________________________________________________________________________#
###----Mean HDD and CDD -- MISO-----###


#Load Population and Temperature Grid Cell Data
cdd_mean <- CDD_Regional[[sel_rto]][[3]]
hdd_mean <- HDD_Regional[[sel_rto]][[3]]
tl_mean <- TL_Regional[[sel_rto]][[3]]


#Plotting Dataset
Plt_Dt_HDD <- data.frame(Years = 1951:2021,
                         DD = hdd_mean,
                         Type = "Heating Demand")

Plt_Dt_CDD <- data.frame(Years = 1950:2021,
                         DD = cdd_mean,
                         Type = "Cooling Demand")

Plt_Dt_TL <- data.frame(Years = 1951:2021,
                        DD = tl_mean,
                        Type = "Total Thermal Demand")

#Combine the Datasets
Plt_Dataset <- rbind(Plt_Dt_CDD, Plt_Dt_HDD, Plt_Dt_TL)


p4 <-  ggplot() +
  geom_line(Plt_Dataset, mapping = aes(x=Years, y = DD, color = Type)) +
  geom_line(Plt_Dt_HDD, mapping = aes(x= Years, 
                                      y=rollmean(DD, 10, na.pad=TRUE)), 
            color = "#ff0000", size = 1.33, linetype = "longdash") +
  geom_line(Plt_Dt_CDD, mapping = aes(x= Years, 
                                      y=rollmean(DD, 10, na.pad=TRUE)), 
            color = "#0000FF", size = 1.33, linetype = "longdash") +
  geom_line(Plt_Dt_TL, mapping = aes(x= Years, 
                                     y=rollmean(DD, 10, na.pad=TRUE)), 
            color = "#000000", size = 1.33, linetype = "longdash") +
  ylim(0, 27) +
  ggtitle(paste0("Annual Mean Demand \n ", RTO_Label)) +
  ylab("Inferred Demand (deg F)") +
  theme_bw() +
  theme(legend.text=element_text(size=24),
        legend.title=element_text(size=24),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18),
        plot.title = element_text(size=24))  +
  scale_color_manual(values=c("#0000FF", "#ff0000", "#000000"))


###---------------Combine the plots------------------------------------------###

#Generate base plot
p_total <- plot_grid(p1,p3,
                     p2 + theme(legend.position="none"),
                     p4 + theme(legend.position="none"),
                     nrow =2,
                     labels = c('A', 'B', 'C', 'D'), 
                     label_size = 14)

#Seperate the legend
legend_b <- get_legend(p2 + guides(color = guide_legend(nrow = 1, 
                                                        override.aes = list(size=2))) +
                         theme(legend.position = "bottom"))

#Add legend to the plot
pf <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .1))


#Plot the results
pdf("figures/ISO_Plots_1.pdf", height=3700/300, width=5000/300)
print(pf)
dev.off()





#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#####---------------Plot 2 - Aggregated Peak HDD and CDD-----------------------# 

  
###----------------------------------Florida---------------------------------###
sel_rto <- 4
RTO_Label <- nerc_labels[sel_rto]
  
#Load Population and Temperature Grid Cell Data
cdd_agg <- CDD_Regional[[sel_rto]][[1]][[sel_block]]/(block_sizes[sel_block])
hdd_agg <- HDD_Regional[[sel_rto]][[1]][[sel_block]]/(block_sizes[sel_block])
tl_agg <- TL_Regional[[sel_rto]][[1]][[sel_block]]/(block_sizes[sel_block])
  
  
#Plotting Dataset
Plt_Dt_HDD <- data.frame(Years = 1951:2021,
                         DD = hdd_agg,
                         Type = "Heating Demand")
  
Plt_Dt_CDD <- data.frame(Years = 1950:2021,
                         DD = cdd_agg,
                         Type = "Cooling Demand")

Plt_Dt_TL <- data.frame(Years = 1951:2021,
                        TL = tl_agg)

#Combine the Datasets
Plt_Dataset <- rbind(Plt_Dt_CDD, Plt_Dt_HDD)
  
p1 <-  ggplot() +
  geom_line(Plt_Dataset, mapping = aes(x=Years, y = DD, color = Type), size = 1) +
  geom_point(Plt_Dt_TL, mapping = aes(x=Years, y = TL), size = 2) +
  ggtitle(paste0("Annual Peak Demand \n", RTO_Label)) +
  ylab(paste0("Inferred Demand (deg F) \n (Averaged over ",
              block_sizes[sel_block], " hours)")) +
  ylim(0, 30) +
  theme_bw() +
  theme(legend.text=element_text(size=24),
        legend.title=element_text(size=24),
        axis.text=element_text(size=20),
        axis.title=element_text(size=16),
        plot.title = element_text(size=23)) +
  scale_color_manual(values=c("#0000FF", "#ff0000"))



###----------------------------------MISO---------------------------------###
sel_rto <- 6
RTO_Label <- nerc_labels[sel_rto]

#Load Population and Temperature Grid Cell Data
cdd_agg <- CDD_Regional[[sel_rto]][[1]][[sel_block]]/(block_sizes[sel_block])
hdd_agg <- HDD_Regional[[sel_rto]][[1]][[sel_block]]/(block_sizes[sel_block])
tl_agg <- TL_Regional[[sel_rto]][[1]][[sel_block]]/(block_sizes[sel_block])


#Plotting Dataset
Plt_Dt_HDD <- data.frame(Years = 1951:2021,
                         DD = hdd_agg,
                         Type = "Heating Demand")

Plt_Dt_CDD <- data.frame(Years = 1950:2021,
                         DD = cdd_agg,
                         Type = "Cooling Demand")

Plt_Dt_TL <- data.frame(Years = 1951:2021,
                        TL = tl_agg)

#Combine the Datasets
Plt_Dataset <- rbind(Plt_Dt_CDD, Plt_Dt_HDD)


p2 <-  ggplot() +
  geom_line(Plt_Dataset, mapping = aes(x=Years, y = DD, color = Type), size = 1) +
  geom_point(Plt_Dt_TL, mapping = aes(x=Years, y = TL), size = 2) +
  ggtitle(paste0("Annual Peak Demand \n", RTO_Label)) +
  ylab(paste0("Inferred Demand (deg F) \n (Averaged over ",
              block_sizes[sel_block], " hours)")) +
  ylim(0, 90) +
  theme_bw() +
  theme(legend.text=element_text(size=24),
        legend.title=element_text(size=24),
        axis.text=element_text(size=20),
        axis.title=element_text(size=16),
        plot.title = element_text(size=23)) +
  scale_color_manual(values=c("#0000FF", "#ff0000"))


###---------------Combine the plots------------------------------------------###

#Generate base plot
p_total <- plot_grid(p1 + theme(legend.position="none"),
                     p2 + theme(legend.position="none"),
                     nrow =1,
                     labels = c('A', 'B'), 
                     label_size = 20)


legend_b <- get_legend(p1 + 
                         guides(color = guide_legend(nrow = 1, override.aes = list(size=2))) +
                         theme(legend.position = "bottom")
)

pf <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .1))


pdf("figures/ISO_Plots_2.pdf", height=1850/300, width=5000/300)
print(pf)
dev.off()




#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#####------------------Plot 3 - Thermal Load Factors---------------------------#

  
#______________________________________________________________________________#
###--------------------------------------Florida-----------------------------###
sel_rto <- 4
RTO_Label <- nerc_labels[sel_rto]


#Load Population and Temperature Grid Cell Data
Thermal_Load <- block_sizes[sel_block]*NERC_TL_Region[[sel_rto]][[3]]/NERC_TL_Region[[sel_rto]][[1]][[4]]
  
#Plotting Dataset
Plt_Dt <- data.frame(Years = 1951:2021,
                      TL = Thermal_Load)
  
  
p1 <-  ggplot(Plt_Dt) +
  geom_line(mapping = aes(x=Years, y = TL)) +
  geom_line(aes(x= Years, y=rollmean(TL, 10, na.pad=TRUE)), 
            color = 'red', size = 1.33, linetype = "dashed") +
  ggtitle(paste0(" Thermal Load Factor - ", RTO_Label)) +
  ylab("Load Factor") +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24),
        plot.title = element_text(size=28))


#______________________________________________________________________________#
###-----------------------------------MISO-----------------------------------###
sel_rto <- 6
RTO_Label <- nerc_labels[sel_rto]


#Load Population and Temperature Grid Cell Data
Thermal_Load <- block_sizes[sel_block]*NERC_TL_Region[[sel_rto]][[3]]/NERC_TL_Region[[sel_rto]][[1]][[4]]

#Plotting Dataset
Plt_Dt <- data.frame(Years = 1951:2021,
                     TL = Thermal_Load)


p2 <-  ggplot(Plt_Dt) +
  geom_line(mapping = aes(x=Years, y = TL)) +
  geom_line(aes(x= Years, y=rollmean(TL, 10, na.pad=TRUE)), 
            color = 'red', size = 1.33, linetype = "dashed") +
  ggtitle(paste0(" Thermal Load Factor - ", RTO_Label)) +
  ylab("Load Factor") +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=24),
        plot.title = element_text(size=28))

  
  
###---------------Combine the plots------------------------------------------###
p_total <- plot_grid(p1,p2, nrow =1,
                     labels = c('A', 'B'), 
                     label_size = 20)

pdf("figures/ISO_Plots_3.pdf", height=1850/300, width=5000/300)
print(p_total)
dev.off()


