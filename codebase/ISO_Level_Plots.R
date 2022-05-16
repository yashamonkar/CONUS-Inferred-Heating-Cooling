#______________________________________________________________________________#
###---Script to generate RTO/ISO Level Plots---###

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
library(zoo)

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


pdf("figures/ISO_Plots.pdf", height=1850/300, width=5000/300)

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
  ggtitle(paste0(RTO_Label)) +
  theme_bw() +
  theme(axis.text=element_text(size=0),
        axis.title=element_text(size=0),
        axis.ticks = element_blank(),
        plot.title = element_text(size=20))



#______________________________________________________________________________#
###----Plot Two: Map of the Aggregated CDD-----###

#Load Population and Temperature Grid Cell Data
CDD_Regional <- get(load("data/processed_data/CDD_Regional_2020.RData"))
cdd_agg <- CDD_Regional[[sel_rto]][[1]][[sel_block]]/24

if(jk == 5){cdd_agg = cdd_agg[-65]}  #Temporary

#Plotting Dataset
Plt_Dt <- data.frame(Years = 1950:2021,
                     CDD = cdd_agg)


p2 <-  ggplot(Plt_Dt) +
  geom_line(mapping = aes(x=Years, y = CDD)) +
  geom_line(aes(x= Years, y=rollmean(CDD, 10, na.pad=TRUE)), color = 'red', size = 1.33) +
  ggtitle(paste0("Peak Inferred Degree Days")) +
  ylab("Population Adjusted Degree Days") +
  theme_bw() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=20))



#______________________________________________________________________________#
###----Plot Three: Map of the Aggregated HDD-----###

#Load Population and Temperature Grid Cell Data
HDD_Regional <- get(load("data/processed_data/HDD_Regional_2020.RData"))
hdd_agg <- HDD_Regional[[sel_rto]][[1]][[sel_block]]/24


#Plotting Dataset
Plt_Dt <- data.frame(Years = 1951:2021,
                     HDD = hdd_agg)


p3 <-  ggplot(Plt_Dt) +
  geom_line(mapping = aes(x=Years, y = HDD)) +
  geom_line(aes(x= Years, y=rollmean(HDD, 10, na.pad=TRUE)), color = 'blue', size = 1.33) +
  ggtitle(paste0("Peak Inferred Degree Days")) +
  ylab("Population Adjusted Degree Days") +
  theme_bw() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=20))


#______________________________________________________________________________#
###----Plot Four: Map of the Aggregated HDD and CDD-----###

#Load Population and Temperature Grid Cell Data
HDD_Regional <- get(load("data/processed_data/HDD_Regional_2020.RData"))
hdd_agg <- HDD_Regional[[sel_rto]][[1]][[sel_block]]/24


#Plotting Dataset
Plt_Dt_HDD <- data.frame(Years = 1951:2021,
                         HDD = hdd_agg)

Plt_Dt_CDD <- data.frame(Years = 1950:2021,
                         CDD = cdd_agg)


p4 <-  ggplot() +
  geom_line(Plt_Dt_HDD, mapping = aes(x=Years, y = HDD), col ='blue') +
  geom_line(Plt_Dt_HDD, mapping = aes(x= Years, 
                                  y=rollmean(HDD, 10, na.pad=TRUE)), 
            color = 'blue', size = 1.33) +
  geom_line(Plt_Dt_CDD, mapping = aes(x=Years, y = CDD), col='red') +
  geom_line(Plt_Dt_CDD, mapping = aes(x= Years, 
                                  y=rollmean(CDD, 10, na.pad=TRUE)), 
            color = 'red', size = 1.33) +
  ggtitle(paste0("Peak Inferred Degree Days")) +
  ylab("Population Adjusted Degree Days") +
  theme_bw() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size=20))


#______________________________________________________________________________#
###----Rose plot for CDD and HDD occurrence -----###

#Function to take summer and winter data and create a rose plot
summer_dates <- CDD_Regional[[sel_rto]][[2]][[sel_block]]
winter_dates <- HDD_Regional[[sel_rto]][[2]][[sel_block]]

#Convert to Day of Year
summer_doy <- as.numeric(strftime(summer_dates, format = "%j"))
winter_doy <- as.numeric(strftime(winter_dates, format = "%j"))

#Convert with May-1st at the starting point
for(i in 1:length(summer_doy)){
  if(summer_doy[i] > 120){
    summer_doy[i] = summer_doy[i] - 120 
  } else {
    summer_doy[i] = summer_doy[i] + 365 - 120 
  }
}

for(i in 1:length(winter_doy)){
  if(winter_doy[i] > 120){
    winter_doy[i] = winter_doy[i] - 120 
  } else {
    winter_doy[i] = winter_doy[i] + 365 - 120 
  }
}


#Dummy Storage
d1 <- summer_doy
d2 <- winter_doy


#Summer
ds = NULL
ds$d1 <- seq(from = min(d1)-10, to = max(d1)+10, by = 1)
ds <- as.data.frame(ds)
data_density <- density(d1)
density_function <- with(data_density, approxfun(x, y, rule=1))
ds$density <- density_function(ds$d1)
ds <- ds[order(ds$density,ds$d1),]
ds$Season <- "Summer"

#Summer
dw = NULL
dw$d2 <- seq(from = min(d2)-10, to = max(d2)+10, by = 1)
dw <- as.data.frame(dw)
data_density <- density(d2)
density_function <- with(data_density, approxfun(x, y, rule=1))
dw$density <- density_function(dw$d2)
dw <- dw[order(dw$density,dw$d2),]
dw$Season <- "Winter"

#Combine them
colnames(dw) <- colnames(ds) <- c("DOY", "Dens", "Season")
df <- rbind(ds,dw)

#Breaks and Labels
mon_breaks <- c(1, 31, 61, 92, 123, 153, 184, 214, 245, 276, 304, 335)
mon_labels <- c( "May", "Jun", "Jul", "Aug", "Sep", "Oct",
                 "Nov", "Dec", "Jan", "Feb", "Mar", "Apr")

#Add Points
D1 <- data.frame(DOY = d1,
                 Season = rep("Summer",72))
D2 <- data.frame(DOY = d2,
                 Season = rep("Winter",71))
Ann_Max <- rbind(D1,D2)

#Colors
group.colors <- c(Summer = "#FF0000", Winter = "#0000FF")




p5 <- ggplot(data=df,aes(x=DOY,y=Dens,fill=Season))+
  geom_bar(stat="identity") +
  geom_point(data = Ann_Max, aes(x=DOY, y =  1.05*max(df$Dens), color = Season)) +
  #geom_point(df, mapping = aes(x = 291, y = 1.05*max(Dens)),
  #           size =4, shape = 8) +
  scale_x_continuous(name = "", breaks=mon_breaks, labels = mon_labels, 
                     limits = c(1, 365)) +
  annotate("text", x=140, y=0.5*max(df$Dens), 
           label= "CDD Peaks", color ="red", size = 5) +
  annotate("text", x=310, y=0.5*max(df$Dens), 
           label= "HDD Peaks", color = "blue", size = 5) +
  scale_fill_manual(values=group.colors)+
  scale_color_manual(values=group.colors)+
  coord_polar() +
  labs(title = "Peak Event Occurrences") +
  theme_bw() +
  theme(plot.title = element_text(size=20),
        plot.subtitle = element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.position = "None",
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=15),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())




#______________________________________________________________________________#

#grid.arrange(p1,p2,p3,p5, nrow = 2)

grid.arrange(p1,p4,p5, nrow = 1)

}


dev.off()
