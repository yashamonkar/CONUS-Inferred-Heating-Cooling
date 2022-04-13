#______________________________________________________________________________#
####Analyze trends on Annual Maximum HDD and HDD across CONUS##################

#Plot 1:- Mean HDD across the AOI
#Plot 2:- Trends in Mean Annual HDD. 
#Plot Type 3:- Trends in Value and Dates of Ann-Max Blocks. 
#Plot Type 4:- Trends in Value and Dates of Ann-Max Blocks of Grid Level Maximum


#AOI -- Area of Interest

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
library(doParallel)
library(foreach)
library(broom)
library(rgdal)


#Source functions
source("functions/Get_MK_Plot.R")
source("functions/Get_Day_Difference.R")
#source("functions/Get_Mean_Variation.R")
#source("functions/Get_PCA_Analysis.R")
#source("functions/Get_GEV_Plots.R")
source("functions/Get_Field_Significance.R")




#Source functions
source("functions/Get_Block_Maximum.R")
source("functions/Get_Day_Difference.R")

#Load the CONUS Locations
load("data/NERC_Regions_lat_lon_index_key.RData")

#Load Population Data
load("data/NERC_Regions_Population_Count.RData")
load("data/NERC_Regions_Population_Density.RData")


#NERC Shapefiles
nerc_sf <- readOGR(dsn= paste0("data/sf/NERC_Regions-shp"),
                   layer="NERC_Regions_EIA")


###MAKE A Selection
nerc_sf$NERC_Label
sel_rto <- 2
grid_locs <- grid_nerc[[sel_rto]]
nerc_cur <- tidy(nerc_sf[sel_rto,])
nerc_label <- nerc_sf$NERC_Label[sel_rto]

#Add the Population Data
grid_locs$Pop_Wts <- Pop_count_nerc[[sel_rto]][,5]/sum(Pop_count_nerc[[sel_rto]][,5])

#Load the Processed CDD Data
load("data/processed_data/MRO_HDD.RData")


#______________________________________________________________________________#
#Consistensy Check

#Plotting the grid points
world <- map_data("world")
us <- map_data("state")


#Grid Points
ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(lon_us[1], lon_us[2]))+
  scale_y_continuous(name = " ", limits = c(lat_us[1], lat_us[2])) +
  geom_point(data = grid_locs, aes(x=Longitude, y = Latitude),
             size = 0.5, color = 'red') +
  geom_point(data = cities, aes(x=Longitude, y = Latitude),
             size = 2, color = 'blue') +
  ggtitle("Temperature Grid Points")


pdf("figures/ERCOT_HDD.pdf")
#______________________________________________________________________________#
#### Analysis of trends in mean HDD across CONUS #####


###Reading the data
mean_HDD <- read.table("data/processed_data/ERCOT_Mean_HDD.txt", sep=" ", 
                       header = TRUE)


###Plot 1 -  Mean HDD across AOI.
ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(lon_us[1], lon_us[2]))+
  scale_y_continuous(name = " ", limits = c(lat_us[1], lat_us[2])) +
  geom_tile(data = grid_locs, aes(x=Longitude, y = Latitude,
                                  fill = colMeans(mean_HDD))) +
  geom_point(data = cities, aes(x=Longitude, y = Latitude),
             size = 2, color = 'brown') +
  geom_text(data = cities, aes(x=Longitude, y = Latitude, label = City),
            hjust=0, vjust=1, size = 5) +
  scale_fill_gradient2(midpoint=mean(colMeans(mean_HDD)),
                       low="blue", mid="white",high="red") +
  labs(title = "Profile of Mean HDD across CONUS",
       fill = "Degree-\nHours") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=22),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.key.height = unit(1, 'cm'))

##Field Significance Test
fs_test <- get_field_significance(Data_Matrix = mean_HDD, p_thresh = 0.05)


###Plot 2 -  Trends in Mean Annual HDD across the AOI###
get_mk_plot(Data_Matrix = mean_HDD,
            Field_Name = "HDD",
            Sub_Title = paste0("Annual Average", fs_test),
            Grids = grid_locs,
            Legend_Title  = "deg-hrs \n /decade",
            Type = "Values",
            lon_bx = lon_us, lat_bx = lat_us,
            Cities = cities)


#______________________________________________________________________________#
#### Analysis of trends in block maxima across CONUS #####

#Block-Sizes
block_sizes <- c(6,12,24,72, 168, 336) #hours

#Load the data
load("data/processed_data/ERCOT_HDD_Site_Level.RData")


###Analysis of trends in the values. 

for(i in 1:length(block_sizes)){
  
  cur_block <- block_sizes[i]
  
  fs_test <- get_field_significance(Data_Matrix = CONUS_HDD_Site_Level[[1]][[i]],
                                    p_thresh = 0.05)
  
  get_mk_plot(Data_Matrix = CONUS_HDD_Site_Level[[1]][[i]],
              Field_Name = "HDD",
              Sub_Title = paste0("Block Size - ", cur_block, " hours", fs_test),
              Grids = grid_locs,
              Legend_Title  = "deg-hrs \n /decade",
              Type = "Values",
              lon_bx = lon_us, lat_bx = lat_us,
              Cities = cities)
}


###Analysis of trends in the dates

for(i in 1:length(block_sizes)){
  
  cur_block <- block_sizes[i]
  
  #Dates corresponding to annual maximum
  cur_dates <- CONUS_HDD_Site_Level[[2]][[i]]
  
  
  hd_Dates <- get_num_of_days(Data_Matrix = cur_dates,
                              Ref_Date = "07-01",
                              Type="HDD")
  
  #Run the field Significance Test
  fs_test <- get_field_significance(Data_Matrix = hd_Dates,
                                    p_thresh = 0.05)
  
  
  get_mk_plot(Data_Matrix = hd_Dates,
              Field_Name = "HDD - Dates",
              Sub_Title = paste0("Block Size - ", cur_block, " hours", fs_test),
              Grids = grid_locs,
              Legend_Title  = "Days \n /decade",
              Type = "Dates",
              lon_bx = lon_us, lat_bx = lat_us)
  
}

CONUS_HDD_Site_Level <- NULL

#______________________________________________________________________________#
#### Analysis of trends in aggregated block maxima across CONUS #####



#Load the data
load("data/processed_data/ERCOT_HDD_Grid_Level.RData")

for(jk in 1:length(block_sizes)){
  
  cur_block <- block_sizes[jk]
  
  #MK-Test on the Results
  mktest <- sens.slope(CONUS_HDD_Grid_Level[[1]][[jk]]/cur_block)
  if(mktest$p.value < 0.05){
    signif <- "Signficant"
  } else {
    signif <- "Non-Significant"
  }
  
  #MK-Test on the Dates.
  Dates <- matrix(data = CONUS_HDD_Grid_Level[[2]][[jk]], ncol = 1)
  hd_Dates <- get_num_of_days(Data_Matrix = Dates,
                              Ref_Date = "01-01",
                              Type="HDD")
  dates_test <- sens.slope(hd_Dates)
  if(dates_test$p.value < 0.05){
    signif_dates <- "Signficant"
  } else {
    signif_dates <- "Non-Significant"
  }
  
  
  par(mar = c(5,5,8,3))
  plot(1951:2021, CONUS_HDD_Grid_Level[[1]][[jk]]/cur_block, type='l',
       xlab = "Year", ylab = "Inferred Demand for Heating (deg C)",
       main = paste0("Aggregated HDD with Block - ", cur_block, " hrs"),
       cex.main = 1.5, cex.lab = 1.25, lwd = 3)
  mtext(paste0("The trend in values ", signif, " (",
               round(mktest$p.value,3),") with slope of ",
               round(mktest$estimates,3), ". \n Trend in Dates is ",
               signif_dates, " (",
               round(dates_test$p.value,3),")" ), side = 3,
        cex = 1.15)
  
  #get_mean_variation(Data_Matrix = CONUS_HDD_Grid_Level[[3]][[jk]],
  #                   Field_Name = "Aggregate HDD",
  #                   Sub_Title = paste0("Block Size - ", cur_block, " hours"),
  #                   Grids = grid_locs,
  #                   Legend_Title  = "deg-hrs",
  #                   lon_bx = lon_us, lat_bx = lat_us)
  
  #Run the field Significance Test
  fs_test <- get_field_significance(Data_Matrix = CONUS_HDD_Grid_Level[[3]][[jk]],
                                    p_thresh = 0.05)
  
  get_mk_plot(Data_Matrix = CONUS_HDD_Grid_Level[[3]][[jk]],
              Field_Name = "Aggregate HDD",
              Sub_Title = paste0("Block Size - ", cur_block, " hours", fs_test),
              Grids = grid_locs,
              Legend_Title  = "deg-hrs \n /decade",
              Type = " ",
              lon_bx = lon_us, lat_bx = lat_us,
              Cities = cities)
  
  #get_pca_analysis(Data_Matrix = CONUS_HDD_Grid_Level[[3]][[jk]],
  #                 npcs = 4,
  #                 Field  ="Aggregate HDD",
  #                 Sub_Title = paste0("Block Size - ", cur_block, " hours"),
  #                 Grids = grid_locs,
  #                lon_bx = lon_us, lat_bx = lat_us)
  
  #get_gev_plots(TS = CONUS_HDD_Grid_Level[[1]][[jk]],
  #              block_size = cur_block,
  #              Type = "HDD",
  #              Return_Levels = c(10,100),
  #              years = 1951:2021,
  #              ngrids = nrow(grid_locs))
}





dev.off()
