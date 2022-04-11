#______________________________________________________________________________#
####Analyze trends on Annual Maximum CDD and HDD across CONUS##################

#Plot 1:- Mean CDD across the AOI
#Plot 2:- Trends in Mean Annual CDD. 
#Plot Type 3:- Trends in Value and Dates of Ann-Max Blocks. 
#Plot Type 4:- Trends in Value and Dates of Ann-Max Blocks of Grid Level Maximum


#AOI -- Area of Interest

#______________________________________________________________________________#
#Set-up Directory
setwd("~/GitHub/TXTemp-Simulator")


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


#Source functions
source("functions/Get_MK_Plot.R")
source("functions/Get_Day_Difference.R")
source("functions/Get_Mean_Variation.R")
source("functions/Get_PCA_Analysis.R")
source("functions/Get_GEV_Plots.R")
source("functions/Get_Field_Significance.R")


#Load the CONUS Locations
grid_locs <- read.table("data/CONUS_0_5_deg_lat_lon_index_key.txt", 
                        header = TRUE, sep=" ")


#Load the Texas Interconnect Locations
grid_locs <- read.table("data/ERCOT_0_5_deg_lat_lon_index_key.csv", 
                        header = TRUE, sep=",")
colnames(grid_locs)[1:2] <- c("Longitude", "Latitude")
grid_locs$Longitude <- grid_locs$Longitude - 360


#Load the Texas Cities Data
cities <- read.table("data/Texas_cities.txt", sep=" ", header = TRUE)


#Lat-Lon Points for CONUS
lat_cn <- c(24,51);lon_cn <- c(-125,-63)

#Lat-Lon Points for CONUS
lat_tx <- c(26,37);lon_tx <- c(-107,-93)

#Select usage
lat_us <- lat_tx; lon_us <- lon_tx


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


pdf("figures/ERCOT_CDD.pdf")
#______________________________________________________________________________#
#### Analysis of trends in mean CDD across CONUS #####


###Reading the data
mean_cdd <- read.table("data/processed_data/ERCOT_Mean_CDD.txt", sep=" ", 
                       header = TRUE)


###Plot 1 -  Mean CDD across AOI.
ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(lon_us[1], lon_us[2]))+
  scale_y_continuous(name = " ", limits = c(lat_us[1], lat_us[2])) +
  geom_tile(data = grid_locs, aes(x=Longitude, y = Latitude,
                                  fill = colMeans(mean_cdd))) +
  geom_point(data = cities, aes(x=Longitude, y = Latitude),
             size = 2, color = 'brown') +
  geom_text(data = cities, aes(x=Longitude, y = Latitude, label = City),
            hjust=0, vjust=1, size = 5) +
  scale_fill_gradient2(midpoint=mean(colMeans(mean_cdd)),
                       low="blue", mid="white",high="red") +
  labs(title = "Profile of Mean CDD across CONUS",
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
fs_test <- get_field_significance(Data_Matrix = mean_cdd, p_thresh = 0.05)

###Plot 2 -  Trends in Mean Annual CDD across the AOI###
get_mk_plot(Data_Matrix = mean_cdd,
            Field_Name = "CDD",
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
load("data/processed_data/ERCOT_CDD_Site_Level.RData")


###Analysis of trends in the values. 

for(i in 1:length(block_sizes)){
  
  cur_block <- block_sizes[i]
  
  fs_test <- get_field_significance(Data_Matrix = CONUS_CDD_Site_Level[[1]][[i]],
                                    p_thresh = 0.05)
  
  get_mk_plot(Data_Matrix = CONUS_CDD_Site_Level[[1]][[i]],
              Field_Name = "CDD",
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
  cur_dates <- CONUS_CDD_Site_Level[[2]][[i]]
  
  
  hd_Dates <- get_num_of_days(Data_Matrix = cur_dates,
                              Ref_Date = "01-01",
                              Type="CDD")
  
  #Run the field Significance Test
  fs_test <- get_field_significance(Data_Matrix = hd_Dates,
                                    p_thresh = 0.05)
  
  
  get_mk_plot(Data_Matrix = hd_Dates,
              Field_Name = "CDD - Dates",
              Sub_Title = paste0("Block Size - ", cur_block, " hours", fs_test),
              Grids = grid_locs,
              Legend_Title  = "Days \n /decade",
              Type = "Dates",
              lon_bx = lon_us, lat_bx = lat_us,
              Cities = cities)
  
}

CONUS_CDD_Site_Level <- NULL

#______________________________________________________________________________#
#### Analysis of trends in aggregated block maxima across CONUS #####



#Load the data
load("data/processed_data/ERCOT_CDD_Grid_Level.RData")

for(jk in 1:length(block_sizes)){
  
  cur_block <- block_sizes[jk]

  #MK-Test on the Results
  mktest <- sens.slope(CONUS_CDD_Grid_Level[[1]][[jk]]/cur_block)
  if(mktest$p.value < 0.05){
    signif <- "Signficant"
  } else {
    signif <- "Non-Significant"
  }
  
  #MK-Test on the Dates.
  Dates <- matrix(data = CONUS_CDD_Grid_Level[[2]][[jk]], ncol = 1)
  hd_Dates <- get_num_of_days(Data_Matrix = Dates,
                              Ref_Date = "01-01",
                              Type="CDD")
  dates_test <- sens.slope(hd_Dates)
  if(dates_test$p.value < 0.05){
    signif_dates <- "Signficant"
  } else {
    signif_dates <- "Non-Significant"
  }
  
  
  par(mar = c(5,5,8,3))
  plot(1950:2020, CONUS_CDD_Grid_Level[[1]][[jk]]/cur_block, type='l',
       xlab = "Year", ylab = "Inferred Demand for Cooling (deg C)",
       main = paste0("Aggregated CDD with Block - ", cur_block, " hrs"),
       cex.main = 1.5, cex.lab = 1.25, lwd = 3)
  mtext(paste0("The trend in values ", signif, " (",
               round(mktest$p.value,3),") with slope of ",
               round(mktest$estimates,3), ". \n Trend in Dates is ",
               signif_dates, " (",
               round(dates_test$p.value,3),")" ), side = 3,
        cex = 1.15)
  
  #get_mean_variation(Data_Matrix = CONUS_CDD_Grid_Level[[3]][[jk]],
  #                   Field_Name = "Aggregate CDD",
  #                   Sub_Title = paste0("Block Size - ", cur_block, " hours"),
  #                   Grids = grid_locs,
  #                   Legend_Title  = "deg-hrs",
  #                   lon_bx = lon_us, lat_bx = lat_us)
  
  #Run the field Significance Test
  fs_test <- get_field_significance(Data_Matrix = CONUS_CDD_Grid_Level[[3]][[jk]],
                                    p_thresh = 0.05)
  
  get_mk_plot(Data_Matrix = CONUS_CDD_Grid_Level[[3]][[jk]]/cur_block,
              Field_Name = "Aggregate CDD",
              Sub_Title = paste0("Block Size - ", cur_block, " hours", fs_test),
              Grids = grid_locs,
              Legend_Title  = "deg-hrs \n /decade",
              Type = " ",
             lon_bx = lon_us, lat_bx = lat_us,
              Cities = cities)
  
  #get_pca_analysis(Data_Matrix = CONUS_CDD_Grid_Level[[3]][[jk]],
  #                 npcs = 4,
  #                 Field  ="Aggregate CDD",
  #                 Sub_Title = paste0("Block Size - ", cur_block, " hours"),
  #                 Grids = grid_locs,
  #                 lon_bx = lon_us, lat_bx = lat_us)
  
  #get_gev_plots(TS = CONUS_CDD_Grid_Level[[1]][[jk]],
  #              block_size = cur_block,
  #              Type = "CDD",
  #              Return_Levels = c(10,100),
  #              years = 1950:2020,
  #              ngrids = nrow(grid_locs))

}





dev.off()
