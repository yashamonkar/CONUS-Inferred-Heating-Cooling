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



#______________________________________________________________________________#
###--------------------------------Mean HDD and CDD -------------------------###
mean_plt <- list()
for(i in 1:length(nerc_labels)) {

  #Sub-Grid Plots
  sel_rto <- i
  RTO_Label <- nerc_labels[sel_rto]

  #Load Population and Temperature Grid Cell Data
  cdd_mean <- CDD_Regional[[sel_rto]][[3]]
  hdd_mean <- HDD_Regional[[sel_rto]][[3]]
  tl_mean <- TL_Regional[[sel_rto]][[3]]


  #Plotting Dataset
  Plt_Dt_HDD <- data.frame(Years = 1951:2021,
                           DD = hdd_mean,
                           Type = "Heating_Demand")

  Plt_Dt_CDD <- data.frame(Years = 1950:2021,
                           DD = cdd_mean,
                           Type = "Cooling_Demand")

  Plt_Dt_TL <- data.frame(Years = 1951:2021,
                          DD = tl_mean,
                          Type = "Total_Thermal_Demand")

  #Combine the Datasets
  Plt_Dataset <- rbind(Plt_Dt_CDD, Plt_Dt_HDD, Plt_Dt_TL)
  group.colors <- c(Heating_Demand ="#ff0000", 
                    Cooling_Demand = "#0000FF",
                    Total_Thermal_Demand = "#000000")


  p1 <-  ggplot() +
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
    ggtitle(paste0(RTO_Label)) +
    ylab("Inferred Demand (deg F)") +
    theme_bw() +
    theme(legend.text=element_text(size=24),
          legend.title=element_text(size=24),
          axis.text=element_text(size=18),
          axis.title=element_text(size=18),
          plot.title = element_text(size=28)) +
    scale_color_manual(values=group.colors)
  
  mean_plt[[i]] <- p1

}


###---------------Combine the plots------------------------------------------###
#Plot the results
pdf("figures/ISO_Plots_Supplement.pdf", height=5550/300, width=5000/300)


#Seperate the legend
legend_b <- get_legend(mean_plt[[1]] + guides(color = guide_legend(nrow = 1, override.aes = list(size=2))) +
                         theme(legend.position = "bottom"))

#Generate base plot
p_total <- plot_grid(mean_plt[[1]] + theme(legend.position="none"),
                     mean_plt[[2]] + theme(legend.position="none"),
                     mean_plt[[3]] + theme(legend.position="none"),
                     mean_plt[[5]] + theme(legend.position="none"),
                     mean_plt[[7]] + theme(legend.position="none"),
                     mean_plt[[8]] + theme(legend.position="none"),
                     nrow =3,
                     labels = c('A', 'B', 'C', 'D', 'E', 'F'), 
                     label_size = 20)
p_total <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .1))
print(p_total)

#Generate base plot
p_total <- plot_grid(mean_plt[[9]] + theme(legend.position="none"),
                     mean_plt[[10]] + theme(legend.position="none"),
                     mean_plt[[11]] + theme(legend.position="none"),
                     mean_plt[[12]] + theme(legend.position="none"),
                     mean_plt[[13]] + theme(legend.position="none"),
                     mean_plt[[14]] + theme(legend.position="none"),
                     nrow =3,
                     labels = c('G', 'H', 'I', 'J', 'K', 'L'), 
                     label_size = 20)
p_total <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .1))
print(p_total)


#Generate base plot
p_total <- plot_grid(mean_plt[[15]] + theme(legend.position="none"),
                     mean_plt[[16]] + theme(legend.position="none"),
                     mean_plt[[17]] + theme(legend.position="none"),
                     mean_plt[[18]] + theme(legend.position="none"),
                     mean_plt[[19]] + theme(legend.position="none"),
                     mean_plt[[20]] + theme(legend.position="none"),
                     nrow =3,
                     labels = c('M', 'N', 'O', 'P', 'Q', 'R'), 
                     label_size = 20)
p_total <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .1))
print(p_total)



#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#####---------------Plot 2 - Aggregated Peak HDD and CDD-----------------------# 
peak_plt <- list()
for(i in 1:length(nerc_labels)) {
  
  #Sub-Grid Plots
  sel_rto <- i
  RTO_Label <- nerc_labels[sel_rto]
  
  #Load Population and Temperature Grid Cell Data
  cdd_agg <- CDD_Regional[[sel_rto]][[1]][[sel_block]]/(block_sizes[sel_block])
  hdd_agg <- HDD_Regional[[sel_rto]][[1]][[sel_block]]/(block_sizes[sel_block])
  tl_agg <- TL_Regional[[sel_rto]][[1]][[sel_block]]/(block_sizes[sel_block])
  
  if(i == 5){cdd_agg = cdd_agg[-65]}
    
  
  #Plotting Dataset
  Plt_Dt_HDD <- data.frame(Years = 1951:2021,
                           DD = hdd_agg,
                           Type = "Heating_Demand")
  
  Plt_Dt_CDD <- data.frame(Years = 1950:2021,
                           DD = cdd_agg,
                           Type = "Cooling_Demand")

  Plt_Dt_TL <- data.frame(Years = 1951:2021,
                          TL = tl_agg)
  
  #Combine the Datasets
  Plt_Dataset <- rbind(Plt_Dt_CDD, Plt_Dt_HDD)
  group.colors <- c(Heating_Demand ="#ff0000", Cooling_Demand = "#0000FF")
  
  p1 <-  ggplot() +
    geom_line(Plt_Dataset, mapping = aes(x=Years, y = DD, color = Type), size = 1) +
    geom_point(Plt_Dt_TL, mapping = aes(x=Years, y = TL), size = 2) +
    ggtitle(paste0( RTO_Label)) +
    ylab(paste0("Inferred Demand (deg F) \n (Averaged over ",
                block_sizes[sel_block], " hours)")) +
    theme_bw() +
    theme(legend.text=element_text(size=24),
          legend.title=element_text(size=24),
          axis.text=element_text(size=20),
          axis.title=element_text(size=16),
          plot.title = element_text(size=28)) +
    scale_color_manual(values=group.colors)
  
  peak_plt[[i]] <- p1
  
}



###---------------Combine the plots------------------------------------------###
#Plot the results


#Seperate the legend
legend_b <- get_legend(peak_plt[[1]] + guides(color = guide_legend(nrow = 1, override.aes = list(size=2))) +
                         theme(legend.position = "bottom"))

#Generate base plot
p_total <- plot_grid(peak_plt[[1]] + theme(legend.position="none"),
                     peak_plt[[2]] + theme(legend.position="none"),
                     peak_plt[[3]] + theme(legend.position="none"),
                     peak_plt[[5]] + theme(legend.position="none"),
                     peak_plt[[7]] + theme(legend.position="none"),
                     peak_plt[[8]] + theme(legend.position="none"),
                     nrow =3,
                     labels = c('A', 'B', 'C', 'D', 'E', 'F'), 
                     label_size = 20)
p_total <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .1))
print(p_total)

#Generate base plot
p_total <- plot_grid(peak_plt[[9]] + theme(legend.position="none"),
                     peak_plt[[10]] + theme(legend.position="none"),
                     peak_plt[[11]] + theme(legend.position="none"),
                     peak_plt[[12]] + theme(legend.position="none"),
                     peak_plt[[13]] + theme(legend.position="none"),
                     peak_plt[[14]] + theme(legend.position="none"),
                     nrow =3,
                     labels = c('G', 'H', 'I', 'J', 'K', 'L'), 
                     label_size = 20)
p_total <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .1))
print(p_total)


#Generate base plot
p_total <- plot_grid(peak_plt[[15]] + theme(legend.position="none"),
                     peak_plt[[16]] + theme(legend.position="none"),
                     peak_plt[[17]] + theme(legend.position="none"),
                     peak_plt[[18]] + theme(legend.position="none"),
                     peak_plt[[19]] + theme(legend.position="none"),
                     peak_plt[[20]] + theme(legend.position="none"),
                     nrow =3,
                     labels = c('M', 'N', 'O', 'P', 'Q', 'R'), 
                     label_size = 20)
p_total <- plot_grid(p_total, legend_b, ncol = 1, rel_heights = c(1, .1))
print(p_total)






#______________________________________________________________________________#
#______________________________________________________________________________#
#______________________________________________________________________________#
#####------------------Plot 3 - Thermal Load Factors---------------------------#
  
tl_plt <- list()
for(i in 1:length(nerc_labels)) {
  
  #Sub-Grid Plots
  sel_rto <- i
  RTO_Label <- nerc_labels[sel_rto]


  #Load Population and Temperature Grid Cell Data
  Thermal_Load <- block_sizes[sel_block]*NERC_TL_Region[[sel_rto]][[3]]/NERC_TL_Region[[sel_rto]][[1]][[4]]
  
  #Plotting Dataset
  Plt_Dt <- data.frame(Years = 1951:2021,
                        TL = Thermal_Load)
  
  
  p1 <-  ggplot(Plt_Dt) +
    geom_line(mapping = aes(x=Years, y = TL)) +
    geom_line(aes(x= Years, y=rollmean(TL, 10, na.pad=TRUE)), 
              color = 'black', size = 1.33, linetype = "dashed") +
    ggtitle(paste0(RTO_Label)) +
    ylab("Load Factor") +
    theme_bw() +
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=20),
          plot.title = element_text(size=28))
  
  tl_plt[[i]] <- p1
  
}


###---------------Combine the plots------------------------------------------###
#Plot the results


#Generate base plot
p_total <- plot_grid(tl_plt[[1]] + theme(legend.position="none"),
                     tl_plt[[2]] + theme(legend.position="none"),
                     tl_plt[[3]] + theme(legend.position="none"),
                     tl_plt[[5]] + theme(legend.position="none"),
                     tl_plt[[7]] + theme(legend.position="none"),
                     tl_plt[[8]] + theme(legend.position="none"),
                     nrow =3,
                     labels = c('A', 'B', 'C', 'D', 'E', 'F'), 
                     label_size = 20)
print(p_total)

#Generate base plot
p_total <- plot_grid(tl_plt[[9]] + theme(legend.position="none"),
                     tl_plt[[10]] + theme(legend.position="none"),
                     tl_plt[[11]] + theme(legend.position="none"),
                     tl_plt[[12]] + theme(legend.position="none"),
                     tl_plt[[13]] + theme(legend.position="none"),
                     tl_plt[[14]] + theme(legend.position="none"),
                     nrow =3,
                     labels = c('G', 'H', 'I', 'J', 'K', 'L'), 
                     label_size = 20)
print(p_total)


#Generate base plot
p_total <- plot_grid(tl_plt[[15]] + theme(legend.position="none"),
                     tl_plt[[16]] + theme(legend.position="none"),
                     tl_plt[[17]] + theme(legend.position="none"),
                     tl_plt[[18]] + theme(legend.position="none"),
                     tl_plt[[19]] + theme(legend.position="none"),
                     tl_plt[[20]] + theme(legend.position="none"),
                     nrow =3,
                     labels = c('M', 'N', 'O', 'P', 'Q', 'R'), 
                     label_size = 20)
print(p_total)

dev.off()
