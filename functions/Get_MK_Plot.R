#_____________________________________________________________________________#
###Function to run the MK/Sen's Slope and plot results across CONUS 


###Input
#1. Data_Matrix
#2. Field_Name
#3. block_size
#4. Grids
#Legend Title

###Output
#1. CONUS Plot




get_mk_plot <- function(Data_Matrix, Field_Name, Grids,
                        Legend_Title, Sub_Title, Type){
  
  #Packages
  library(ggplot2)
  library(gridExtra)
  
  #Plotting the grid points
  world <- map_data("world")
  us <- map_data("state")
  
  #Set-up lists
  sens_sl <- list(); p_st <- list(); sig_st <- list()
  p_thresh <- 0.05
  
  #Compute Sen's Slope and MK Trend Test Significance
  
  for(i in 1:ncol(Data_Matrix)){
    #Sens-Slope
    test <- sens.slope(Data_Matrix[,i])
    sens_sl[[i]] <- test$estimates
    
    #Mann-Kendall
    test <- mk.test(Data_Matrix[,i], alternative = "two.sided")
    
    #Convert NA's to 1.
    if(is.nan(test$p.value) == TRUE){
            test$p.value = 1
    }
    
    p_st[[i]] <- test$p.value
    if(p_st[[i]] < p_thresh){
      sig_st[[i]] <- "Signif"
    } else {
      sig_st[[i]] <- "Non-Signif"
    }
  }
  Significane <- unlist(sig_st)
  Sens_Sl <- unlist(sens_sl)
  
  #Start Plotting
  #Plot the results
  p1 <- ggplot() +
    geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    scale_x_continuous(name = " ", limits = c(-125, -60))+
    scale_y_continuous(name = " ", limits = c(20, 55)) +
    geom_tile(data = Grids, aes(x=Longitude, y = Latitude,
                                 fill = Significane)) +
    labs(title = paste0(Field_Name),
         color = Legend_Title,
         subtitle = paste0(Sub_Title)) +
    theme(plot.title = element_text(size=22),
          plot.subtitle = element_text(size=10),
          legend.text = element_text(size=10),
          legend.title = element_text(size=10),
          legend.key.height = unit(1, 'cm'),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  
  
  p2 <- ggplot() +
    geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    scale_x_continuous(name = " ", limits = c(-125, -60))+
    scale_y_continuous(name = " ", limits = c(20, 55)) +
    geom_tile(data = Grids, aes(x=Longitude, y = Latitude, 
                                     fill = 10*Sens_Sl)) +
    scale_fill_gradient2(midpoint=0, low="blue", mid="white",high="red") +
    labs(title = paste0(Field_Name),
         fill = Legend_Title,
         subtitle = paste0(Sub_Title)) +
    theme_bw() +
    theme(plot.title = element_text(size=22),
          plot.subtitle = element_text(size=15),
          legend.text = element_text(size=15),
          legend.title = element_text(size=12),
          legend.key.height = unit(1, 'cm'),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  
  #Compute the mean
  mean_dd <- colMeans(Data_Matrix)
  
  p3 <- ggplot() +
    geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    scale_x_continuous(name = " ", limits = c(-125, -60))+
    scale_y_continuous(name = " ", limits = c(20, 55)) +
    geom_tile(data = Grids, aes(x=Longitude, y = Latitude, 
                                fill = 10*100*Sens_Sl/mean_dd)) +
    scale_fill_gradient2(midpoint=0, 
                         low="blue", mid="white",high="red") +
    labs(title = paste0(Field_Name),
         fill = "% Change \n (Decade) ",
         subtitle = paste0(Sub_Title)) +
    theme_bw() +
    theme(plot.title = element_text(size=22),
          plot.subtitle = element_text(size=15),
          legend.text = element_text(size=15),
          legend.title = element_text(size=12),
          legend.key.height = unit(1, 'cm'),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  
  #Plot the results
  p4 <- ggplot() +
    geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    scale_x_continuous(name = " ", limits = c(-125, -60))+
    scale_y_continuous(name = " ", limits = c(20, 55)) +
    geom_point(data = Grids, aes(x=Longitude, y = Latitude,
                                size = Significane, color = 10*Sens_Sl)) +
    scale_size_discrete(range = c(0.5, 2)) +
    scale_color_gradient2(midpoint=0, low="blue", mid="white",high="red") +
    labs(title = paste0(Field_Name),
         color = Legend_Title,
         subtitle = paste0(Sub_Title)) +
    theme(plot.title = element_text(size=22),
          plot.subtitle = element_text(size=10),
          legend.text = element_text(size=10),
          legend.title = element_text(size=10),
          legend.key.height = unit(0.5, 'cm'),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  
  #Plotting the results
  
  if(Type == "Values"){
  plot(p2)
    #plot(p1);plot(p2);plot(p3);plot(p4)
    #grid.arrange(p1,p2,p3,p4, nrow = 2) 
  } else {
      plot(p3)
    }
}
