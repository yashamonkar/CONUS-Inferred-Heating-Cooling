#______________________________________________________________________________#
####SCRIPT TO PROCESS THE POPULATION COUNT and DENSITY DATASET################
###OUTPUT ON THE NERC LEVEL#####

#Population Dataset:- Gridded Population of the World (GPW), v4.
#Source:- https://sedac.ciesin.columbia.edu/data/collection/gpw-v4
#Institution - SEDAC, CIESIN, NASA


###INPUT
#1. Temperature Grid Cells within each NERC Region.
#2. Population Data


###OUTPUT
#1. Population Count and Density for each grid cell within CONUS. 


#______________________________________________________________________________#
###Load Dependencies and Data

#Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Packages
library(ncdf4)
library(ggplot2)

#Plotting the grid points
world <- map_data("world")
us <- map_data("state")


#Get the Texas Grid Points
load("~/GitHub/CONUS-Inferred-Heating-Cooling/data/NERC_Regions_lat_lon_index_key.RData")
grid_locs <- bind_rows(lapply(grid_nerc,data.frame))
nerc_regions <- length(grid_nerc)




#------------------------------------------------------------------------------#
###Population Count

#Read a population data
name <- c("gpw_v4_population_count_rev11_15_min.nc")
nc_data <- nc_open(name)
print(nc_data) 

#Get the points
lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude")
ras <- ncvar_get(nc_data, "raster")
pop_nc <- ncvar_get(nc_data, "Population Count, v4.11 (2000, 2005, 2010, 2015, 2020): 15 arc-minutes")
nc_close(nc_data) #Closing the Netcdf file. 

#Make a lat-lon box
all_grids <- data.frame(Longitude = rep(lon, length(lat)),
                        Latitude = rep(lat, each = length(lon)))


#Plot the results to just check
#Grid Points
ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -66))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  geom_point(data = grid_locs, aes(x=Longitude, y = Latitude), color = 'red') +
  geom_point(data = all_grids, aes(x=Longitude, y = Latitude), 
             color = 'blue', size = 0.5) +
  ggtitle("Popluation and Temperature Grid Points - Visual Check")


#------------------------------------------------------------------------------#
#Compute Population Count per nerc region
Pop_count_nerc <- list()

for(n in 1:nerc_regions) {
  
  grid_locs <- grid_nerc[[n]]
  
  #Set-up Population Storage Dataset
  pop_count <- matrix(NA, ncol = 5, nrow = nrow(grid_locs))
  colnames(pop_count) <- c("2000_Count", "2005_Count", "2010_Count", 
                         "2015_Count", "2020_Count")

  ###Function to get lat-lon
  for(j in 1:ncol(pop_count)){

    pop <- list()
    for(i in 1:nrow(grid_locs)){
  
      #Get the 4 sub-grid points
      gr1 <- c(grid_locs$Longitude[i]+0.125, grid_locs$Lat[i]+0.125)
      gr2 <- c(grid_locs$Longitude[i]-0.125, grid_locs$Lat[i]+0.125)
      gr3 <- c(grid_locs$Longitude[i]+0.125, grid_locs$Lat[i]-0.125)
      gr4 <- c(grid_locs$Longitude[i]-0.125, grid_locs$Lat[i]-0.125)
  
      #Population Count
      p1 <- pop_nc[which(lon == gr1[1]), which(lat == gr1[2]),j]
      p2 <- pop_nc[which(lon == gr2[1]), which(lat == gr2[2]),j]
      p3 <- pop_nc[which(lon == gr3[1]), which(lat == gr3[2]),j]
      p4 <- pop_nc[which(lon == gr4[1]), which(lat == gr4[2]),j]
      pop[[i]] <- p1+p2+p3+p4
      
      }
  
    pop_count[,j] <- unlist(pop)
    
    }

  Pop_count_nerc[[n]] <- pop_count

#Final Visual Check
grid_locs$Pop <- pop_count[,1]
#Grid Points
p <- ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -66))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  geom_tile(data = grid_locs, aes(x=Longitude, y = Latitude, 
                                   fill = Pop)) +
  scale_fill_gradient2(midpoint=median(grid_locs$Pop, na.rm = TRUE),
                       low="blue", mid="white",high="red") +
  ggtitle("Popluation Count and Temperature Grid Points")

print(p)

}


#------------------------------------------------------------------------------#
###Population Density

#Read a population data
name <- c("gpw_v4_population_density_rev11_15_min.nc")
nc_data <- nc_open(name)
print(nc_data) 

#Get the points
lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude")
ras <- ncvar_get(nc_data, "raster")
pop_nc <- ncvar_get(nc_data, "Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 15 arc-minutes")
nc_close(nc_data) #Closing the Netcdf file. 

#Make a lat-lon box
all_grids <- data.frame(Longitude = rep(lon, length(lat)),
                        Latitude = rep(lat, each = length(lon)))


#------------------------------------------------------------------------------#
#Compute Population Density per nerc region
Pop_dens_nerc <- list()

for(n in 1:nerc_regions) {
  
  grid_locs <- grid_nerc[[n]]
  
  #Set-up Population Storage Dataset
  pop_dens <- matrix(NA, ncol = 5, nrow = nrow(grid_locs))
  colnames(pop_dens) <- c("2000_Density", "2005_Density", "2010_Density", 
                        "2015_Density", "2020_Density")

  ###Function to get lat-lon
  for(j in 1:ncol(pop_dens)){
  
    pop <- list()
    for(i in 1:nrow(grid_locs)){
    
      #Get the 4 sub-grid points
      gr1 <- c(grid_locs$Longitude[i]+0.125, grid_locs$Lat[i]+0.125)
      gr2 <- c(grid_locs$Longitude[i]-0.125, grid_locs$Lat[i]+0.125)
      gr3 <- c(grid_locs$Longitude[i]+0.125, grid_locs$Lat[i]-0.125)
      gr4 <- c(grid_locs$Longitude[i]-0.125, grid_locs$Lat[i]-0.125)
    
      #Population Count
      p1 <- pop_nc[which(lon == gr1[1]), which(lat == gr1[2]),j]
      p2 <- pop_nc[which(lon == gr2[1]), which(lat == gr2[2]),j]
      p3 <- pop_nc[which(lon == gr3[1]), which(lat == gr3[2]),j]
      p4 <- pop_nc[which(lon == gr4[1]), which(lat == gr4[2]),j]
      pop[[i]] <- p1+p2+p3+p4
    }
  
    pop_dens[,j] <- unlist(pop)
  
  }
  
  Pop_dens_nerc[[n]] <- pop_dens


  #Final Visual Check
  grid_locs$Density <- pop_dens[,1]
  #Grid Points
  p <- ggplot() +
    geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#D3D3D3", color = "#000000", size = 0.15) +
    scale_x_continuous(name = " ", limits = c(-125, -66))+
    scale_y_continuous(name = " ", limits = c(24, 50)) +
    geom_tile(data = grid_locs, aes(x=Longitude, y = Latitude, 
                                    fill = Density)) +
    scale_fill_gradient2(midpoint=median(grid_locs$Density, na.rm = TRUE),
                         low="blue", mid="white",high="red") +
    ggtitle("Popluation Density and Temperature Grid Points")
  
  print(p)

}

#______________________________________________________________________________#
#Final Plot Check
grid_locs <- bind_rows(lapply(grid_nerc,data.frame))
grid_pop <- bind_rows(lapply(Pop_count_nerc,data.frame))

grid_locs$Pop <- grid_pop[,1]
ggplot() +
  geom_map(dat = world, map = world, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  geom_map(dat = us, map = us, aes(x=long, y=lat, map_id = region),
           fill = "#D3D3D3", color = "#000000", size = 0.15) +
  scale_x_continuous(name = " ", limits = c(-125, -66))+
  scale_y_continuous(name = " ", limits = c(24, 50)) +
  geom_tile(data = grid_locs, aes(x=Longitude, y = Latitude, 
                                  fill = Pop)) +
  scale_fill_gradient2(midpoint=median(grid_locs$Pop, na.rm = TRUE),
                       low="blue", mid="white",high="red") +
  ggtitle("Popluation Count and Temperature Grid Points")


#------------------------------------------------------------------------------#
##Final Stage Post Processing


#Saving the results -- Population Count
save(Pop_count_nerc, 
     file = paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/NERC_Regions_Population_Count.RData"))


#Saving the results -- Population Count
save(Pop_dens_nerc, 
     file = paste0("~/GitHub/CONUS-Inferred-Heating-Cooling/data/NERC_Regions_Population_Density.RData"))
