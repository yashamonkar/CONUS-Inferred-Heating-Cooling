#______________________________________________________________________________#
###---Function to convert the EGrids to needed shape---###


###Input
#1. EGrids Shapefiles. 


###Output
#1. Shapefiles within CONUS
#2. Labels


get_egrids <- function(egrids_sf){
  
  #Set-up Storage
  conus_sf <- conus_labels <-  list()
  
  ###
  #---Remove Alaskan Regions -- 1,2
  conus_sf[[1]] <- egrids_sf[3,];conus_labels[[1]] <- egrids_sf$ZipSubregi[3] #Arizona
  conus_sf[[2]] <- egrids_sf[4,];conus_labels[[2]] <- egrids_sf$ZipSubregi[4] #California
  conus_sf[[3]] <- egrids_sf[5,];conus_labels[[3]] <- egrids_sf$ZipSubregi[5] #Texas
  conus_sf[[4]] <- egrids_sf[6,];conus_labels[[4]] <- egrids_sf$ZipSubregi[6] #Florida
  
  #---Remove Hawaii Regions -- 7,8
  conus_sf[[5]] <- egrids_sf[9,];conus_labels[[5]] <- egrids_sf$ZipSubregi[9]
  conus_sf[[6]] <- egrids_sf[10,];conus_labels[[6]] <- egrids_sf$ZipSubregi[10]
  conus_sf[[7]] <- egrids_sf[11,];conus_labels[[7]] <- egrids_sf$ZipSubregi[11]
  conus_sf[[8]] <- egrids_sf[12,];conus_labels[[8]] <- egrids_sf$ZipSubregi[12]

  ###--- Merge New York NYCW[13] + NYLI[14] + NYUP[15]
  conus_sf[[9]] <- rbind(egrids_sf[13,],egrids_sf[14,], egrids_sf[15,])
  conus_labels[[9]] <- "NYISO"
  
  ###Add the rest
  for(j in 1:11){
    conus_sf[[9+j]] <- egrids_sf[15+j,]
    conus_labels[[9+j]] <- egrids_sf$ZipSubregi[15+j]
  }
  
  ###Return
  out = list(Shapefiles = conus_sf,
             Labels = unlist(conus_labels))

  
}