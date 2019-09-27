##8/27/2019, updated 9/9/2019
#MBowen
#FINAL SCRIPT FOR LSM

##load packages
library(tidyverse) #data cleaning and manipulation
library(raster) ##load and manipulate rasters
library(sf) ##load shapefiles
library(sp) ##additional functions for spatial objects
##library(spdplyr) #language to use dplyr verbs on SP* objects
##library(spatstat) ##work with shapefiles ##DO NOT USE ON REMOTE SERVER
library(rgdal) ##for projections and coord systems
##library(gfcanalysis) ##package specifically for Hansen Forestry Data! ##DO NOT USE ON REMOTE SERVER
library(landscapemetrics) ##R equivalent to FRAGSTATS


##need to set the working directory on the remote server
##setwd("R/Amazon_Malaria_Project/")


##load shapefiles
muni_shp <- st_read(dsn = 'municipality_shapefiles', layer = "Amazon_Municipios")
##need to convert the municipality .shp to a "SpatialDataFrame"
spd <- sf::as_Spatial(st_geometry(muni_shp),
                      IDs = as.character(1:nrow(muni_shp)))
muni_df <- as.data.frame(muni_shp)
muni_spd <- sp::SpatialPolygonsDataFrame(spd, data = muni_df)

##separate by codigo
codigo_shp <- muni_shp %>% 
  dplyr::select(codigo_ibg)

codigospd <- sf::as_Spatial(st_geometry(codigo_shp),
                             IDs = as.character(1:nrow(codigo_shp)))
codigo_df <- as.data.frame(codigo_shp)
codigo_spd <- sp::SpatialPolygonsDataFrame(codigospd, data = codigo_df)

##################YEAR: 2004##################################
##############NOT FINISHED ONLY GOT 722 in 4.5 hours#######################
muni_2001 <- raster("muni_2001.tif")

###output: empty data frame for year X
lsm_2001_df <- data.frame() #empty dataframe

#for loop for every polygon (municipality), for year X
system.time(for (i in 1:length(codigo_spd)){
  one_poly <- codigo_spd[i,] #extract single polygon
  clip1 <- crop(muni_2001, extent(one_poly)) #crop to polygon extent (mask will go faster) ##CHANGE YEAR
  clip2 <- mask(clip1, one_poly) #mask polygon to above cropped area (takes less time)
  
  lsm_te <- lsm_l_te(clip2) %>%  #calculate landscape metric (total edge)
    rename(metric_te = metric, value_te = value) 
  lsm_ed <- lsm_l_ed(clip2) %>%  #calculate landscape metric (edge density)     
    dplyr::select(metric, value) %>% 
    rename(metric_ed = metric, value_ed = value)
  lsm_patch <- lsm_l_area_mn(clip2) %>% 
    dplyr::select(metric, value) %>% 
    rename(metric_mean_patch= metric, value_mean_area = value)
  lsm_cover <- lsm_l_ta(clip2, directions = 8) %>% 
    dplyr::select(metric, value) %>% 
    rename(metric_total_area = metric, value_total_area = value)
  lsm_2 <- cbind(one_poly@data$codigo_ibg, lsm_te, lsm_ed, lsm_patch, lsm_cover) %>% 
    mutate(year = 2001) #cbind any/all metrics
  lsm_2001_df <- rbind(lsm_2, lsm_2001_df) #rbind every loop ##CHANGE YEAR X2

})

write.csv(lsm_2001_df, 'lsm_2001_df.csv')

##elapsed time: 15349.856 (4.26 hr, on the remote server, 2.66 hr on Bren)

##huge ups to stackexchange here: https://gis.stackexchange.com/questions/130522/increasing-speed-of-crop-mask-extract-raster-by-many-polygons-in-r

