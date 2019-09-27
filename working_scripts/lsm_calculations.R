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
##need to convert the municipality .shp to a "SpatialDataFrame" so gfcanalysis can grab the forestry tiles online
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

##load year rasters

muni_2001 <- raster("muni_2001.tif")
muni_2002 <- raster("muni_2002.tif")
muni_2003 <- raster("muni_2003.tif")
muni_2004 <- raster("muni_2004.tif")
muni_2005 <- raster("muni_2005.tif")
muni_2006 <- raster("muni_2006.tif")

##################YEAR: 2001#############################3

###output: empty data frame for year X
lsm_2001_df <- data.frame() #empty dataframe

#for loop for every polygon (municipality), for year X
system.time(for (i in 1:length(codigo_spd)){
  one_poly <- codigo_spd[i,] #extract single polygon
  clip1 <- crop(muni_2001, extent(one_poly)) #crop to polygon extent (mask will go faster)
  clip2 <- mask(clip1, one_poly) #mask polygon to above cropped area (takes less time)
  
  lsm_te <- lsm_l_te(clip2) %>% #calculate landscape metric (total edge)
    rename(metric_te = metric, value_te = value) 
  lsm_ed <- lsm_l_ed(clip2) %>% #calculate landscape metric (edge density)
    dplyr::select(metric, value) %>% 
    rename(metric_ed = metric, value_ed = value)
  lsm_patch <- lsm_l_area_mn(clip2) %>% 
    dplyr::select(metric, value) %>% 
    rename(metric_mean_patch= metric, value_mean_area = value)
  lsm_cover <- lsm_l_ta(clip2, directions = 8) %>% 
    dplyr::select(metric, value) %>% 
    rename(metric_total_area = metric, value_total_area = value) 
  lsm_clump <- lsm_c_clumpy(clip2) %>% 
    dplyr::select(metric, value) %>% 
    rename(metric_clump = metric, value_clump = value)
  lsm_2 <- cbind(one_poly@data$codigo_ibg, lsm_te, lsm_ed, lsm_patch, lsm_cover, lsm_clump)  %>% 
    mutate(year = 2001) #cbind any/all metrics
  
  lsm_2001_df <- rbind(lsm_2, lsm_2001_df) #rbind every loop

})

write.csv(lsm_2001_df, "lsm_2001_df.csv")

##elapsed time: 15349.856 (4.26 hr on server)


##################YEAR: 2002#############################3

###output: empty data frame for year X
lsm_2002_df <- data.frame() #empty dataframe

#for loop for every polygon (municipality), for year X
system.time(for (i in 1:length(codigo_spd)){
  one_poly <- codigo_spd[i,] #extract single polygon
  clip1 <- crop(muni_2002, extent(one_poly)) #####NOTE CHANGE YEAR
  #crop to polygon extent (mask will go faster)
  clip2 <- mask(clip1, one_poly) #mask polygon to above cropped area (takes less time)
  
  lsm_te <- lsm_l_te(clip2) %>% #calculate landscape metric (total edge)
    rename(metric_te = metric, value_te = value) 
  lsm_ed <- lsm_l_ed(clip2) %>% #calculate landscape metric (edge density)
    dplyr::select(metric, value) %>% 
    rename(metric_ed = metric, value_ed = value)
  lsm_patch <- lsm_l_area_mn(clip2) %>% 
    dplyr::select(metric, value) %>% 
    rename(metric_mean_patch= metric, value_mean_area = value)
  lsm_cover <- lsm_l_ta(clip2, directions = 8) %>% 
    dplyr::select(metric, value) %>% 
    rename(metric_total_area = metric, value_total_area = value) 
  lsm_clump <- lsm_c_clumpy(clip2) %>% 
    dplyr::select(metric, value) %>% 
    rename(metric_clump = metric, value_clump = value)
  lsm_2 <- cbind(one_poly@data$codigo_ibg, lsm_te, lsm_ed, lsm_patch, lsm_cover, lsm_clump)  %>% 
    mutate(year = 2002) #cbind any/all metrics #####NOTE CHANGE YEAR
  
  lsm_2002_df <- rbind(lsm_2, lsm_2002_df) #rbind every loop ####NOTE CHANGE YEAR
  
})

write.csv(lsm_2002_df, "lsm_2002_df.csv")


##################YEAR: 2003#############################3

###output: empty data frame for year X
lsm_2003_df <- data.frame() #empty dataframe

#for loop for every polygon (municipality), for year X
system.time(for (i in 1:length(codigo_spd)){
  one_poly <- codigo_spd[i,] #extract single polygon
  clip1 <- crop(muni_2003, extent(one_poly)) #####NOTE CHANGE YEAR
  #crop to polygon extent (mask will go faster)
  clip2 <- mask(clip1, one_poly) #mask polygon to above cropped area (takes less time)
  
  lsm_te <- lsm_l_te(clip2) %>% #calculate landscape metric (total edge)
    rename(metric_te = metric, value_te = value) 
  lsm_ed <- lsm_l_ed(clip2) %>% #calculate landscape metric (edge density)
    dplyr::select(metric, value) %>% 
    rename(metric_ed = metric, value_ed = value)
  lsm_patch <- lsm_l_area_mn(clip2) %>% 
    dplyr::select(metric, value) %>% 
    rename(metric_mean_patch= metric, value_mean_area = value)
  lsm_cover <- lsm_l_ta(clip2, directions = 8) %>% 
    dplyr::select(metric, value) %>% 
    rename(metric_total_area = metric, value_total_area = value) 
  lsm_clump <- lsm_c_clumpy(clip2) %>% 
    dplyr::select(metric, value) %>% 
    rename(metric_clump = metric, value_clump = value)
  lsm_2 <- cbind(one_poly@data$codigo_ibg, lsm_te, lsm_ed, lsm_patch, lsm_cover, lsm_clump)  %>% 
    mutate(year = 2003) #cbind any/all metrics #####NOTE CHANGE YEAR
  
  lsm_2003_df <- rbind(lsm_2, lsm_2003_df) #rbind every loop ####NOTE CHANGE YEAR
  
})

write.csv(lsm_2003_df, "lsm_2003_df.csv")














##huge ups to stackexchange here: https://gis.stackexchange.com/questions/130522/increasing-speed-of-crop-mask-extract-raster-by-many-polygons-in-r

