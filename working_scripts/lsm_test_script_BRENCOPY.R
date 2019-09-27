##LSM Metrics Subset -- TEST

#Finalized 9/9/2019
#MBowen

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
#setwd("R/Amazon_Malaria_Project/")


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




#############SUBSET:: codigo_test, which contains the first 50 municipalities############

codigo_test <- codigo_spd[1:50,]

##load year rasters
muni_2001 <- raster("all_years/muni_2001.tif") ##contains only 2001


##function to crop, mask, extract
##huge ups to stackexchange here: https://gis.stackexchange.com/questions/130522/increasing-speed-of-crop-mask-extract-raster-by-many-polygons-in-r


###output: empty data frame
test_df <- data.frame() #matrix(nrow=3, ncol=7)

############function FOR SUBSET ONLY!!!##############

system.time(for (i in 1:length(codigo_test)){
  one_poly <- codigo_test[i,] #extract single polygon
  clip1 <- crop(muni_2001, extent(one_poly)) #crop to polygon extent (mask will go faster)
  clip2 <- mask(clip1, one_poly) #mask polygon to above cropped area (takes less time)
  
  lsm_te <- lsm_l_te(clip2) %>%  #calculate landscape metric (total edge)
    rename(metric_te = metric, value_lsm = value) 
  lsm_ed <- lsm_l_ed(clip2) %>%  #calculate landscape metric (edge density)     
    dplyr::select(metric, value) %>% 
    rename(metric_ed = metric, value_ed = value)
  lsm_2 <- cbind(one_poly@data$codigo_ibg, lsm_te, lsm_ed) %>% 
    mutate(year = 2001) #cbind any/all metrics
  test_df <- rbind(lsm_2, test_df) #rbind every loop **this is crucial, because if you don't rbind then it will only show the last output!
  
})

##return: test_df. time elapsed = 54 sec for 3 munis, 606 sec for 50 (10 min)




########SUBSET: ALL YEARS (1-18), needs second for loop###############################
##DO NOT USE: THIS IS THE FULL RASTER AND TAKES TOO MUCH MEMORY
#muni_rast <- raster("lossyear_arc_COPY.tif") ##contains all years 1-18
#years <- as.numeric(1:3) #delineate values of the raster (1 to 18)


###output: empty data frame
all_lsm_yr <- data.frame() 

############function FOR SUBSET ONLY!!!##############


testlist <- list()
raster_files <- list.files(path = ".", pattern = 'muni_2')

for(i in 1:length(raster_files)){
  temp <- raster(raster_files[i])
  testlist[[i]] <- list(temp)}

rastest <- head(testlist)
codigo_test2 <- codigo_spd[1:3,]

##NOTE: the cropping is taking FOREVER!!!!! (muni rast %in% X)
system.time(for(i in 1:length(rastest)){
  
  muni_rast_temp <- rastest[[i]][[1]] #call one of muni rasters from list above
  
  for (j in 1:length(codigo_test2)){
  one_poly <- codigo_test2[j,] #extract single polygon
  clip1 <- crop(muni_rast_temp, extent(one_poly)) #crop to polygon extent (mask will go faster)
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
    mutate(year = 2000+i) #cbind any/all metrics
  all_lsm_yr <- rbind(lsm_2, all_lsm_yr) #rbind every loop **this is crucial, because if you don't rbind then it will only show the last output!
  
}
})

write.csv(all_lsm_yr, "sixyr_3muni_df.csv")

##return: test_yr ##elapsed 12670 sec (3.5 hr), and it didn't finish running.
##and now it's giving me "invalid argument" for saving





##########HERE IS THE SUBSET FOR GOING FORWARD!!!!
k <- extent(codigo_spd$codigo_ibg == '2102309')
k2 <- codigo_sp_list$`2102309`
testmask <- crop(muni_2001, codigo_spd$codigo_ibg == "2102309")
testmask2 <- crop(muni_2001, k2)
testmask1 <- mask(testmask, codigo_sp_list$`2102309`)
test_te <- lsm_l_te(testmask1) %>% 
  mutate(year = 2001) %>% 
  mutate(codigo = 2102309)
################

