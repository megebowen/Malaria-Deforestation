##For Loop -- Separate Muni_Mask by each year

##8/14/2019
##upd 8/20
##MB


##load packages
library(tidyverse) #data cleaning and manipulation
library(raster) ##load and manipulate rasters
library(sf) ##load shapefiles
library(sp) ##additional functions for spatial objects
##library(spatstat) ##work with shapefiles ##DO NOT USE ON REMOTE SERVER
library(rgdal) ##for projections and coord systems
##library(gfcanalysis) ##package specifically for Hansen Forestry Data! ##DO NOT USE ON REMOTE SERVER
library(landscapemetrics) ##R equivalent to FRAGSTATS

##need to set the working directory on the remote server
##setwd("R/Amazon_Malaria_Project/")

##load shapefiles
muni_shp <- read_sf(dsn = 'municipality_shapefiles', layer = "Amazon_Municipios")
##need to convert the municipality .shp to a "SpatialDataFrame" so gfcanalysis can grab the forestry tiles online
spd <- sf::as_Spatial(st_geometry(muni_shp),
                      IDs = as.character(1:nrow(muni_shp)))
muni_df <- as.data.frame(muni_shp)
muni_spd <- sp::SpatialPolygonsDataFrame(spd, data = muni_df)

##separate by codigo
codigo_shp <- muni_shp %>% 
  dplyr::select(codigo_ibg)

codigo_spd <- sf::as_Spatial(st_geometry(codigo_shp),
                             IDs = as.character(1:nrow(codigo_shp)))
codigo_df <- as.data.frame(codigo_shp)
codigo_spd <- sp::SpatialPolygonsDataFrame(codigo_spd, data = codigo_df)


##load raster
muni_mask <- raster("munimask.tif")

##MANUAL SEPARATION and WRITE TO RASTER
muni_2001 <- muni_mask %in% 1:1 %>% 
  writeRaster(filename = "muni_2001.tif", format = "GTiff")

muni_2002 <- muni_mask %in% 1:2 %>% 
  writeRaster(filename = "muni_2002.tif", format = "GTiff")

muni_2003 <- muni_mask %in% 1:3 %>% 
  writeRaster(filename = "muni_2003.tif", format = "GTiff")

muni_2004 <- muni_mask %in% 1:4 %>% 
  writeRaster(filename = "muni_2004.tif", format = "GTiff")

muni_2005 <- muni_mask %in% 1:5 %>% 
  writeRaster(filename = "muni_2005.tif", format = "GTiff")

muni_2006 <- muni_mask %in% 1:6 %>% 
  writeRaster(filename = "muni_2006.tif", format = "GTiff")

muni_2007 <- muni_mask %in% 1:7 %>% 
  writeRaster(filename = "muni_2007.tif", format = "GTiff")

muni_2008 <- muni_mask %in% 1:8 %>% 
  writeRaster(filename = "muni_2008.tif", format = "GTiff")

muni_2009 <- muni_mask %in% 1:9 %>% 
  writeRaster(filename = "muni_2009.tif", format = "GTiff")

muni_2010 <- muni_mask %in% 1:10 %>% 
  writeRaster(filename = "muni_2010.tif", format = "GTiff")

muni_2011 <- muni_mask %in% 1:11 %>% 
  writeRaster(filename = "muni_2011.tif", format = "GTiff")

muni_2012 <- muni_mask %in% 1:12 %>% 
  writeRaster(filename = "muni_2012.tif", format = "GTiff")

muni_2013 <- muni_mask %in% 1:13 %>% 
  writeRaster(filename = "muni_2013.tif", format = "GTiff")

muni_2014 <- muni_mask %in% 1:14 %>% 
  writeRaster(filename = "muni_2014.tif", format = "GTiff")

muni_2015 <- muni_mask %in% 1:15 %>% 
  writeRaster(filename = "muni_2015.tif", format = "GTiff")

muni_2016 <- muni_mask %in% 1:16 %>% 
  writeRaster(filename = "muni_2016.tif", format = "GTiff")

muni_2017 <- muni_mask %in% 1:17 %>% 
  writeRaster(filename = "muni_2017.tif", format = "GTiff")

muni_2018 <- muni_mask %>% ##don't need 1:18 since it's the whole raster
  writeRaster(filename = "muni_2018.tif", format = "GTiff") 



###output
year_rast_list <- list()

#function
#for (i in 1:18){
#  year_rast_list[[i]] <- muni_mask %in% 1:i
#}

#return


##test: LSM
te_2001 <- lsm_l_te(muni_2001)

