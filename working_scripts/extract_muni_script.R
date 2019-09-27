##script to mask and extract ALL municipalities

##MB
##7/30/19

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
lossyear <- raster("lossyear_arc_COPY.tif")



##CROP & MASK
muni_crop <- crop(lossyear, extent(codigo_spd))

##WHOLE SHEBANG
muni_mask <- mask(muni_crop, codigo_spd, filename="munimask.tif") 

##subset muni_mask1 worked!! 
#muni_crop1 <- muni_crop %in% 1
#muni_mask1 <- mask(muni_crop1, codigo_spd, filename ="munimask_1test.tif")

