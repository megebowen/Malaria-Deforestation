##Meghan Bowen
##Date:
##Code to download all Hansen et al forest loss tiles (2018, v1.6) for the legal Amazon
#
#
#
#
##Load packages

library(tidyverse) 
library(raster) ##load and manipulate rasters
library(sf) ##load shapefiles
library(sp) ##spatial objects
library(spatstat) ##work with shapefiles
library(gfcanalysis) ##package specifically for Hansen Forestry Data!
library(rgdal) ##working with all spatial data
library(gdalUtils) ##functions to work with rgdal


##Load in shapefile of all Amazon municipalities
muni_shp <- read_sf(dsn = 'municipality_shapefiles', layer = "Amazon_Municipios")


##Convert .shp ("multipolygon") to spatial polygons
spd <- sf::as_Spatial(st_geometry(muni_shp),
                      IDs = as.character(1:nrow(muni_shp)))

muni_df <- as.data.frame(muni_shp)

muni_spd <- sp::SpatialPolygonsDataFrame(spd, data = muni_df)


##Find the # of tiles needed. For the ENTIRE AMAZON, need 12 tiles
tiles <- calc_gfc_tiles(aoi = muni_spd)
tiles

##convert aoi to match CRS
aoi <- spTransform(muni_spd, CRS(proj4string(tiles)))


##Plot for the tiles and Amazon boundary below
#plot(tiles)
#plot(muni_spd, add=T)

##Download all tiles


download_tiles(tiles, output_folder,
               dataset = "GFC-2018-v1.6",
               images = c("treecover2000", "gain", "lossyear", "datamask"))



##Load all **LOSSYEAR** tiles into R

setwd("\\\\esm.ucsb.edu/research/research-1-4tb/macdonald-research/Amazon_Malaria_Project/tiles")



##test with subset took 25 minutes!!
zeroNlist <- list.files(getwd(), pattern = "lossyear_00N", all.files = T, full.names = F)
zeroNrasters <- lapply(zeroNlist, raster)
merge_zeroN <- do.call(merge, zeroNrasters) ##final product looks good


writeRaster(merge_zeroN, filename = "lossyear_00N.tif", format = "GTiff") ##writeraster about 11 min, 444KB


##the big daddy raster
#prep: get all rasters loaded, format into list
rastlist <- list.files(getwd(), pattern = "lossyear", all.files = T, full.names = F) 
all_rasters <- lapply(rastlist, raster)

#merge all: 1.5 hrs
merge_hansen <- do.call(merge, all_rasters)

#write all: 1.0 hrs. about a 1GB raster
writeRaster(merge_hansen, filename = "lossyear_all.tif", format = "GTiff")


#%>% 
#map(raster) %>% 
#map(raster::values) ##ERROR: cannot allocate vector of size 6.0gb
#for(i in rastlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) } 

##Merge all tiles using gdalutils

#1. build a list from rastlist  :: 
all_rast <- as.list(rastlist)
##BUT NEED TO LOAD ALL RASTERS IN R FIRST< THEN MERGE
##extract using gfcanalysis
output_folder <- getwd()
extract_gfc(muni_spd, output_folder, stack = "change", filename = "hansen_merge.tif")

#merge_hansen <- Reduce(merge, lapply(rastlist, raster))