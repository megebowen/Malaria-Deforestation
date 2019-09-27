##########LANDSCAPE METRICS ANALYSIS: HANSEN FORESTRY DATA, AMAZON RAINFOREST##########
##########FINAL SCRIPT##########
##########MEGHAN BOWEN##########
##########6/2019-9/2019##########




####################load packages####################
library(tidyverse) ##manipulate R language

library(raster) ##load and manipulate rasters
library(sf) ##load shapefiles
library(sp) ##spatial objects
library(rgdal) ##working with all spatial data

library(gfcanalysis) ##package specifically for Hansen Forestry Data. used in this script to download & view tiles only
library(landscapemetrics) ##R equivalent to FRAGSTATS


####################PART 0: MUNICIPALITY BOUNDARIES####################

##Load in shapefile of all Amazon municipalities
muni_shp <- read_sf(dsn = 'municipality_shapefiles', layer = "Amazon_Municipios")


##Convert .shp ("multipolygon") to spatial polygons "spd"
##R will only work with type "spd" for further raster analyses
spd <- sf::as_Spatial(st_geometry(muni_shp),
                      IDs = as.character(1:nrow(muni_shp)))
muni_df <- as.data.frame(muni_shp)
muni_spd <- sp::SpatialPolygonsDataFrame(spd, data = muni_df)





####################PART 1: DOWNLOAD TILES####################
##########use package 'gfcanalysis' to grab and download Hansen forestry tiles

##Find the # of tiles needed. For the ENTIRE AMAZON, need 12 tiles
tiles <- calc_gfc_tiles(aoi = muni_spd)
tiles

##convert aoi to match CRS
aoi <- spTransform(muni_spd, CRS(proj4string(tiles)))

##Test Plot for the tiles and Amazon boundary below: confirm tiles and boundaries are correct
#plot(tiles)
#plot(muni_spd, add=T)

##Download all tiles
output_folder <- "."
download_tiles(tiles, output_folder,
               dataset = "GFC-2018-v1.6",
               images = c("treecover2000", "gain", "lossyear", "datamask"))





####################PART 2: MERGE RASTER TILES####################

####################PART 2A: USE R####################
##########NOTE: THIS DID NOT WORK CORRECTLY WHEN I RAN IT!
##########this method should in theory work in R, but the output was not correct.
##########I used the Arc method below to get the correct raster

##prep: get all rasters loaded, format into list
rastlist <- list.files(output_folder, pattern = "lossyear", all.files = T, full.names = F) 
all_rasters <- lapply(rastlist, raster)

##merge all: 1.5 hrs
merge_hansen <- do.call(merge, all_rasters)

##write all: 1.0 hrs. about a 1GB raster
writeRaster(merge_hansen, filename = "lossyear_R_all.tif", format = "GTiff")


####################PART 2B: USE ARCGIS####################
##########NOTE: WILL NEED TO USE SOME FORM OF ARCGIS FOR THIS PART!

###tool: Mosaic to Raster
###INPUT RASTERS: Load in all rasters containing "lossyear"
###NEW NAME: write to new raster, I called it "lossyear_arc_all.tif"
###NUMBER OF BANDS: 1 (each year is a cell value, so every cell will be a number from 0-18)





####################PART 3: SUBSET COMPLETE RASTER BY YEAR####################
##load big raster
lossyear <- raster("lossyear_arc_all.tif")

##CROP & MASK
muni_crop <- crop(lossyear, extent(codigo_spd))

##WHOLE SHEBANG
muni_mask <- mask(muni_crop, codigo_spd, filename="munimask.tif") 

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

##muni_2018: don't need 1:18 since it's the whole raster
##SO FOR 2018, use the "lossyear_X-all.tif" file






####################PART 4: LOOP FOR ALL LANDSCAPE METRICS####################

####################PART 4A: LOOP BY EACH YEAR####################
####################Use this method after you separate the large raster by each year value!

##reminder: call the municipality spatialdatapolyon df from above! I filtered by codigo (municipality code) to save on time
##separate by codigo to make analyses run faster without excess info
codigo_shp <- muni_shp %>% 
  dplyr::select(codigo_ibg)

#convert from shp to spd
codigospd <- sf::as_Spatial(st_geometry(codigo_shp),
                            IDs = as.character(1:nrow(codigo_shp)))
codigo_df <- as.data.frame(codigo_shp)
codigo_spd <- sp::SpatialPolygonsDataFrame(codigospd, data = codigo_df)

##RASTER prep: load raster year X
muni_2001 <- raster("muni_2001.tif") ##CHANGE YEAR FOR EACH LOOP!!!!!

######################LOOP: calculate landscape metrics for each municipality for year X######################
###output: empty data frame for year X
###NOTE; for year 2018 use the original raster!
lsm_2001_df <- data.frame() #empty dataframe ##CHANGE YEAR

###for loop for every polygon (municipality), for year X
###NOTE: the rbind at the end is crucial, because if you don't rbind then it will only show the last output!

system.time(for (i in 1:length(codigo_spd)){
  one_poly <- codigo_spd[i,] #extract single polygon
  clip1 <- crop(muni_2001, extent(one_poly)) #crop to polygon extent (mask will go faster) ##CHANGE YEAR
  clip2 <- mask(clip1, one_poly) #mask polygon to above cropped area (takes less time than if use mask only)
  
  lsm_te <- lsm_l_te(clip2) %>%  #calculate landscape metric (total edge)
    rename(metric_te = metric, value_te = value) 
  lsm_ed <- lsm_l_ed(clip2) %>%  #calculate landscape metric (edge density)     
    dplyr::select(metric, value) %>% 
    rename(metric_ed = metric, value_ed = value)
  lsm_patch <- lsm_l_area_mn(clip2) %>% #calculate landscape metric (mean patch area) 
    dplyr::select(metric, value) %>% 
    rename(metric_mean_patch= metric, value_mean_area = value)
  lsm_cover <- lsm_l_ta(clip2, directions = 8) %>%  #calculate landscape metric (total area of patches)
    dplyr::select(metric, value) %>% 
    rename(metric_total_area = metric, value_total_area = value)
  lsm_2 <- cbind(one_poly@data$codigo_ibg, lsm_te, lsm_ed, lsm_patch, lsm_cover) %>% #cbind any/all metrics
    mutate(year = 2001) ##CHANGE YEAR
  lsm_2001_df <- rbind(lsm_2, lsm_2001_df) #rbind every loop ##CHANGE YEAR X2 
})

###write output to .csv
write.csv(lsm_2001_df, 'lsm_2001_df.csv') ##CHANGE YEAR X2

##lsm_clump <- lsm_c_clumpy(clip2) %>% 
#  dplyr::select(metric, value) %>% 
#  rename(metric_clump = metric, value_clump = value)



#############MERGE: ALL INDIVIDUAL YEARS###################
##load csvs
##load csvs
lsm_2001 <- read_csv("lsm_2001_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2002 <- read_csv("lsm_2002_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2003 <- read_csv("lsm_2003_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2004 <- read_csv("lsm_2004_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2005 <- read_csv("lsm_2005_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2006 <- read_csv("lsm_2006_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2007 <- read_csv("lsm_2007_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2008 <- read_csv("lsm_2008_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2009 <- read_csv("lsm_2009_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2010 <- read_csv("lsm_2010_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2011 <- read_csv("lsm_2011_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2012 <- read_csv("lsm_2012_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2013 <- read_csv("lsm_2013_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2014 <- read_csv("lsm_2014_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2015 <- read_csv("lsm_2015_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2016 <- read_csv("lsm_2016_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2017 <- read_csv("lsm_2017_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)
lsm_2018 <- read_csv("lsm_2018_df.csv")[,2:15] %>% 
  rename(codigo = `one_poly@data$codigo_ibg`) %>% 
  dplyr::select(year, codigo, value_te, value_ed, value_mean_area, value_total_area)


lsm_years <- full_join(lsm_2001, lsm_2002) %>% 
  full_join(lsm_2003) %>%
  full_join(lsm_2004) %>% 
  full_join(lsm_2005) %>% 
  full_join(lsm_2006) %>% 
  full_join(lsm_2007) %>%
  full_join(lsm_2008) %>%
  full_join(lsm_2009) %>%
  full_join(lsm_2010) %>%
  full_join(lsm_2011) %>%
  full_join(lsm_2012) %>%
  full_join(lsm_2013) %>%
  full_join(lsm_2014) %>%
  full_join(lsm_2015) %>%
  full_join(lsm_2016) %>%
  full_join(lsm_2017) %>%
  full_join(lsm_2018)


#write output to csv
write.csv(lsm_years, "all_lsm.csv")






####################PART 4B: LOOP FOR ALL YEARS####################
##########NOTE: THIS DID NOT WORK CORRECTLY WHEN I RAN IT!
##########R did not have enough memory to calculate for every municipality boundary by each raster value (year)

#prep: load all individual year rasters (2001-2018) from directory
raster_files <- list.files(path = ".", pattern = 'muni_2')

######################LOOP: load all year rasters######################
###output: empty list to load in all raster years
rastlist <- list()

for(i in 1:length(raster_files)){
  temp <- raster(raster_files[i])
  rastlist[[i]] <- list(temp)}


######################LOOP: call all year rasters, run landscape metrics for all municipalities######################
###output: empty data frame for ALL YEARS
all_lsm_yr <- data.frame() 

##NOTE: the cropping part takes FOREVER!!!!! (muni rast %in% X)
system.time(for(i in 1:length(rastlist)){
  
  muni_rast_temp <- rastlist[[i]][[1]] #call one of muni rasters from list above
  
  for (j in 1:length(codigo_spd)){
    one_poly <- codigo_spd[j,] #extract single polygon
    clip1 <- crop(muni_rast_temp, extent(one_poly)) #crop to polygon extent (mask will go faster)
    clip2 <- mask(clip1, one_poly) #mask polygon to above cropped area (takes less time)
    
    lsm_te <- lsm_l_te(clip2) %>% #calculate landscape metric (total edge)
      rename(metric_te = metric, value_te = value) 
    lsm_ed <- lsm_l_ed(clip2) %>% #calculate landscape metric (edge density)
      dplyr::select(metric, value) %>% 
      rename(metric_ed = metric, value_ed = value)
    lsm_patch <- lsm_l_area_mn(clip2) %>% #calculate landscape metric (mean patch area)
      dplyr::select(metric, value) %>% 
      rename(metric_mean_patch= metric, value_mean_area = value) 
    lsm_cover <- lsm_l_ta(clip2, directions = 8) %>% #calculate landscape metric (total patch area)
      dplyr::select(metric, value) %>% 
      rename(metric_total_area = metric, value_total_area = value) 
    lsm_2 <- cbind(one_poly@data$codigo_ibg, lsm_te, lsm_ed, lsm_patch, lsm_cover)  %>% 
      mutate(year = 2000+i) #cbind all metrics
    all_lsm_yr <- rbind(lsm_2, all_lsm_yr) #rbind every loop 
    
  }
})

#write output to csv
write.csv(all_lsm_yr, "final_lsm_df.csv")