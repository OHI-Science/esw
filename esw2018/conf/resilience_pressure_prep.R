
#-------------#

#resilience

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Resilience")
options(scipen=999)

library(tidyverse)

#fiheries by-laws by region (n)

t <- read.csv("source/fisheries_bylaws_by_region_20190731.csv")

t <- t %>% 
  mutate(index = 1) %>% 
  group_by(region) %>% 
  summarise(by_laws = sum(index, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(year = 2018) %>% 
  mutate(resilience_score = by_laws / max(by_laws)) %>% 
  select(rgn_id = region, year, resilience_score)

#resilience: fp_fisbylaw_esw2018.csv
write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Resilience/prep/fp_fisbylaw_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fp_fisbylaw_esw2018.csv", row.names = F) #to git

#-------------#

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Resilience")
options(scipen=999)

library(raster)
library(tidyverse)
library(rgdal)
#library(tmaptools)

#protected areas within EEZ (to 12nm) and 3nm
#no areas outide the 12
#calculate % of total by region
#write as .csv
#UPDATED TO CODE - only use areas with restrictions on BTG

#read in 3nm protected areas 
#shp1 <- readOGR(dsn = "source", layer = "protected_areas_SW_dissolve_3nm_BNG")
shp1 <- readOGR(dsn = "source", layer = "regions_no_BTG_3nm_BNG")
plot(shp1)
t1 <- shp1@data
t1$rgn_id <- as.numeric(as.character(t1$rgn_id))
shp2 <- readOGR(dsn = "source", layer = "ESW_regions_3nm_buf_20190122_BNG")
#plot(shp2)
t2 <- shp2@data
t2$rgn_id <- as.numeric(as.character(t2$rgn_id))

t3 <- left_join(t1, t2, by = "rgn_id")
t3 <- t3 %>% 
  mutate(prop = area_km2.x / area_km2.y) %>% 
  mutate(year = 2018) %>% 
  select(rgn_id, year, resilience_score = prop) %>% 
  arrange(rgn_id)

#resilience: hd_mpa_coast_esw2018.csv
write.csv(t3, "C:/Folders/Dropbox/Projects/OHI/Data/Resilience/prep/hd_mpa_coast_esw2018.csv", row.names = F)
write.csv(t3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hd_mpa_coast_esw2018.csv", row.names = F) #to git
#resilience: fp_mpa_coast_esw2018.csv
write.csv(t3, "C:/Folders/Dropbox/Projects/OHI/Data/Resilience/prep/fp_mpa_coast_esw2018.csv", row.names = F)
write.csv(t3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fp_mpa_coast_esw2018.csv", row.names = F) #to git

#read in EEZ protected areas (this is only out to the 12)
#shp1 <- readOGR(dsn = "source", layer = "protected_areas_SW_dissolve_EEZ_BNG")
shp1 <- readOGR(dsn = "source", layer = "regions_no_BTG_EEZ_BNG")
plot(shp1)

t1 <- shp1@data
t1$rgn_id <- as.numeric(as.character(t1$rgn_id))
shp2 <- readOGR(dsn = "source", layer = "ESW_regions_20190122_BNG")
t2 <- shp2@data
t2$rgn_id <- as.numeric(as.character(t2$rgn_id))

t3 <- left_join(t1, t2, by = "rgn_id")
t3 <- t3 %>% 
  mutate(prop = area_km2.x / area_km2.y) %>% 
  mutate(year = 2018) %>% 
  select(rgn_id, year, resilience_score = prop) %>% 
  arrange(rgn_id)

#resilience: hd_mpa_eez_esw2018.csv
write.csv(t3, "C:/Folders/Dropbox/Projects/OHI/Data/Resilience/prep/hd_mpa_eez_esw2018.csv", row.names = F)
write.csv(t3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hd_mpa_eez_esw2018.csv", row.names = F) #to git
#resilience: fp_mpa_eez_esw2018.csv
write.csv(t3, "C:/Folders/Dropbox/Projects/OHI/Data/Resilience/prep/fp_mpa_eez_esw2018.csv", row.names = F)
write.csv(t3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fp_mpa_eez_esw2018.csv", row.names = F) #to git


#-------------#


#pressures

#plastics
rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
options(scipen=999)

library(raster)
library(tidyverse)
library(rgdal)
#library(tmaptools)

ex <- extent(c(-11, -2.3, 47.3, 52))
ex2 <- as(ex,'SpatialPolygons') #convert to SpatialPolygon
proj4string(ex2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
r1 <- raster(ex2, resolution = 0.009, crs = "+proj=longlat +datum=WGS84", vals = NA) #approx. 1km res - all values NA

#import global raster (ohi global data: rescaled) reprojected in ArcMap to WGS84
#source data are rescaled 0-1
#data source: https://mazu.nceas.ucsb.edu/data/#ohi_pressure_data (Ericson 2014)
r2 <- raster("C:/Folders/Dropbox/Projects/OHI/Data/Goals/CW/source/plastics_rescaled/plastics_wgs84.tif")

r1 <- projectRaster(r2, r1)

#read in region poly
#shp <- readOGR(dsn = "extent", layer = "ESW_regions_20190122_BNG")
shp <- readOGR(dsn = "extent", layer = "ESW_regions_12nm_buf_20191205_BNG")
#reproject to wgs84
#plot(shp)
plot(r1)
proj <- crs(r1)
shp <- spTransform(shp, proj)

r1 <- mask(r1, shp)
plot(r1)

#rescale to 1
q <- quantile(r1, 1, na.rm = T)
fun = function(x){ifelse(x == q, 1, x / q)}
r1 <- calc(r1, fun = fun)

#extract a mean value per region id
d <- raster::extract(r1, shp, fun = mean, na.rm = T, sp = T)
t <- d@data

t <- t %>%  
  arrange(rgn_id) %>% 
  mutate(year = 2015) %>% 
  dplyr::select(rgn_id, year, pressure_score = layer)

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/po_trash_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/po_trash_esw2018.csv", row.names = F) #to git


#-------------#


#coastal population (residential)
rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
options(scipen=999)

library(raster)
library(tidyverse)
#library(rgdal)
#library(tmaptools)

ex <- extent(c(-11, -2.3, 47.3, 52))
ex2 <- as(ex,'SpatialPolygons') #convert to SpatialPolygon
proj4string(ex2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
r1 <- raster(ex2, resolution = 0.009, crs = "+proj=longlat +datum=WGS84", vals = NA) #approx. 1km res - all values NA

#import uk population raster
r2 <- raster("C:/Folders/Dropbox/Projects/OHI/Data/Pressures/source/uk_residential_population/UK_residential_population_2011_1_km.tif")
#plot(r2)
r1 <- projectRaster(r2, r1)

#?projectRaster

#read in region poly
#sum population within 5km of the coast by rgn
#rescale
shp <- readOGR(dsn = "extent", layer = "ESW_regions_5km_intbuf_20190122_BNG")
#plot(shp)
#reproject to wgs84
proj <- crs(r1)
shp <- spTransform(shp, proj)
#extract a mean value per region id
d <- raster::extract(r1, shp, fun = sum, na.rm = T, sp = T)
t <- d@data

#plot(r1)

t <- t %>%  
  arrange(rgn_id) %>% 
  mutate(year = 2011) %>% 
  mutate(pressure_score = UK_residential_population_2011_1_km / max(UK_residential_population_2011_1_km)) %>% 
  dplyr::select(rgn_id, year, pressure_score) 
  

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/hd_intertidal_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hd_intertidal_esw2018.csv", row.names = F) #to git


#-------------#


#slr
rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
options(scipen=999)

library(raster)
library(tidyverse)
library(rgdal)
#library(tmaptools)

ex <- extent(c(-11, -2.3, 47.3, 52))
ex2 <- as(ex,'SpatialPolygons') #convert to SpatialPolygon
proj4string(ex2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
r1 <- raster(ex2, resolution = 0.009, crs = "+proj=longlat +datum=WGS84", vals = NA) #approx. 1km res - all values NA

#import global raster (raw data)
r2 <- raster("C:/Folders/Dropbox/Projects/OHI/Data/Pressures/source/pressure_one_2013_slr_mol_20150714052551/slr_combo.tif")
r1 <- projectRaster(r2, r1)

#read in region poly
shp <- readOGR(dsn = "extent", layer = "ESW_regions_3nm_buf_20190122_BNG")
#plot(shp)
#reproject to wgs84
proj <- crs(r1)
shp <- spTransform(shp, proj)

r1 <- mask(r1, shp)

#rescale to 1
q <- quantile(r1, 1, na.rm = T)
fun = function(x){ifelse(x == q, 1, x / q)}
r1 <- calc(r1, fun = fun)

plot(r1)

#extract a mean value per region id
d <- raster::extract(r1, shp, fun = mean, na.rm = T, sp = T)
t <- d@data

t <- t %>%  
  arrange(rgn_id) %>% 
  mutate(year = 2013) %>% 
  dplyr::select(rgn_id, year, pressure_score = layer)

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/cc_slr_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cc_slr_esw2018.csv", row.names = F) #to git


#-------------#


#sst
rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
options(scipen=999)

library(raster)
library(tidyverse)
library(rgdal)
#library(tmaptools)

ex <- extent(c(-11, -2.3, 47.3, 52))
ex2 <- as(ex,'SpatialPolygons') #convert to SpatialPolygon
proj4string(ex2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
r1 <- raster(ex2, resolution = 0.009, crs = "+proj=longlat +datum=WGS84", vals = NA) #approx. 1km res - all values NA

#import global raster (raw data)
r2 <- raster("C:/Folders/Dropbox/Projects/OHI/Data/Pressures/source/pressure_one_2013_sst_mol_20150714052619/sst_combo.tif")
r1 <- projectRaster(r2, r1)

#read in region poly
#shp <- readOGR(dsn = "extent", layer = "ESW_regions_20190122_BNG")
shp <- readOGR(dsn = "extent", layer = "ESW_regions_12nm_buf_20191205_BNG")
plot(shp)
#reproject to wgs84
proj <- crs(r1)
shp <- spTransform(shp, proj)

r1 <- mask(r1, shp)

#rescale to 1
q <- quantile(r1, 1, na.rm = T)
fun = function(x){ifelse(x == q, 1, x / q)}
r1 <- calc(r1, fun = fun)

plot(r1)

#extract a mean value per region id
d <- raster::extract(r1, shp, fun = mean, na.rm = T, sp = T)
t <- d@data

t <- t %>%  
  arrange(rgn_id) %>% 
  mutate(year = 2013) %>% 
  dplyr::select(rgn_id, year, pressure_score = layer)

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/cc_sst_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cc_sst_esw2018.csv", row.names = F) #to git


#-------------#


#po_chemicals_eez
#combines: pesticdes, pollution from vessels and urban run-off

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
options(scipen=999)

library(raster)
library(tidyverse)
#library(rgdal)
#library(tmaptools)

ex <- extent(c(-11, -2.3, 47.3, 52))
ex2 <- as(ex,'SpatialPolygons') #convert to SpatialPolygon
proj4string(ex2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
r <- raster(ex2, resolution = 0.009, crs = "+proj=longlat +datum=WGS84", vals = NA) #approx. 1km res - all values NA

###########

#plumes of pesticides
#import global raster (raw data)
r2 <- raster("C:/Folders/Dropbox/Projects/OHI/Data/Pressures/source/pressure_one_2013_plumes_pest_mol_20150714052522/plumes_pest_combo.tif")
r1 <- projectRaster(r2, r)

#rescale to 1 - for this rescale at the table level
#q <- quantile(r1, 1, na.rm = T)
#fun = function(x){ifelse(x == q, 1, x / q)}
#r1_pesticides <- calc(r1, fun = fun)
#plot(r1)

#read in region poly
#shp <- readOGR(dsn = "extent", layer = "ESW_regions_20190122_BNG")
shp <- readOGR(dsn = "extent", layer = "ESW_regions_12nm_buf_20191205_BNG")
plot(shp)
#reproject to wgs84
proj <- crs(r1)
shp <- spTransform(shp, proj)
#extract a mean value per region id
d <- raster::extract(r1, shp, fun = mean, na.rm = T, sp = T)
t <- d@data
#rescale to 1
pest <- t %>% 
  dplyr::select(rgn_id, pressure = plumes_pest_combo) %>% 
  mutate(pressure = pressure /max(pressure))

###########

#polution from vessels
#AIS
AIS_2011 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2011/2011.tif")
#?resample - xtent - cell size - projection
AIS_2011_sw <- resample(AIS_2011, r, "bilinear")
plot(AIS_2011_sw)
AIS_2012 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2012/2012.tif")
AIS_2012_sw <- resample(AIS_2012, r, "bilinear")
AIS_2013 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2013/2013.tif")
AIS_2013_sw <- resample(AIS_2013, r, "bilinear")
AIS_2014 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2014/2014.tif")
AIS_2014_sw <- resample(AIS_2014, r, "bilinear")
AIS_2015 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2015/2015.tif")
AIS_2015_sw <- resample(AIS_2015, r, "bilinear")

#plot(AIS_2011_sw)
#plot(AIS_2012_sw)
#plot(AIS_2013_sw)
#plot(AIS_2014_sw)
#plot(AIS_2015_sw)

AIS <- stack(AIS_2011_sw, AIS_2012_sw, AIS_2013_sw, AIS_2014_sw, AIS_2015_sw)
AIS <- calc(AIS, mean, na.rm = T)
#plot(AIS)
#read in region poly
#shp <- readOGR(dsn = "extent", layer = "ESW_regions_20190122_BNG")
shp <- readOGR(dsn = "extent", layer = "ESW_regions_12nm_buf_20191205_BNG")
plot(shp)
#reproject to wgs84
proj <- crs(r1)
shp <- spTransform(shp, proj)
#extract a mean value per region id
d <- raster::extract(AIS, shp, fun = mean, na.rm = T, sp = T)
t <- d@data
#rescale to 1
ves_pol <- t %>% 
  dplyr::select(rgn_id, ves_pol = layer) %>% 
  mutate(pressure = ves_pol /max(ves_pol)) %>% 
  dplyr::select(rgn_id, pressure)

###########

#urban run off
run_off <- read.csv("C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_inorganic_run_off2_esw2018.csv")
run_off <- run_off %>%
  group_by(rgn_id) %>% 
  summarise(pressure = mean(inorg_run_off, na.rm = T)) %>% 
  mutate(pressure = pressure /max(pressure)) %>% 
  ungroup() %>% 
  as.data.frame()
  
###########

pest
run_off
ves_pol

po_chemicals_eez <- rbind(pest, run_off, ves_pol)
po_chemicals_eez <- po_chemicals_eez %>% 
  group_by(rgn_id) %>% 
  summarise(pressure_score = mean(pressure, na.rm =T)) %>%
  ungroup() %>% 
  mutate(year = 2017) %>% 
  as.data.frame()

write.csv(po_chemicals_eez, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/po_chemicals_esw2018.csv", row.names = F)
write.csv(po_chemicals_eez, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/po_chemicals_esw2018.csv", row.names = F) #to git


#-------------#


#po_chemicals_3nm
#combines: pesticdes, pollution from vessels and urban run-off

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
options(scipen=999)

library(raster)
library(tidyverse)
#library(rgdal)
#library(tmaptools)

ex <- extent(c(-11, -2.3, 47.3, 52))
ex2 <- as(ex,'SpatialPolygons') #convert to SpatialPolygon
proj4string(ex2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
r <- raster(ex2, resolution = 0.009, crs = "+proj=longlat +datum=WGS84", vals = NA) #approx. 1km res - all values NA

###########

#plumes of pesticides
#import global raster (raw data)
r2 <- raster("C:/Folders/Dropbox/Projects/OHI/Data/Pressures/source/pressure_one_2013_plumes_pest_mol_20150714052522/plumes_pest_combo.tif")
r1 <- projectRaster(r2, r)

#rescale to 1 - for this rescale at the table level
#q <- quantile(r1, 1, na.rm = T)
#fun = function(x){ifelse(x == q, 1, x / q)}
#r1_pesticides <- calc(r1, fun = fun)
#plot(r1)

#read in region poly
shp <- readOGR(dsn = "extent", layer = "ESW_regions_3nm_buf_20190122_BNG")
#plot(shp)
#reproject to wgs84
proj <- crs(r1)
shp <- spTransform(shp, proj)
#extract a mean value per region id
d <- raster::extract(r1, shp, fun = mean, na.rm = T, sp = T)
t <- d@data
pest <- t %>% 
  dplyr::select(rgn_id, pressure = plumes_pest_combo) %>% 
  mutate(pressure = pressure /max(pressure))

###########

#polution from vessels
#AIS
AIS_2011 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2011/2011.tif")
#?resample - xtent - cell size - projection
AIS_2011_sw <- resample(AIS_2011, r, "bilinear")
plot(AIS_2011_sw)
AIS_2012 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2012/2012.tif")
AIS_2012_sw <- resample(AIS_2012, r, "bilinear")
AIS_2013 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2013/2013.tif")
AIS_2013_sw <- resample(AIS_2013, r, "bilinear")
AIS_2014 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2014/2014.tif")
AIS_2014_sw <- resample(AIS_2014, r, "bilinear")
AIS_2015 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2015/2015.tif")
AIS_2015_sw <- resample(AIS_2015, r, "bilinear")

#plot(AIS_2011_sw)
#plot(AIS_2012_sw)
#plot(AIS_2013_sw)
#plot(AIS_2014_sw)
#plot(AIS_2015_sw)

AIS <- stack(AIS_2011_sw, AIS_2012_sw, AIS_2013_sw, AIS_2014_sw, AIS_2015_sw)
AIS <- calc(AIS, mean, na.rm = T)
#plot(AIS)
#read in region poly
shp <- readOGR(dsn = "extent", layer = "ESW_regions_3nm_buf_20190122_BNG")
#plot(shp)
#reproject to wgs84
proj <- crs(r1)
shp <- spTransform(shp, proj)
#extract a mean value per region id
d <- raster::extract(AIS, shp, fun = mean, na.rm = T, sp = T)
t <- d@data
ves_pol <- t %>% 
  dplyr::select(rgn_id, ves_pol = layer) %>% 
  mutate(pressure = ves_pol /max(ves_pol)) %>% 
  dplyr::select(rgn_id, pressure)

###########

#urban run off
run_off <- read.csv("C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_inorganic_run_off2_esw2018.csv")
run_off <- run_off %>%
  group_by(rgn_id) %>% 
  summarise(pressure = mean(inorg_run_off, na.rm = T)) %>% 
  mutate(pressure = pressure /max(pressure)) %>% 
  ungroup() %>% 
  as.data.frame()

###########

pest
run_off
ves_pol

po_chemicals_3nm <- rbind(pest, run_off, ves_pol)
po_chemicals_3nm <- po_chemicals_3nm %>% 
  group_by(rgn_id) %>% 
  summarise(pressure_score = mean(pressure, na.rm =T)) %>%
  ungroup() %>% 
  mutate(year = 2017) %>% 
  as.data.frame()

write.csv(po_chemicals_3nm, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/po_chemicals_3nm_esw2018.csv", row.names = F)
write.csv(po_chemicals_3nm, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/po_chemicals_3nm_esw2018.csv", row.names = F) #to git


#-------------#


#nutrients
rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
options(scipen=999)

library(raster)
library(tidyverse)
library(rgdal)
#library(tmaptools)

ex <- extent(c(-11, -2.3, 47.3, 52))
ex2 <- as(ex,'SpatialPolygons') #convert to SpatialPolygon
proj4string(ex2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
r1 <- raster(ex2, resolution = 0.009, crs = "+proj=longlat +datum=WGS84", vals = NA) #approx. 1km res - all values NA

#import global raster (raw data)
r2 <- raster("C:/Folders/Dropbox/Projects/OHI/Data/Pressures/source/pressure_one_2013_plumes_fert_mol_20150714052515/plumes_fert_combo.tif")
r1 <- projectRaster(r2, r1)

plot(r1)

#read in region poly
#shp <- readOGR(dsn = "extent", layer = "ESW_regions_20190122_BNG")
shp <- readOGR(dsn = "extent", layer = "ESW_regions_12nm_buf_20191205_BNG")
shp3nm <- readOGR(dsn = "extent", layer = "ESW_regions_3nm_buf_20190122_BNG")

#reproject to wgs84
proj <- crs(r1)
shp <- spTransform(shp, proj)
shp3nm <- spTransform(shp3nm, proj)

lines(shp)
lines(shp3nm)
#plot(shp)
#plot(shp3nm)
#?mask

r1 <- mask(r1, shp)
#rescale to 1
q <- quantile(r1, 1, na.rm = T)
fun = function(x){ifelse(x == q, 1, x / q)}
r1 <- calc(r1, fun = fun)

plot(r1)

#extract a mean value per region id
#d <- raster::extract(r1, shp, fun = mean, na.rm = T, sp = T) #eez (12nm)
d <- raster::extract(r1, shp3nm, fun = mean, na.rm = T, sp = T) #3nm
t <- d@data

t <- t %>%  
  arrange(rgn_id) %>% 
  mutate(year = 2013) %>% 
  dplyr::select(rgn_id, year, pressure_score = layer)

#write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/po_nutrients_esw2018.csv", row.names = F)
#write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/po_nutrients_esw2018.csv", row.names = F) #to git

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/po_nutrients_3nm_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/po_nutrients_3nm_esw2018.csv", row.names = F) #to git


#-------------#


#carstairs deprivation index - high values - high deprivation
#use coastal population code - line 126 above
#read in raster
#extract to 5km - calculate mean per region - rescale to 1 - write

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
options(scipen=999)

library(raster)
library(tidyverse)
library(rgdal)
#library(tmaptools)

ex <- extent(c(-11, -2.3, 47.3, 52))
ex2 <- as(ex,'SpatialPolygons') #convert to SpatialPolygon
proj4string(ex2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
r1 <- raster(ex2, resolution = 0.009, crs = "+proj=longlat +datum=WGS84", vals = NA) #approx. 1km res - all values NA

#import carstairs raster
r2 <- raster("C:/Folders/Dropbox/Projects/OHI/Data/Pressures/source/Carstairs_raster/carstairs/w001001.adf")
plot(r2)
r1 <- projectRaster(r2, r1)

#read in region poly
#sum carstairs index within 5km of the coast by rgn
#rescale
shp <- readOGR(dsn = "extent", layer = "ESW_regions_5km_intbuf_20190122_BNG")
#plot(shp)
#reproject to wgs84
proj <- crs(r1)
shp <- spTransform(shp, proj)
#extract a mean value per region id
d <- raster::extract(r1, shp, fun = mean, na.rm = T, sp = T)
t <- d@data

t <- t %>%  
  arrange(rgn_id) %>% 
  mutate(year = 2011) %>% 
  mutate(w001001adjusted = w001001 + abs(min(w001001))) %>% 
  mutate(pressure_score = w001001adjusted / max(w001001adjusted)) %>% 
  dplyr::select(rgn_id, year, pressure_score) 

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/soc_deprivation_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/soc_deprivation_esw2018.csv", row.names = F) #to git


#-------------#

#recreation pressure
#read in recreation score - DO NOT INVERT - values show greatest recreation  possibility
#just rescale to  max 1 pressure

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/github/esw_copy/esw2018/layers")

t <- read.csv("tr_rec_esw2018.csv")
t <- t %>% 
  mutate(pressure_score = mean_tscore / max(mean_tscore)) %>% 
  dplyr::select(rgn_id, year, pressure_score)

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/tr_rec_pres_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/tr_rec_pres_esw2018.csv", row.names = F) #to git


#-------------#

#AIS vessel density layer - as proxy for underwater noise from vessels

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
options(scipen=999)

library(raster)
library(tidyverse)
library(rgdal)
#library(tmaptools)

ex <- extent(c(-11, -2.3, 47.3, 52))
ex2 <- as(ex,'SpatialPolygons') #convert to SpatialPolygon
proj4string(ex2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
r <- raster(ex2, resolution = 0.009, crs = "+proj=longlat +datum=WGS84", vals = NA) #approx. 1km res - all values NA

AIS_2011 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2011/2011.tif")
#?resample - xtent - cell size - projection
AIS_2011_sw <- resample(AIS_2011, r, "bilinear")
plot(AIS_2011_sw)
AIS_2012 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2012/2012.tif")
AIS_2012_sw <- resample(AIS_2012, r, "bilinear")
AIS_2013 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2013/2013.tif")
AIS_2013_sw <- resample(AIS_2013, r, "bilinear")
AIS_2014 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2014/2014.tif")
AIS_2014_sw <- resample(AIS_2014, r, "bilinear")
AIS_2015 <- raster("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_density_grid_2015/2015.tif")
AIS_2015_sw <- resample(AIS_2015, r, "bilinear")

#plot(AIS_2011_sw)
#plot(AIS_2012_sw)
#plot(AIS_2013_sw)
#plot(AIS_2014_sw)
#plot(AIS_2015_sw)

AIS <- stack(AIS_2011_sw, AIS_2012_sw, AIS_2013_sw, AIS_2014_sw, AIS_2015_sw)
AIS <- calc(AIS, mean, na.rm = T)
plot(AIS)

#read in region poly
#shp <- readOGR(dsn = "extent", layer = "ESW_regions_20190122_BNG")
shp <- readOGR(dsn = "extent", layer = "ESW_regions_12nm_buf_20191205_BNG")
plot(shp)
#reproject to wgs84
proj <- crs(AIS)
shp <- spTransform(shp, proj)
#extract a mean value per region id
d <- raster::extract(AIS, shp, fun = mean, na.rm = T, sp = T)

t <- d@data

ves_noise <- t %>% 
  dplyr::select(rgn_id, ves_noise = layer) %>% 
  mutate(pressure_score = ves_noise / max(ves_noise)) %>% 
  arrange(rgn_id) %>% 
  mutate(year = 2015) %>% 
  dplyr::select(rgn_id, year, pressure_score)

write.csv(ves_noise, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/po_ves_noise_esw2018.csv", row.names = F)
write.csv(ves_noise, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/po_ves_noise_esw2018.csv", row.names = F) #to git


#-------------#

#Operation Neptune data as pressure - change in coastal areas

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
options(scipen=999)

library(raster)
library(rgeos)
library(tidyverse)
library(rgdal)

#read in region poly
shp <- readOGR(dsn = "extent", layer = "ESW_regions_20190122_BNG")
plot(shp)

###

#urban sprawl

#neptune 2014
shp_nep14 <- readOGR(dsn = "source/Neptune", layer = "Neptune_Coastline_Campaign_Land_Use_2014_BNG")
plot(shp_nep14)

nep14_esw <- raster::intersect(shp_nep14, shp)

t <- nep14_esw@data

urb_14 <- t %>% 
  filter(LU_2014 == "Urban/Built-up") %>% 
  group_by(rgn_id) %>% 
  summarise(urb_area_14_km2 = sum(Hectares, na.rm = T) / 100) %>% 
  ungroup

urb_65 <- t %>% 
  filter(LU_1965 == "Urban/Built-up") %>% 
  group_by(rgn_id) %>% 
  summarise(urb_area_65_km2 = sum(Hectares, na.rm = T) / 100) %>% 
  ungroup

urb <- urb_65 %>% 
  left_join(urb_14, "rgn_id") %>% 
  mutate(inc = urb_area_14_km2 - urb_area_65_km2) %>% 
  mutate(pcnt_inc1 = (inc / urb_area_65_km2) * 100) %>% 
  mutate(pcnt_inc = pmax(0, pcnt_inc1)) %>% 
  mutate(pressure_score = pcnt_inc / max(pcnt_inc)) %>% 
  arrange(rgn_id) %>% 
  mutate(year = 2014) %>% 
  dplyr::select(rgn_id, year, pressure_score)
 
write.csv(urb, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/urban_sprawl_esw2018.csv", row.names = F)
write.csv(urb, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/urban_sprawl_esw2018.csv", row.names = F) #to git 

###

#industrial sprawl

#neptune 2014
shp_nep14 <- readOGR(dsn = "source/Neptune", layer = "Neptune_Coastline_Campaign_Land_Use_2014_BNG")
#plot(shp_nep65)

nep14_esw <- raster::intersect(shp_nep14, shp)
#plot(nep14_esw)
t <- nep14_esw@data

ind_14 <- t %>% 
  filter(LU_2014 == "Industry") %>% 
  group_by(rgn_id) %>% 
  summarise(ind_area_14_km2 = sum(Hectares, na.rm = T) / 100) %>% 
  ungroup

ind_65 <- t %>% 
  filter(LU_1965 == "Industry") %>% 
  group_by(rgn_id) %>% 
  summarise(ind_area_65_km2 = sum(Hectares, na.rm = T) / 100) %>% 
  ungroup

ind <- ind_65 %>% 
  full_join(ind_14, "rgn_id") 


#no data for regions 4
rgn_id <- as.integer(4)
ind_area_65_km2 <- as.integer(NA)
ind_area_14_km2 <- as.integer(NA)

ios <- data.frame(cbind(rgn_id, ind_area_65_km2,ind_area_14_km2))

ind <- rbind(ind, ios)
  
ind <- ind %>% 
  mutate(inc = ind_area_14_km2 - ind_area_65_km2) %>% 
  mutate(pcnt_inc1 = (inc / ind_area_65_km2) * 100) %>% 
  mutate(pcnt_inc = pmax(0, pcnt_inc1)) %>% 
  mutate(pressure_score = pcnt_inc / max(pcnt_inc, na.rm = T)) %>% 
  arrange(rgn_id) %>% 
  mutate(year = 2014) %>% 
  dplyr::select(rgn_id, year, pressure_score)

ind[is.na(ind)] <- 0

write.csv(ind, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/industrial_sprawl_esw2018.csv", row.names = F)
write.csv(ind, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/industrial_sprawl_esw2018.csv", row.names = F) #to git 


#-------------#

#fisheries pressure by gear type
#use: offshore_all_fish, inshore_all_fish

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
options(scipen=999)

#library(raster)
#library(rgeos)
library(tidyverse)
#library(rgdal)


###

#t <- read.csv("source/offshore_all_fish.csv")
t <- read.csv("source/offshore_all_fish_12nm.csv")
#trawls
t <- t %>% 
  filter(Gear == "trawlers") %>% 
  select(rgn_id = Region, year = Year, pressure = Intensity) %>% 
  arrange(rgn_id, year) %>% 
  group_by(rgn_id) %>% 
  summarise(pressure2 = mean(pressure, na.rm = T)) %>% 
  mutate(pressure_score = pressure2 / max(pressure2, na.rm = T)) %>% 
  mutate(year = 2018) %>% 
  select(rgn_id, year, pressure_score)
  
write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/fis_offshore_trawl_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fis_offshore_trawl_esw2018.csv", row.names = F) #to git 

###

#t <- read.csv("source/offshore_all_fish.csv")
t <- read.csv("source/offshore_all_fish_12nm.csv")
#fixed gear
t <- t %>% 
  filter(Gear == "fixed_gear") %>% 
  select(rgn_id = Region, year = Year, pressure = Intensity) %>% 
  arrange(rgn_id, year) %>% 
  group_by(rgn_id) %>% 
  summarise(pressure2 = mean(pressure, na.rm = T)) %>% 
  mutate(pressure_score = pressure2 / max(pressure2, na.rm = T)) %>% 
  mutate(year = 2018) %>% 
  select(rgn_id, year, pressure_score)

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/fis_offshore_fixed_gear_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fis_offshore_fixed_gear_esw2018.csv", row.names = F) #to git 

###

#t <- read.csv("source/offshore_all_fish.csv")
t <- read.csv("source/offshore_all_fish_12nm.csv")
#drifting longlines
t <- t %>% 
  filter(Gear == "drifting_longlines") %>% 
  select(rgn_id = Region, year = Year, pressure = Intensity) %>% 
  arrange(rgn_id, year) %>% 
  group_by(rgn_id) %>% 
  summarise(pressure2 = mean(pressure, na.rm = T)) %>% 
  mutate(pressure_score = pressure2 / max(pressure2, na.rm = T)) %>% 
  mutate(year = 2018) %>% 
  select(rgn_id, year, pressure_score)

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/fis_offshore_longlines_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fis_offshore_longlines_esw2018.csv", row.names = F) #to git 

###

#t <- read.csv("source/inshore_all_fish.csv")
t <- read.csv("source/inshore_all_fish_12nm.csv")
#dredges
t <- t %>% 
  filter(Gear == "DREDGING") %>% 
  select(rgn_id = Region, pressure = Intensity) %>% 
  arrange(rgn_id) %>% 
  mutate(pressure_score = pressure / max(pressure, na.rm = T)) %>% 
  mutate(year = 2018) %>% 
  select(rgn_id, year, pressure_score)

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/fis_inshore_dredging_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fis_inshore_dredging_esw2018.csv", row.names = F) #to git 

###

#t <- read.csv("source/inshore_all_fish.csv")
t <- read.csv("source/inshore_all_fish_12nm.csv")
#lining
t <- t %>% 
  filter(Gear == "LINING") %>% 
  select(rgn_id = Region, pressure = Intensity) %>% 
  arrange(rgn_id) %>% 
  mutate(pressure_score = pressure / max(pressure, na.rm = T)) %>% 
  mutate(year = 2018) %>% 
  select(rgn_id, year, pressure_score)

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/fis_inshore_lining_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fis_inshore_lining_esw2018.csv", row.names = F) #to git 

###

#t <- read.csv("source/inshore_all_fish.csv")
t <- read.csv("source/inshore_all_fish_12nm.csv")
#netting
t <- t %>% 
  filter(Gear == "NETTING") %>% 
  select(rgn_id = Region, pressure = Intensity) %>% 
  arrange(rgn_id) %>% 
  mutate(pressure_score = pressure / max(pressure, na.rm = T)) %>% 
  mutate(year = 2018) %>% 
  select(rgn_id, year, pressure_score)

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/fis_inshore_netting_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fis_inshore_netting_esw2018.csv", row.names = F) #to git

###

#t <- read.csv("source/inshore_all_fish.csv")
t <- read.csv("source/inshore_all_fish_12nm.csv")
#potting
t <- t %>% 
  filter(Gear == "POTTING") %>% 
  select(rgn_id = Region, pressure = Intensity) %>% 
  arrange(rgn_id) %>% 
  mutate(pressure_score = pressure / max(pressure, na.rm = T)) %>% 
  mutate(year = 2018) %>% 
  select(rgn_id, year, pressure_score)

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/fis_inshore_potting_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fis_inshore_potting_esw2018.csv", row.names = F) #to git 

###

#t <- read.csv("source/inshore_all_fish.csv")
t <- read.csv("source/inshore_all_fish_12nm.csv")
#trawling
t <- t %>% 
  filter(Gear == "TRAWLING") %>% 
  select(rgn_id = Region, pressure = Intensity) %>% 
  arrange(rgn_id) %>% 
  mutate(pressure_score = pressure / max(pressure, na.rm = T)) %>% 
  mutate(year = 2018) %>% 
  select(rgn_id, year, pressure_score)

write.csv(t, "C:/Folders/Dropbox/Projects/OHI/Data/Pressures/prep/fis_inshore_trawling_esw2018.csv", row.names = F)
write.csv(t, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fis_inshore_trawling_esw2018.csv", row.names = F) #to git 


#-------------#


rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
options(scipen=999)

#library(raster)
#library(rgeos)
library(tidyverse)
#library(rgdal)

#Subtidal soft bottom habitat destruction
#softbottom trawl intensity (inter-regional rescale)
#sb <- read.csv("source/offshore_hab_fish.csv")
sb <- read.csv("source/offshore_hab_fish_12nm.csv")

t1 <- sb %>%
  filter(Gear == "trawlers") %>% 
  filter(Hab_Type == "soft_bottom") %>% 
  mutate(habitat = "cms") %>% 
  dplyr::select(rgn_id = Region, habitat, year = Year, trwl_int = Intensity) %>% 
  arrange(rgn_id, year) %>% 
  group_by(rgn_id) %>% 
  summarise(mean_trwl_int = mean(trwl_int, na.rm = T)) %>% 
  mutate(year = 2018) %>% 
  mutate(pressure_score = mean_trwl_int / max(mean_trwl_int, na.rm = T)) %>% 
  select(rgn_id, year, pressure_score)

write.csv(t1, "prep/hd_subtidal_sb_esw2018.csv", row.names = F)
write.csv(t1, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hd_subtidal_sb_esw2018.csv", row.names = F) #to git

###

#Subtidal hard bottom habitat destruction
#hardbottom trawl intensity (inter-regional rescale)
#hb <- read.csv("source/offshore_hab_fish.csv")
hb <- read.csv("source/offshore_hab_fish_12nm.csv")

t1 <- hb %>%
  filter(Gear == "trawlers") %>% 
  filter(Hab_Type == "rocky_reef") %>% 
  mutate(habitat = "rr") %>% 
  dplyr::select(rgn_id = Region, habitat, year = Year, trwl_int = Intensity) %>% 
  arrange(rgn_id, year) %>% 
  group_by(rgn_id) %>% 
  summarise(mean_trwl_int = mean(trwl_int, na.rm = T)) %>% 
  mutate(year = 2018) %>% 
  mutate(pressure_score = mean_trwl_int / max(mean_trwl_int, na.rm = T)) %>% 
  select(rgn_id, year, pressure_score)

#no data for regions 4 and 5
rgn_id <- as.integer(c(4, 5))
year <- as.integer(c(2018, 2018))
pressure_score <- as.integer(c(0, 0))

rgn45 <- data.frame(cbind(rgn_id, year, pressure_score))

t1 <- rbind(t1, rgn45)
t1 <- t1 %>% 
  arrange(rgn_id)

write.csv(t1, "prep/hd_subtidal_hb_esw2018.csv", row.names = F)
write.csv(t1, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hd_subtidal_hb_esw2018.csv", row.names = F) #to git


#-------------#

#library(raster)
#library(rgeos)
library(tidyverse)
library(rgdal)

#water framework directive

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#read in water body site within 5km of coast
#extract data frame
#read in failure data
#join to water body site data

setwd("C:/Folders/Dropbox/Projects/OHI/Data/Pressures")
shp <- readOGR(dsn = "source/water_framework_directive", layer = "WaterBody_5km_int_BNG")
plot(shp)

t <- shp@data
#names(t)

t1 <- t %>% 
  select(rgn_id, River_Basi, Operationa, water_body, ngr) %>% 
  arrange(rgn_id)
  
d <- read.csv("source/water_framework_directive/reason-for-failure.csv")
d1 <- d %>% 
  select(ngr, year = Year, reason = Activity) %>% 
  left_join(t1, by = "ngr")

d1 <- d1[complete.cases(d1), ]
d1$rgn_id <- as.numeric(as.character(d1$rgn_id))

#from here count desn't work!!!
d2 <- d1 %>% 
  arrange(rgn_id, year) %>% 
  select(rgn_id) %>% 
  mutate(index = 1) %>% 
  group_by(rgn_id) %>% 
  summarise(fails = sum(index))
  
#IOS zero fails
rgn_id <- as.integer(4)
fails <- as.integer(0)

rgn4 <- data.frame(cbind(rgn_id, fails))

d2 <- rbind(d2, rgn4)

d3 <- d2 %>% 
  arrange(rgn_id) %>% 
  mutate(year = 2018) %>% 
  mutate(pressure_score = fails / max(fails, na.rm = T)) %>% 
  select(rgn_id, year, pressure_score)

write.csv(d3, "prep/po_wfdir_esw2018.csv", row.names = F)
write.csv(d3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/po_wfdir_esw2018.csv", row.names = F) #to git

#-------------#













