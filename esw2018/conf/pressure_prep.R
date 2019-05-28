
#-------------#

#resilience

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Resilience")
options(scipen=999)

#library(raster)
library(tidyverse)
library(rgdal)
#library(tmaptools)

#protected areas within EEZ and 3nm
#calculate % of total by region
#write as .csv

#read in 3nm protected areas 
shp1 <- readOGR(dsn = "source", layer = "protected_areas_SW_dissolve_3nm_BNG")
t1 <- shp1@data
t1$rgn_id <- as.numeric(as.character(t1$rgn_id))
shp2 <- readOGR(dsn = "source", layer = "ESW_regions_3nm_buf_20190122_BNG")
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

#read in EEZ protected areas 
shp1 <- readOGR(dsn = "source", layer = "protected_areas_SW_dissolve_EEZ_BNG")
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
shp <- readOGR(dsn = "extent", layer = "ESW_regions_20190122_BNG")
#reproject to wgs84
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
shp <- readOGR(dsn = "extent", layer = "ESW_regions_20190122_BNG")
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
shp <- readOGR(dsn = "extent", layer = "ESW_regions_20190122_BNG")
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
shp <- readOGR(dsn = "extent", layer = "ESW_regions_20190122_BNG")
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
shp <- readOGR(dsn = "extent", layer = "ESW_regions_20190122_BNG")
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

#extract a mean value per region id
#d <- raster::extract(r1, shp, fun = mean, na.rm = T, sp = T) #eez
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




