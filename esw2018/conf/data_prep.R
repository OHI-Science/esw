

#-------------#


rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#FP
#FIS

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/FP/FIS")
options(scipen=999)

#catch data by major FAO fishing area, sub area and division that encompass south west region: ie 27.7e, 27.7h, 27.7j2, 27.7g, 27.7f
#data source: http://www.ices.dk/marine-data/dataset-collections/Pages/Fish-catch-and-stock-assessment.aspx
#taxon key from: http://www.fao.org/fishery/collection/asfis/en

#source data edited in excel - columns appended for: scientific_name, stock_id and taxon_penalty (from taxon key data sourced above)
#see ICESCatchDataset2006-2016_processed_stock_id.xlsx
######

library(tidyverse)

#fisheries catch data (PREP)
fc <- read.csv("source/ICES_OfficialNominalCatches_2006_2016/ICESCatchDataset2006-2016_processed_stock_id.csv")
#check for duplications and tidy data set
nrow(fc)
nrow(distinct(fc))
fc[duplicated(fc),]
fc <- distinct(fc)

rp <- read.csv("source/FAO_areas_rgn_ids_props.csv")
rp <- dplyr::select(rp, c(rgn_id, FAO_code, rgn_prop))
rp <- rp[order(rp$FAO_code,rp$rgn_id),]

#reshape data: gather year columns
fc2 <- gather(fc, key = "year", value = "catch", 8:18)

#remove 'scientific_name' and 'units', rename Area to FAO_code
fc2 <- dplyr::select(fc2, c(-scientific_name, -Units))
fc2$year <- gsub("X", "", fc2$year)
colnames(fc2)[colnames(fc2) == "Area"] <- "FAO_code"

#sum(is.na(fc2$catch)) #check for nas in catch data

#group catch data by species, stock_id, taxon_penalty_code, FAO_code, year (sum all country data together)
fc3 <- fc2 %>%
  dplyr::group_by(Species, stock_id, taxon_penalty_code, FAO_code, year) %>%
  dplyr::summarise(SumCatch = sum(catch)) %>%
  ungroup()

#check catch sums
sum(fc2$catch)
sum(fc3$SumCatch)
#select data for sw regions only

#select by FAO code for SW regions append rgn_id and prop value
#NOTE - got to be a better way to code this !!
F277e_3 <- filter(fc3,FAO_code == "27.7.e")
F277e_3 ["rgn_id"] <- 3
F277e_3 ["rgn_prop"] <- 0.21956318

#head(F277e_3)

F277e_4 <- filter(fc3,FAO_code == "27.7.e")
F277e_4 ["rgn_id"] <- 4
F277e_4 ["rgn_prop"] <- 0.03020809

F277e_5 <- filter(fc3,FAO_code == "27.7.e")
F277e_5 ["rgn_id"] <- 5
F277e_5 ["rgn_prop"] <- 0.10981616

F277e_6 <- filter(fc3,FAO_code == "27.7.e")
F277e_6 ["rgn_id"] <- 6
F277e_6 ["rgn_prop"] <- 0.04589938

F277e <- rbind(F277e_3, F277e_4, F277e_5, F277e_6)


F277f_1 <- filter(fc3,FAO_code == "27.7.f")
F277f_1 ["rgn_id"] <- 1
F277f_1 ["rgn_prop"] <- 0.04512193

F277f_2 <- filter(fc3,FAO_code == "27.7.f")
F277f_2 ["rgn_id"] <- 2
F277f_2 ["rgn_prop"] <- 0.15907867

F277f_3 <- filter(fc3,FAO_code == "27.7.f")
F277f_3 ["rgn_id"] <- 3
F277f_3 ["rgn_prop"] <- 0.5741751

F277f_4 <- filter(fc3,FAO_code == "27.7.f")
F277f_4 ["rgn_id"] <- 4
F277f_4 ["rgn_prop"] <- 0.04120366

F277f <- rbind(F277f_1, F277f_2, F277f_3, F277f_4)


F277g_2 <- filter(fc3,FAO_code == "27.7.g")
F277g_2 ["rgn_id"] <- 2
F277g_2 ["rgn_prop"] <- 0.01802273

F277g_3 <- filter(fc3,FAO_code == "27.7.g")
F277g_3 ["rgn_id"] <- 3
F277g_3 ["rgn_prop"] <- 0.20973236

F277g <- rbind(F277g_2, F277g_3)


F277h <- filter(fc3,FAO_code == "27.7.h")
F277h ["rgn_id"] <- 3
F277h ["rgn_prop"] <- 0.50071751


F277j <- filter(fc3,FAO_code == "27.7.j.2")
F277j ["rgn_id"] <- 3
F277j ["rgn_prop"] <- 0.06458867


F278d <- filter(fc3,FAO_code == "27.8.d.2")
F278d ["rgn_id"] <- 3
F278d ["rgn_prop"] <- 0.01435861


fc4 <- rbind(F277e, F277f, F277g, F277h, F277j, F278d)

#check for duplication
nrow(fc4)
nrow(distinct(fc4))
fc4[duplicated(fc4),]

#remove entries with zero catch
fc4 <- fc4[fc4$SumCatch > 0,]

#head(fc4)
fc4["rgn_catch_T"] <- fc4$SumCatch*fc4$rgn_prop

fc5 <- dplyr::select(fc4, c(rgn_id, stock_id, year, taxon_penalty_code, rgn_catch_T))
fc6 <- fc5[order(fc5$rgn_id,fc5$year,fc5$stock_id),]

fc7 <- fc6 %>% 
  group_by(stock_id, year) %>% 
  mutate(mean_catch = mean(rgn_catch_T)) %>% 
  ungroup()

nrow(fc7)
nrow(distinct(fc7))
sum(duplicated(fc7) == TRUE)

write.csv(fc7, "prep/fis_ICESCatchDataset2006_2016_mean_catch_esw2018.csv", row.names = F) #to prep
write.csv(fc7, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fis_ICESCatchDataset2006_2016_mean_catch_esw2018.csv", row.names = F) #to git


#-------------#


rm(list=ls())
cat("\014")

#FP
#MAR

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/FP/MAR")

library(rgdal)
library(tidyverse)

#read in harvest totals for sw
#these data are replicated by region id
total_harvest <- read.csv("source/mar_harvest_total_tonnes_SW.csv")

#unique(total_harvest$species)
#[1] Mussels      Cockles      Tapes spp    M mercenaria P oysters    turbot

#read in site metadata: production area (km2) and site water classification
shp <- readOGR(dsn = "source/shapefiles", layer = "Recordset_9761Polygon_SW_BNG")
site_data <- shp@data
colnames(site_data)[colnames(site_data) == "SPECIES"] <- "species"
colnames(site_data)[colnames(site_data) == "CLASSIFICA"] <- "water_class"

##--------##
#calculate proportion of harvest by species and region
sites_area <- dplyr::select(site_data, "species", "rgn_id", "area_km2")

#group by rgn_id and species (area by species and region)
#group by species - sum area
#calculate regional proportions
rgn_prop <- sites_area %>%
  group_by(rgn_id, species) %>%
  summarise(sum_area_by_rgn = sum(area_km2)) %>%
  group_by(species) %>%
  mutate(sum_area_all = sum(sum_area_by_rgn)) %>%
  mutate(rgn_prop_prod = sum_area_by_rgn / sum_area_all) %>%
  ungroup()

rp <- dplyr::select(rgn_prop, "rgn_id", "species", "rgn_prop_prod")

#format species
rp$species <- as.character(rp$species)
total_harvest$species <- as.character(total_harvest$species)

#unique(rp$species)
#unique(total_harvest$species)

#subset regional prop data (rp)
rp2 <- rp[rp$species == "Mussels" | rp$species == "Cockles"| rp$species == "Tapes spp" |
            rp$species == "M mercenaria" | rp$species == "P oysters",]

#subset harvest data
total_harvest <- total_harvest[total_harvest$species == "Mussels" | total_harvest$species == "Cockles"| total_harvest$species == "Tapes spp" |
                                 total_harvest$species == "M mercenaria" | total_harvest$species == "P oysters",]

#join harvest and regional proportion data
#note: only harvest data with a corresponding regional proportion will be kept
reg_harvest <- merge(total_harvest, rp2, by=c("rgn_id","species"))
reg_harvest <- reg_harvest[order(reg_harvest$rgn_id, reg_harvest$species, reg_harvest$year),]
#calculate regional production
reg_harvest$rgn_prod_T <- reg_harvest$tonnes * reg_harvest$rgn_prop_prod

#names(rp3)
rp4 <- dplyr::select(reg_harvest, c(rgn_id, taxa_code, year, rgn_prod_T))

write.csv(rp4, "prep/mar_harvest_tonnes_esw2018.csv", row.names = F) #to prep
write.csv(rp4, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/mar_harvest_tonnes_esw2018.csv", row.names = F) #to git

##--------##

#calculate water clasifications per site for local sustainability score

water_q <- dplyr::select(site_data, "species", "rgn_id", "water_class")

#names(water_q)
#look up table for water quality/status score
wq_category <- data.frame(water_class = c('1', '2', '5', '6', '3', '4', '7'),
                          wq_status_score = c(1,  0.75,  0.75,  NA, 0.5, 0.25, NA))
#join status score to table
water_q <- water_q %>% left_join(wq_category, by = 'water_class')

#remove NAs
water_q <- na.omit(water_q)

wm <- water_q %>%
  group_by(rgn_id, species) %>%
  summarise(wq_rg_sp = mean(wq_status_score))
wm["year"] <- 2017

wm2 <- select(wm, "rgn_id", "year",  "species", "wq_rg_sp")
sp_taxa_code <- unique(total_harvest[,c("taxa_code", "species")])
wm2 <- left_join(sp_taxa_code, wm2, "species")

write.csv(wm2, "prep/mar_sustainability_score_esw2018.csv", row.names = F) #to prep
write.csv(wm2, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/mar_sustainability_score_esw2018.csv", row.names = F) #to git


#-------------#


rm(list=ls())
cat("\014")

#AO

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/AO")
options(scipen=999)

library(tidyverse)
library(rgdal)

######

access <- read.csv("source/Access.csv")
access <- mutate(access, year = "2018") %>%
  select(rgn_id = Region, year, access_perc = Access_percentage)
write.csv(access, "prep/ao_access_esw2018.csv", row.names = F)
write.csv(access, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/ao_access_esw2018.csv", row.names = F) #to git

######

boats <- read.csv("source/boats_by_year.csv")
boats <- select(boats, rgn_id, year, boats)
write.csv(boats, "prep/ao_fleet_size_esw2018.csv", row.names = F)
write.csv(boats, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/ao_fleet_size_esw2018.csv", row.names = F) #to git

######

red <- read.csv("source/fuel_cost_year.csv")
write.csv(red, "prep/ao_fuel_cost_esw2018.csv", row.names = F)
write.csv(red, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/ao_fuel_cost_esw2018.csv", row.names = F) #to git

######

effort <- read.csv("source/effort.csv")
effort <- select(effort, rgn_id, year, effort_tph)
effort$effort_tph <- as.numeric(as.character(effort$effort_tph))
#replace NAs with zeros
#NOT A GOOD IDEA!! - NEED PROXY
#effort[is.na(effort)] <- 0

write.csv(effort, "prep/ao_effort_catch_esw2018.csv", row.names = F)
write.csv(effort, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/ao_effort_catch_esw2018.csv", row.names = F) #to git

#-------------#


rm(list=ls())
cat("\014")

#CS and CP and BD:HAB

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB")
options(scipen=999)

library(tidyverse)
library(rgdal)

#######

#saltmash extent
sm_ex <- read.csv("source/hab_littoral_extent.csv")

sm_ex <- mutate(sm_ex, year = "2017") %>%
  mutate(habitat = "saltmarsh") %>%
  select(rgn_id, habitat, year, km2 = area_km2)

write.csv(sm_ex, "prep/hab_saltmarsh_extent_esw2018.csv", row.names = F)
write.csv(sm_ex, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_saltmarsh_extent_esw2018.csv", row.names = F) #to git

#######

#saltmash condition
sm_cnd <- read.csv("source/hab_littoral_condition.csv")

#for calculating status
#look up table for condition score
#sm_condition <- data.frame(condition = c('FAVOURABLE', 'UNFAVOURABLE RECOVERING', 'UNFAVOURABLE NO CHANGE', 'UNFAVOURABLE DECLINING'),
#sm_cond = c(1,  0.75,  0.5,  0.25))

sm_condition <- data.frame(condition = c('FAVOURABLE', 'UNFAVOURABLE RECOVERING', 'UNFAVOURABLE NO CHANGE', 'UNFAVOURABLE DECLINING'),
                           sm_cond = c(1,  0,  0,  0))
sm_cnd <- sm_cnd %>%
  left_join(sm_condition, by = 'condition')

#----------#
#relative change - calculuated - but not used in ohi - current condition used - see below
#head(sm_cnd)
sm_cnd_2017 <- filter(sm_cnd, year == max(year)) %>%
  select(rgn_id, year, sm_cond) %>%
  group_by(rgn_id, year) %>%
  summarise(health_2017 = mean(sm_cond, na.rm = T))

sm_cnd_2003 <- filter(sm_cnd, year == 2003) %>%
  select(rgn_id, year, sm_cond) %>%
  group_by(rgn_id, year) %>%
  summarise(health_2003 = mean(sm_cond, na.rm = T))

sm_cnd2 <- left_join(sm_cnd_2017, sm_cnd_2003, by = c("rgn_id")) %>%
  mutate(prop_cnd_chng = pmin(1, health_2017 / health_2003) ) %>%
  select(rgn_id, year = year.x, health = prop_cnd_chng) %>%
  mutate(habitat = "saltmarsh")
#----------#

write.csv(sm_cnd2, "prep/hab_saltmarsh_health_prop_chng_esw2018.csv", row.names = F) 
write.csv(sm_cnd2, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_saltmarsh_health_prop_chng_esw2018.csv", row.names = F) #to git

#for calcuating trend and current year status
sm_cnd <- read.csv("source/hab_littoral_condition.csv")

#sm_condition <- data.frame(condition = c('FAVOURABLE', 'UNFAVOURABLE RECOVERING', 'UNFAVOURABLE NO CHANGE', 'UNFAVOURABLE DECLINING'),
#sm_cond = c(1,  0.75,  0.5,  0.25))

sm_condition <- data.frame(condition = c('FAVOURABLE', 'UNFAVOURABLE RECOVERING', 'UNFAVOURABLE NO CHANGE', 'UNFAVOURABLE DECLINING'),
                           sm_cond = c(1,  0,  0,  0))
sm_cnd <- sm_cnd %>%
  left_join(sm_condition, by = 'condition') %>%
  select(rgn_id, year, sm_cond) %>%
  group_by(rgn_id, year) %>%
  summarise(health = mean(sm_cond, na.rm = T)) %>%
  mutate(habitat = "saltmarsh")

write.csv(sm_cnd, "prep/hab_saltmarsh_health_esw2018.csv", row.names = F)
write.csv(sm_cnd, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_saltmarsh_health_esw2018.csv", row.names = F) #to git

#######

#sand dune extent
d_ex <- read.csv("source/hab_dune_extent.csv")

d_ex <- mutate(d_ex, year = "2017") %>%
  mutate(habitat = "sand dune") %>%
  select(rgn_id, habitat, year, km2 = area_km2)

write.csv(d_ex, "prep/hab_sand_dune_extent_esw2018.csv", row.names = F)
write.csv(d_ex, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_sand_dune_extent_esw2018.csv", row.names = F) #to git

#######

#sand dune condition
d_cnd <- read.csv("source/hab_dune_condition.csv")

#for calculating status
#look up table for condition score
#d_condition <- data.frame(condition = c('FAVOURABLE', 'UNFAVOURABLE RECOVERING', 'UNFAVOURABLE NO CHANGE', 'UNFAVOURABLE DECLINING', "DESTROYED", "Not assessed"),
#d_cond = c(1,  0.75,  0.5,  0.25, 0, NA))

d_condition <- data.frame(condition = c('FAVOURABLE', 'UNFAVOURABLE RECOVERING', 'UNFAVOURABLE NO CHANGE', 'UNFAVOURABLE DECLINING', "DESTROYED", "Not assessed"),
                          d_cond = c(1,  0,  0,  0, 0, NA))
d_cnd <- d_cnd %>%
  left_join(d_condition, by = 'condition')

#----------#
#relative change - calculuated - but not used in ohi - current condition used - see below
d_cnd_2017 <- filter(d_cnd, year == max(year)) %>%
  select(rgn_id, year, d_cond) %>%
  group_by(rgn_id, year) %>%
  summarise(health_2017 = mean(d_cond, na.rm = T))

d_cnd_2007 <- filter(d_cnd, year == 2007) %>%
  select(rgn_id, year, d_cond) %>%
  group_by(rgn_id, year) %>%
  summarise(health_2007 = mean(d_cond, na.rm = T))

d_cnd2 <- left_join(d_cnd_2017, d_cnd_2007, by = c("rgn_id")) %>%
  mutate(prop_cnd_chng = pmin(1, health_2017 / health_2007) ) %>%
  select(rgn_id, year = year.x, health = prop_cnd_chng) %>%
  mutate(habitat = "sand dune")
#----------#

write.csv(d_cnd2, "prep/hab_sand_dune_health_prop_chng_esw2018.csv", row.names = F) 
write.csv(d_cnd2, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_sand_dune_health_prop_chng_esw2018.csv", row.names = F) #to git

#for calcuating trend and current year status
d_cnd <- read.csv("source/hab_dune_condition.csv") # this now includes data for 2012

#look up table for condition score
#d_condition <- data.frame(condition = c('FAVOURABLE', 'UNFAVOURABLE RECOVERING', 'UNFAVOURABLE NO CHANGE', 'UNFAVOURABLE DECLINING', "DESTROYED", "Not assessed"),
#d_cond = c(1,  0.75,  0.5,  0.25, 0, NA))

d_condition <- data.frame(condition = c('FAVOURABLE', 'UNFAVOURABLE RECOVERING', 'UNFAVOURABLE NO CHANGE', 'UNFAVOURABLE DECLINING', "DESTROYED", "Not assessed"),
                          d_cond = c(1,  0,  0,  0, 0, NA))
d_cnd <- d_cnd %>%
  left_join(d_condition, by = 'condition') %>%
  select(rgn_id, year, d_cond) %>%
  group_by(rgn_id, year) %>%
  summarise(health = mean(d_cond, na.rm = T))%>%
  mutate(habitat = "sand dune")

write.csv(d_cnd, "prep/hab_sand_dune_health_esw2018.csv", row.names = F)
write.csv(d_cnd, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_sand_dune_health_esw2018.csv", row.names = F) #to git

#######

#seagrass extent
sg_ex <- read.csv("source/seagrass_extent.csv")

sg_ex <- mutate(sg_ex, year = "2017") %>%
  mutate(habitat = "seagrass") %>%
  select(rgn_id, habitat, year, km2 = area_km2)

write.csv(sg_ex, "prep/hab_seagrass_extent_esw2018.csv", row.names = F)
write.csv(sg_ex, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_seagrass_extent_esw2018.csv", row.names = F) #to git

#######

#seagrass condition
sg_cnd <- read.csv("source/seagrass_condition.csv")

sg_cnd <- mutate(sg_cnd, year = "2018") %>%
  mutate(habitat = "seagrass") %>%
  select(rgn_id, year, health = condition, habitat)
sg_cnd$health <- as.numeric(as.character(sg_cnd$health))
#replace NAs with zeros
sg_cnd[is.na(sg_cnd)] <- 0
write.csv(sg_cnd, "prep/hab_seagrass_health_esw2018.csv", row.names = F)
write.csv(sg_cnd, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_seagrass_health_esw2018.csv", row.names = F) #to git

#######

#seagrass trend (from literature)
sg_trnd <- read.csv("source/seagrass_trend.csv")

sg_trnd <- mutate(sg_trnd, year = "2017") %>%
  mutate(habitat = "seagrass") %>%
  select(rgn_id, habitat, year, trend)

write.csv(sg_trnd, "prep/hab_seagrass_trend_esw2018.csv", row.names = F)
write.csv(sg_trnd, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_seagrass_trend_esw2018.csv", row.names = F) #to git

#######

#softbottom

#process rasters
#raster are now trawl intensity on soft bttom

setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB")
options(scipen=999)

library(rgdal)
library(raster)

#######

#softbottom extent
sb <- read.csv("source/hab_trawl.csv")
sb_extent <- sb %>%
  group_by(rgn_id = Region, year = Year) %>%
  summarise(km2 = sum(Habitat_Area)) %>%
  mutate(habitat = "ccms") %>%
  select(rgn_id, habitat, year, km2)

write.csv(sb_extent, "prep/hab_ccms_extent_esw2018.csv", row.names = F)
write.csv(sb_extent, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_ccms_extent_esw2018.csv", row.names = F) #to git

#######

#read in region poly (all)
shp <- readOGR(dsn = "source/shapefiles", layer = "ESW_regions_20190122_BNG")
#reproject to wgs84
proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
shp <- spTransform(shp, proj)
#plot(shp)

r_list <- list.files("source/shapefiles/soft_bottom", full.names = T, pattern = "tif$")

z <- NULL

for (i in 1:length(r_list)){
  r <- raster(r_list[i])
  #extract a mean value per region id
  d <- raster::extract(r, shp, fun = mean, na.rm = T, sp = T)
  t <- d@data
  t <- t[order(t$rgn_id),]
  t$year <- substr(r_list[i], 40, 43)
  colnames(t) <- c("rgn_id", "area_km2", "trwl_int", "year")
  t <- t[,c("year", "rgn_id", "trwl_int")]
  z <- rbind(z, t)
}

t1 <- z

#rescale data 0-1 using 99th quantile of all data
#this is OK here as only 5 years of data
#if any more data become available move to functions code
q <- quantile(t1$trwl_int,0.99, na.rm = T)
fun = function(x){ifelse(x>q, 1, x/q)}
t1$trwl_int_rs <- as.numeric(lapply(t1$trwl_int,fun))
t1$trwl_int_rs_inv <- 1 - t1$trwl_int_rs
t1$habitat <- "ccms"

sb_trwl_int <- t1 %>%
  select(rgn_id, habitat, year, health = trwl_int_rs_inv)

write.csv(sb_trwl_int, "prep/hab_ccms_trawl_int_esw2018.csv", row.names = F)
write.csv(sb_trwl_int, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_ccms_trawl_int_esw2018.csv", row.names = F) #to git


#-------------#


rm(list=ls())
cat("\014")

#TR

#captures the 'value' that people have from experiencing and enjoying coastal areas. 
#A score of 100 means a region utilizes its full recreational potential

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/TR")
options(scipen=999)

library(tidyverse)
library(rgdal)

###############
###############

#tourism

#data source: 
#domestic tourism - https://www.visitbritain.org/archive-great-britain-tourism-survey-overnight-data 2004-2016
#international tourism - https://www.visitbritain.org/nation-region-county-data 2004-2016
#But only 2010-2014 used due to dicrepances in data and change in survey protocol

tour <- read.csv("source/tourism_allovernight_nights_thousands.csv") 

shp1 <- readOGR(dsn = "source/shapefiles", layer = "gb_local_auhorities_SW_coastal_BNG") #coastal local authorities
t1 <- shp1@data
#plot(shp1)
t1 <- select(t1, rgn_id, LA)
t1$rgn_id <- as.numeric(t1$rgn_id)
#select tourism data for coastal local authorities
t1 <- left_join(t1, tour, by = "LA")

#coastal area within 1km buffer by region
shp2 <- readOGR(dsn = "source/shapefiles", layer = "ESW_regions_1km_intbuf_20190122_BNG")
rgn_coast_km <- shp2@data
rgn_coast_km  <- rgn_coast_km[order(rgn_coast_km$rgn_id),]
rgn_coast_km$rgn_id <- as.numeric(rgn_coast_km$rgn_id)

#reshape data: gather year columns
t2 <- gather(t1, key = "year", value = "o_night_k", 3:7)
t2$year <- gsub("X", "", t2$year)

#sum overnight stays by region
t3 <- t2 %>%
  group_by(rgn_id, year) %>%
  summarise(o_night_k = sum(o_night_k))

#read in accommodation data (hotel, guest houses campsites & hostels) by coastal LA and by 1km internal coastal buffer
#source: https://overpass-turbo.eu/
ac_all <- read.csv("source/accommodation_SW_all_rgns.csv") 
ac_cst <- read.csv("source/accommodation_SW_1km.csv") # 46% of accommodation occurs within 1km of the coast

ac_cst <- ac_cst %>%
  group_by(rgn_id) %>%
  summarise(ac_cst = sum(index))

ac_all <- ac_all %>%
  group_by(rgn_id) %>%
  summarise(ac_all = sum(index))

ac <- left_join(ac_cst, ac_all, by = "rgn_id")
ac$prop_cst <- ac$ac_cst / ac$ac_all

#apportion overnight stays by coastal accom (1km coastal buffer) to all accom by coastal LAs
#correct for coastal area (1km)
#write to prep folder
t3 <- left_join(t3, ac, by = "rgn_id")
t3$prop_o_night <- t3$o_night_k * t3$prop_cst
t3 <- left_join(t3, rgn_coast_km, by = "rgn_id")
t3$ons <- t3$prop_o_night / t3$area_km2
t3$year <- as.integer(t3$year)
t3 <- select(t3, c("rgn_id", "year", "ons"))

write.csv(t3, "prep/tr_ons_1km_esw2018.csv", row.names = F)
write.csv(t3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/tr_ons_1km_esw2018.csv", row.names = F) #to git

######

#read in viewshed data add year - write to prep folder
#source: https://environment.data.gov.uk/DefraDataDownload/?mapService=MMO/LandWithSeaViews&Mode=spatial
sv <- read.csv("source/seaview.csv")
sv <- sv %>%
  select(c("rgn_id", perc_sv = "Percentage")) %>%
  mutate(year = 2018)

write.csv(sv, "prep/tr_viewshed_esw2018.csv", row.names = F)
write.csv(sv, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/tr_viewshed_esw2018.csv", row.names = F) #to git

#####

#not used in OHI calculation
#read in yearly TTCI scores for UK
#http://www.oceanhealthindex.org/methodology/components/tourism-and-recreation-tci-travel-and-tourism-competitiveness-index
#TTCI: measures the factors and policies that make a country a viable place to invest within the Travel and Tourism sector
TTCI <- read.csv("source/TTCI_scores.csv")

#add region ids
#write to prep folder
z <- NULL
for(i in 1:6 ){
  TTCI2 <- TTCI
  TTCI2$rgn_id <- rep(i, nrow(TTCI2))
  z <- rbind(z, TTCI2)
}

TTCI3 <- z
TTCI3 <- select(TTCI3, c("rgn_id", "year", "TTCI"))
TTCI3 <- TTCI3[order(TTCI3$rgn_id, TTCI3$year),]

write.csv(TTCI3, "prep/tr_ttci_esw2018.csv", row.names = F)
write.csv(TTCI3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/tr_ttci_esw2018.csv", row.names = F) #to git

###############
###############

#recreation

#read in modelled recreation data
#souce: https://environment.data.gov.uk/ds/catalogue/index.jsp#/catalogue
#this will represent status data

rec <- read.csv("source/recreation_all.csv")
write.csv(rec, "prep/tr_rec_esw2018.csv", row.names = F)
write.csv(rec, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/tr_rec_esw2018.csv", row.names = F) #to git

######

#read in tourism and population data to calculate receational trend in functions
#this does not need to be corrected for coastal length
#data source: 
#domestic tourism - https://www.visitbritain.org/archive-great-britain-tourism-survey-overnight-data 2004-2016
#international tourism - https://www.visitbritain.org/nation-region-county-data 2004-2016
#But only 2010-2015 used due to dicrepances in data and change in survey protocol
tour <- read.csv("source/tourism_allovernight_nights_thousands.csv") 

shp1 <- readOGR(dsn = "source/shapefiles", layer = "gb_local_auhorities_SW_coastal_BNG") #coastal local authorities
t1 <- shp1@data
#plot(shp1)

t1 <- select(t1, rgn_id, LA)
t1$rgn_id <- as.numeric(t1$rgn_id)
#select tourism data for coastal local authorities
t1 <- left_join(t1, tour, by = "LA")

#reshape data: gather year columns
t2 <- gather(t1, key = "year", value = "o_night_k", 3:7)
t2$year <- gsub("X", "", t2$year)

#sum overnight stays by region
t3 <- t2 %>%
  group_by(rgn_id, year) %>%
  summarise(o_night_k = sum(o_night_k))

write.csv(t3, "prep/tr_ons_all_LA_esw2018.csv", row.names = F)
write.csv(t3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/tr_ons_all_LA_esw2018.csv", row.names = F) #to git

######

#read in population by LA by year
#data source: -	https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/regionalgvaibylocalauthorityintheuk
#this also includes population data by year

popn <- read.csv("source/population_by_LA.csv")

shp1 <- readOGR(dsn = "source/shapefiles", layer = "gb_local_auhorities_SW_coastal_BNG") #coastal local authorities
t1 <- shp1@data

t1 <- select(t1, rgn_id, LA)
t1$rgn_id <- as.numeric(t1$rgn_id)
#select population data for coastal local authorities
t1 <- left_join(t1, popn, by = "LA")

#reshape data: gather year columns
t2 <- gather(t1, key = "year", value = "pop_n", 3:21)
t2$year <- gsub("X", "", t2$year)

t3 <- t2 %>%
  group_by(rgn_id, year) %>%
  summarise(pop_n = sum(pop_n))

write.csv(t3, "prep/tr_popn_LA_esw2018.csv", row.names = F)
write.csv(t3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/tr_popn_LA_esw2018.csv", row.names = F) #to git


#-------------#


rm(list=ls())
cat("\014")

#LE LIV

#data
#wages_median.csv - median wage per employee - all industries by local authority
#mar_workforce.csv - marine related employment data - n employed

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/LE")
options(scipen=999)

library(tidyverse)
library(rgdal)

#data source: -	www.nomisweb.co.uk
wage <- read.csv("LIV/source/wages_median.csv") #median wage 2002-2018
#head(wage)

#tidy column names
colnames(wage)[1] <- "LA"
col_yrs <- paste("X", seq(2002,2018), sep = "")

#replace 'no data' text characters with NA
wages <- dplyr::select(wage, LA, c(col_yrs))
wages[wages == "#" | wages == "-" | wages == "!"] <- NA

shp1 <- readOGR(dsn = "LIV/source/shapefiles", layer = "gb_local_auhorities_SW_coastal_BNG") #coastal local authorities
t1 <- shp1@data
#plot(shp1)

t1 <- select(t1, rgn_id, LA)
t1$rgn_id <- as.numeric(t1$rgn_id)
#select wage data for coastal local authorities
t1 <- left_join(t1, wages, by = "LA")

#head(t1)
#reshape data: gather year columns
t2 <- gather(t1, key = "year", value = "wages", 3:19)
t2$year <- gsub("X", "", t2$year)

#read in employment data
#data source: -	www.nomisweb.co.uk
marw <- read.csv("ECO/source/mar_workforce.csv") #marine workforce 2010-2018
marw2 <- gather(marw, key = "year", value = "mar_wrkfrc", 3:11)
marw2$year <- gsub("x", "", marw2$year)
marw2 <- select(marw2, LA = area, year, mar_wrkfrc)

t3 <- left_join(t2, marw2, by = c("year", "LA"))
t3 <- t3[!is.na(t3$mar_wrkfrc),]

t4 <- t3[order(t3$LA),]

#fill data gaps where a region has no data

#IOS has no data - assign Cornwall data to IOS as proxy
t4[t4$LA == "Isles of Scilly", "wages"] <- t4[t4$LA == "Cornwall", "wages"]

#West Somerset only has data for 2018 - gap fill 2010-2017 with averaged (mean) data from adjoining LAs (North Devon & Sedgemoor)
gf <- apply(cbind(as.numeric(t4[t4$LA == "North Devon", "wages"]), as.numeric(t4[t4$LA == "Sedgemoor", "wages"])), 1, mean, na.rm = T)
t4[t4$LA == "West Somerset", "wages"] <-
  ifelse(is.na(t4[t4$LA == "West Somerset", "wages"]),  gf, t4[t4$LA == "West Somerset", "wages"])

#Forest of Dean has no data for 2016 - gap fill with data from Tewksbury (NB: but no marine workforce)
t4[t4$LA == "Forest of Dean", "wages"] <-
  ifelse(is.na(t4[t4$LA == "Forest of Dean", "wages"]), t4[t4$LA == "Tewkesbury", "wages"], t4[t4$LA == "Forest of Dean", "wages"])

#Sedgemoor no data for 2014 - gap fill with averaged data from adjoining West Somerset and North somerset
gf <- apply(cbind(as.numeric(t4[t4$LA == "West Someret", "wages"]), as.numeric(t4[t4$LA == "North Somerset", "wages"])), 1, mean, na.rm = T)
t4[t4$LA == "Sedgemoor", "wages"] <-
  ifelse(is.na(t4[t4$LA == "Sedgemoor", "wages"]),  gf, t4[t4$LA == "Sedgemoor", "wages"])

#South Hams no data 2014, 2015, 2016 - gap fill with averaged data from adjoining Plymouth and Torbay
gf <- apply(cbind(as.numeric(t4[t4$LA == "Plymouth", "wages"]), as.numeric(t4[t4$LA == "Torbay", "wages"])), 1, mean, na.rm = T)
t4[t4$LA == "South Hams", "wages"] <-
  ifelse(is.na(t4[t4$LA == "South Hams", "wages"]),  gf, t4[t4$LA == "South Hams", "wages"])

#Tewkesbury has no data for 2018 - gap fill with averaged data from adjoing Forest of Dean and Gloucester (NB: but no marine workforce)
gf <- apply(cbind(as.numeric(t4[t4$LA == "Forest of Dean", "wages"]), as.numeric(t4[t4$LA == "Gloucester", "wages"])), 1, mean, na.rm = T)
t4[t4$LA == "Tewkesbury", "wages"] <-
  ifelse(is.na(t4[t4$LA == "Tewkesbury", "wages"]),  gf, t4[t4$LA == "Tewkesbury", "wages"])

#West Devon has no data for 2018 - gap fill with averaged data from adjoing Cornwall and South Hams (NB: but no marine workforce)
gf <- apply(cbind(as.numeric(t4[t4$LA == "Cornwall", "wages"]), as.numeric(t4[t4$LA == "South Hams", "wages"])), 1, mean, na.rm = T)
t4[t4$LA == "West Devon", "wages"] <-
  ifelse(is.na(t4[t4$LA == "West Devon", "wages"]),  gf, t4[t4$LA == "West Devon", "wages"])

#group by rgn and year
#calculate regional marine wages per capita
rgn_wages <- t4 %>%
  group_by(rgn_id, year) %>%
  summarise(wages_gbp = mean(as.numeric(wages))) %>%
  select(year, rgn_id, wages_gbp)

#marine jobs
#sum marine jobs by region
rgn_jobs <- t4 %>%
  group_by(rgn_id, year) %>%
  summarise(mar_jobs = sum(mar_wrkfrc)) %>%
  select(year, rgn_id, mar_jobs)

write.csv(rgn_wages, "LIV/prep/le_wage_esw2018.csv", row.names = F)
write.csv(rgn_wages, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/le_wage_esw2018.csv", row.names = F) #to git

write.csv(rgn_jobs, "LIV/prep/le_jobs_esw2018.csv", row.names = F)
write.csv(rgn_jobs, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/le_jobs_esw2018.csv", row.names = F) #to git


#-------------#


rm(list=ls())
cat("\014")

#LE ECO

#data
#revenue_GVA_all_ind.csv - revenue data for all industries by local authority

#mar_workforce.csv - marine related employment data - n employed
#all_workforce.csv - workfoce n employed??

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/LE")
options(scipen=999)

library(tidyverse)
library(rgdal)

#data source: -	https://www.ons.gov.uk/economy/grossvalueaddedgva/datasets/regionalgvaibylocalauthorityintheuk
rev <- read.csv("ECO/source/revenue_GVA_all_ind.csv") #GVA 1997-2015
#head(rev)
rev <- select(rev, -Region, -LAU1.code, -SIC07.code, -SIC07.Industry)
colnames(rev)[1] <- "LA"

shp1 <- readOGR(dsn = "ECO/source/shapefiles", layer = "gb_local_auhorities_SW_coastal_BNG") #coastal local authorities
t1 <- shp1@data
#plot(shp1)

t1 <- select(t1, rgn_id, LA)
t1$rgn_id <- as.numeric(t1$rgn_id)
#select revenue data for coastal local authorities
t1 <- left_join(t1, rev, by = "LA")

#reshape data: gather year columns
t2 <- gather(t1, key = "year", value = "rev_gbp_M", 3:21)
t2$year <- gsub("X", "", t2$year)

#read in employment data
#data source: -	www.nomisweb.co.uk
marw <- read.csv("ECO/source/mar_workforce.csv") #marine workforce 2010-2018
marw2 <- gather(marw, key = "year", value = "mar_wrkfrc", 3:11)
marw2$year <- gsub("x", "", marw2$year)
marw2 <- select(marw2, LA = area, year, mar_wrkfrc)

allw <- read.csv("ECO/source/all_workforce.csv") #total workforce 2010-2018
allw2 <- gather(allw, key = "year", value = "all_wrkfrc", 3:11)
allw2$year <- gsub("x", "", allw2$year)
allw2 <- select(allw2, LA = area, year, all_wrkfrc)

t3 <- left_join(t2, marw2, by = c("year", "LA"))
t4 <- left_join(t3, allw2, by = c("year", "LA"))

t4$prop_marw <- t4$mar_wrkfrc / t4$all_wrkfrc
t4$rev_gbp_M_mar <- t4$rev_gbp_M * t4$prop_marw

#remove na data
t4 <- na.omit(t4)

#group by rgn and year
#calculate regional marine GVA (sum LA data)
t5 <- t4 %>%
  group_by(rgn_id, year) %>%
  summarise(rev_gbp_M = sum(rev_gbp_M_mar))

write.csv(t5, "ECO/prep/le_gva_esw2018.csv", row.names = F)
write.csv(t5, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/le_gva_esw2018.csv", row.names = F) #to git


#-------------#

rm(list=ls())
cat("\014")

#SP ICO

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/SP")

library(tidyverse)

#read in ranked_iconic_species_20190129.csv
ico <- read.csv("ICO/source/ranked_iconic_species_20190129.csv")
iucn_class <- read.csv("ICO/source/ICO_SP_IUCN.csv") #no regional differentiation

#remove data without IUCN listing
ico <- ico[ico$IUCN_listing_check != 0 & ico$IUCN_listing_check != "#N/A",]
ico <- dplyr::select(ico, -IUCN_listing_check)
#add new column with sub total for sum of postcards, magnets and tour operators
ico <- mutate(ico, pmt = postcards + magnets + tour_operators)

#format
ico$Species <- as.character(ico$Species)
iucn_class$Species <- as.character(iucn_class$Species)

ico2 <- left_join(iucn_class, ico, by = "Species")

#remove NAs
ico2 <- na.omit(ico2)
#select last 10 years data
ico2 <- dplyr::filter(ico2, Assess_year >= 2000)

z <- NULL
for(i in 1:6 ){
  ico3 <- ico2
  ico3$rgn_id <- rep(i, nrow(ico3))
  z <- rbind(z, ico3)
}

ico4 <- z
ico5 <- dplyr::select(ico4, year = Assess_year, rgn_id, species = Species, common_name = Common_name.x,
                      category = Assess_code, postcards, magnets, tour_operators, pmt, wildlife_trusts)

#check for duplicated data
nrow(ico5)
nrow(distinct(ico5))
sum(duplicated(ico5) == TRUE)

write.csv(ico5, "ICO/prep/ico_spp_iucn_status_esw2018.csv", row.names = F)
write.csv(ico5, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/ico_spp_iucn_status_esw2018.csv", row.names = F) #to git


#-------------#

rm(list=ls())
cat("\014")

#CW

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/CW")
options(scipen=999)

######

library(tidyverse)
library(rgdal)

#nutrient polution
#fertilisers (PREP)

#data source: http://www.fao.org/faostat/en/#data/RFN
uk_nut <- read.csv("source/UK_nutrient_data_FAOSTAT_20181207.csv") #fertiliser use 2002-2016
#group fertiliser use by year and sum
tn <- uk_nut %>% group_by(Year) %>%
  summarise(Tonnes = sum(Value))

#farmland within 5 km of the coast (SW regions) - extracted in ArcMap from CEH 25m landcover data
shp <- readOGR(dsn = "source/shapefiles", layer = "ESW_rgns_5km_int_arable_grassland_BNG")
rgn_agri <- shp@data
rgn_agri <- rgn_agri[order(rgn_agri$rgn_id),]

#coastline length by region
shp2 <- readOGR(dsn = "source/shapefiles", layer = "ESW_coastline_regions_20190122_BNG")
rgn_coast_km <- shp2@data
rgn_coast_km  <- rgn_coast_km[order(rgn_coast_km$rgn_id),]

#join region farm land areas and coastal lengths
rgn_agri <- left_join(rgn_agri, rgn_coast_km, by = "rgn_id")

#sum agri land in SW coastal zone
sw_agri <- sum(rgn_agri$area_ha)
#propotionalise SW rgn_id agri land as prop of all SW coastal area
rgn_agri["rgn_id_agri_prop"] <- rgn_agri$area_ha/sw_agri

uk_agri <- 11746094 #total uk land (ha) farmed as 'arable' or 'improved grassland' extracted from CEH 25m landcover data (Edina)

#calculate fertiliser use by year for SW coastal area
tn["sw_fert_tonnes"] <- (tn$Tonnes/uk_agri)*sw_agri

z <- NULL
for(i in 1:6 ){
  tn2 <- tn
  tn2$rgn_id <- rep(i, nrow(tn2))
  z <- rbind(z, tn2)
}

tn3 <- z
tn3$rgn_id <- as.factor(tn3$rgn_id)
rgn_agri$rgn_id <- as.factor(rgn_agri$rgn_id)
tn4 <- left_join(tn3, rgn_agri, by = "rgn_id")

#fertiliser use by region
tn4["rgn_fertiliser_use"] <- tn4$sw_fert_tonnes*tn4$rgn_id_agri_prop
#correct for coastal length
tn4["rgn_fert_cst_lngth"] <- tn4$rgn_fertiliser_use/tn4$c_lngth_km

tn5 <- dplyr::select(tn4, Year, rgn_id, rgn_fert_cst_lngth)
colnames(tn5) <- c("year", "rgn_id", "nutrients")

write.csv(tn5, "prep/cw_nutrients_esw2018.csv", row.names = F)
write.csv(tn5, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_nutrients_esw2018.csv", row.names = F) #to git

#####

library(tidyverse)
library(rgdal)

#chemical polution
#pesticides (PREP)

#data source: http://www.fao.org/faostat/en/#data/EP/visualize
uk_pest <- read.csv("source/UK_pesticide_data_FAOSTAT_2018106.csv") #pesticde use 1990-2016

#farmland within 5 km of the coast (SW regions) - extracted in ArcMap from CEH 25m landcover data
shp <- readOGR(dsn = "source/shapefiles", layer = "ESW_rgns_5km_int_arable_grassland_BNG")
rgn_agri <- shp@data
rgn_agri <- rgn_agri [order(rgn_agri$rgn_id),]

#coastline length by region
shp2 <- readOGR(dsn = "source/shapefiles", layer = "ESW_coastline_regions_20190122_BNG")
rgn_coast_km <- shp2@data
rgn_coast_km  <- rgn_coast_km[order(rgn_coast_km$rgn_id),]

#join region farm land areas and coastal lengths
rgn_agri <- left_join(rgn_agri,rgn_coast_km,by="rgn_id")

#sum agri land in SW coastal zone
sw_agri <- sum(rgn_agri$area_ha)
#propotionalise SW rgn_id agri land as prop of all SW coastal area
rgn_agri["rgn_id_agri_prop"] <- rgn_agri$area_ha/sw_agri

pest <- dplyr::select(uk_pest, Year, Value)
colnames(pest) <- c("year", "kg_ha")

pest["sw_pest_kg"] <- pest$kg_ha*sw_agri

z <- NULL
for(i in 1:6 ){
  pest2 <- pest
  pest2$rgn_id <- rep(i, nrow(pest2))
  z <- rbind(z, pest2)
}

pest3 <- z
pest3$rgn_id <- as.factor(pest3$rgn_id)
rgn_agri$rgn_id <- as.factor(rgn_agri$rgn_id)
pest4 <- left_join(pest3, rgn_agri, by = "rgn_id")

#pesticide use by region
pest4["rgn_pest_use"] <- pest4$sw_pest_kg*pest4$rgn_id_agri_prop
#correct for coastal length
pest4["rgn_pest_cst_lngt"] <- pest4$rgn_pest_use/pest4$c_lngth_km

pest5 <- dplyr::select(pest4, year, rgn_id, "rgn_pest_cst_lngt")
colnames(pest5) <- c("year", "rgn_id", "pesticides")

write.csv(pest5, "prep/cw_pesticides_esw2018.csv", row.names = F)
write.csv(pest5, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_pesticides_esw2018.csv", row.names = F) #to git

################
################

library(tidyverse)

#water quality indicator - bathing water (PREP)

#data source: https://environment.data.gov.uk/bwq/profiles/data-download.html?country=England
#bathing water classification data assigned lon lats in excel (vlookup)
#data cropped to ESW region and assigned rgn_id (in ArcMap)
#.csv output: ESW_bathing_water_classifications_1997_2017_rgn_id.csv

#read in ESW_bathing_water_classifications_1997_2017_rgn_id.csv
t1 <- read.csv("source/bathing_water_data/ESW_bathing_water_classifications_1997_2017_rgn_id.csv")

t1$regime <- as.character(t1$regime)
#select data for 2006 directive
t2 <- t1[t1$regime == "2006 directive",]

#look up table for beach classifications/beach status score
beach_category <- data.frame(classifica = c('Excellent', 'Good', 'Sufficient', 'Poor', 'Closed'),
                             beach_status_score = c(1,  0.75,  0.5,  0.25, 0))
#join status score to table
t3 <- t2 %>% left_join(beach_category, by = 'classifica')
t4 <- select(t3, year, rgn_id, beach_status_score)

#write .csv for processing in OHI (for layer folder)
write.csv(t4, "prep/cw_bathing_water_class_esw2018.csv", row.names = F)
write.csv(t4, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_bathing_water_class_esw2018.csv", row.names = F) #to git

################
################

library(rgdal)
library(raster)
library(ncdf4)

#water quality indicator - gelbstoff and detrital material (PREP)
#NOTE: high value poorer water quality - more suspended material

#import yearly global rasters(wgs84)
#data source: https://oceancolor.gsfc.nasa.gov/cgi/l3

r_list <- list.files("source/Global_ Absorption_gelbstoff_&_detrital_material_L3_4km_yearly_2003-2017",full.names = T)
#3nm coastal buffer
ext <- readOGR(dsn = "source/shapefiles", layer = "ESW_regions_3nm_buf_20190122_BNG")

#projections
wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#re-project bng extent to wgs84
ext_wgs84 <- spTransform(ext, wgs84)

#extract a mean value per region id for each year
z <- NULL

for (i in 1:length(r_list)){
  #check ncdf4 variable name
  #nc <- nc_open(r_list[i])
  #print(nc)
  r <- raster(r_list[i], varname = "adg_443_giop")
  #plot(r)
  d <- extract(r, ext_wgs84, fun = mean, na.rm = T, sp = T)
  t <- d@data
  t2 <- t[order(t$rgn_id),]
  colnames(t2) <- c("rgn_id", "area_km2", "sus_mat")
  t2["year"] <- substr(r_list[i], 82, 85)
  t3 <- t2[,c("year", "rgn_id", "sus_mat")]
  z <- rbind(z, t3)
}

t4 <- z

write.csv(t4, "prep/cw_sus_material_esw2018.csv", row.names = F)
write.csv(t4, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_sus_material_esw2018.csv", row.names = F) #to git


################
################

library(raster)
library(rgdal)

#trash data

#import global raster (ohi global data: rescaled) reprojected in ArcMap to WGS84
#source data are rescaled 0-1
#1 = high 0 = low
#needs inverting
#data source: https://mazu.nceas.ucsb.edu/data/#ohi_pressure_data (Ericson 2014)
r <- raster("source/plastics_rescaled/plastics_wgs84.tif")
#3nm coastal buffer
ext <- readOGR(dsn = "source/shapefiles", layer = "ESW_regions_3nm_buf_20190122_BNG")

#projections
wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#re-project bng extent to wgs84
ext_wgs84 <- spTransform(ext, wgs84)

#extract a mean value per region id
d <- raster::extract(r, ext_wgs84, fun = mean, na.rm = T, sp = T)
t <- d@data
t2 <- t[order(t$rgn_id),]
colnames(t2) <- c("rgn_id", "area_km2", "trash")
t2["year"] <- 2014
#invert
t2$trash <- 1 - t2$trash
#rescale to 1 again
q <- quantile(t2$trash, 1, na.rm = T)
fun = function(x){ifelse(x>q, 1, x/q)}
t2$trash <- as.numeric(lapply(t2$trash, fun))
t3 <- t2[,c("year", "rgn_id", "trash")]

write.csv(t3, "prep/cw_trash_esw2018.csv", row.names = F)
write.csv(t3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_trash_esw2018.csv", row.names = F) #to git

################
################

library(raster)
library(rgdal)

#nt sed in ohi calculation
#chemical polution - inorganic run-off

#import global raster (ohi global data: rescaled)
#source data are rescaled 0-1
#data source: https://mazu.nceas.ucsb.edu/data/#ohi_pressure_data
r <- raster("source/inorganic_run_off_rescaled/inorganic_wgs84.tif")
#3nm coastal buffer
ext <- readOGR(dsn = "source/shapefiles", layer = "ESW_regions_3nm_buf_20190122_BNG")

#projections
wgs84 <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#re-project bng extent to wgs84
ext_wgs84 <- spTransform(ext, wgs84)

#extract a mean value per region id
d <- extract(r, ext_wgs84, fun = mean, na.rm = T, sp = T)
t <- d@data
t2 <- t[order(t$rgn_id),]
colnames(t2) <- c("rgn_id", "area_km2", "inorg_run_off")
t2["year"] <- 2013
#high run-off = 1 low = 0
#needs inverting
#low = 1 high = 0

t2["inv_inorg_run_off"] <- 1 - t2$inorg_run_off

t3 <- t2[,c("year", "rgn_id", "inv_inorg_run_off")]
colnames(t3) <- c("year", "rgn_id", "inorg_run_off")

write.csv(t3,"prep/cw_inorganic_run_off_esw2018.csv", row.names = F)
write.csv(t3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_inorganic_run_off_esw2018.csv", row.names = F) #to git

################
################

library(rgdal)
library(raster)
library(ncdf4)
library(tidyverse)

#chemical polution - inorganic run-off #2
#import UK raster: monthly rainfall
#data source: https://catalogue.ceh.ac.uk/datastore/eidchub/33604ea0-c238-4488-813d-0ad9ab7c51ca/GB/monthly/CEH_GEAR_monthly_GB_xxxx.nc

#check ncdf4 variable name before running
#nc <- nc_open("source/UK_rainfall_monthly_1km_gridded/CEH_GEAR_monthly_GB_2006.nc")
#print(nc)
#time <- ncvar_get(nc,varid = "time") #this reads the time stamps
#nc_close(nc)

r_list <- list.files("source/UK_rainfall_monthly_1km_gridded",full.names = T)
#urban & suburban areas within 5km of the coast
ext <- readOGR(dsn = "source/shapefiles", layer = "ESW_rgns_5km_int_urban_suburban_BNG")

#coastline length by region
shp2 <- readOGR(dsn = "source/shapefiles", layer = "ESW_coastline_regions_20190122_BNG")
rgn_coast_km <- shp2@data
rgn_coast_km  <- rgn_coast_km[order(rgn_coast_km$rgn_id),]

bng <- proj4string(ext)

z <- NULL

for (i in 1:length(r_list)){
  r <- brick(r_list[i], varname = "rainfall_amount")
  crs(r) <- bng
  sum_r <- sum(r, na.rm = T)
  #extract sum of rain fall within regions (mm)
  d <- raster::extract(sum_r, ext, fun = sum, na.rm = T, sp = T)
  t <- d@data
  colnames(t) <- c("rgn_id", "area_km2", "rain_fall_mm")
  t$rgn_id <- as.factor(t$rgn_id)
  rgn_coast_km$rgn_id <- as.factor(rgn_coast_km$rgn_id)
  t <- left_join(t, rgn_coast_km, by = "rgn_id")
  t2 <- t[order(t$rgn_id),]
  #plot(r)
  t2["year"] <- substr(r_list[i], 60, 63)
  z <- rbind(z, t2)
}

t3 <- z

#correct for coastal length
t3["rain_fall_mm_cst_lngth"] <- t3$rain_fall_mm / t3$c_lngth_km

t4 <- t3 %>%
  dplyr::select(year = year, rgn_id = rgn_id, inorg_run_off = rain_fall_mm_cst_lngth)

write.csv(t4, "prep/cw_inorganic_run_off2_esw2018.csv", row.names = F)
write.csv(t4, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_inorganic_run_off2_esw2018.csv", row.names = F) #to git


################
################

#AIS

setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/CW")
options(scipen=999)

library(rgdal)
library(raster)

ex <- extent(c(-11, -2.3, 47.3, 52))
ex2 <- as(ex,'SpatialPolygons') #convert to SpatialPolygon
proj4string(ex2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
r1 <- raster(ex2, resolution = 0.009, crs = "+proj=longlat +datum=WGS84", vals = NA) #approx. 1km res - all values NA

#read in region poly (3nm)
#shp <- readOGR(dsn = "source/shapefiles", layer = "ESW_regions_20190122_BNG")
shp <- readOGR(dsn = "source/shapefiles", layer = "ESW_regions_3nm_buf_20190122_BNG")
#reproject to wgs84
proj <- crs(r1)
shp <- spTransform(shp, proj)

r_list <- list.files("C:/Folders/Dropbox/GIS/GIS_data/MMO_AIS_vessel_density_grids_2011-2015/Vessel_Density_Grids_2011_2015", full.names = T)

z <- NULL

for (i in 1:length(r_list)){
  r <- raster(r_list[i])
  AIS <- resample(r, r1, "bilinear")
  #mask to region
  AIS <- mask(AIS, shp)
  #extract a mean value per region id
  d <- raster::extract(AIS, shp, fun = mean, na.rm = T, sp = T)
  t <- d@data
  t <- t[order(t$rgn_id),]
  t$year <- substr(r_list[i], 103, 106)
  colnames(t) <- c("rgn_id", "area_km2", "vessels", "year")
  t <- t[,c("year", "rgn_id", "vessels")]
  z <- rbind(z, t)
}

t1 <- z
#rescale data 0-1 using 99th quantile of all data
#this is ok here - only 5 years of data available
q <- quantile(t1$vessels,0.99, na.rm = T)
fun = function(x){ifelse(x>q, 1, x/q)}
t1$vessels2 <- as.numeric(lapply(t1$vessels,fun))

#high vessel movements = 1 low = 0
#needs inverting
#so low  = 1 high  = 0
t1$vessels_inv <- 1 - t1$vessels2
t1 <- t1 %>%
  dplyr::select(year = year, rgn_id = rgn_id, vessel_pol = vessels_inv)

write.csv(t1, "prep/cw_vessel_pol_esw2018.csv", row.names = F)
write.csv(t1, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_vessel_pol_esw2018.csv", row.names = F) #to git


#-------------#

rm(list=ls())
cat("\014")

#BD SPP
#NOTE: for BD HAB see earlier code

#Species status - mean species 'risk' weighted by proportion of area:

#The reference point is to have the risk status of all iconic species at lowest risk of extinction - ie 1 - least concern (LC)

########
#Risk is a scaled value representing the species extinction risk category:
#'LC' = 0.0, 'NT' = 0.2, 'VU' = 0.4, 'EN' = 0.6, 'CR' = 0.8, 'EX' = 1.0
#The regional risk values are converted to species status scores by subtracting the risk values from 1
########

#trend average of population trend assessments (latest IUCN data) for all species within a region
#0.5 increasing
#0 stable
#-0.5 decreasing

########

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/SPP")
options(scipen=999)

library(tidyverse)
library(rgdal)

#######

sp <- read.csv("source/BD_SPP_marLIN.csv")
shp <- readOGR(dsn = "source/shapefiles", layer = "ESW_regions_20190122_BNG")
rgn_km2 <- shp@data
rgn_km2$rgn_id <- as.numeric(rgn_km2$rgn_id)

#note: Assess_year is the latest year of IUCN assesment

#head(sp)
sp2 <- sp %>% 
  select(rgn_id = Region, year = Assess_year, sp = Species, km2 = Area, IUCN_cat = Assess_code, trend = Trend) %>% 
  arrange(rgn_id, sp, year)

#remove white spaces from IUCN_cat
sp2$IUCN_cat <- as.factor(trimws(sp2$IUCN_cat, "both"))

#add status scores (1 - extinction risks)
IUCN_status <- data.frame(IUCN_cat = c('LC', 'NT', 'CD', 'VU', 'EN', 'CR', 'EX', 'DD'),
                          IUCN_status = c(1,  0.8,  0.7,  0.6,  0.4,  0.2,  0, NA))
sp2 <- sp2 %>%
  left_join(IUCN_status, by = 'IUCN_cat')

#remove NA from status
sp2 <- sp2 %>% 
  drop_na(IUCN_status) %>% 
  left_join(rgn_km2, by = "rgn_id") %>% 
  mutate(rgn_area_prop = pmin(1, km2 / area_km2)) %>% 
  mutate(IUCN_status_prop = IUCN_status * rgn_area_prop) #calculate area-weighted species status (poportion of sp distribution to whole region) - for each region

#add population trend scores
pop_trend <- data.frame(trend = c("Decreasing", "Increasing", "Stable", "Unknown"),
                        pop_trend = c(-0.5, 0.5, 0, NA))  
sp2 <- sp2 %>%
  left_join(pop_trend, by = 'trend')

#group by rgn_id and calculate mean regional status score
sp3 <- sp2 %>% 
  group_by(rgn_id) %>% 
  summarise(score = mean(IUCN_status_prop, na.rm = T))

write.csv(sp3, "prep/spp_status_esw2018.csv", row.names = F)
write.csv(sp3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/spp_status_esw2018.csv", row.names = F) #to git

#calculate trend  
sp4 <- sp2 %>% 
  group_by(rgn_id) %>% 
  summarise(score = mean(pop_trend, na.rm = T))

write.csv(sp4, "prep/spp_trend_esw2018.csv", row.names = F)
write.csv(sp4, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/spp_trend_esw2018.csv", row.names = F) #to git













