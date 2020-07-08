
#-------------#

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#FP####
#FIS####

library(tidyverse)

#landings to port
#data source: https://www.gov.uk/government/statistical-data-sets/uk-and-foreign-vessels-landings-by-uk-port-and-uk-vessel-landings-abroad
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/FP/FIS")
options(scipen=999)

fl <- read.csv("source/landings_to_port.csv") #read in landings data
tpc <- read.csv("source/taxon_penalty_code.csv") #read in taxon penalty codes
#taxon penalty codes from global OHI ... amended to include local stock assessments with data from:
#https://www.cefas.co.uk/cefas-data-hub/dois/uk-ices-fish-stock-and-shellfish-stock-assessment-data-2017/
#http://data.cefas.co.uk/#/View/18741
tpc$stock_id <- as.character(tpc$stock_id)

#group landings data by species, stock_id, year (sum species-specific landings data by region)
#names(fl)

fl2 <- fl %>%
  dplyr::group_by(rgn_id, year, stock_id, species) %>%
  dplyr::summarise(SumCatch = sum(landed_weight_tonnes)) %>%
  filter(year != 2019) %>% #remove data for 2019 as incomplete time-series
  ungroup() %>% 
  filter(SumCatch != 0) 

zz <- NULL
rgns <- unique(fl2$rgn_id)

#gap fill years to contain zeros for empty 'no catch' years

for (i in 1 : length(rgns)){
  #i = 3
  rglnd <- filter(fl2, rgn_id == i)
  rglnd$rgn_id <- as.numeric(rglnd$rgn_id)
  rglnd$species <- as.character(rglnd$species)
  rglnd$year <- as.character(rglnd$year)
  rglnd$stock_id <- as.character(rglnd$stock_id)
    
  rglnd$SumCatch[rglnd$SumCatch == 0] <- NA
  
  sp <- unique(rglnd$species)
  stk_id <- unique(rglnd$stock_id) 
  
  year <- rep(c(2014:2018), length(sp))
  rgn_id <- rep(i, length(year))
  species <- as.character(rep(sp, each = length(unique(year))))
  stock_id <- as.character(rep(stk_id, each = length(unique(year)))) 
  #something up with n of stock_id 200 should be 205 #its crustacea -need shrimp/prawns and crabs .... also same in taxon penalty code
  
  z <- as.data.frame(cbind(rgn_id, year, stock_id, species))
  z$rgn_id <- as.numeric(as.character(z$rgn_id))
  z$year <- as.character(z$year)
  z$species <- as.character(z$species)
  z$stock_id <- as.character(z$stock_id)
  
  rglnd <- z %>% 
    left_join(rglnd, by = c("rgn_id", "year", "stock_id", "species"))
  
  #replace Nas with 0
  rglnd[is.na(rglnd)] <- 0 
  
  rglnd <- rglnd %>% 
    left_join(tpc, by = "stock_id" )
  
  zz <- rbind(zz, rglnd)
}

write.csv(zz, "prep/fis_2014_2018_landings_to_port_esw2018.csv", row.names = F) #to prep
write.csv(zz, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/fis_2014_2018_landings_to_port_esw2018.csv", row.names = F) #to git

#END

#--------------#

#weightings

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

library(tidyverse)

setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/FP/FIS")
options(scipen=999)

t <- read.csv("prep/fis_2014_2018_landings_to_port_esw2018.csv")
#names(t)

t <- t %>% 
  select(rgn_id, year, SumCatch) %>% 
  group_by(year) %>% 
  summarise(TotCatch = sum(SumCatch)) %>% 
  ungroup()

write.csv(t, "prep/weights_esw2018.csv", row.names = F) #to prep


#END

#-------------#

#MAR####

rm(list=ls())
cat("\014")

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/FP/MAR")

library(tidyverse)

#read in harvest totals for sw
#these data are replicated by region id
total_harvest <- read.csv("source/mariculture_tonnes.csv")

mar <- total_harvest %>% 
  select(rgn_id = rgn, year, rgn_prod_T = tonnes) %>% 
  arrange(rgn_id, year)

write.csv(mar, "prep/mar_harvest_tonnes_esw2018.csv", row.names = F) #to prep
write.csv(mar, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/mar_harvest_tonnes_esw2018.csv", row.names = F) #to git

#-------------#

rm(list=ls())
cat("\014")

#AO####

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/AO")
options(scipen=999)

library(tidyverse)
library(rgdal)

###

catch <- read.csv("source/catch_proportion.csv")
catch_under10 <- catch %>%
  filter(Length == "10m&Under") %>% 
  select(rgn_id, year = Year, catch_prop = Proportion)
write.csv(catch_under10, "prep/ao_catch_under10_prop_esw2018.csv", row.names = F)
write.csv(catch_under10, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/ao_catch_under10_prop_esw2018.csv", row.names = F) #to git

###

value <- read.csv("source/value_proportion.csv")
value_under10 <- value %>%
  filter(Length == "10m&Under") %>% 
  select(rgn_id, year = Year, value_prop = Proportion)
write.csv(value_under10, "prep/ao_value_under10_prop_esw2018.csv", row.names = F)
write.csv(value_under10, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/ao_value_under10_prop_esw2018.csv", row.names = F) #to git

###

access <- read.csv("source/Access.csv")
access <- mutate(access, year = "2018") %>%
  select(rgn_id = Region, year, access_perc = Access_percentage)
write.csv(access, "prep/ao_access_esw2018.csv", row.names = F)
write.csv(access, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/ao_access_esw2018.csv", row.names = F) #to git

####

boats <- read.csv("source/boats_by_year.csv")
boats <- select(boats, rgn_id, year, boats)
write.csv(boats, "prep/ao_fleet_size_esw2018.csv", row.names = F)
write.csv(boats, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/ao_fleet_size_esw2018.csv", row.names = F) #to git

####

red <- read.csv("source/fuel_cost_year.csv")
write.csv(red, "prep/ao_fuel_cost_esw2018.csv", row.names = F)
write.csv(red, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/ao_fuel_cost_esw2018.csv", row.names = F) #to git

####

effort <- read.csv("source/effort.csv")
effort <- select(effort, rgn_id, year, effort_tph)
effort$effort_tph <- as.numeric(as.character(effort$effort_tph))

#replace NAs with zeros region 1 has a fleet but no effort)
#effort[is.na(effort)] <- 0

write.csv(effort, "prep/ao_effort_catch_esw2018.csv", row.names = F)
write.csv(effort, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/ao_effort_catch_esw2018.csv", row.names = F) #to git

#-------------#


rm(list=ls())
cat("\014")

#CST#### 
#CPR####
#BD:HAB####

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB")
options(scipen=999)

library(tidyverse)
library(rgdal)

###

#saltmash extent
sm_ex <- read.csv("source/hab_littoral_extent.csv")

sm_ex <- mutate(sm_ex, year = "2017") %>%
  mutate(habitat = "saltmarsh") %>%
  select(rgn_id, habitat, year, km2 = area_km2)

write.csv(sm_ex, "prep/hab_saltmarsh_extent_esw2018.csv", row.names = F)
write.csv(sm_ex, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_saltmarsh_extent_esw2018.csv", row.names = F) #to git

###

#saltmash condition and monitoring (current year status and trend)
sm_cnd_m <- read.csv("source/hab_littoral_date.csv")
#head(sm_cnd_m)
sm_condition <- data.frame(condition = c('FAVOURABLE', 'UNFAVOURABLE RECOVERING', 'UNFAVOURABLE NO CHANGE', 'UNFAVOURABLE DECLINING'),
                           sm_cond = c(1,  0.5,  0.5,  0))
sm_cnd_m <- sm_cnd_m %>%
  left_join(sm_condition, by = 'condition') %>% 
  select(rgn_id, year, sm_cond, sm_mon = value) %>%
  rowwise() %>% 
  mutate(cond_mon = mean(c(sm_cond, sm_mon))) %>% 
  ungroup() %>% 
  group_by(rgn_id, year) %>%
  summarise(health = mean(cond_mon, na.rm = T)) %>%
  mutate(habitat = "saltmarsh")

write.csv(sm_cnd_m, "prep/hab_saltmarsh_health2_esw2018.csv", row.names = F)
write.csv(sm_cnd_m, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_saltmarsh_health2_esw2018.csv", row.names = F) #to git
sm_cnd_m <- sm_cnd_m %>%
  mutate(status = health * 100, data_year = year, region_id = rgn_id) 
write.csv(sm_cnd_m, "C:/Folders/Dropbox/github/esw_copy/esw2018/status_data/cat/HAB_saltmarsh_status2.csv", row.names = F)

###

#sand dune extent
d_ex <- read.csv("source/hab_dune_extent.csv")

d_ex <- mutate(d_ex, year = "2017") %>%
  mutate(habitat = "sand dune") %>%
  select(rgn_id, habitat, year, km2 = area_km2)

write.csv(d_ex, "prep/hab_sand_dune_extent_esw2018.csv", row.names = F)
write.csv(d_ex, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_sand_dune_extent_esw2018.csv", row.names = F) #to git

###

#sand dune condition and monitoring (current year status and trend)
d_cnd_m <- read.csv("source/hab_dune_date.csv")
#head(d_cnd_m)
d_condition <- data.frame(condition = c('FAVOURABLE', 'UNFAVOURABLE RECOVERING', 'UNFAVOURABLE NO CHANGE', 'UNFAVOURABLE DECLINING', "DESTROYED", "Not assessed"),
                          d_cond = c(1,  0.5,  0.5,  0, 0, NA))
d_cnd_m <- d_cnd_m %>%
  left_join(d_condition, by = 'condition') %>% 
  select(rgn_id, year, d_cond, d_mon = value) %>%
  rowwise() %>% 
  mutate(cond_mon = mean(c(d_cond, d_mon))) %>% 
  ungroup() %>% 
  group_by(rgn_id, year) %>%
  summarise(health = mean(cond_mon, na.rm = T)) %>%
  mutate(habitat = "sand dune")

write.csv(d_cnd_m, "prep/hab_sand_dune_health2_esw2018.csv", row.names = F)
write.csv(d_cnd_m, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_sand_dune_health2_esw2018.csv", row.names = F) #to git
d_cnd_m <- d_cnd_m %>%
  mutate(status = health * 100, data_year = year, region_id = rgn_id) 
write.csv(d_cnd_m, "C:/Folders/Dropbox/github/esw_copy/esw2018/status_data/cat/HAB_sand_dune_status2.csv", row.names = F)

###

#seagrass extent
sg_ex <- read.csv("source/hab_seagrass_extent.csv")

sg_ex <- mutate(sg_ex, year = "2017") %>%
  mutate(habitat = "seagrass") %>%
  select(rgn_id, habitat, year, km2 = area_km2)

write.csv(sg_ex, "prep/hab_seagrass_extent_esw2018.csv", row.names = F)
write.csv(sg_ex, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_seagrass_extent_esw2018.csv", row.names = F) #to git

###

#seagrass condition AND monitoring
sg_cnd <- read.csv("source/hab_seagrass_condition.csv")
sg_cnd$condition <- as.numeric(as.character(sg_cnd$condition))
sg_mon <- read.csv("source/hab_seagrass_date.csv")
sg_mon$value <- as.numeric(as.character(sg_mon$value))

sg_cnd <- left_join(sg_cnd, sg_mon, by = "rgn_id")

sg_cnd <- mutate(sg_cnd, year = "2018") %>%
  select(rgn_id, condition, year, mon = value) %>% 
  mutate(habitat = "seagrass") %>%
  rowwise() %>% 
  mutate(health = mean(c(condition, mon))) %>% 
  ungroup() %>% 
  select(rgn_id, year, health, habitat)

write.csv(sg_cnd, "prep/hab_seagrass_health2_esw2018.csv", row.names = F)
write.csv(sg_cnd, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_seagrass_health2_esw2018.csv", row.names = F) #to git
sg_cnd <- sg_cnd %>%
 mutate(status = health * 100, data_year = year, region_id = rgn_id)
write.csv(sg_cnd, "C:/Folders/Dropbox/github/esw_copy/esw2018/status_data/hist/HAB_seagrass_status2.csv", row.names = F)


#seagrass trend (from literature)
sg_trnd <- read.csv("source/hab_seagrass_trend.csv")

sg_trnd <- mutate(sg_trnd, year = "2017") %>%
  mutate(habitat = "seagrass") %>%
  select(rgn_id, habitat, year, trend)

write.csv(sg_trnd, "prep/hab_seagrass_trend_esw2018.csv", row.names = F)
write.csv(sg_trnd, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_seagrass_trend_esw2018.csv", row.names = F) #to git

###

#maerl extent
m_ex <- read.csv("source/hab_maerl_extent.csv")

m_ex <- mutate(m_ex, year = "2017") %>%
  mutate(habitat = "maerl") %>%
  select(rgn_id, habitat, year, km2 = area_km2)

write.csv(m_ex, "prep/hab_maerl_extent_esw2018.csv", row.names = F)
write.csv(m_ex, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_maerl_extent_esw2018.csv", row.names = F) #to git

#maerl condition NOT monitoring
m_cnd <- read.csv("source/hab_maerl_condition.csv")
m_cnd$condition <- as.numeric(as.character(m_cnd$condition))
#sg_mon <- read.csv("source/hab_seagrass_date.csv")
#sg_mon$value <- as.numeric(as.character(sg_mon$value))

#sg_cnd <- left_join(sg_cnd, sg_mon, by = "rgn_id")

m_cnd <- mutate(m_cnd, year = "2018") %>%
  #select(rgn_id, condition, year, mon = value) %>% 
  mutate(habitat = "maerl") %>%
  select(rgn_id, year, health = condition, habitat)


write.csv(m_cnd, "prep/hab_maerl_health_esw2018.csv", row.names = F)
write.csv(m_cnd, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_maerl_health_esw2018.csv", row.names = F) #to git
m_cnd <- m_cnd %>%
  mutate(status = health * 100, data_year = year, region_id = rgn_id)
write.csv(m_cnd, "C:/Folders/Dropbox/github/esw_copy/esw2018/status_data/hist/HAB_maerl_status.csv", row.names = F)

#maerl trend (from literature: IUCN A5.51 Atlantic maerl beds)
m_trnd <- read.csv("source/hab_maerl_trend.csv")

m_trnd <- mutate(m_trnd, year = "2018") %>%
  mutate(habitat = "maerl") %>%
  select(rgn_id, habitat, year, trend)

write.csv(m_trnd, "prep/hab_maerl_trend_esw2018.csv", row.names = F)
write.csv(m_trnd, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_maerl_trend_esw2018.csv", row.names = F) #to git

###


#-------------#

rm(list=ls())
cat("\014")

#softbottom and hardbottom extent and condition 
#>>>>>>>12nm<<<<<<<<<

setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB")
options(scipen=999)

#library(rgdal)
#library(raster)
library(tidyverse)

###

#softbottom extent
#calculate area from offshore trawl data (but could also use inshore data)
sb <- read.csv("source/offshore_hab_fish_12nm.csv")
sb_extent <- sb %>%
  dplyr::select(rgn_id = Region, Hab_Type, year = Year, km2 = Hab_Area_km2) %>% 
  distinct() %>% 
  filter(Hab_Type == "soft_bottom") %>% 
  mutate(habitat = "cms") %>% 
  dplyr::select(rgn_id, habitat, year, km2) %>% 
  arrange(rgn_id, year)

write.csv(sb_extent, "prep/hab_cms_extent_esw2018.csv", row.names = F)
write.csv(sb_extent, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_cms_extent_esw2018.csv", row.names = F) #to git

###

#hardbottom extent
hb <- read.csv("source/offshore_hab_fish_12nm.csv")
hb_extent <- hb %>%
  dplyr::select(rgn_id = Region, Hab_Type, year = Year, km2 = Hab_Area_km2) %>% 
  distinct() %>% 
  filter(Hab_Type == "rocky_reef") 

hb_extent <- hb_extent %>% 
  mutate(habitat = "rr") %>% 
  dplyr::select(rgn_id, habitat, year, km2) %>% 
  arrange(rgn_id, year)

write.csv(hb_extent, "prep/hab_rr_extent_esw2018.csv", row.names = F)
write.csv(hb_extent, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_rr_extent_esw2018.csv", row.names = F) #to git

###

#softbottom trawl intensity (inter-regional rescale)
sb <- read.csv("source/offshore_hab_fish_12nm.csv")

t1 <- sb %>%
  filter(Gear == "trawlers") %>% 
  filter(Hab_Type == "soft_bottom") %>% 
  mutate(habitat = "cms") %>% 
  dplyr::select(rgn_id = Region, habitat, year = Year, trwl_int = Intensity) %>% 
  arrange(rgn_id, year) %>% 
  filter(year >= 2014)
#NB extract recent 5 years

t2 <- t1 %>% 
  arrange(rgn_id) %>% 
  mutate(max_trwl_int = max(trwl_int)) %>% 
  mutate(status = 1 - (trwl_int / max_trwl_int)) 
  #mutate(max_status_inv = max(status_inv)) %>% 
  #mutate(status = status_inv + (1 - max_status_inv)) #no need for this correction here as max_status_inv = 1

#write.csv(t2, "C:/Folders/Dropbox/Projects/OHI/Report/Methods/BD_HAB_CS_CP/cms_trawl_int_inter.csv", row.names = F)

sb_trwl_int <- t2 %>%
  dplyr::select(rgn_id, habitat, year, health = status)

write.csv(sb_trwl_int, "prep/hab_cms_trawl_int_inter_esw2018.csv", row.names = F)
write.csv(sb_trwl_int, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_cms_trawl_int_inter_esw2018.csv", row.names = F) #to git
sb_trwl_int <- sb_trwl_int %>%
  mutate(status = health * 100, data_year = year, region_id = rgn_id)
write.csv(sb_trwl_int, "C:/Folders/Dropbox/github/esw_copy/esw2018/status_data/rescale/HAB_cms_trawl_int_inter_status.csv", row.names = F)

###

#softbottom trawl intensity (intra-regional rescale)
sb <- read.csv("source/offshore_hab_fish_12nm.csv")

t1 <- sb %>%
  filter(Gear == "trawlers") %>% 
  filter(Hab_Type == "soft_bottom") %>% 
  mutate(habitat = "cms") %>% 
  dplyr::select(rgn_id = Region, habitat, year = Year, trwl_int = Intensity) %>% 
  arrange(rgn_id, year) %>% 
  filter(year >= 2014)
#NB extract recent 5 years

#write.csv(t1, "C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB/source/cms_trawl_int.csv", row.names = F)

t2 <- t1 %>% 
  arrange(rgn_id) %>% 
  group_by(rgn_id) %>% 
  mutate(max_trwl_int = max(trwl_int)) %>% 
  mutate(status_inv = 1 - (trwl_int / max_trwl_int)) %>% 
  mutate(max_status_inv = max(status_inv)) %>% 
  mutate(status = status_inv + (1 - max_status_inv)) %>% 
  ungroup() 

#replace NaNs with 0
t2$status[is.nan(t2$status)] <- 1 

#write.csv(t2, "C:/Folders/Dropbox/Projects/OHI/Report/Methods/BD_HAB_CS_CP/cms_trawl_int_intra.csv", row.names = F)

sb_trwl_int <- t2 %>%
  dplyr::select(rgn_id, habitat, year, health = status)

write.csv(sb_trwl_int, "prep/hab_cms_trawl_int_intra_esw2018.csv", row.names = F)
write.csv(sb_trwl_int, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_cms_trawl_int_intra_esw2018.csv", row.names = F) #to git
sb_trwl_int <- sb_trwl_int %>%
  mutate(status = health * 100, data_year = year, region_id = rgn_id)
write.csv(sb_trwl_int, "C:/Folders/Dropbox/github/esw_copy/esw2018/status_data/rescale/HAB_cms_trawl_int_intra_status.csv", row.names = F)

###

#hardbottom trawl intensity (inter-regional rescale)
hb <- read.csv("source/offshore_hab_fish_12nm.csv")

t1 <- hb %>%
  filter(Gear == "trawlers") %>% 
  filter(Hab_Type == "rocky_reef") %>% 
  mutate(habitat = "rr") %>% 
  dplyr::select(rgn_id = Region, habitat, year = Year, trwl_int = Intensity) %>% 
  arrange(rgn_id, year) %>% 
  filter(year >= 2014)
#NB extract recent 5 years

t2 <- t1 %>% 
  arrange(rgn_id) %>% 
  mutate(max_trwl_int = max(trwl_int, na.rm = TRUE)) %>% 
  mutate(status = 1 - (trwl_int / max_trwl_int))
  #mutate(max_status_inv = max(status_inv, na.rm = TRUE)) %>% 
  #mutate(status = status_inv + (1 - max_status_inv)) #no need for this correction here as max_status_inv = 1

#write.csv(t2, "C:/Folders/Dropbox/Projects/OHI/Report/Methods/BD_HAB_CS_CP/rr_trawl_int_inter.csv", row.names = F)

hb_trwl_int <- t2 %>%
  dplyr::select(rgn_id, habitat, year, health = status)

write.csv(hb_trwl_int, "prep/hab_rr_trawl_int_inter_esw2018.csv", row.names = F)
write.csv(hb_trwl_int, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_rr_trawl_int_inter_esw2018.csv", row.names = F) #to git
hb_trwl_int <- hb_trwl_int %>%
  mutate(status = health * 100, data_year = year, region_id = rgn_id)
write.csv(hb_trwl_int, "C:/Folders/Dropbox/github/esw_copy/esw2018/status_data/rescale/HAB_rr_trawl_int_inter_status.csv", row.names = F)

###

#hardbottom trawl intensity (intra-regional rescale)
hb <- read.csv("source/offshore_hab_fish_12nm.csv")

t1 <- hb %>%
  filter(Gear == "trawlers") %>% 
  filter(Hab_Type == "rocky_reef") %>% 
  mutate(habitat = "rr") %>% 
  dplyr::select(rgn_id = Region, habitat, year = Year, trwl_int = Intensity) %>% 
  arrange(rgn_id, year)%>% 
  filter(year >= 2014)
#NB extract recent 5 years

#write.csv(t1, "C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB/source/rr_trawl_int.csv", row.names = F)

t2 <- t1 %>% 
  arrange(rgn_id) %>% 
  group_by(rgn_id) %>% 
  mutate(max_trwl_int = max(trwl_int)) %>% 
  mutate(status_inv = 1 - (trwl_int / max_trwl_int)) %>% 
  mutate(max_status_inv = max(status_inv)) %>% 
  mutate(status = status_inv + (1 - max_status_inv)) %>% 
  ungroup() 

#replace NaNs with 1 (region 1 has rr but no trawling)
t2$status[is.nan(t2$status)] <- 1 

#write.csv(t2, "C:/Folders/Dropbox/Projects/OHI/Report/Methods/BD_HAB_CS_CP/rr_trawl_int_intra.csv", row.names = F)

hb_trwl_int <- t2 %>%
  dplyr::select(rgn_id, habitat, year, health = status)

write.csv(hb_trwl_int, "prep/hab_rr_trawl_int_intra_esw2018.csv", row.names = F)
write.csv(hb_trwl_int, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/hab_rr_trawl_int_intra_esw2018.csv", row.names = F) #to git
hb_trwl_int <- hb_trwl_int %>%
  mutate(status = health * 100, data_year = year, region_id = rgn_id)
write.csv(hb_trwl_int, "C:/Folders/Dropbox/github/esw_copy/esw2018/status_data/rescale/HAB_rr_trawl_int_intra_status.csv", row.names = F)


#-------------#


rm(list=ls())
cat("\014")

#TR####

#captures the 'value' that people have from experiencing and enjoying coastal areas. 
#A score of 100 means a region utilizes its full recreational potential

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/TR")
options(scipen=999)

library(tidyverse)
library(rgdal)

###
###

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

#write.csv(t3, "C:/Folders/Dropbox/OHI/Report/Methods/TR/TR_data_prep.csv", row.names = F)

write.csv(t3, "prep/tr_ons_1km_esw2018.csv", row.names = F)
write.csv(t3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/tr_ons_1km_esw2018.csv", row.names = F) #to git

###

#read in viewshed data add year - write to prep folder
#source: https://environment.data.gov.uk/DefraDataDownload/?mapService=MMO/LandWithSeaViews&Mode=spatial
sv <- read.csv("source/seaview.csv")
sv <- sv %>%
  select(c("rgn_id", perc_sv = "Percentage")) %>%
  mutate(year = 2018)

write.csv(sv, "prep/tr_viewshed_esw2018.csv", row.names = F)
write.csv(sv, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/tr_viewshed_esw2018.csv", row.names = F) #to git

###
###

#recreation

#read in modelled recreation data
#souce: https://environment.data.gov.uk/ds/catalogue/index.jsp#/catalogue
#this will represent status data

rec <- read.csv("source/recreation_all.csv")
write.csv(rec, "prep/tr_rec_esw2018.csv", row.names = F)
write.csv(rec, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/tr_rec_esw2018.csv", row.names = F) #to git

###

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

###

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

#LE:LIV####

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


####

#CPI

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/LE/LIV")
options(scipen=999)

library(tidyverse)

CPI <- read.csv("source/CPI_year.csv")

#add region ids
#write to prep folder
z <- NULL
for(i in 1:6 ){
  CPI2 <- CPI
  CPI2$rgn_id <- rep(i, nrow(CPI2))
  z <- rbind(z, CPI2)
}

CPI3 <- z
CPI3 <- select(CPI3, c("rgn_id", "year", "cpi"))
CPI3 <- CPI3[order(CPI3$rgn_id, CPI3$year),]

write.csv(CPI3, "prep/le_cpi_esw2018.csv", row.names = F)
write.csv(CPI3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/le_cpi_esw2018.csv", row.names = F) #to git

#-------------#


rm(list=ls())
cat("\014")

#LE:ECO####

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

#SP:ICO archive####

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

#DA:LAN####

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/DA")
options(scipen=999)

###

library(tidyverse)

d <- read.csv("DA_LAN/source/LAN_1_3_rgnid.csv")
d <- d %>% 
  arrange(min_year) %>% 
  select(rgn_id, area_km2, min_year)

years <- c(2010:2018)

zm <- NULL
zt <- NULL

for(i in 1:length(years)){
  #min_year = 2019
  dd <- d %>% 
    filter(min_year <= years[i]) %>% 
    group_by(rgn_id) %>% 
    summarise(area_km2 = sum(area_km2)) %>% 
    ungroup() %>% 
    mutate(year = years[i])
  
  dd$rgn_id <- as.character(dd$rgn_id)
  
  dm <- dd %>% 
    slice(1:6)
  
  dt <- dd %>% 
    slice(7:12)
  
  zm <- rbind(zm, dm)
  zt <- rbind(zt, dt)
}
  
zm$rgn_id <- sub("m", "", zm$rgn_id)
zm <- zm %>% 
  select(rgn_id, year, area_km2_m = area_km2)

write.csv(zm, "DA_LAN/prep/da_lan_M_3nm_esw2018.csv", row.names = F)
write.csv(zm, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/da_lan_M_3nm_esw2018.csv", row.names = F) #to git

zt$rgn_id <- sub("t", "", zt$rgn_id) 
zt <- zt %>% 
  select(rgn_id, year, area_km2_t = area_km2)

write.csv(zt, "DA_LAN/prep/da_lan_T_1km_esw2018.csv", row.names = F)
write.csv(zt, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/da_lan_T_1km_esw2018.csv", row.names = F) #to git

#END

#-------------#

rm(list=ls())
cat("\014")

#DA:ECL####

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/DA")
options(scipen=999)

###

library(tidyverse)

d <- read.csv("DA_ECL/source/ECL_1_12_rgnid.csv")
d <- d %>% 
  arrange(min_year) %>% 
  select(rgn_id, area_km2, min_year)

years <- c(2010:2018)

zm <- NULL
zt <- NULL

for(i in 1:length(years)){
  #min_year = 2019
  dd <- d %>% 
    filter(min_year <= years[i]) %>% 
    group_by(rgn_id) %>% 
    summarise(area_km2 = sum(area_km2)) %>% 
    ungroup() %>% 
    mutate(year = years[i])
  
  dd$rgn_id <- as.character(dd$rgn_id)
  
  dm <- dd %>% 
    slice(1:6)
  
  dt <- dd %>% 
    slice(7:12)
  
  zm <- rbind(zm, dm)
  zt <- rbind(zt, dt)
}

zm$rgn_id <- sub("m", "", zm$rgn_id)
zm <- zm %>% 
  select(rgn_id, year, area_km2_m = area_km2)

write.csv(zm, "DA_ECL/prep/da_ecl_M_12nm_esw2018.csv", row.names = F)
write.csv(zm, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/da_ecl_M_12nm_esw2018.csv", row.names = F) #to git

zt$rgn_id <- sub("t", "", zt$rgn_id) 
zt <- zt %>% 
  select(rgn_id, year, area_km2_t = area_km2)

write.csv(zt, "DA_ECL/prep/da_ecl_T_1km_esw2018.csv", row.names = F)
write.csv(zt, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/da_ecl_T_1km_esw2018.csv", row.names = F) #to git

#END

#-------------#

#SP:LSP archive####

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/SP")
options(scipen=999)

###

library(tidyverse)
library(maptools) #unionSpatialPolygons
library(rgdal) #readOGR / writeOGR
library(raster) #intersect #area
library(tmap) #append_data

#protected areas by year prepared in ArcMap as follows:
#data: Heritage Coast, AONB, RSPB reserves, SSSIs, Protected wrecks, SACs, SPAs, RAMSAR sites, MCZs #UPDATE
#data for cSACs and proposed MCZs excluded
#clipped to SW region to minimise file size
#designation years checked and where necessary added to shapefile attribute tables (i.e. SSSIs)
#clipped to merged and dissolved 1km terrestrial and 3nm marine coastal buffer - then unioned
#in ArcMap attribute table field added for earliest designation year (where multiple designaions apply to a geographic area)
#data extracted by progressive cumulative years (e.g. 1957, 1957 to 1959, 1957 to 1960, 1957 to 1963 etc)
#these data are read in as "list" - (line 32)

shp1km <- readOGR(dsn = "regions_shapefiles", layer = "ESW_regions_1km_intbuf_20190122_BNG") #coast inland to 1km
t1 <- shp1km@data
plot(shp1km)

shp3nm <- readOGR(dsn = "regions_shapefiles", layer = "ESW_regions_3nm_buf_20190122_BNG") #coastal waters to 3nm
t2 <- shp3nm@data
plot(shp3nm)

list <- list.files("LSP/source/shapefiles/protected_areas_1km_3nm/protected_areas_1km_3nm_by_year", pattern = ".shp$")

zzt <- NULL
zzm <- NULL

#sequentialy read in year specific protected area shapefiles

#i = 3
for(i in 1:length(list)){
  zt <- data.frame(rgn_id = seq(c(1:6)))
  zm <- data.frame(rgn_id = seq(c(1:6)))
  shp <- sub(".shp","",list[i])
  p1 <- readOGR(dsn = "LSP/source/shapefiles/protected_areas_1km_3nm/protected_areas_1km_3nm_by_year", layer = shp)
  ID <- rep(1,nrow(p1@data)) #ID for defining output polygon objects 
  #?unionSpatialPolygons - aggregate (dissolve) polygons based on ID vecor
  p2 <- unionSpatialPolygons(p1, ID = ID) #creates SpatialPolygons object
  #SpatialPolygons to SpatialPolygonsDataFrame
  ID2 <- data.frame(id = seq(1, length(p2@polygons)))
  p3 <- SpatialPolygonsDataFrame(p2, ID2)
  
  #write dissolved shapefiles to sense check
  #writeOGR(p3, dsn = "LSP/source/shapefiles/protected_areas_1km_3nm/protected_areas_1km_3nm_by_year_dis",
  #layer = paste(shp, "_d", sep = ""), driver = "ESRI Shapefile", overwrite_layer = T)
  
  #calculate areas of region intersects and overwrite poly slot data (this has been double checke aand is OK!)
  ter <- raster::intersect(shp1km, p3)
  ter@data$area_km2 <- area(ter) / 1000000 #area in km2 per region
  
  #write shapefiles to sense check
  #writeOGR(ter, dsn = "LSP/source/shapefiles/protected_areas_1km_3nm/protected_areas_1km_3nm_by_year_T_M",
  #layer = paste(shp, "_t_rgn", sep = ""), driver = "ESRI Shapefile", overwrite_layer = T)
  
  mar <- raster::intersect(shp3nm, p3)
  mar@data$area_km2 <- area(mar) / 1000000 #area in km2 per region
  #write shapefiles to sense check
  #writeOGR(mar, dsn = "LSP/source/shapefiles/protected_areas_1km_3nm/protected_areas_1km_3nm_by_year_T_M",
  #layer = paste(shp, "_m_rgn", sep = ""), driver = "ESRI Shapefile", overwrite_layer = T)
  
  dt <- data.frame(ter@data)
  dt$rgn_id <- as.numeric(dt$rgn_id)
  dt <- dplyr::select(dt, rgn_id , area_km2_t = area_km2)
  zt <- left_join(zt, dt, by = "rgn_id")
  zt$year <- substring(shp, nchar(shp)-4+1)
  
  dm <- data.frame(mar@data)
  dm$rgn_id <- as.numeric(dm$rgn_id)
  dm <- dplyr::select(dm, rgn_id , area_km2_m = area_km2)
  zm <- left_join(zm, dm, by = "rgn_id")
  zm$year <- substring(shp, nchar(shp)-4+1)
  
  zzt <- rbind(zzt, zt)
  zzm <- rbind(zzm, zm)
  print(unique(zt$year))
  print(unique(zm$year))
  
}

zzt <- dplyr::select(zzt, rgn_id, year, area_km2_t)
zzt[is.na(zzt)] <- 0
zzm <- dplyr::select(zzm, rgn_id, year, area_km2_m)
zzm[is.na(zzm)] <- 0

#write temporary files 
#lsp_prot_area_inland1km_esw2018.csv
#lsp_prot_area_offshore3nm_esw2018.csv
#to C:\Folders\Dropbox\Projects\OHI\Data\Goals\SP\LSP\prep
#>>>>>>#

#then read back in

#terrestrial data
zzt <- read.csv("LSP/prep/lsp_prot_area_inland1km_esw2018.csv")

#create blank data frame with rgn ids and all years
years <- seq(min(zzt$year),max(zzt$year))
rgns <- data.frame(rgn_id = c(1:6))
zz <- NULL

for(i in 1:length(years)){
  z <- NULL
  z <- rbind(z,rgns)
  z$year <- years[i]
  zz <- rbind(zz,z)
}

#join protected area data (specific years) to blank data frame (all years)
zzt <- left_join(zz, zzt, by = c("rgn_id", "year"))
rgnids <- unique(zzt$rgn_id)

#extract data by rgn and gap fill missing years
ay <- NULL
for(i in 1:length(rgnids)){
  rgn <- dplyr::filter(zzt, rgn_id == rgnids[i])
  colnames(rgn)[3] <- "area_km2_t_gf"
  q = 0
  for(j in 1:nrow(rgn)){
    if(is.na(rgn[j,3])){rgn[j,3] = q}
    if(rgn[j,3] > 0) {q = rgn[j,3]}
  }
  ay <- rbind(ay, rgn)
}

#and join to source data
zzt <- left_join(zzt, ay, by = c("rgn_id", "year"))
write.csv(zzt, "LSP/prep/lsp_prot_area_inland1km_esw2018.csv", row.names = F)
write.csv(zzt, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/lsp_prot_area_inland1km_esw2018.csv", row.names = F) #to git

#>>>>>>#

#marine data
zzm <- read.csv("LSP/prep/lsp_prot_area_offshore3nm_esw2018.csv")

#create blank data frame with rgn ids and all years
years <- seq(min(zzm$year),max(zzm$year))
rgns <- data.frame(rgn_id = c(1:6))
zz <- NULL

for(i in 1:length(years)){
  z <- NULL
  z <- rbind(z,rgns)
  z$year <- years[i]
  zz <- rbind(zz,z)
}

#join protected area data (specific years) to blank data frame (all years)
zzm <- left_join(zz, zzm, by = c("rgn_id", "year"))
rgnids <- unique(zzm$rgn_id)

#extract data by rgn and gap fill missing years
ay <- NULL
for(i in 1:length(rgnids)){
  rgn <- dplyr::filter(zzm, rgn_id == rgnids[i])
  colnames(rgn)[3] <- "area_km2_m_gf"
  q = 0
  for(j in 1:nrow(rgn)){
    if(is.na(rgn[j,3])){rgn[j,3] = q}
    if(rgn[j,3] > 0) {q = rgn[j,3]}
  }
  ay <- rbind(ay, rgn)
}

#and join to source data
zzm <- left_join(zzm, ay, by = c("rgn_id", "year"))
write.csv(zzm, "LSP/prep/lsp_prot_area_offshore3nm_esw2018.csv", row.names = F)
write.csv(zzm, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/lsp_prot_area_offshore3nm_esw2018.csv", row.names = F) #to git



#-------------#

rm(list=ls())
cat("\014")

#CW####

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/CW")
options(scipen=999)

###

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

###

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

###
###

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

###
###

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


###
###

library(raster)
library(rgdal)

#trash data from Global OHI - REMOVE FROM OHI SW - AND REPLACE WITH MCS BEACH CLEAN DATA

#import global raster (ohi global data: rescaled) reprojected in ArcMap to WGS84
#source data are rescaled 0-1
#1 = high plastic 0 = low plastic
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

#invert and rescale to 1
t2 <- t2 %>% 
  dplyr::select(rgn_id, trash = plastics_wgs84) %>% 
  mutate(trash_inv = 1 - trash) %>% 
  mutate(max_trash_inv = max(trash_inv)) %>% 
  mutate(status = trash_inv + (1 - max_trash_inv)) %>% 
  mutate(year = 2014) %>% 
  dplyr::select(rgn_id, year, trash = status)

write.csv(t2, "prep/cw_trash_esw2018.csv", row.names = F)
write.csv(t2, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_trash_esw2018.csv", row.names = F) #to git

###
###

rm(list=ls())
cat("\014")

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/CW")
options(scipen=999)

###

library(tidyverse)
library(rgdal)

t <- read.csv("C:/Folders/Dropbox/Projects/OHI/Data/Goals/CW/source/MCS_beach_clean_rgn_id_BNG.csv")

#names(t)

t2 <- t %>% 
  select(rgn_id, year, items_per_m = items_per_) %>% 
  filter(rgn_id > 0) %>% 
  arrange(rgn_id, year)

t3 <- t2 %>% 
  group_by(rgn_id, year) %>% 
  summarise(mean_items = mean(items_per_m)) %>% 
  ungroup() %>% 
  group_by(rgn_id) %>% 
  summarise(mean_items = mean(mean_items)) %>% 
  ungroup() %>% 
  mutate(year = 2018)

write.csv(t3, "C:/Folders/Dropbox/Projects/OHI/Data/Goals/CW/source/cw_trash.csv", row.names = F)
#rescale to 1 for status data -  high values bad - high density = 1 - so invert
t4 <- t3 %>% 
  mutate(max_mean_items = max(mean_items)) %>% 
  mutate(status_inv = 1 - (mean_items / max_mean_items)) %>% 
  mutate(max_status_inv = max(status_inv)) %>% 
  mutate(status = status_inv + (1 - max_status_inv)) %>% 
  dplyr::select(rgn_id, year, trash = status)

write.csv(t4, "prep/cw_trash_esw2018.csv", row.names = F)
write.csv(t4, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_trash_esw2018.csv", row.names = F) #to git

###
###

library(raster)
library(rgdal)

#not used in SW ohi calculation
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

#write.csv(t3,"prep/cw_inorganic_run_off_esw2018.csv", row.names = F)
#write.csv(t3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_inorganic_run_off_esw2018.csv", row.names = F) #to git

###
###

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


###
###

#AIS

setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/CW")
options(scipen=999)

library(rgdal)
library(raster)
library(tidyverse)

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

#rescale data 0-1
#this is ok here - only 5 years of data available

#high vessel movements = 1 low = 0
#needs inverting
#so low  = 1 high  = 0

#intra_regional rescale
t2 <- t1 %>% 
  arrange(rgn_id) %>% 
  group_by(rgn_id) %>% 
  mutate(max_vessels = max(vessels)) %>% 
  mutate(status_inv = 1 - (vessels / max_vessels)) %>% 
  mutate(max_status_inv = max(status_inv)) %>% 
  mutate(status = status_inv + (1 - max_status_inv)) %>% 
  ungroup() 
 
t2 <- t2 %>%
  dplyr::select(year = year, rgn_id = rgn_id, vessel_pol = status)

write.csv(t2, "prep/cw_vessel_pol_intra_esw2018.csv", row.names = F)
write.csv(t2, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_vessel_pol_intra_esw2018.csv", row.names = F) #to git

#inter_regional rescale
t2 <- t1 %>% 
  arrange(rgn_id) %>% 
  mutate(max_vessels = max(vessels)) %>% 
  mutate(status_inv = 1 - (vessels / max_vessels)) %>% 
  mutate(max_status_inv = max(status_inv)) %>% 
  mutate(status = status_inv + (1 - max_status_inv)) 

t2 <- t2 %>%
  dplyr::select(year = year, rgn_id = rgn_id, vessel_pol = status)

write.csv(t2, "prep/cw_vessel_pol_inter_esw2018.csv", row.names = F)
write.csv(t2, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/cw_vessel_pol_inter_esw2018.csv", row.names = F) #to git

###
###

rm(list=ls())
cat("\014")

#BD:SPP NEW DATA####

#NOTE: for BD HAB see earlier code
#!!!!!!!!!!!! code also used to calculate resilience layers: species_diversity_3nm_esw2018.csv & species_diversity_eez_esw2018.csv ??????

###

#trend average of population trend assessments (latest IUCN data) for all species within a region
#0.5 increasing
#0 stable
#-0.5 decreasing

###

#set working directory to local source
setwd("C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/SPP")
options(scipen=999)

library(tidyverse)

###

sp <- read.csv("source/BD_SPP#3.csv")
#calculatin for goal status ... and  for EEZ resilience.csv
#shp <- readOGR(dsn = "source/shapefiles", layer = "ESW_regions_20190122_BNG")

#add 3nm here for resilience.csv
#shp <- readOGR(dsn = "source/shapefiles", layer = "ESW_regions_3nm_buf_20190122_BNG")

#plot(shp)
#rgn_km2 <- shp@data
#rgn_km2$rgn_id <- as.numeric(rgn_km2$rgn_id)

#note: Assess_year is the latest year of IUCN assesment

#head(sp)
sp2 <- sp %>% 
  select(rgn_id = region, sp = species, status, trend) %>% 
  arrange(rgn_id, sp)

#sp2$trend

#add population trend scores
pop_trend <- data.frame(trend = c("decreasing", "increasing", "stable", "unknown", "unspecified"),
                        pop_trend = c(-0.5, 0.5, 0, NA, NA))  
sp2 <- sp2 %>%
  left_join(pop_trend, by = 'trend')

#group by rgn_id and calculate mean regional status score
sp3 <- sp2 %>% 
  group_by(rgn_id) %>% 
  summarise(score = mean(status, na.rm = T)) %>% 
  ungroup()

write.csv(sp3, "prep/spp_status_esw2018.csv", row.names = F)
write.csv(sp3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/spp_status_esw2018.csv", row.names = F) #to git

sp3 <- sp3 %>%
  mutate(status = score * 100, data_year = 2018, region_id = rgn_id) %>%
  select(region_id, status, data_year)

write.csv(sp3, "C:/Folders/Dropbox/github/esw_copy/esw2018/status_data/hist/SPP_status.csv", row.names = F)

#------------#

#resilience: species_diversity_eez
#write.csv(sp3, "C:/Folders/Dropbox/Projects/OHI/Data/Resilience/prep/species_diversity_eez_esw2018.csv", row.names = F)
#write.csv(sp3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/species_diversity_eez_esw2018.csv", row.names = F) #to git
#resilience: species_diversity_3nm
#write.csv(sp3, "C:/Folders/Dropbox/Projects/OHI/Data/Resilience/prep/species_diversity_3nm_esw2018.csv", row.names = F)
#write.csv(sp3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/species_diversity_3nm_esw2018.csv", row.names = F) #to git

#resilience: species_diversity
write.csv(sp3, "C:/Folders/Dropbox/Projects/OHI/Data/Resilience/prep/species_diversity_esw2018.csv", row.names = F)
write.csv(sp3, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/species_diversity_esw2018.csv", row.names = F) #to git

#------------#

#calculate trend  
sp4 <- sp2 %>% 
  group_by(rgn_id) %>% 
  summarise(score = mean(pop_trend, na.rm = T))

write.csv(sp4, "prep/spp_trend_esw2018.csv", row.names = F)
write.csv(sp4, "C:/Folders/Dropbox/github/esw_copy/esw2018/layers/spp_trend_esw2018.csv", row.names = F) #to git

###
###


#BD:HAB substrata vs fisheries####

#offshore_hab (GFW data)

cat("\014")
rm(list=ls())

library(rgdal)
library(raster)
library(tidyverse)

options(scipen=999)

gears <- c("drifting_longlines", "fixed_gear", "purse_seines", "squid_jigger", "trawlers")
years <- c("2012", "2013", "2014", "2015", "2016", "2017", "2018")

rgn_sb <- readOGR("C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB/source/shapefiles", "ESW_regions_sb")
#rgn_sb@data
rgn_sb <- spTransform(rgn_sb, CRS("+init=epsg:4326"))
#plot(rgn_sb)

rgn_rr <- readOGR("C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB/source/shapefiles", "ESW_regions_rr")
#rgn_rr@data
rgn_rr <- spTransform(rgn_rr, CRS("+init=epsg:4326"))
#plot(rgn_rr)

z <- data.frame()

for(k in 1:length(gears)){
  gear <- gears[k]
  print(gear)
  
  for(i in 1:length(years)){
    year <- years[i]
    print(year)
    
    f1 <- paste0("C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB/source/shapefiles/fisheries/GFW/", gear, "/", gear, "_", year, ".tif")  
    r1 <- raster(f1)
    proj4string(r1) <- CRS("+init=epsg:4326")
    
    for(j in 1:2){
      if(j == 1){
        rgn_habt <- rgn_rr
        hab_type <- "rocky_reef"
      }
      if(j == 2){
        rgn_habt <- rgn_sb
        hab_type <- "soft_bottom"
      }
      print(hab_type)
      d <- raster::extract(r1, rgn_habt, fun = sum, na.rm = T, sp = T)
      t <- d@data
      colnames(t)[4] <- "Fishing"
      
      t <- t %>% 
        arrange(rgn_id) %>% 
        mutate(Region = rgn_id, Total_Area_km2 = area_km2, Hab_Type = hab_type, Hab_Area_km2 = hab_km2, Year = year, Gear = gear) %>% 
        mutate(Intensity = Fishing / Hab_Area_km2) %>% 
        select(Region, Total_Area_km2, Year, Hab_Type, Hab_Area_km2, Gear, Fishing, Intensity)
      
      z <- rbind(z, t)
    }
  }
}

zz <- z 
#write.csv(zz, "C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB/source/offshore_hab_fish_12nm.csv", row.names = F)

zzz <- zz %>% 
  select(-Intensity) %>% 
  group_by(Region, Year, Gear) %>% 
  summarise(Hab_Area_km2 = sum(Hab_Area_km2), Fishing = sum(Fishing)) %>% 
  mutate(Hab_Type = "all") %>% 
  mutate(Intensity = Fishing / Hab_Area_km2) %>% 
  select(Region, Year, Hab_Type, Hab_Area_km2, Gear, Fishing, Intensity)

write.csv(zzz, "C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB/source/offshore_all_fish_12nm.csv", row.names = F)


###

#inshore_hab (Breen data)

cat("\014")
rm(list=ls())

library(tidyverse)

options(scipen=999)

gears <- c("DREDGING", "LINING", "NETTING", "POTTING", "TRAWLING")

rgn_rr <- read.csv("C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB/source/inshore_fisheries_rr.csv")
rgn_sb <- read.csv("C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB/source/inshore_fisheries_sb.csv")

z <- data.frame()

for(j in 1:2){
  if(j == 1){
    rgn_habt <- rgn_rr
    hab_type <- "rocky_reef"
  }
  if(j == 2){
    rgn_habt <- rgn_sb
    hab_type <- "soft_bottom"
  }
  print(hab_type)
 
for(k in 1:length(gears)){
  gear <- gears[k]
  print(gear)
  
  t <- rgn_habt %>% 
    select(Region = rgn_id, Total_Area_km2 = area_km2, Hab_Area_km2 = hab_km2, Effort = gear) %>% 
    group_by(Region, Total_Area_km2, Hab_Area_km2) %>% 
    summarise(Fishing = sum(Effort, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(Gear = gear, Hab_Type = hab_type) %>%
    mutate(Intensity = Fishing / Hab_Area_km2) %>% 
    select(Region, Total_Area_km2, Hab_Type, Hab_Area_km2, Gear, Fishing, Intensity)
  
    z <- rbind(z, t)
  }
}

zz <- z 
write.csv(zz, "C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB/source/inshore_hab_fish_12nm.csv", row.names = F)

zzz <- zz %>% 
  select(-Intensity) %>% 
  group_by(Region, Gear) %>% 
  summarise(Hab_Area_km2 = sum(Hab_Area_km2), Fishing = sum(Fishing)) %>% 
  mutate(Hab_Type = "all") %>% 
  mutate(Intensity = Fishing / Hab_Area_km2) %>% 
  select(Region, Hab_Type, Hab_Area_km2, Gear, Fishing, Intensity)

write.csv(zzz, "C:/Folders/Dropbox/Projects/OHI/Data/Goals/BD/HAB/source/inshore_all_fish_12nm.csv", row.names = F)


###############
###############
