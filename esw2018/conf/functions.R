## functions.R.
## Each OHI goal model is a separate R function.
## The function name is the 2- or 3- letter code for each goal or subgoal;
## for example, FIS is the Fishing subgoal of Food Provision (FP).

FIS <- function(layers) {
  scen_year <- layers$data$scenario_year
  
  #catch data
  c <- AlignDataYears(layer_nm = "fis_ICESCatchDataset2006_2016_mean_catch", layers_obj = layers) %>%
    select(
      region_id = rgn_id,
      year = scenario_year,
      stock_id,
      taxon_penalty_code,
      catch = mean_catch
    )
  
  #  b_bmsy data
  b <- AlignDataYears(layer_nm = "fis_b_bmsy", layers_obj = layers) %>%
    select(region_id = rgn_id, stock_id, year = scenario_year, bbmsy)
  
  # The following stocks are fished in multiple regions and have high b/bmsy values
  # Due to the underfishing penalty, this actually penalizes the regions that have the highest
  # proportion of catch of these stocks.  The following corrects this problem:
  # tmp <- filter(b, stock_id %in% c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47')) %>%
  #   arrange(stock_id, year) %>%
  #   data.frame()
  #skipjack tuna, atlantic herring, cape horse mackerel, round sardinella, king mackerel
  
  high_bmsy <- c(
    'Katsuwonus_pelamis-71',
    'Clupea_harengus-27',
    'Trachurus_capensis-47',
    'Sardinella_aurita-34',
    'Scomberomorus_cavalla-31'
  )
  
  b <- b %>%
    mutate(bbmsy = ifelse(stock_id %in% high_bmsy &
                            bbmsy > 1, 1, bbmsy))
  
  # general formating:
  c <- c %>%
    mutate(catch = as.numeric(catch)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(region_id = as.numeric(as.character(region_id))) %>%
    mutate(taxon_penalty_code = as.numeric(as.character(taxon_penalty_code))) %>%
    mutate(stock_id = as.character(stock_id)) %>%
    select(region_id, year, stock_id, taxon_penalty_code, catch)
  
  # general formatting:
  b <- b %>%
    mutate(bbmsy = as.numeric(bbmsy)) %>%
    mutate(region_id = as.numeric(as.character(region_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(stock_id = as.character(stock_id))
  
  ####
  # STEP 1. Calculate scores for Bbmsy values
  #SKP NOTE - underfishing high bbmsy stock is penalised ...
  #SKP NOTE - in the next section of code bbmsy values above 1 get reduced to below 1 - larger values get reduced more
  #example:
  #  region_id                      stock_id year     bbmsy     score
  #  1         1 Glyptocephalus_cynoglossus-27 2008 0.8108882 0.8108882
  #  2         1 Lepidorhombus_whiffiagonis-27 2008 0.4560616 0.4560616
  #  3         1   Micromesistius_poutassou-27 2008 1.1341302 0.9579349
  #  4         1                Molva_molva-27 2008 1.2161870 0.9169065
  ####
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05
  
  b$score = ifelse(
    b$bbmsy < lowerBuffer,
    b$bbmsy,
    ifelse (b$bbmsy >= lowerBuffer &
              b$bbmsy <= upperBuffer, 1, NA)
  )
  b$score = ifelse(!is.na(b$score),
                   b$score,
                   ifelse(
                     1 - alpha * (b$bbmsy - upperBuffer) > beta,
                     1 - alpha * (b$bbmsy - upperBuffer),
                     beta
                   ))
  
  ####
  # STEP 1. Merge the b/bmsy data with catch data
  ####
  data_fis <- c %>%
    left_join(b, by = c('region_id', 'stock_id', 'year')) %>%
    select(region_id, stock_id, year, taxon_penalty_code, catch, bbmsy, score)
  
  ###
  # STEP 2. Estimate scores for taxa without b/bmsy values
  # Median score of other fish in the region is the starting point
  # Then a penalty is applied based on the level the taxa are reported at
  ###
  
  ## this takes the median score within each region and year
  data_fis_gf <- data_fis %>%
    group_by(region_id, year) %>%
    mutate(Median_score = quantile(score, probs = c(0.5), na.rm = TRUE)) %>%
    ungroup()
  
  ## this takes the median score across all regions within a year(when no stocks have scores within a region)
  data_fis_gf <- data_fis_gf %>%
    group_by(year) %>%
    mutate(Median_score_global = quantile(score, probs = c(0.5), na.rm =
                                            TRUE)) %>%
    ungroup() %>%
    mutate(Median_score = ifelse(is.na(Median_score), Median_score_global, Median_score)) %>%
    select(-Median_score_global) #remove Median_score_global from the data frame
  
  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************
  
  #  ***********************************************
  #OHI penalty code
  #penaltyTable <- data.frame(TaxonPenaltyCode = 1:6,
  #penalty = c(0.1, 0.25, 0.5, 0.8, 0.9, 1))
  #  ***********************************************
  
  penaltyTable <- data.frame(taxon_penalty_code = c(0,2,5,7),
                             penalty = c(1, 0.9, 0.8, 0.5))
  
  data_fis_gf <- data_fis_gf %>%
    left_join(penaltyTable, by = 'taxon_penalty_code') %>%
    mutate(score_gf = Median_score * penalty) %>%
    mutate(method = ifelse(is.na(score), "Median gapfilled", NA)) %>%
    mutate(gapfilled = ifelse(is.na(score), 1, 0)) %>%
    mutate(score = ifelse(is.na(score), score_gf, score))
  
  #head(data_fis_gf)
  
  gap_fill_data <- data_fis_gf %>%
    select(region_id,
           stock_id,
           taxon_penalty_code,
           year,
           catch,
           score,
           gapfilled,
           method) %>%
    filter(year == scen_year)
  write.csv(gap_fill_data, 'temp/FIS_summary_gf.csv', row.names = FALSE)
  
  status_data <- data_fis_gf %>%
    select(region_id, stock_id, year, catch, score)
  
  ###
  # STEP 4. Calculate status for each region
  ###
  
  # 4a. To calculate the weight (i.e, the relative catch of each stock per region),
  # the mean catch of taxon i is divided by the
  # sum of mean catch of all species in region/year
  
  status_data <- status_data %>%
    group_by(year, region_id) %>%
    mutate(SumCatch = sum(catch)) %>%
    ungroup() %>%
    mutate(wprop = catch / SumCatch)
  
  #stock status scores for all the stocks within a region were averaged using a weighted mean
  #weighted by the relative catch of each stock per region
  #weighted.mean()
  
  #---------------#
  #weighted geometric mean (applied in OHI globl)
  #status_data <- status_data %>%
  #group_by(region_id, year) %>%
  #summarize(status = prod(score ^ wprop)) %>%
  #ungroup()
  #---------------#
  #OR
  #---------------#
  #weighted mean (applied in Arctic)
  status_data <- status_data %>%
    group_by(region_id, year) %>%
    summarize(status = weighted.mean(score, wprop)) %>%
    ungroup()
  #---------------#
  
  ###
  # STEP 5. Get yearly status and trend
  ###
  
  status <-  status_data %>%
    filter(year == scen_year) %>%
    mutate(score = round(status * 100, 1),
           dimension = 'status') %>%
    select(region_id, score, dimension)
  
  # calculate trend
  trend_years <- (scen_year - 4):(scen_year)
  
  trend <- CalculateTrend(status_data = status_data, trend_years = trend_years)
  
  scores <- rbind(status, trend) %>%
    mutate(goal = 'FIS') %>%
    data.frame()
  
  return(scores)
}


MAR <- function(layers) {
  #note - harvest data 1984 to 2010
  scen_year <- layers$data$scenario_year
  
  harvest_tonnes <-
    AlignDataYears(layer_nm = "mar_harvest_tonnes", layers_obj = layers)
  
  sustainability_score <-
    AlignDataYears(layer_nm = "mar_sustainability_score", layers_obj = layers)
  
  coastal_length <-
    AlignDataYears(layer_nm = "mar_coastal_length", layers_obj = layers)
  
  rky <-  harvest_tonnes %>%
    left_join(sustainability_score,
              by = c('rgn_id', 'taxa_code', 'scenario_year')) %>%
    select(rgn_id, scenario_year, taxa_code, rgn_prod_T, wq_rg_sp)
  
  #remove nas
  rky <- na.omit(rky)
  
  #remove zero harvest
  rky <- rky[rky$rgn_prod_T > 0,]

  m <- rky

  #apply sustainability coeficient derived from water quality monitoring at mariculture sites 
  m <- m %>%
    mutate(sust_tonnes = rgn_prod_T * wq_rg_sp)
  
  #aggregate per region, and divide by region coastal length
  ry = m %>%
    group_by(rgn_id, scenario_year) %>%
    summarize(sust_tonnes_sum = sum(sust_tonnes, na.rm = TRUE)) %>%  #na.rm = TRUE assumes that NA values are 0
    left_join(coastal_length, by = c('rgn_id', 'scenario_year')) %>%
    mutate(mar_cst = sust_tonnes_sum / c_lngth_km) %>%
    ungroup()
  
  #extract last 5 years of data (note: data not updated after 2010 so data years: 2006-2010)
  ry <- ry %>% 
    filter(scenario_year >= 2006 & scenario_year <= 2010)
  
  #get reference quantile based on max regional value for last 5 years data 
  ref_99pct <- quantile(ry$mar_cst, 0.99, na.rm = TRUE)
  
  ry = ry %>%
    mutate(status = ifelse(mar_cst / ref_99pct > 1, 1, mar_cst / ref_99pct))
  
  status <- ry %>%
    filter(scenario_year == 2010) %>% #2010 - last year of data
    mutate(dimension = "status") %>%
    select(region_id = rgn_id, score = status, dimension) %>%
    mutate(score = round(score * 100, 2))
  
  #add zero for IOS region 4 - no mariculture
  status <- rbind(status, c(4, 0, "status"))
  status <- status[order(status$region_id),]
  
  #calculate trend
  #need to calculate trend on data prior to 2010
  trend_years <- (2010 - 4):(2010)
  
  trend <- CalculateTrend(status_data = ry, trend_years = trend_years)
  
  #add zero for IOS region 4 - no mariculture
  trend <- rbind(trend, c(4, 0, "trend"))
  trend <- trend[order(trend$region_id),]
  
  #general formating
  status$region_id <- as.numeric(status$region_id)
  status$score<- as.numeric(status$score)
  trend$region_id <- as.numeric(trend$region_id)
  trend$score<- as.numeric(trend$score)
  
  # return scores
  scores = rbind(status, trend) %>%
    mutate(goal = 'MAR') %>%
    data.frame()

  return(scores)
}


FP <- function(layers, scores) {
  scen_year <- layers$data$scenario_year
  
  w <-
    AlignDataYears(layer_nm = "fp_wildcaught_weight", layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, w_fis)
  
  # scores
  s <- scores %>%
    filter(goal %in% c('FIS', 'MAR')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    left_join(w, by = "region_id")  %>%
    mutate(w_mar = 1 - w_fis) %>%
    mutate(weight = ifelse(goal == "FIS", w_fis, w_mar))
  
  
  ## Some warning messages due to potential mismatches in data:
  # NA score but there is a weight
  tmp <-
    filter(s,
           goal == 'FIS' &
             is.na(score) & (!is.na(w_fis) & w_fis != 0) & dimension == "score")
  if (dim(tmp)[1] > 0) {
    warning(paste0(
      "Check: these regions have a FIS weight but no score: ",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }
  
  tmp <-
    filter(s,
           goal == 'MAR' &
             is.na(score) & (!is.na(w_mar) & w_fis != 0) & dimension == "score")
  if (dim(tmp)[1] > 0) {
    warning(paste0(
      "Check: these regions have a MAR weight but no score: ",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }
  
  # score, but the weight is NA or 0
  tmp <-
    filter(
      s,
      goal == 'FIS' &
        (!is.na(score) &
           score > 0) &
        (is.na(w_fis) | w_fis == 0) & dimension == "score" & region_id != 0
    )
  if (dim(tmp)[1] > 0) {
    warning(paste0(
      "Check: these regions have a FIS score but no weight: ",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }
  
  tmp <-
    filter(
      s,
      goal == 'MAR' &
        (!is.na(score) &
           score > 0) &
        (is.na(w_mar) | w_mar == 0) & dimension == "score" & region_id != 0
    )
  if (dim(tmp)[1] > 0) {
    warning(paste0(
      "Check: these regions have a MAR score but no weight: ",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }
  
  s <- s  %>%
    group_by(region_id, dimension) %>%
    summarize(score = weighted.mean(score, weight, na.rm = TRUE)) %>%
    mutate(goal = "FP") %>%
    ungroup() %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()
  
  # return all scores
  return(rbind(scores, s))
}


AO <- function(layers) {
  
  #calculate status scores for 4 inputs
  #effort - to add - trend?
  #trend for fleet size and fuel
  #tend 0 for access
  
  scen_year <- layers$data$scenario_year
  
  #access - no need to rescale 0-1 (100) as already a percentage of accessible coast
  access_status <- AlignDataYears(layer_nm = "ao_access", layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id = rgn_id, score = access_perc, dimension)
  
  region_id <- c(1:6)
  score <- 0
  dimension <- "trend"
  access_trend <- data.frame(region_id, score, dimension)
  
  #fleet size
  fleet <- AlignDataYears(layer_nm = "ao_fleet_size", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = boats, scenario_year)
  
  #select last 5 years of data - rescale - select current year for status
  fleet_status <- fleet %>%
    filter(scenario_year <= scen_year & scenario_year >= scen_year - 4) %>%
    mutate(dimension = "status") %>%
    mutate(score = (status / max(status)) * 100) %>% 
    filter(scenario_year == scen_year) %>% 
    dplyr::select(region_id, score, dimension)
  
  trend_years <- (scen_year - 4):(scen_year)
  fleet_trend <- CalculateTrend(status_data = fleet, trend_years = trend_years)
  
  #red diesel
  red <- AlignDataYears(layer_nm = "ao_fuel_cost", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = cost_ppl, scenario_year)
  
  #select last 5 years of data - rescale and invert (high score = low fuel price) - select current year for status
  red_status <- red %>%
    filter(scenario_year <= scen_year & scenario_year >= scen_year - 4) %>%
    mutate(status = (1 - status/max(status)) * 100) %>% 
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score = status, dimension)
  
  trend_years <- (scen_year - 4):(scen_year)
  red_trend <- CalculateTrend(status_data = red, trend_years = trend_years)
  
  #effort - restricted to data years 2012 - 2016
  #higher value indicates less effort to catch more fish
  effort <- AlignDataYears(layer_nm = "ao_effort_catch", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = effort_tph, scenario_year)
  status2 <- na.omit(effort$status) #omit NAs
  ref <- max(status2)
   
  #select last 5 years of data (all data as restricted to data years 2010-2016) - rescale - select current year for status
  effort_status <- effort %>%
    mutate(dimension = "status") %>%
    mutate(score = (status / ref) * 100) %>%
    filter(scenario_year == scen_year) %>% 
    dplyr::select(region_id, score, dimension)
  
  #trend_years <- (scen_year - 4):(scen_year)
  effort_trend <- CalculateTrend(status_data = effort, trend_years = trend_years)
  
  #bring status and trend scores together
  status <- rbind (access_status, fleet_status, red_status, effort_status) 
  
  status <- status %>%
    group_by(region_id) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>% 
    mutate(dimension = "status")%>%
    ungroup()
  
  trend <- rbind(access_trend, fleet_trend, red_trend, effort_trend)
  
  trend <- trend %>%
    group_by(region_id) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>% 
    mutate(dimension = "trend") %>%
    ungroup()
  
  ao_scores <-  rbind(status, trend) %>%
    mutate('goal'='AO') %>%
    select(goal, dimension, region_id, score) %>%
    data.frame()

  return(ao_scores)
}


NP <- function(scores, layers) {
  scen_year <- layers$data$scenario_year
  
  np_status <- AlignDataYears(layer_nm = "np_status", layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, score = status) %>%
    mutate(dimension = 'status', goal = 'NP')
  
  np_trend <- AlignDataYears(layer_nm = "np_trend", layers_obj = layers) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, score = trend) %>%
    mutate(dimension = 'trend', goal = 'NP')

  # return scores
  np_scores <- rbind(np_status, np_trend)
  return(np_scores)
}


CS <- function(layers) {
  scen_year <- layers$data$scenario_year

  # layers for carbon storage
  extent_lyrs <-
    c(
      'hab_seagrass_extent',
      'hab_saltmarsh_extent'
    )
  #prop change analysis
  #health_lyrs <-
    #c(
      #'hab_seagrass_health',
      #'hab_saltmarsh_health_prop_chng'
    #)
  #current year only analysis
  health_lyrs <-
    c(
      'hab_seagrass_health',
      'hab_saltmarsh_health'
    )
  #get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, extent = km2) %>%
    mutate(habitat = as.character(habitat))
  
  health <- AlignManyDataYears(health_lyrs) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))
  
  #join extent and health
  d <-  extent %>%
    full_join(health, by = c("region_id", "habitat")) 
  
  #assigning ranks penalises regions that naturally lack one habitat 
  #(ie rgn 1 - no seagrass meadows due to natural water turbidity, rgn 4 limited saltmarsh extent due to restrictive area of islands)
  
  #set ranks for each habitat
  #only used for pressure layer
  habitat.rank <- c('saltmarsh' = 210,
                    'seagrass' = 83)  
  
  #proportionalise habitat health scores by relative area of habitat extent (seagrass, saltmarsh) and sum
  d <- d %>% 
    group_by(region_id) %>% 
    mutate(extent_tot = sum(extent, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(extent_prop = extent / extent_tot) %>%
    mutate(health_prop = health * extent_prop)
  
  #status
  status_CS <- d %>%
    group_by(region_id) %>%
    summarize(score = sum(health_prop, na.rm = TRUE) * 100) %>%
    mutate(dimension = 'status')

  #trend
  #trend calculation for saltmarsh
  #trend read in for seagrass
  
  sm_health <- AlignDataYears(layer_nm = "hab_saltmarsh_health", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = health, scenario_year)
  sm_health <- sm_health[order(sm_health$region_id, sm_health$scenario_year),]
  trend_years <- (scen_year-4):(scen_year)
  sm_trend <- CalculateTrend(status_data = sm_health, trend_years = trend_years)
  sm_trend <- sm_trend %>% 
    mutate(habitat = "saltmarsh") %>% 
    select(region_id, habitat, trend = score)
  
  sg_trend <- AlignDataYears(layer_nm = "hab_seagrass_trend", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, trend, scenario_year) %>% 
    filter(scenario_year == scen_year) %>% 
    mutate(habitat = "seagrass") %>% 
    select(region_id, habitat, trend)
  
  hab_trend <- rbind(sm_trend, sg_trend)
  d <- left_join(d, hab_trend, c("region_id", "habitat"))
  d$trend_prop <- d$trend * d$extent_prop

  #trend
  trend_CS <- d %>%
    group_by(region_id) %>%
    summarize(score = sum(trend_prop, na.rm = TRUE)) %>%
    mutate(dimension = "trend") 
  
  #finalize scores_CS
  scores_CS <- rbind(status_CS, trend_CS) %>%
    mutate(goal = 'CS') %>%
    select(region_id, goal, dimension, score)

  #create weights file for pressures/resilience calculations
  weights <- extent %>%
    filter(extent > 0) %>%
    mutate(rank = habitat.rank[habitat]) %>%
    mutate(extent_rank = extent * rank) %>%
    mutate(layer = "element_wts_cs_km2_x_storage") %>%
    select(rgn_id = region_id, habitat, extent_rank, layer)

  write.csv(
    weights,
    sprintf("temp/element_wts_cs_km2_x_storage_%s.csv", scen_year),
    row.names = FALSE
  )

  layers$data$element_wts_cs_km2_x_storage <- weights

  # return scores
  return(scores_CS)
}


CP <- function(layers) {
  
scen_year <- layers$data$scenario_year

  #layers for coastal protection
  extent_lyrs <-
    c(
      'hab_seagrass_extent',
      'hab_saltmarsh_extent',
      'hab_sand_dune_extent'
    )
  #prop change analysis
  #health_lyrs <-
    #c(
      #'hab_seagrass_health',
      #'hab_saltmarsh_health_prop_chng',
      #'hab_sand_dune_health_prop_chng'
    #)
  #current year only analysis
  health_lyrs <-
    c(
      'hab_seagrass_health',
      'hab_saltmarsh_health',
      'hab_sand_dune_health'
    )
  #get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, extent = km2) %>%
    mutate(habitat = as.character(habitat))

  health <- AlignManyDataYears(health_lyrs) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  #join extent and health
  d <-  extent %>%
    full_join(health, by = c("region_id", "habitat")) 

  #set ranks for each habitat
  #only used for pressure layer
  habitat.rank <- c(
    'sand dune' = 3,
    'saltmarsh' = 3,
    'seagrass' = 1
  )

  #proportionalise habitat health scores by relative area of habitat extent (seagrass, saltmarsh) and sum
  d <- d %>% 
    group_by(region_id) %>% 
    mutate(extent_tot = sum(extent, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(extent_prop = extent / extent_tot) %>%
    mutate(health_prop = health * extent_prop)
  
  #status
  status_CP <- d %>%
    group_by(region_id) %>%
    summarize(score = round(sum(health_prop, na.rm = TRUE) * 100)) %>%
    mutate(dimension = 'status')
  
  #trend
  #trend calculation for saltmarsh and sand dune condition
  #trend read in for seagrass (this should be updated)
  
  sm_health <- AlignDataYears(layer_nm = "hab_saltmarsh_health", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = health, scenario_year)
  sm_health <- sm_health[order(sm_health$region_id, sm_health$scenario_year),]
  trend_years <- (scen_year-4):(scen_year)
  sm_trend <- CalculateTrend(status_data = sm_health, trend_years = trend_years)
  sm_trend <- sm_trend %>% 
    mutate(habitat = "saltmarsh") %>% 
    select(region_id, habitat, trend = score)
  
  sd_health <- AlignDataYears(layer_nm = "hab_sand_dune_health", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = health, scenario_year)
  sd_health <- sd_health[order(sd_health$region_id, sd_health$scenario_year),]
  trend_years <- (scen_year-4):(scen_year)
  sd_trend <- CalculateTrend(status_data = sd_health, trend_years = trend_years)
  sd_trend <- sd_trend %>% 
    mutate(habitat = "sand dune") %>% 
    select(region_id, habitat, trend = score)
  
  sg_trend <- AlignDataYears(layer_nm = "hab_seagrass_trend", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, trend, scenario_year) %>% 
    filter(scenario_year == scen_year) %>% 
    mutate(habitat = "seagrass") %>% 
    select(region_id, habitat, trend)
  
  hab_trend <- rbind(sm_trend, sd_trend, sg_trend)
  d <- left_join(d, hab_trend, c("region_id", "habitat"))
  d$trend_prop <- d$trend * d$extent_prop
  
  #trend
  trend_CP <- d %>%
    group_by(region_id) %>%
    summarize(score = sum(trend_prop, na.rm = TRUE)) %>%
    mutate(dimension = "trend")  
  
  #finalize scores_CP
  scores_CP <- rbind(status_CP, trend_CP) %>%
    mutate(goal = 'CP') %>%
    select(region_id, goal, dimension, score)

  #create weights file for pressures/resilience calculations
  weights <- extent %>%
    filter(extent > 0) %>%
    mutate(rank = habitat.rank[habitat]) %>%
    mutate(extent_rank = extent * rank) %>%
    mutate(layer = "element_wts_cp_km2_x_protection") %>%
    select(rgn_id = region_id, habitat, extent_rank, layer)

  write.csv(
    weights,
    sprintf("temp/element_wts_cp_km2_x_protection_%s.csv", scen_year),
    row.names = FALSE
  )

  layers$data$element_wts_cp_km2_x_protection <- weights

  #return scores
  return(scores_CP)

}


TR <- function(layers) {
  
  scen_year <- layers$data$scenario_year

#tourism
  #read in layers
  #note data not available after 2014 (scenario year 2015) - so trend needs to be calculated 2011 - 2015
  tourism <- AlignDataYears(layer_nm = "tr_ons_1km", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, tr_ons = ons, scenario_year)
  tourism <- tourism[order(tourism$region_id, tourism$scenario_year),]

  sustain <- AlignDataYears(layer_nm = "tr_ttci", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, TTCI, scenario_year)
  sustain <- sustain[order(sustain$region_id, sustain$scenario_year),]
  
  #disable TTCI score
  #set sustainabilty score to 1 for all regions
  
  tr_data  <- full_join(tourism, sustain, by = c('region_id', 'scenario_year'))
  
  tr_model <- tr_data %>%
    mutate(tr = tr_ons,
           s = (TTCI) / (6),
           #scale TTCI score 0 - 1
           #str = tr * s) #disable sustainability calculation
           str = tr)

  #read in viewshed data
  rgn_sv <- AlignDataYears(layer_nm = "tr_viewshed", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, perc_sv, scenario_year)

  #incorporate viewshed data to model 
  #remove NAs
  tr_model <- tr_model %>%
    left_join(rgn_sv, by = c('region_id', 'scenario_year')) %>%
    na.omit()
  tr_model$str_vs <- tr_model$str * (tr_model$perc_sv /100)
  
  #select data for last 5 years of data (2011 to 2015 (data years 2010 to 2014))
  tr_model <- tr_model %>% 
    filter(scenario_year >= 2011 & scenario_year <= 2015)

  #rescale data 0-1 using 99th quantile of all data - as per China and Global model
  q <- quantile(tr_model$str_vs, 0.99, na.rm = T)
  fun = function(x){ifelse(x>q, 1, x/q)}
  tr_model$status <- as.numeric(lapply(tr_model$str_vs, fun))
  
  #get status - select senario year 2015 as most current data values (data not updated after 2014)
  status_1 <- tr_model %>%
    filter(scenario_year == 2015) %>% 
    select(region_id = region_id, score = status) %>%
    mutate(score = score * 100) %>%
    mutate(dimension = 'status')

  #calculate trend
  trend_years <- 2011:2015
  trend_1 <- CalculateTrend(status_data = tr_model, trend_years = trend_years)

#recreation
  #read in modelled recreation data (status)
  status_2 <- AlignDataYears(layer_nm = "tr_rec", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, score = mean_tscore, scenario_year) %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>%
    select(-scenario_year)
  
  #read in tourism and population data (ONS) calculate trend and aggregate (mean)
  tour <- AlignDataYears(layer_nm = "tr_ons_all_LA", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = o_night_k, scenario_year) 
  tour <- tour[order(tour$region_id, tour$scenario_year),]
  
  popn <- AlignDataYears(layer_nm = "tr_popn_LA", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = pop_n, scenario_year) 
  popn <- popn[order(popn$region_id, popn$scenario_year),]
  
  trend_years <- 2011:2015 #tourism data limited to 2015
  tour_trend <- CalculateTrend(status_data = tour, trend_years = trend_years)
  
  trend_years <- (scen_year - 4):(scen_year)
  popn_trend <- CalculateTrend(status_data = popn, trend_years = trend_years)
  
  trend_2 <- left_join(tour_trend, popn_trend, by = "region_id")
  trend_2$score <- apply(cbind(trend_2$score.x, trend_2$score.y), 1, mean,  na.rm = T)
  trend_2 <- dplyr::select(trend_2, "region_id", "score") %>%
    mutate(dimension = "trend")

  #aggregate status_1 and status_2
  tr_status <- left_join(status_1, status_2, by = "region_id")
  tr_status$score <- apply(cbind(tr_status$score.x, tr_status$score.y), 1, mean,  na.rm = T)
  tr_status <- dplyr::select(tr_status, "region_id", "score") %>%
    mutate(dimension = "status")
  
  #aggregate trend_1 and trend_2
  tr_trend <- left_join(trend_1, trend_2, by = "region_id")
  tr_trend$score <- apply(cbind(tr_trend$score.x, tr_trend$score.y), 1, mean,  na.rm = T)
  tr_trend <- dplyr::select(tr_trend, "region_id", "score") %>%
    mutate(dimension = "trend")
  
  # bind status and trend by rows
  tr_score <- bind_rows(tr_status, tr_trend) %>%
    mutate(goal = 'TR')
  
  # return final scores
  scores <- tr_score %>%
    select(region_id, goal, dimension, score)

  return(scores)
}


LIV <- function(layers){
  
  scen_year <- layers$data$scenario_year
  
  wages <- AlignDataYears(layer_nm = "le_wage", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, wages_gbp = wages_gbp, scenario_year)
  
  jobs <- AlignDataYears(layer_nm = "le_jobs", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, mar_jobs = mar_jobs, scenario_year)

  liv <- jobs %>%
    left_join(wages, by=c('region_id','scenario_year')) %>%
    arrange(scenario_year, region_id)

  # LIV status
  #jobs reference: temporal comparison (by region) using a 4 year (due to data restrictions) moving window
  #value in the current year (scenario year) relative to the value in a recent moving reference period
  #a score of 100 would indicate no loss of jobs relative to 4 years previous
  #wages reference: moving temporal reference: max regional wages per year

  liv_status <- liv %>%
    arrange(region_id, scenario_year) %>%
    group_by(region_id) %>%
      mutate(mar_jobs_ref  = dplyr::lag(mar_jobs, 4)) %>%
      ungroup() %>%
      group_by(scenario_year) %>%
      mutate(wages_gbp_ref = max(wages_gbp)) %>%
      ungroup() %>%
      mutate(
        x_jobs  = pmax(-1, pmin(1, mar_jobs / mar_jobs_ref)),
        x_wages = pmax(-1, pmin(1, wages_gbp / wages_gbp_ref)))
        
  #calculate mean status score (jobs and wages) - by year and region
  liv_status$status <- apply(cbind(liv_status$x_jobs, liv_status$x_wages), 1, mean, na.rm = T) * 100 

  # filter for most recent year
  status <- liv_status %>%
      filter(scenario_year == scen_year) %>%
      dplyr::select(region_id, score = status) %>%
      mutate(dimension = 'status', goal = 'LIV')

  # LIV trend
  if (scen_year == 2018) {trend_years <- (scen_year - 4):(scen_year)} #2018 function - calculate trend over 5 years
  if (scen_year == 2017) {trend_years <- (scen_year - 3):(scen_year)} #due to data limitations trend for 2017 calculated over 4 years 
  trend <- CalculateTrend(status_data = liv_status, trend_years = trend_years)
  trend <- trend %>%
    mutate(goal = "LIV")

  ## create scores and rbind to other goal scores
  scores = rbind(status, trend) %>%
    dplyr::select(region_id, score, dimension, goal)

  return(scores)

}


ECO <- function(layers){
  
  scen_year <- layers$data$scenario_year
  
  #regional marine related GVA
  #data not updated after from 2015
  revn <- AlignDataYears(layer_nm = "le_gva", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, gva = rev_gbp_M, scenario_year)
  revn <- revn %>%
    filter(scenario_year <= 2015)
  
  jobs <- AlignDataYears(layer_nm = "le_jobs", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, mar_jobs = mar_jobs, scenario_year)
  
  #calculate gva per capita
  rev_cap <- left_join(revn, jobs, by = c("region_id", "scenario_year"))
  rev_cap$gva_per_cap <- rev_cap$gva / rev_cap$mar_jobs
  
  #ECO status
  #assess economic productivity (per capita) by year across regions
  #moving temporal reference: reference point max regional gva per capita per year
  rev_cap <- rev_cap %>%
    group_by(scenario_year) %>%
    mutate(ref = max(gva_per_cap)) %>%
    ungroup() %>%
    mutate(status = (gva_per_cap / ref) * 100) %>%
    arrange(region_id, scenario_year)

  #filter for most recent year
  status <- rev_cap %>%
    filter(scenario_year == max(scenario_year)) %>%
    # format
    dplyr::select(region_id, score = status) %>%
    mutate(dimension = 'status', goal = 'ECO')

  #ECO trend
  trend_years <- (max(rev_cap$scenario_year) - 4) : (max(rev_cap$scenario_year))
  trend <- CalculateTrend(status_data = rev_cap, trend_years = trend_years)
  trend <- trend %>%
    mutate(goal = "ECO")
  
  #rbind status and trend
  scores = rbind(status, trend) %>%
    dplyr::select(region_id, score, dimension, goal)

  return(scores)

}

LE <- function(scores, layers){

  # calculate LE scores
  scores.LE = scores %>%
    filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
    spread(key = goal, value = score) %>%
    mutate(score = rowMeans(cbind(ECO, LIV), na.rm=TRUE)) %>%
    select(region_id, dimension, score) %>%
    mutate(goal  = 'LE')

  # rbind to all scores
  scores = scores %>%
    rbind(scores.LE)

  # return scores
  return(scores)
}


ICO <- function(layers){
  
  scen_year <- layers$data$scenario_year

  rk <- AlignDataYears(layer_nm="ico_spp_iucn_status", layers_obj = layers) %>%
    select(region_id = rgn_id, species, iucn_cat = category, scenario_year, ico_spp_iucn_status_year, pmt, wildlife_trusts) %>%
    mutate(iucn_cat = as.character(iucn_cat)) %>%
    mutate(region_id = as.numeric(region_id))
  
#unique(rk$iucn_cat)
#[1] "VU"    "LR/lc" "LR/nt" "LR/cd" "DD"    "LC"    "NT"   
  
  #tidy IUCN clasifications
  rk$iucn_cat[rk$iucn_cat == "LR/lc"] <- "LC"
  rk$iucn_cat[rk$iucn_cat == "LR/nt"] <- "NT"
  rk$iucn_cat[rk$iucn_cat == "LR/cd"] <- "CD"
  
  #extract sub-set of data
  #wildlife tusts only
  #IUCN clasifications: VU (0.4), LC (0), NT (0.2), CD (0.3),  DD (NA)
                         
  rk <- filter(rk, wildlife_trusts == 1)
  
  #wildlife trust and postcard or magnet or tour operator
  #wildlife trust and 1 other (this only retains IUCN categories: LC (scores 1) or DD (scores NA)
  #score = 100 trend = 0
                              
  #rk <- filter(rk, c(wildlife_trusts == 1 & pmt == 1))
  
  # lookup for weights status
  #  LC <- "LOWER RISK/LEAST CONCERN (LR/LC)"
  #  NT <- "LOWER RISK/NEAR THREATENED (LR/NT)"
  #  T  <- "THREATENED (T)" treat as "EN"
  #  VU <- "VULNERABLE (V)"
  #  EN <- "ENDANGERED (E)"
  #  CD <- "LOWER RISK/CONSERVATION DEPENDENT (LR/CD)" treat as between VU and NT
  #  CR <- "VERY RARE AND BELIEVED TO BE DECREASING IN NUMBERS"
  #  DD <- "INSUFFICIENTLY KNOWN (K)"
  #  DD <- "INDETERMINATE (I)"
  #  DD <- "STATUS INADEQUATELY KNOWN-SURVEY REQUIRED OR DATA SOUGHT"
  w.risk_category = data.frame(iucn_cat = c('LC', 'NT', 'CD', 'VU', 'EN', 'CR', 'EX', 'DD'),
                               risk_score = c(0,  0.2,  0.3,  0.4,  0.6,  0.8,  1, NA)) %>%
    mutate(status_score = 1-risk_score) %>%
    mutate(iucn_cat = as.character(iucn_cat))
  

  ####### status
  # STEP 1: take mean of subpopulation scores
  r.status_spp <- rk %>%
    left_join(w.risk_category, by = 'iucn_cat') %>%
    group_by(region_id, species, scenario_year, ico_spp_iucn_status_year) %>%
    summarize(spp_mean = mean(status_score, na.rm=TRUE)) %>%
    ungroup()

  # STEP 2: take mean of populations within regions
  r.status <- r.status_spp %>%
    group_by(region_id, scenario_year, ico_spp_iucn_status_year) %>%
    summarize(status = mean(spp_mean, na.rm=TRUE)) %>%
    ungroup()

  ####### status
  status <- r.status %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    select(region_id, score, dimension)

  ####### trend
  trend_years <- (scen_year-9):(scen_year)

  trend <- CalculateTrend(status_data = r.status, trend_years=trend_years)

  # return scores
  scores <-  rbind(status, trend) %>%
    mutate('goal'='ICO') %>%
    select(region_id, score, goal, dimension) %>%
    data.frame()
  
  return(scores)

}


LSP <- function(layers) {
  
  scen_year <- layers$data$scenario_year

  ref_pct_cmpa <- 100 #re-scaling factor - 100% of coast
  ref_pct_cpa <- 100 #re-scaling factor - 100% of coast

  # select data
  total_area <-
    rbind(layers$data$rgn_area_inland1km,
          layers$data$rgn_area_offshore3nm) %>% #total offshore/inland areas
    select(region_id = rgn_id, area, layer) %>%
    mutate(region_id = as.numeric(region_id)) %>%
    spread(layer, area) %>%
    select(region_id,
           area_inland1km = rgn_area_inland1km,
           area_offshore3nm = rgn_area_offshore3nm)

  offshore <-
    AlignDataYears(layer_nm = "lsp_prot_area_offshore3nm", layers_obj = layers) %>%
    select(region_id = rgn_id,
           year = scenario_year,
           cmpa = area_km2_m_gf)
  inland <-
    AlignDataYears(layer_nm = "lsp_prot_area_inland1km", layers_obj = layers) %>%
    select(region_id = rgn_id,
           year = scenario_year,
           cpa = area_km2_t_gf)

  lsp_data <- full_join(offshore, inland, by = c("region_id", "year"))

  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
  # and calculate status score
  status_data <- lsp_data %>%
    full_join(total_area, by = "region_id") %>%
    arrange(region_id, year) %>%
    mutate(
      pct_cpa = cpa / area_inland1km * 100,
      pct_cmpa = cmpa / area_offshore3nm * 100,
      status = (pmin(pct_cpa / ref_pct_cpa, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1)) / 2
    )

  #status
  status <- status_data %>%
    filter(year == scen_year) %>%
    mutate(score = status * 100) %>%
    select(region_id, score) %>%
    mutate(dimension = "status")

  # calculate trend
  trend_years <- (scen_year - 4):(scen_year)
  trend <- CalculateTrend(status_data = status_data, trend_years = trend_years)

  # return scores
  scores <- bind_rows(status, trend) %>%
    mutate(goal = "LSP") %>%
    data.frame()
  
  return(scores)
  #return(scores[, c('region_id', 'goal', 'dimension', 'score')])
}

SP <- function(scores) {
  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP
  s <- scores %>%
    filter(goal %in% c('ICO', 'LSP'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "SP") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


CW <- function(layers) {
  
  #data:
  #cw_bathing_water_class
  #cw_inorganic_run_off
  #cw_nutrients
  #cw_pesticides
  #cw_sus_material
  #cw_trash
  #cw_vessel_pol
  
  #cw_trash_trend
  
  scen_year <- layers$data$scenario_year

  ### function to calculate geometric mean:
  #geometric.mean2 <- function (x, na.rm = TRUE) {
    #if (is.null(nrow(x))) {
      #exp(mean(log(x), na.rm = TRUE))
    #}
    #else {
      #exp(apply(log(x), 2, mean, na.rm = na.rm))
    #}
  #}
  
  #pesticides
  pest <- AlignDataYears(layer_nm = "cw_pesticides", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = pesticides, scenario_year)
  
  pest <- pest %>%
    filter(scenario_year <= scen_year & scenario_year >= scen_year - 4)
  #rescale data 0-1 using 99th quantile of all data
  q <- quantile(pest$status, 0.99, na.rm = T)
  fun = function(x){ifelse(x>q, 1, x/q)}
  pest$status <- as.numeric(lapply(pest$status, fun))
  
  #high pesticide use = 1 low pesticide use = 0
  #needs inverting so
  #low pesticide = 1 high pesticide = 0
  pest$status <- 1 - pest$status
  
  pest_status <- pest %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score, dimension)
  
  trend_years <- (scen_year - 4):(scen_year)
  pest_trend <- CalculateTrend(status_data = pest, trend_years = trend_years)
  
  #inorganic run-off
  run_off <- AlignDataYears(layer_nm = "cw_inorganic_run_off2", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = inorg_run_off, scenario_year)
  
  run_off <- run_off %>%
    filter(scenario_year <= scen_year & scenario_year >= scen_year - 4)
  
  #rescale data 0-1 using 99th quantile of all data
  q <- quantile(run_off$status, 0.99, na.rm=T)
  fun = function(x){ifelse(x>q, 1, x/q)}
  run_off$status <- as.numeric(lapply(run_off$status, fun))
  
  #high rain fall (run-off) = 1 low = 0
  #needs inverting
  #low  = 1 high  = 0
  run_off$status <- 1 - run_off$status
  
  run_off_status <- run_off %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score, dimension)
  
  trend_years <- (scen_year - 4):(scen_year)
  run_off_trend <- CalculateTrend(status_data = run_off, trend_years = trend_years)

  #nutrients
  nut <- AlignDataYears(layer_nm = "cw_nutrients", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = nutrients, scenario_year)
  
  nut <- nut %>%
    filter(scenario_year <= scen_year & scenario_year >= scen_year - 4)
  
  #function to rescale SW coastal data 0-1 using 99th quantile of all data
  q <- quantile(nut$status, 0.99, na.rm = T)
  fun = function(x){ifelse(x>q, 1, x/q)}
  nut$status <- as.numeric(lapply(nut$status, fun))
  #high fertilizer use = 1 low fertilizer use = 0
  #needs inverting
  #low fertiliser = 1 high fertiliser = 0
  nut$status <- 1 - nut$status

  nut_status <- nut %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score, dimension)
  
  trend_years <- (scen_year - 4):(scen_year)
  nut_trend <- CalculateTrend(status_data = nut, trend_years = trend_years)
  
  #bathing water classification
  #calculate mean beach status by region_id & year
  bw <- AlignDataYears(layer_nm = "cw_bathing_water_class", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = beach_status_score, scenario_year)
  bw <- bw %>%
    group_by(region_id, scenario_year) %>%
    summarise(status = mean (status, na.rm=TRUE)) %>%
    ungroup()
  
  #no data for IOS (region 4)
  #assign adjacent region data (Cornwall: region 3) to IOS
  crn <- bw[bw$region_id == 3,]
  crn$region_id <- 4
  bw <- rbind(bw, crn)
  bw <- bw[order (bw$region_id),]
  
  bw_status <- bw %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score, dimension)
  
  trend_years <- (scen_year - 9):(scen_year)
  bw_trend <- CalculateTrend(status_data = bw, trend_years = trend_years)
  
  #water quality - suspended material
  wq <- AlignDataYears(layer_nm = "cw_sus_material", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = sus_mat, scenario_year)
  
  wq <- wq %>%
    filter(scenario_year <= scen_year & scenario_year >= scen_year - 4)
  
  #rescale data 0-1 using 99th quantile of all data
  q <- quantile(wq$status,0.99, na.rm = T)
  fun = function(x){ifelse(x>q, 1, x/q)}
  wq$status <- as.numeric(lapply(wq$status, fun))
  
  #high suspended material = 1 low = 0
  #needs inverting
  #low = 1 high = 0
  wq$status <- 1 - wq$status
  
  wq_status <- wq %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score, dimension)
  
  trend_years <- (scen_year - 4):(scen_year)
  wq_trend <- CalculateTrend(status_data = wq, trend_years = trend_years)
  
  #plastics at sea 'trash'
  #note no trend data - read in modelled trend from ohi global data
  trash <- AlignDataYears(layer_nm = "cw_trash", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = trash, scenario_year)
  
  trash_status <- trash %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score, dimension)
  
  trash_trend <- AlignDataYears(layer_nm = "cw_trash_trend", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, score = trend, scenario_year) %>%
    filter(scenario_year == scen_year)  %>%
    mutate(dimension = "trend") %>%
    dplyr::select(region_id, score, dimension)
  
  #pollution from vesels
  ves <- AlignDataYears(layer_nm = "cw_vessel_pol", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = vessel_pol, scenario_year)
  
  ves_status <- ves %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score, dimension)
  
  trend_years <- (2011:2015)
  ves_trend <- CalculateTrend(status_data = ves, trend_years = trend_years)

  #bring status and trend scores together
  status <- rbind (bw_status, nut_status, pest_status, run_off_status, trash_status, wq_status, ves_status) 
  
  status <- status %>%
    group_by(region_id) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>% 
    mutate(dimension = "status")%>%
    ungroup()
  
  trend <- rbind(bw_trend, nut_trend, pest_trend, run_off_trend, trash_trend, wq_trend, ves_trend)
  
  trend <- trend %>%
    group_by(region_id) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>% 
    mutate(dimension = "trend") %>%
    ungroup()
  
  scores <-  rbind(status, trend) %>%
    mutate('goal'='CW') %>%
    select(goal, dimension, region_id, score) %>%
    data.frame()

  return(scores)
}

HAB <- function(layers) {
  
  scen_year <- layers$data$scenario_year

  #note: extent not used to calculate HAB goal score - but used to determine if health is included by rgn
  extent_lyrs <-
    c(
      'hab_seagrass_extent',
      'hab_saltmarsh_extent',
      'hab_ccms_extent'
    )
  
  health_lyrs <-
    c(
      'hab_seagrass_health',
      'hab_saltmarsh_health',
      'hab_ccms_trawl_int'
    )

  #get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, extent = km2) %>%
    mutate(habitat = as.character(habitat))

  health <- AlignManyDataYears(health_lyrs) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  #join extent and health
  d <- health %>%
    full_join(extent, by = c('region_id', 'habitat')) %>%
    mutate(w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
    filter(!is.na(w))

  #calculate scores
  status_HAB <- d %>%
    group_by(region_id) %>%
    filter(!is.na(health)) %>%
    summarize(score = pmin(1, sum(health) / sum(w)) * 100, dimension = 'status')
  
  #trend calculation for saltmarsh and ccms
  #trend read in for seagrass (this should be updated)
  sm_health <- AlignDataYears(layer_nm = "hab_saltmarsh_health", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = health, scenario_year)
  sm_health <- sm_health[order(sm_health$region_id, sm_health$scenario_year),]
  
  trend_years <- (scen_year-4):(scen_year)
  sm_trend <- CalculateTrend(status_data = sm_health, trend_years = trend_years)
  sm_trend <- sm_trend %>% 
    mutate(habitat = "saltmarsh") %>% 
    select(region_id, habitat, trend = score)
  
  ccms_health <- AlignDataYears(layer_nm = "hab_ccms_trawl_int", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = health, scenario_year)
  ccms_health <- ccms_health[order(ccms_health$region_id, ccms_health$scenario_year),]
  
  trend_years <- (2013):(2017) # data limited to 2012 - 2016
  ccms_trend <- CalculateTrend(status_data = ccms_health, trend_years = trend_years)
  ccms_trend <- ccms_trend %>% 
    mutate(habitat = "ccms") %>% 
    select(region_id, habitat, trend = score)
  
  sg_trend <- AlignDataYears(layer_nm = "hab_seagrass_trend", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, trend, scenario_year) %>% 
    filter(scenario_year == scen_year) %>% 
    mutate(habitat = "seagrass") %>% 
    select(region_id, habitat, trend)
  
  hab_trend <- rbind(sm_trend, ccms_trend, sg_trend)
  d <- left_join(d, hab_trend, c("region_id", "habitat"))
  
  trend_HAB <- d %>%
    group_by(region_id) %>%
    filter(!is.na(trend)) %>%
    summarize(score =  sum(trend) / sum(w), dimension = 'trend')  

  scores_HAB <- rbind(status_HAB, trend_HAB) %>%
    mutate(goal = "HAB") %>%
    select(region_id, goal, dimension, score)
  
  #create weights file for pressures/resilience calculations
  weights <- extent %>%
    filter(
      habitat %in% c(
        'seagrass',
        'saltmarsh',
        'ccms'
      )
    ) %>%
    filter(extent > 0) %>%
    mutate(boolean = 1) %>%
    mutate(layer = "element_wts_hab_pres_abs") %>%
    select(rgn_id = region_id, habitat, boolean, layer)

  write.csv(weights,
            sprintf("temp/element_wts_hab_pres_abs_%s.csv", scen_year),
            row.names = FALSE)

  layers$data$element_wts_hab_pres_abs <- weights

  #return scores
  return(scores_HAB)
}


SPP <- function(layers) {
  
  #see SPP.R code for prep
  scores <- rbind(layers$data$spp_status, layers$data$spp_trend) %>%
    mutate(goal = 'SPP') %>%
    mutate(dimension = ifelse(layer == "spp_status", "status", "trend")) %>%
    mutate(score = ifelse(dimension == 'status', score * 100, score)) %>%
    select(region_id = rgn_id, goal, dimension, score)

  return(scores)
}

BD <- function(scores) {
  d <- scores %>%
    filter(goal %in% c('HAB', 'SPP')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    mutate(goal = 'BD') %>%
    data.frame()

  # return all scores
  return(rbind(scores, d[, c('region_id', 'goal', 'dimension', 'score')]))
}

PreGlobalScores <- function(layers, conf, scores) {
  # get regions
  name_region_labels <- conf$config$layer_region_labels
  rgns <- layers$data[[name_region_labels]] %>%
    select(id_num = rgn_id, val_chr = label)

  # limit to just desired regions and global (region_id==0)
  scores <- subset(scores, region_id %in% c(rgns[, 'id_num'], 0))

  # apply NA to Antarctica
  id_ant <- subset(rgns, val_chr == 'Antarctica', id_num, drop = TRUE)
  scores[scores$region_id == id_ant, 'score'] = NA

  return(scores)
}

FinalizeScores <- function(layers, conf, scores) {
  # get regions
  name_region_labels <- conf$config$layer_region_labels
  rgns <- layers$data[[name_region_labels]] %>%
    select(id_num = rgn_id, val_chr = label)


  # add NAs to missing combos (region_id, goal, dimension)
  d <- expand.grid(list(
    score_NA  = NA,
    region_id = c(rgns[, 'id_num'], 0),
    dimension = c(
      'pressures',
      'resilience',
      'status',
      'trend',
      'future',
      'score'
    ),
    goal      = c(conf$goals$goal, 'Index')
  ),
  stringsAsFactors = FALSE)
  head(d)
  d <- subset(d,!(
    dimension %in% c('pressures', 'resilience', 'trend') &
      region_id == 0
  ) &
    !(
      dimension %in% c('pressures', 'resilience', 'trend', 'status') &
        goal == 'Index'
    ))
  scores <-
    merge(scores, d, all = TRUE)[, c('goal', 'dimension', 'region_id', 'score')]

  # order
  scores <- arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score <- round(scores$score, 2)

  return(scores)
}
