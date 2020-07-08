## functions.R.
## Each OHI goal model is a separate R function.
## The function name is the 2- or 3- letter code for each goal or subgoal;
## for example, FIS is the Fishing subgoal of Food Provision (FP).


FIS <- function(layers) {
  scen_year <- layers$data$scenario_year
  
  #landings data
  c <- AlignDataYears(layer_nm = "fis_2014_2018_landings_to_port", layers_obj = layers) %>%
    select(
      region_id = rgn_id,
      year = scenario_year,
      data_year = fis_2014_2018_landings_to_port_year,
      stock_id,
      taxon_penalty_code,
      catch = SumCatch
    )
  
  #b_bmsy data
  b <- AlignDataYears(layer_nm = "fis_b_bmsy", layers_obj = layers) %>%
    select(region_id = rgn_id, stock_id, year = scenario_year, bbmsy)
  b$stock_id <- trimws(b$stock_id) #trim tailing white space in stock id
  
  # general formating:
  c <- c %>%
    mutate(catch = as.numeric(catch)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(data_year = as.numeric(as.character(data_year))) %>%
    mutate(region_id = as.numeric(as.character(region_id))) %>%
    mutate(taxon_penalty_code = as.numeric(as.character(taxon_penalty_code))) %>%
    mutate(stock_id = as.character(stock_id)) %>%
    select(region_id, year, data_year, stock_id, taxon_penalty_code, catch)
  
  # general formatting:
  b <- b %>%
    mutate(bbmsy = as.numeric(bbmsy)) %>%
    mutate(region_id = as.numeric(as.character(region_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(stock_id = as.character(stock_id))
  
  ####
  # STEP 1. Calculate scores for Bbmsy values
  # underfishing high bmsy stock is penalised
  # as such: 
  #1)bbmsy < 0.95, bbmsy = unchanged, 
  #2)bbmsy >= 0.95 & <= 1.05, bbmsy = 1 
  #3)bbmsy > 1.05 reduced by sliding scale until minimum threshold reached (0.25)
  # as such bbmsy values above 1 get reduced to below 1 - larger values get reduced more
  #example:
  #  region_id                      stock_id year     bbmsy     score
  #  1         1 Glyptocephalus_cynoglossus-27 2008 0.8108882 0.8108882
  #  2         1 Lepidorhombus_whiffiagonis-27 2008 0.4560616 0.4560616
  #  3         1   Micromesistius_poutassou-27 2008 1.1341302 0.9579349
  #  4         1                Molva_molva-27 2008 1.2161870 0.9169065
  ####
  ####
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05
  
#no under-fishing penalty
  b$score = ifelse(b$bbmsy < lowerBuffer, b$bbmsy,
    ifelse (b$bbmsy >= lowerBuffer, 1, NA))
  b$score = ifelse(!is.na(b$score),
                   b$score,
                   ifelse(
                     1 - alpha * (b$bbmsy - upperBuffer) > beta,
                     1 - alpha * (b$bbmsy - upperBuffer),
                     beta
                   ))
  
  write.csv(b, 'temp/FIS_bbmsy_penalties.csv', row.names = FALSE)
  
  ####
  # STEP 1. Merge the b/bmsy data with catch data
  ####
  data_fis <- c %>%
    left_join(b, by = c('region_id', 'stock_id', 'year')) %>%
    select(region_id, stock_id, year, data_year, taxon_penalty_code, catch, bbmsy, score)
  
  ###
  # STEP 2. Estimate scores for taxa without b/bmsy values
  # Mean score of other fish in the region is the starting point
  # Then a penalty is applied based on the level the taxa are reported at
  ###

  ## this takes the mean score within each region and year
  data_fis_gf <- data_fis %>%
    group_by(region_id, year) %>%
    mutate(mean_score = mean(score, na.rm = TRUE)) %>%
    ungroup()
  
#next step - not applicable to esw data
  ## this takes the median score across all regions within a year(used when no stocks have scores within a region) 
  #data_fis_gf <- data_fis_gf %>%
    #group_by(year) %>%
    #mutate(Median_score_global = quantile(score, probs = c(0.5), na.rm = TRUE)) %>%
    #ungroup() %>%
    #mutate(Median_score = ifelse(is.na(Median_score), Median_score_global, Median_score)) %>%
    #select(-Median_score_global) #remove Median_score_global from the data frame

#------------#
#apply taxon penalty
#!taxon penalty not applied
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
  
  #penaltyTable <- data.frame(taxon_penalty_code = c(0,2,5,7),
                             #penalty = c(1, 0.9, 0.8, 0.5))
  
  #data_fis_gf <- data_fis_gf %>%
    #left_join(penaltyTable, by = 'taxon_penalty_code') %>%
    #mutate(score_gf = mean_score * penalty) %>%
    #mutate(method = ifelse(is.na(score), "mean gapfilled", NA)) %>%
    #mutate(gapfilled = ifelse(is.na(score), 1, 0)) %>%
    #mutate(score = ifelse(is.na(score), score_gf, score * penalty)) %>% 
    #arrange(year)
#------------#
  
  data_fis_gf <- data_fis_gf %>%
    #left_join(penaltyTable, by = 'taxon_penalty_code') %>%
    mutate(score_gf = mean_score) %>%
    mutate(method = ifelse(is.na(score), "mean gapfilled", NA)) %>%
    mutate(gapfilled = ifelse(is.na(score), 1, 0)) %>%
    mutate(score = ifelse(is.na(score), score_gf, score)) %>% 
    arrange(year)
  
  gap_fill_data <- data_fis_gf %>%
    select(region_id,
           stock_id,
           year,
           taxon_penalty_code,
           catch,
           bbmsy,
           score,
           mean_score,
           #penalty,
           score_gf,
           method,
           gapfilled)
  
  write.csv(gap_fill_data, 'temp/FIS_summary_gf_all_years.csv', row.names = FALSE)
  
  status_data <- data_fis_gf %>%
    select(region_id, stock_id, year, data_year, catch, score) %>% 
    arrange(region_id, year)
  
  ###
  # STEP 4. Calculate status for each region
  ###
  
  # 4a. calculate relative catch (proportion) of each species per region,
  # apply prop to 'score' (bbmsy value)
  # sum prop scores by region / year
  
  options(scipen = 999)
  
  status_data <- status_data %>%
    group_by(region_id, year) %>%
    mutate(SumCatch = sum(catch)) %>%
    ungroup() %>%
    mutate(prop = catch / SumCatch) %>% 
    mutate(prop_score = score * prop) %>% 
    group_by(region_id, year, data_year) %>% 
    summarise(status = sum(prop_score)) %>% 
    mutate(status = round(status * 100, 1)) %>% 
    ungroup()
  
  ###
  # STEP 5. Get yearly status and trend
  ###
  
  status <-  status_data %>%
    filter(year == scen_year) %>%
    mutate(dimension = 'status') %>%
    select(region_id, score = status, dimension)
  
  # calculate trend
  trend_years <- (scen_year - 4):(scen_year)
  
  trend <- CalculateTrend(status_data = status_data, trend_years = trend_years)
  
  scores <- rbind(status, trend) %>%
    mutate(goal = 'FIS') %>%
    as.data.frame()
  
  return(scores)
}


MAR <- function(layers) {
  
  scen_year <- layers$data$scenario_year
  
  harvest_tonnes <-
    AlignDataYears(layer_nm = "mar_harvest_tonnes", layers_obj = layers)
  #head(harvest_tonnes)

  #used for correcting producion (by coastal length for inter-regional ref)
  coastal_length <-
    AlignDataYears(layer_nm = "mar_coastal_length", layers_obj = layers) %>% 
    select(scenario_year, region_id = rgn_id, c_lngth_km)
  
  mar_prod <-  harvest_tonnes %>%
    select(region_id = rgn_id, scenario_year, data_year = mar_harvest_tonnes_year, rgn_prod_T)
  #remove nas
  mar_prod <- na.omit(mar_prod)
  #remove zero harvest
  mar_prod <- mar_prod [mar_prod$rgn_prod_T > 0,]

  #aggregate production by region
  #extract last 5 years of data (note: data not updated after 2016 so data years: 2012-2016)
  
  #rescale (intra-regional ref - maximum regional MAR prod in last 5 year)
  #select current year for status
  if (ref == "intra"){
    mar_prod <- mar_prod %>%
      group_by(region_id, scenario_year, data_year) %>%
      summarize(sum_tonnes = sum(rgn_prod_T, na.rm = TRUE)) %>% 
      filter(scenario_year >= 2014 & scenario_year <= 2018) %>% 
      group_by(region_id) %>%
      mutate(max_sum_tonnes = max(sum_tonnes)) %>% 
      mutate(status = (sum_tonnes / max_sum_tonnes) * 100) %>% 
      mutate(dimension = "status") %>% 
      ungroup()
  }

  #rescale (inter-regional ref - maximum MAR prod in last 5 year across all regions (production corrected for coastal length))
  #select current year for status
  if (ref == "inter"){
    mar_prod <- mar_prod %>%
      group_by(region_id, scenario_year, data_year) %>%
      summarize(sum_tonnes = sum(rgn_prod_T, na.rm = TRUE)) %>% 
      filter(scenario_year >= 2014 & scenario_year <= 2018) %>% 
      ungroup() %>% 
      left_join(coastal_length, by = c("region_id", "scenario_year")) %>% 
      mutate(sum_tonnes_clngth = sum_tonnes / c_lngth_km) %>% #correct production for coastal length
      mutate(max_sum_tonnes_clngth = max(sum_tonnes_clngth)) %>% 
      mutate(status = (sum_tonnes_clngth / max_sum_tonnes_clngth) * 100) %>% 
      mutate(dimension = "status")
  }

  status <- mar_prod %>%
    filter(scenario_year == 2018) %>% #2018 - last year of data
    select(region_id, score =  status, dimension) %>% 
    as.data.frame()
  
  #add zero for IOS region 4 - no mariculture
  region_id <- 4
  score <- 0
  dimension <- "status"
  IOS <- data.frame(region_id, score, dimension)
  status <- rbind(status, IOS)
  status <- status[order(status$region_id),]
  
  #calculate trend
  trend_years <- (2014):(2018)
  trend <- CalculateTrend(status_data = mar_prod, trend_years = trend_years)
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
    as.data.frame()

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
    as.data.frame()
  
  # return all scores
  return(rbind(scores, s))
}


AO <- function(layers) {
  
  #only run with within (intra) regional comparison
  
  scen_year <- layers$data$scenario_year

  #catch prop (tonnes) for <10m fleet
  catch_prop <- AlignDataYears(layer_nm = "ao_catch_under10_prop", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, catch_prop, scenario_year, data_year = ao_catch_under10_prop_year)

#-------------#  
  #if (ref == "intra"){
    ##select last 5 years of data - rescale (intra-regional with maximum fleet size in last 5 year) - select current year for status
    #fleet <- fleet %>%
      #filter(scenario_year <= scen_year & scenario_year >= scen_year - 4) %>%
      #mutate(dimension = "status") %>%
      #group_by(region_id) %>%
      #mutate(max_boats = max(boats)) %>% 
      #mutate(status = (boats / max_boats) * 100) %>% 
      #ungroup() %>% 
      #arrange(region_id)
  #}
  
  #if (ref == "inter"){
    ##select last 5 years of data - rescale (inter-regional with maximum fleet size in last 5 year) - select current year for status
    #fleet <- fleet %>%
      #filter(scenario_year <= scen_year & scenario_year >= scen_year - 4) %>%
      #mutate(dimension = "status") %>%
      #mutate(max_boats = max(boats)) %>% 
      #mutate(status = (boats / max_boats) * 100) %>% 
      #arrange(region_id)
  #}
#-------------#
  
  #select last 5 years of data - rescale (intra-regional with maximum catch prop in last 5 year) - select current year for status
  catch_prop <- catch_prop %>%
    filter(scenario_year <= scen_year & scenario_year >= scen_year - 4) %>%
    mutate(dimension = "status") %>%
    group_by(region_id) %>%
    mutate(max_catch_prop = max(catch_prop, na.rm = T)) %>% 
    mutate(status = (catch_prop / max_catch_prop) * 100) %>% 
    ungroup() %>% 
    arrange(region_id)
  
  catch_prop_status <- catch_prop %>% 
    arrange(region_id) %>% 
    filter(scenario_year == scen_year)%>% 
    dplyr::select(region_id, score = status, dimension) 
  
  trend_years <- (scen_year - 4):(scen_year)
  catch_prop_trend <- CalculateTrend(status_data = catch_prop, trend_years = trend_years)
  
  effort <- AlignDataYears(layer_nm = "ao_effort_catch", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, effort_tph, scenario_year, data_year = ao_effort_catch_year)  

#-------------#
  #if (ref == "intra"){
    ##rescale (intra-regional with maximum effort/catch size in last 5 year) - select current year for status
    #effort <- effort %>% 
      #arrange(region_id) %>% 
      #group_by(region_id) %>% 
      #mutate(max_effort_tph = max(effort_tph, na.rm = TRUE)) %>% 
      #mutate(status = (effort_tph / max_effort_tph) * 100) %>%
      #ungroup()   
  #}
  
  #if (ref == "inter"){
    ##rescale (inter-regional with maximum effort/catch size in last 5 year) - select current year for status
    #effort <- effort %>% 
      #arrange(region_id) %>% 
      #mutate(max_effort_tph = max(effort_tph, na.rm = TRUE)) %>% 
      #mutate(status = (effort_tph / max_effort_tph) * 100)
  #}
#-------------#  

  #rescale (intra-regional with maximum effort/catch size in last 5 year) - select current year for status
  effort <- effort %>% 
    arrange(region_id) %>% 
    group_by(region_id) %>% 
    mutate(max_effort_tph = max(effort_tph, na.rm = TRUE)) %>% 
    mutate(status = (effort_tph / max_effort_tph) * 100) %>%
    ungroup()   
  
  effort_status <- effort %>% 
    filter(scenario_year == scen_year) %>% 
    mutate(dimension = "status") %>% 
    dplyr::select(region_id, score = status, dimension) 
    
  trend_years <- (scen_year - 4):(scen_year)
  effort_trend <- CalculateTrend(status_data = effort, trend_years = trend_years)
  
  #red diesel
  red <- AlignDataYears(layer_nm = "ao_fuel_cost", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, cost_ppl, scenario_year, data_year = ao_fuel_cost_year)
  
  #select last 5 years of data - rescale and invert (high score = low fuel price)
  #invert and rescale to maximum of 1
  #note: same price across regions
  red <- red %>%
    filter(scenario_year <= scen_year & scenario_year >= scen_year - 4) %>%
    mutate(max_cost_ppl = max(cost_ppl)) %>% 
    mutate(status_inv = 1 - (cost_ppl / max_cost_ppl)) %>% 
    mutate(max_status_inv = max(status_inv)) %>% 
    mutate(status = status_inv + (1 - max_status_inv)) %>% 
    mutate(status = status * 100) %>% 
    arrange(region_id)
  
  red_status <- red %>% 
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score = status, dimension)

  trend_years <- (scen_year - 4):(scen_year)
  red_trend <- CalculateTrend(status_data = red, trend_years = trend_years)
  
  #bring status and trend scores together
  status <- rbind (effort_status, catch_prop_status, red_status) 
  
  status <- status %>%
    group_by(region_id) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>% 
    mutate(dimension = "status")%>%
    ungroup()
  
  trend <- rbind(catch_prop_trend, red_trend, effort_trend)
  
  trend <- trend %>%
    group_by(region_id) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>% 
    mutate(dimension = "trend") %>%
    ungroup()
  
  ao_scores <-  rbind(status, trend) %>%
    mutate('goal'='AO') %>%
    select(goal, dimension, region_id, score) %>%
    as.data.frame()

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


CST <- function(layers) {
  
  scen_year <- layers$data$scenario_year
  
  # layers for carbon storage
  extent_lyrs <-
    c(
      'hab_seagrass_extent',
      'hab_saltmarsh_extent',
      'hab_maerl_extent'
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
      'hab_seagrass_health2',
      'hab_saltmarsh_health2',
      'hab_maerl_health'
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
                    'seagrass' = 83,
                    'maerl' = 83)  
  
  #proportionalise habitat health scores by relative area of habitat extent (seagrass, saltmarsh, maerl) and sum
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
  #trend read in for seagrass & maerl
  
  sm_health <- AlignDataYears(layer_nm = "hab_saltmarsh_health2", layers_obj = layers) %>%
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
  
  m_trend <- AlignDataYears(layer_nm = "hab_maerl_trend", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, trend, scenario_year) %>% 
    filter(scenario_year == scen_year) %>% 
    mutate(habitat = "maerl") %>% 
    select(region_id, habitat, trend)
  
  hab_trend <- rbind(sm_trend, sg_trend, m_trend)
  d <- left_join(d, hab_trend, c("region_id", "habitat"))
  d$trend_prop <- d$trend * d$extent_prop

  #trend
  trend_CS <- d %>%
    group_by(region_id) %>%
    summarize(score = sum(trend_prop, na.rm = TRUE)) %>%
    mutate(dimension = "trend") 
  
  #finalize scores_CS
  scores_CS <- rbind(status_CS, trend_CS) %>%
    mutate(goal = 'CST') %>%
    select(region_id, goal, dimension, score) %>% 
    as.data.frame()

  #create weights file for pressures/resilience calculations
  weights <- extent %>%
    filter(extent > 0) %>%
    mutate(rank = habitat.rank[habitat]) %>%
    mutate(extent_rank = extent * rank) %>%
    mutate(layer = "element_wts_cst_km2_x_storage") %>%
    select(rgn_id = region_id, habitat, extent_rank, layer)

  write.csv(
    weights,
    sprintf("temp/element_wts_cst_km2_x_storage_%s.csv", scen_year),
    row.names = FALSE
  )

  layers$data$element_wts_cst_km2_x_storage <- weights

  # return scores
  return(scores_CS)
}


CPR <- function(layers) {
  
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
      'hab_seagrass_health2',
      'hab_saltmarsh_health2',
      'hab_sand_dune_health2'
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

  #proportionalise habitat health scores by relative area of habitat extent (seagrass, saltmarsh, sand dune) and sum
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
  #trend read in for seagrass
  
  sm_health <- AlignDataYears(layer_nm = "hab_saltmarsh_health2", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = health, scenario_year)
  sm_health <- sm_health[order(sm_health$region_id, sm_health$scenario_year),]
  trend_years <- (scen_year-4):(scen_year)
  sm_trend <- CalculateTrend(status_data = sm_health, trend_years = trend_years)
  sm_trend <- sm_trend %>% 
    mutate(habitat = "saltmarsh") %>% 
    select(region_id, habitat, trend = score)
  
  sd_health <- AlignDataYears(layer_nm = "hab_sand_dune_health2", layers_obj = layers) %>%
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
    mutate(goal = 'CPR') %>%
    select(region_id, goal, dimension, score) %>% 
    as.data.frame()

  #create weights file for pressures/resilience calculations
  weights <- extent %>%
    filter(extent > 0) %>%
    mutate(rank = habitat.rank[habitat]) %>%
    mutate(extent_rank = extent * rank) %>%
    mutate(layer = "element_wts_cpr_km2_x_protection") %>%
    select(rgn_id = region_id, habitat, extent_rank, layer)

  write.csv(
    weights,
    sprintf("temp/element_wts_cpr_km2_x_protection_%s.csv", scen_year),
    row.names = FALSE
  )

  layers$data$element_wts_cpr_km2_x_protection <- weights

  #return scores
  return(scores_CP)

}


CS <- function(scores) {
  ## to calculate the four CS (Coastal Systems) dimesions, average those dimensions for CST and CPR
  s <- scores %>%
    filter(goal %in% c('CST', 'CPR'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "CS") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()
  
  # return all scores
  return(rbind(scores, s))
}


TR <- function(layers) {
  
  scen_year <- layers$data$scenario_year

  #note data not available after 2014 (scenario year 2015) - so trend needs to be calculated 2011 - 2015
  tourism <- AlignDataYears(layer_nm = "tr_ons_1km", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, tr_ons = ons, scenario_year, data_year = tr_ons_1km_year) %>% 
    arrange(region_id, scenario_year)

  #read in viewshed data
  rgn_sv <- AlignDataYears(layer_nm = "tr_viewshed", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, perc_sv, scenario_year)
  
  #select tourism data for last 5 years of data (2011 to 2015 (data years 2010 to 2014))
  tr_model <- tourism %>% 
    filter(scenario_year >= 2011 & scenario_year <= 2015)

#------------# 
  #swith off intra vs inter comparison
  #if (ref == "intra"){
  ##INTRA-region comparison
  #tr_model <- tr_model %>% 
    #group_by(region_id) %>% 
    #mutate(max_tr_ons = max(tr_ons)) %>% 
    #mutate(status = tr_ons / max_tr_ons) %>% 
    #ungroup() %>% 
    #mutate(status = status * 100) %>%
    #arrange(region_id)
  #}
  
  #if (ref == "inter"){
  ##INTER-region comparison
  #tr_model <- tr_model %>% 
    #mutate(max_tr_ons = max(tr_ons)) %>% 
    #mutate(status = tr_ons / max_tr_ons) %>% 
    #mutate(status = status * 100) %>%
    #arrange(region_id)
  #}
#------------# 

  #INTRA-region comparison
  tr_model <- tr_model %>% 
    group_by(region_id) %>% 
    mutate(max_tr_ons = max(tr_ons)) %>% 
    mutate(status = tr_ons / max_tr_ons) %>% 
    ungroup() %>% 
    mutate(status = status * 100) %>%
    arrange(region_id)
  
  #get status - select senario year 2015 as most current data values (data not updated after 2014)
  status_1 <- tr_model %>%
    filter(scenario_year == 2015) %>% 
    select(region_id = region_id, score = status) %>%
    mutate(dimension = 'status')

  #calculate trend
  trend_years <- 2011:2015
  tour_trend <- CalculateTrend(status_data = tr_model, trend_years = trend_years)
  
  #sea view - no time series
  status_2 <- rgn_sv %>% 
    filter(scenario_year == scen_year) %>% 
    select(region_id = region_id, score = perc_sv) %>%
    mutate(dimension = 'status')
  
  TR_rgn_sv <- status_2 %>% 
    mutate(status = score, data_year = 2018) %>% 
    select(region_id, status, data_year)
  
#recreation
  #read in modelled recreation data (status)
  #no time series
  status_3 <- AlignDataYears(layer_nm = "tr_rec", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, score = mean_tscore, scenario_year) %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>%
    select(-scenario_year)
  
  TR_rec_status <- status_3 %>% 
    select(region_id, status = score, dimension) %>% 
    mutate(data_year = 2018)
  
  #aggregate status scores
  tr_status <- rbind(status_1, status_2, status_3)
  tr_status <- tr_status %>% 
    group_by(region_id) %>% 
    summarise(score = mean(score, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(dimension = "status")

  tr_trend <- tour_trend
  
  # bind status and trend by rows
  tr_score <- bind_rows(tr_status, tr_trend) %>%
    mutate(goal = 'TR')
  
  # return final scores
  scores <- tr_score %>%
    select(region_id, goal, dimension, score) %>% 
    as.data.frame()

  return(scores)
}


LIV <- function(layers){
  
  scen_year <- layers$data$scenario_year
  
  wages <- AlignDataYears(layer_nm = "le_wage", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, wages_gbp = wages_gbp, scenario_year, data_year = le_wage_year)
  
  jobs <- AlignDataYears(layer_nm = "le_jobs", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, mar_jobs = mar_jobs, scenario_year, data_year = le_jobs_year)
  
  cpi <- AlignDataYears(layer_nm = "le_cpi", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, cpi, scenario_year, data_year = le_cpi_year)

  liv <- jobs %>%
    left_join(wages, by = c('region_id','scenario_year')) %>%
    left_join(cpi, by = c('region_id','scenario_year')) %>%
    arrange(region_id, scenario_year) %>% 
    select(-data_year.x,  -data_year.y)
  
  # LIV status
  
    #INTRA-region comparison - INTRA is OK for numbers employed as not all regions can support similar number of jobs
    
    #jobs reference: temporal comparison (by region) using a 4 year (due to data restrictions) moving window
    #value in the current year (scenario year) relative to the value in a recent moving reference period
    #a score of 100 would indicate no loss of jobs relative to 4 years previous - otherewise % reduction in jobs
    
    #wages reference: temporal comparison, percentage increase/decrease in nominal wage year on year by region 
    #tracked against Consumer Price Index (CPI) 
    #0 = annual nominal wage increase less than CPI
    #1 = annual nominal wage increase greater than CPI
    
    liv_status <- liv %>%
      arrange(region_id, scenario_year) %>%
      group_by(region_id) %>%
      mutate(mar_jobs_ref  = dplyr::lag(mar_jobs, 4)) %>%
      mutate(wage_ref = dplyr::lag(wages_gbp, 1)) %>% 
      mutate(wage_inc = wages_gbp - wage_ref) %>% 
      mutate(pcnt_inc = (wage_inc / wage_ref) * 100) %>% 
      ungroup() %>%
      #filter(scenario_year <= scen_year & scenario_year >= scen_year - 4) %>% 
      mutate(
        x_jobs  = pmax(-1, pmin(1, mar_jobs / mar_jobs_ref)),
        x_wages = ifelse(pcnt_inc - cpi < 0, 0, 1))
 
    #calculate mean status score (jobs and wages) - by year and region
    liv_status$status <- apply(cbind(liv_status$x_jobs, liv_status$x_wages), 1, mean, na.rm = T) * 100 
    
    liv_status <- liv_status %>% 
      select(region_id, mar_jobs, wages_gbp, scenario_year, data_year, status)

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
    dplyr::select(region_id, score, dimension, goal) %>% 
    as.data.frame()

  return(scores)

}


ECO <- function(layers){
  
  scen_year <- layers$data$scenario_year
  
  #regional marine related GVA
  #data not updated after from 2015
  revn <- AlignDataYears(layer_nm = "le_gva", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, gva = rev_gbp_M, scenario_year, data_year = le_gva_year)
  revn <- revn %>%
    filter(scenario_year <= 2015)
  
  jobs <- AlignDataYears(layer_nm = "le_jobs", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, mar_jobs = mar_jobs, scenario_year)

  #calculate gva per capita
  rev_cap <- left_join(revn, jobs, by = c("region_id", "scenario_year"))
  
  rev_cap <- rev_cap %>% 
    mutate(gva_per_cap = rev_cap$gva / rev_cap$mar_jobs) %>% 
    arrange(region_id, scenario_year)
  
  #ECO status
  if (ref == "intra"){
    #INTRA-region comparison
    #assess economic productivity (GVA per capita) (intra-regional - maximum GVA per capita in last 5 years (data years 2011 to 2015))
    rev_cap <- rev_cap %>% 
      arrange(region_id) %>% 
      group_by(region_id) %>% 
      filter(data_year >= 2011) %>% 
      mutate(ref = max(gva_per_cap)) %>%
      mutate(status = (gva_per_cap / ref) * 100) %>%
      ungroup() %>% 
      arrange(region_id, scenario_year)
  }
  
  if (ref == "inter"){
    #INTER-region comparison
    #assess economic productivity (GVA per capita) (maximum GVA per capita in last 5 years across all regions (data years 2011 to 2015))
    rev_cap <- rev_cap %>% 
      filter(data_year >= 2011) %>% 
      mutate(ref = max(gva_per_cap)) %>%
      mutate(status = (gva_per_cap / ref) * 100) %>%
      arrange(region_id, scenario_year)
  }
  
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
    dplyr::select(region_id, score, dimension, goal) %>% 
    as.data.frame()

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


ECL <- function(layers) {
  
  scen_year <- layers$data$scenario_year
  
  ref_pct_cpa <- 100 #re-scaling factor - 100% of coast
  
  # select data
  total_area <-
    rbind(layers$data$rgn_area_inland1km,
          layers$data$rgn_area_offshore12nm) %>% #total offshore/inland areas
    select(region_id = rgn_id, area, layer) %>%
    mutate(region_id = as.numeric(region_id)) %>%
    spread(layer, area) %>%
    select(region_id,
           area_inland1km = rgn_area_inland1km,
           area_offshore12nm = rgn_area_offshore12nm)
  
  offshore <-
    AlignDataYears(layer_nm = "da_ecl_M_12nm", layers_obj = layers) %>%
    select(region_id = rgn_id,
           year = scenario_year,
           data_year = da_ecl_M_12nm_year,
           cmpa = area_km2_m)
  
  inland <-
    AlignDataYears(layer_nm = "da_ecl_T_1km", layers_obj = layers) %>%
    select(region_id = rgn_id,
           year = scenario_year,
           data_year = da_ecl_T_1km_year,
           cpa = area_km2_t)
  
  lsp_data <- full_join(offshore, inland, by = c("region_id", "year"))
  
  # get percent of total area that is protected for inland1km (cp) and offshore12nm (cmpa) (combined) per year - by region
  # and calculate status score
  status_data <- lsp_data %>%
    full_join(total_area, by = "region_id") %>%
    arrange(region_id, year) %>%
    mutate(pa = cmpa + cpa) %>% 
    mutate(area_12nm_1km = area_inland1km + area_offshore12nm) %>% 
    mutate(
      pct_pa = pa / area_12nm_1km * 100,
      status = (pmin(pct_pa / ref_pct_cpa, 1))
    ) %>% 
    mutate(status = status * 100) %>% 
    select(region_id, year, data_year = data_year.x, status)

  #status
  status <- status_data %>%
    filter(year == scen_year) %>%
    select(region_id, score = status) %>%
    mutate(dimension = "status")
  
  # calculate trend
  trend_years <- (scen_year - 4):(scen_year)
  trend <- CalculateTrend(status_data = status_data, trend_years = trend_years)
  
  # return scores
  scores <- bind_rows(status, trend) %>%
    mutate(goal = "ECL") %>%
    as.data.frame()
  
  return(scores)
}


LAN <- function(layers) {
  
  scen_year <- layers$data$scenario_year

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
    AlignDataYears(layer_nm = "da_lan_M_3nm", layers_obj = layers) %>%
    select(region_id = rgn_id,
           year = scenario_year,
           data_year = da_lan_M_3nm_year,
           cmpa = area_km2_m)
  inland <-
    AlignDataYears(layer_nm = "da_lan_T_1km", layers_obj = layers) %>%
    select(region_id = rgn_id,
           year = scenario_year,
           data_year = da_lan_T_1km_year,
           cpa = area_km2_t)

  lsp_data <- full_join(offshore, inland, by = c("region_id", "year"))

  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) (combined) per year - by region
  # and calculate status score 
  status_data <- lsp_data %>%
    full_join(total_area, by = "region_id") %>%
    arrange(region_id, year) %>%
    mutate(pa = cmpa + cpa) %>% 
    mutate(area_3nm_1km = area_inland1km + area_offshore3nm) %>% 
    mutate(
      pct_pa = pa / area_3nm_1km * 100,
      status = (pmin(pct_pa / ref_pct_cpa, 1))
    ) %>% 
    mutate(status = status * 100) %>% 
    select(region_id, year, data_year = data_year.x, status)
  
  #status
  status <- status_data %>%
    filter(year == scen_year) %>%
    select(region_id, score = status) %>%
    mutate(dimension = "status")

  # calculate trend
  trend_years <- (scen_year - 4):(scen_year)
  trend <- CalculateTrend(status_data = status_data, trend_years = trend_years)

  # return scores
  scores <- bind_rows(status, trend) %>%
    mutate(goal = "LAN") %>%
    as.data.frame()
  
  return(scores)
}

DA <- function(scores) {
  ## to calculate the four DA dimesions, average those dimensions for ECL and LAN
  s <- scores %>%
    filter(goal %in% c('ECL', 'LAN'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "DA") %>%
    select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


CW <- function(layers) {

  scen_year <- layers$data$scenario_year

  ### function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }
  
  #pesticides
  pest <- AlignDataYears(layer_nm = "cw_pesticides", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, pst = pesticides, scenario_year, data_year = cw_pesticides_year)
  
  pest <- pest %>%
    filter(scenario_year <= scen_year & scenario_year >= scen_year - 4)
  
  if (ref == "intra"){
    #INTRA-region comparison
    pest <- pest %>% 
      mutate(dimension = "status") %>%
      arrange(region_id) %>% 
      group_by(region_id) %>% 
      mutate(max_pst = max(pst)) %>% 
      mutate(status_inv = 1 - (pst / max_pst)) %>% 
      mutate(max_status_inv = max(status_inv)) %>% 
      mutate(status = status_inv + (1 - max_status_inv)) %>% 
      mutate(status = status * 100) %>% 
      ungroup()
  }
  
  if (ref == "inter"){
    #INTER-region comparison
    pest <- pest %>% 
      mutate(dimension = "status") %>%
      arrange(region_id) %>% 
      mutate(max_pst = max(pst)) %>% 
      mutate(status_inv = 1 - (pst / max_pst)) %>% 
      mutate(max_status_inv = max(status_inv)) %>% 
      mutate(status = status_inv + (1 - max_status_inv)) %>% 
      mutate(status = status * 100)
  }

  pest_status <- pest %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score = status, dimension)
  
  trend_years <- (scen_year - 4):(scen_year)
  pest_trend <- CalculateTrend(status_data = pest, trend_years = trend_years)
  
  #inorganic run-off
  run_off <- AlignDataYears(layer_nm = "cw_inorganic_run_off2", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, iro = inorg_run_off, scenario_year, data_year = cw_inorganic_run_off2_year)
  
  run_off <- run_off %>%
    filter(scenario_year <= scen_year & scenario_year >= scen_year - 4)
  
  if (ref == "intra"){
    #INTRA-region comparison
    run_off <- run_off %>% 
      mutate(dimension = "status") %>%
      arrange(region_id) %>% 
      group_by(region_id) %>% 
      mutate(max_iro = max(iro)) %>% 
      mutate(status_inv = 1 - (iro / max_iro)) %>% 
      mutate(max_status_inv = max(status_inv)) %>% 
      mutate(status = status_inv + (1 - max_status_inv)) %>% 
      mutate(status = status * 100) %>% 
      ungroup()
  }
  
  if (ref == "inter"){
    #INTER-region comparison
    run_off <- run_off %>% 
      mutate(dimension = "status") %>%
      arrange(region_id) %>% 
      mutate(max_iro = max(iro)) %>% 
      mutate(status_inv = 1 - (iro / max_iro)) %>% 
      mutate(max_status_inv = max(status_inv)) %>% 
      mutate(status = status_inv + (1 - max_status_inv)) %>% 
      mutate(status = status * 100)
  }
  
  run_off_status <- run_off %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>% 
    dplyr::select(region_id, score = status, dimension)
  
  trend_years <- (scen_year - 4):(scen_year)
  run_off_trend <- CalculateTrend(status_data = run_off, trend_years = trend_years)

  #nutrients
  nut <- AlignDataYears(layer_nm = "cw_nutrients", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, nt = nutrients, scenario_year, data_year = cw_nutrients_year) %>% 
    arrange(region_id)
  
  nut <- nut %>%
    filter(scenario_year <= scen_year & scenario_year >= scen_year - 4)
  
  if (ref == "intra"){
    #INTRA-region comparison
    nut <- nut %>% 
      mutate(dimension = "status") %>%
      arrange(region_id) %>% 
      group_by(region_id) %>% 
      mutate(max_nt = max(nt)) %>% 
      mutate(status_inv = 1 - (nt / max_nt)) %>% 
      mutate(max_status_inv = max(status_inv)) %>% 
      mutate(status = status_inv + (1 - max_status_inv)) %>% 
      mutate(status = status * 100) %>% 
      ungroup()
  }
  
  if (ref == "inter"){
    #INTER-region comparison
    nut <- nut %>% 
      mutate(dimension = "status") %>%
      arrange(region_id) %>% 
      mutate(max_nt = max(nt)) %>% 
      mutate(status_inv = 1 - (nt / max_nt)) %>% 
      mutate(max_status_inv = max(status_inv)) %>% 
      mutate(status = status_inv + (1 - max_status_inv)) %>% 
      mutate(status = status * 100)
  }

  nut_status <- nut %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score = status, dimension)
  
  trend_years <- (scen_year - 4):(scen_year)
  nut_trend <- CalculateTrend(status_data = nut, trend_years = trend_years)
  
  #bathing water classification
  #calculate mean beach status by region_id & year
  bw <- AlignDataYears(layer_nm = "cw_bathing_water_class", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = beach_status_score, scenario_year, data_year = cw_bathing_water_class_year)
  bw <- bw %>%
    group_by(region_id, scenario_year, data_year) %>%
    summarise(status = mean (status, na.rm=TRUE)) %>%
    mutate(status = status * 100) %>% 
    ungroup()

  #no data for IOS (region 4)
  #assign adjacent region data (Cornwall: region 3) to IOS
  crn <- bw[bw$region_id == 3,]
  crn$region_id <- 4
  bw <- rbind(bw, crn)
  bw <- bw[order (bw$region_id),]
  
  bw_status <- bw %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score = status, dimension)
  
  trend_years <- (scen_year - 4):(scen_year)
  bw_trend <- CalculateTrend(status_data = bw, trend_years = trend_years)
  
  #water quality - suspended material
  wq <- AlignDataYears(layer_nm = "cw_sus_material", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, sus_mat, scenario_year, data_year = cw_sus_material_year) %>% 
    arrange(region_id)
  
  wq <- wq %>%
    filter(scenario_year <= scen_year & scenario_year >= scen_year - 4)
  
  if (ref == "intra"){
    #INTRA-region comparison
    wq <- wq %>% 
      mutate(dimension = "status") %>%
      arrange(region_id) %>% 
      group_by(region_id) %>% 
      mutate(max_sus_mat = max(sus_mat)) %>% 
      mutate(status_inv = 1 - (sus_mat / max_sus_mat)) %>% 
      mutate(max_status_inv = max(status_inv)) %>% 
      mutate(status = status_inv + (1 - max_status_inv)) %>% 
      mutate(status = status * 100) %>% 
      ungroup()
  }
  
  if (ref == "inter"){
    #INTER-region comparison
    wq <- wq %>% 
      mutate(dimension = "status") %>%
      arrange(region_id) %>% 
      mutate(max_sus_mat = max(sus_mat)) %>% 
      mutate(status_inv = 1 - (sus_mat / max_sus_mat)) %>% 
      mutate(max_status_inv = max(status_inv)) %>% 
      mutate(status = status_inv + (1 - max_status_inv)) %>% 
      mutate(status = status * 100)
  }
  
  wq_status <- wq %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score = status, dimension)
  
  trend_years <- (scen_year - 4):(scen_year)
  wq_trend <- CalculateTrend(status_data = wq, trend_years = trend_years)
  
  #coastal plastics 'trash' - MCS beach clean data
  #note no trend data - read in modelled trend from ohi global data
  trash <- AlignDataYears(layer_nm = "cw_trash", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = trash, scenario_year, data_year = cw_trash_year) %>% 
    mutate(status = status * 100) %>%
    arrange(region_id)
  
  CW_trash_status <- trash %>% 
    filter(scenario_year == scen_year) 

  trash_status <- trash %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score = status, dimension)
  
  #read in trash trend and invert 
  trash_trend <- AlignDataYears(layer_nm = "cw_trash_trend", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, score = trend, scenario_year) %>%
    filter(scenario_year == scen_year)  %>%
    mutate(dimension = "trend") %>%
    mutate(score = -1 * score) %>% 
    dplyr::select(region_id, score, dimension)
  
  #pollution from vessels
  if (ref == "intra"){
    #INTRA-region comparison
    ves <- AlignDataYears(layer_nm = "cw_vessel_pol_intra", layers_obj = layers) %>%
      dplyr::select(region_id = rgn_id, status = vessel_pol, scenario_year, data_year = cw_vessel_pol_intra_year) %>%
      mutate(status = status * 100) %>%
      arrange(region_id)
    
    ves_status <- ves %>%
      filter(scenario_year == scen_year) %>%
      mutate(dimension = "status") %>%
      dplyr::select(region_id, score = status, dimension)
    
    trend_years <- (2011:2015)
    ves_trend <- CalculateTrend(status_data = ves, trend_years = trend_years)
  }
  
  if (ref == "inter"){
    #INTER-region comparison
    ves <- AlignDataYears(layer_nm = "cw_vessel_pol_inter", layers_obj = layers) %>%
      dplyr::select(region_id = rgn_id, status = vessel_pol, scenario_year, data_year = cw_vessel_pol_inter_year) %>%
      mutate(status = status * 100) %>%
      arrange(region_id)
    
    ves_status <- ves %>%
      filter(scenario_year == scen_year) %>%
      mutate(dimension = "status") %>%
      dplyr::select(region_id, score = status, dimension)
    
    trend_years <- (2011:2015)
    ves_trend <- CalculateTrend(status_data = ves, trend_years = trend_years)
  }

  #bring status and trend scores together
  status <- rbind (bw_status, nut_status, pest_status, run_off_status, trash_status, wq_status, ves_status) 
  
  status <- status %>%
    group_by(region_id) %>%
    summarize(score = geometric.mean2(score, na.rm=TRUE)) %>% 
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
    as.data.frame()

  return(scores)
}

HAB <- function(layers) {
  
  scen_year <- layers$data$scenario_year

  #note: extent not used to calculate HAB goal score - but used to determine if health is included by rgn
  extent_lyrs <-
    c(
      'hab_seagrass_extent',
      'hab_saltmarsh_extent',
      'hab_maerl_extent',
      'hab_cms_extent',
      'hab_rr_extent'
    )
  
  if (ref == "intra"){
     health_lyrs <-
    c(
      'hab_seagrass_health2',
      'hab_saltmarsh_health2',
      'hab_maerl_health',
      'hab_cms_trawl_int_intra',
      'hab_rr_trawl_int_intra'
    )
  }
     
  if (ref == "inter"){
     health_lyrs <-
    c(
      'hab_seagrass_health2',
      'hab_saltmarsh_health2',
      'hab_maerl_health',
      'hab_cms_trawl_int_inter',
      'hab_rr_trawl_int_inter'
    )
  }
 
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
  
  #trend calculation for saltmarsh, sand dunes, cms and rr
  #trend read in for seagrass & maerl
  sm_health <- AlignDataYears(layer_nm = "hab_saltmarsh_health2", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = health, scenario_year)
  sm_health <- sm_health[order(sm_health$region_id, sm_health$scenario_year),]
  
  trend_years <- (scen_year-4):(scen_year)
  sm_trend <- CalculateTrend(status_data = sm_health, trend_years = trend_years)
  sm_trend <- sm_trend %>% 
    mutate(habitat = "saltmarsh") %>% 
    select(region_id, habitat, trend = score)

  if (ref == "intra"){
    cms_health <- AlignDataYears(layer_nm = "hab_cms_trawl_int_intra", layers_obj = layers) %>%
      dplyr::select(region_id = rgn_id, status = health, scenario_year)
    cms_health <- cms_health[order(cms_health$region_id, cms_health$scenario_year),]
    
    trend_years <- (scen_year-4):(scen_year)
    cms_trend <- CalculateTrend(status_data = cms_health, trend_years = trend_years)
    cms_trend <- cms_trend %>% 
      mutate(habitat = "cms") %>% 
      select(region_id, habitat, trend = score)
  }
  
  if (ref == "inter"){
    cms_health <- AlignDataYears(layer_nm = "hab_cms_trawl_int_inter", layers_obj = layers) %>%
      dplyr::select(region_id = rgn_id, status = health, scenario_year)
    cms_health <- cms_health[order(cms_health$region_id, cms_health$scenario_year),]
    
    trend_years <- (scen_year-4):(scen_year)
    cms_trend <- CalculateTrend(status_data = cms_health, trend_years = trend_years)
    cms_trend <- cms_trend %>% 
      mutate(habitat = "cms") %>% 
      select(region_id, habitat, trend = score)
  }
  
  if (ref == "intra"){
    rr_health <- AlignDataYears(layer_nm = "hab_rr_trawl_int_intra", layers_obj = layers) %>%
      dplyr::select(region_id = rgn_id, status = health, scenario_year)
    rr_health <- rr_health[order(rr_health$region_id, rr_health$scenario_year),]
    
    trend_years <- (scen_year-4):(scen_year)
    rr_trend <- CalculateTrend(status_data = rr_health, trend_years = trend_years)
    rr_trend <- rr_trend %>% 
      mutate(habitat = "rr") %>% 
      select(region_id, habitat, trend = score)
  }
  
  if (ref == "inter"){
    rr_health <- AlignDataYears(layer_nm = "hab_rr_trawl_int_inter", layers_obj = layers) %>%
      dplyr::select(region_id = rgn_id, status = health, scenario_year)
    rr_health <- rr_health[order(rr_health$region_id, rr_health$scenario_year),]
    
    trend_years <- (scen_year-4):(scen_year)
    rr_trend <- CalculateTrend(status_data = rr_health, trend_years = trend_years)
    rr_trend <- rr_trend %>% 
      mutate(habitat = "rr") %>% 
      select(region_id, habitat, trend = score)
  }
  
  sg_trend <- AlignDataYears(layer_nm = "hab_seagrass_trend", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, trend, scenario_year) %>% 
    filter(scenario_year == scen_year) %>% 
    mutate(habitat = "seagrass") %>% 
    select(region_id, habitat, trend)
  
  m_trend <- AlignDataYears(layer_nm = "hab_maerl_trend", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, trend, scenario_year) %>% 
    filter(scenario_year == scen_year) %>% 
    mutate(habitat = "maerl") %>% 
    select(region_id, habitat, trend)
  
  hab_trend <- rbind(sm_trend, cms_trend, rr_trend, sg_trend, m_trend)
  d <- left_join(d, hab_trend, c("region_id", "habitat"))
  
  trend_HAB <- d %>%
    group_by(region_id) %>%
    filter(!is.na(trend)) %>%
    summarize(score =  sum(trend) / sum(w), dimension = 'trend')  

  scores_HAB <- rbind(status_HAB, trend_HAB) %>%
    mutate(goal = "HAB") %>%
    select(region_id, goal, dimension, score) %>% 
    as.data.frame()
  
  #create weights file for pressures/resilience calculations
  weights <- extent %>%
    filter(
      habitat %in% c(
        'seagrass',
        'saltmarsh',
        'maerl',
        'cms',
        'rr'
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
