## functions.R.
## Each OHI goal model is a separate R function.
## The function name is the 2- or 3- letter code for each goal or subgoal;
## for example, FIS is the Fishing subgoal of Food Provision (FP).

FIS <- function(layers) {
  scen_year <- layers$data$scenario_year
  
  #catch data
  c <- AlignDataYears(layer_nm = "fis_ICESCatchDataset2006_2016", layers_obj = layers) %>%
    select(
      region_id = rgn_id,
      year = scenario_year,
      stock_id,
      taxon_penalty_code,
      catch = rgn_catch_T
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
  #weighted mean
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
  #note - harvest data 1984 to 2010 - watch trend calculation
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
  
  # 4-year rolling mean of data
  #m <- rky %>%
  #group_by(rgn_id, taxa_code, wq_rg_sp) %>%
  #arrange(rgn_id, taxa_code, scenario_year) %>%
  #mutate(sm_tonnes = zoo::rollapply(rgn_prod_T, 4, mean, na.rm = TRUE, partial = TRUE)) %>%
  #ungroup()
  
  m <- rky
  
  # smoothed mariculture harvest * sustainability coefficient
  #m <- m %>%
  #mutate(sust_tonnes = sust_coeff * sm_tonnes)
  
  m <- m %>%
    mutate(sust_tonnes = rgn_prod_T * wq_rg_sp)
  
  # aggregate all weighted timeseries per region, and divide by coastal human population
  ry = m %>%
    group_by(rgn_id, scenario_year) %>%
    summarize(sust_tonnes_sum = sum(sust_tonnes, na.rm = TRUE)) %>%  #na.rm = TRUE assumes that NA values are 0
    left_join(coastal_length, by = c('rgn_id', 'scenario_year')) %>%
    mutate(mar_cst = sust_tonnes_sum / c_lngth_km) %>%
    ungroup()
  
  # get reference quantile based on argument years
  
  ref_95pct <- quantile(ry$mar_cst, 0.95, na.rm = TRUE)
  
  ry = ry %>%
    mutate(status = ifelse(mar_cst / ref_95pct > 1,
                           1,
                           mar_cst / ref_95pct))
  status <- ry %>%
    filter(scenario_year == scen_year) %>%
    mutate(dimension = "status") %>%
    select(region_id = rgn_id, score = status, dimension) %>%
    mutate(score = round(score * 100, 2))
  
  #add zero for IOS region 4 - no mariculture
  status <- rbind(status, c(4, 0, "status"))
  status <- status[order(status$region_id),]
  
  # calculate trend
  #watch trend years here - need to calcculate trend on data prior to 2010
  #no trend in data after 2010
  #trend_years <- (scen_year - 4):(scen_year)
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
  Sustainability <- 1.0

  scen_year <- layers$data$scenario_year

  r <- AlignDataYears(layer_nm = "ao_access", layers_obj = layers) %>%
    rename(region_id = rgn_id, access = value) %>%
    na.omit()

  ry <-
    AlignDataYears(layer_nm = "ao_need", layers_obj = layers) %>%
    rename(region_id = rgn_id, need = value) %>%
    left_join(r, by = c("region_id", "scenario_year"))

  # model
  ry <- ry %>%
    mutate(Du = (1 - need) * (1 - access)) %>%
    mutate(status = (1 - Du) * Sustainability)

  # status
  r.status <- ry %>%
    filter(scenario_year == scen_year) %>%
    select(region_id, status) %>%
    mutate(status = status * 100) %>%
    select(region_id, score = status) %>%
    mutate(dimension = 'status')

  # trend

  trend_years <- (scen_year - 4):(scen_year)

  r.trend <- CalculateTrend(status_data = ry, trend_years = trend_years)

  # return scores
  scores <- rbind(r.status, r.trend) %>%
    mutate(goal = 'AO')

  return(scores)
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
    c('hab_mangrove_extent',
      'hab_seagrass_extent',
      'hab_saltmarsh_extent')
  
health_lyrs <-
    c('hab_mangrove_health',
      'hab_seagrass_health',
      'hab_saltmarsh_health')
  trend_lyrs <-
    c('hab_mangrove_trend',
      'hab_seagrass_trend',
      'hab_saltmarsh_trend')
  # get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    filter(!(habitat %in% c(
      "mangrove_inland1km", "mangrove_offshore"
    ))) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, extent = km2) %>%
    mutate(habitat = as.character(habitat))

  health <- AlignManyDataYears(health_lyrs) %>%
    filter(!(habitat %in% c(
      "mangrove_inland1km", "mangrove_offshore"
    ))) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <- AlignManyDataYears(trend_lyrs) %>%
    filter(!(habitat %in% c(
      "mangrove_inland1km", "mangrove_offshore"
    ))) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))

  ## join layer data
  d <-  extent %>%
    full_join(health, by = c("region_id", "habitat")) %>%
    full_join(trend, by = c("region_id", "habitat"))

  ## set ranks for each habitat
  habitat.rank <- c('mangrove'         = 139,
                    'saltmarsh'        = 210,
                    'seagrass'         = 83)

  ## limit to CS habitats and add rank
  d <- d %>%
    mutate(rank = habitat.rank[habitat],
           extent = ifelse(extent == 0, NA, extent))

  # status
  status <- d %>%
    filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
    group_by(region_id) %>%
    summarize(score = pmin(1, sum(rank * health * extent, na.rm = TRUE) / (sum(
      extent * rank, na.rm = TRUE
    ))) * 100,
    dimension = 'status') %>%
    ungroup()

  # trend

  trend <- d %>%
    filter(!is.na(rank) & !is.na(trend) & !is.na(extent)) %>%
    group_by(region_id) %>%
    summarize(score = sum(rank * trend * extent, na.rm = TRUE) / (sum(extent *
                                                                        rank, na.rm = TRUE)),
              dimension = 'trend') %>%
    ungroup()


  scores_CS <- rbind(status, trend)  %>%
    mutate(goal = 'CS') %>%
    select(goal, dimension, region_id, score)

  ## create weights file for pressures/resilience calculations
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
  ## read in layers
  scen_year <- layers$data$scenario_year

  # layers for coastal protection
  extent_lyrs <-
    c(
      'hab_mangrove_extent',
      'hab_seagrass_extent',
      'hab_saltmarsh_extent',
      'hab_coral_extent',
      'hab_seaice_extent'
    )
  health_lyrs <-
    c(
      'hab_mangrove_health',
      'hab_seagrass_health',
      'hab_saltmarsh_health',
      'hab_coral_health',
      'hab_seaice_health'
    )
  trend_lyrs <-
    c(
      'hab_mangrove_trend',
      'hab_seagrass_trend',
      'hab_saltmarsh_trend',
      'hab_coral_trend',
      'hab_seaice_trend'
    )

  # get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    filter(!(habitat %in% "seaice_edge")) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, extent = km2) %>%
    mutate(habitat = as.character(habitat))

  health <- AlignManyDataYears(health_lyrs) %>%
    filter(!(habitat %in% "seaice_edge")) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <- AlignManyDataYears(trend_lyrs) %>%
    filter(!(habitat %in% "seaice_edge")) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))

  ## sum mangrove_offshore + mangrove_inland1km = mangrove to match with extent and trend
  mangrove_extent <- extent %>%
    filter(habitat %in% c('mangrove_inland1km', 'mangrove_offshore'))

  if (nrow(mangrove_extent) > 0) {
    mangrove_extent <- mangrove_extent %>%
      group_by(region_id) %>%
      summarize(extent = sum(extent, na.rm = TRUE)) %>%
      mutate(habitat = 'mangrove') %>%
      ungroup()
  }

  extent <- extent %>%
    filter(!habitat %in% c('mangrove', 'mangrove_inland1km', 'mangrove_offshore')) %>%  #do not use all mangrove
    rbind(mangrove_extent)  #just the inland 1km and offshore

  ## join layer data
  d <-  extent %>%
    full_join(health, by = c("region_id", "habitat")) %>%
    full_join(trend, by = c("region_id", "habitat"))

  ## set ranks for each habitat
  habitat.rank <- c(
    'coral'            = 4,
    'mangrove'         = 4,
    'saltmarsh'        = 3,
    'seagrass'         = 1,
    'seaice_shoreline' = 4
  )

  ## limit to CP habitats and add rank
  d <- d %>%
    filter(habitat %in% names(habitat.rank)) %>%
    mutate(rank = habitat.rank[habitat],
           extent = ifelse(extent == 0, NA, extent))


  # status
  scores_CP <- d %>%
    filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
    group_by(region_id) %>%
    summarize(score = pmin(1, sum(rank * health * extent, na.rm = TRUE) /
                             (sum(
                               extent * rank, na.rm = TRUE
                             ))) * 100) %>%
    mutate(dimension = 'status') %>%
    ungroup()

  # trend
  d_trend <- d %>%
    filter(!is.na(rank) & !is.na(trend) & !is.na(extent))

  if (nrow(d_trend) > 0) {
    scores_CP <- dplyr::bind_rows(
      scores_CP,
      d_trend %>%
        group_by(region_id) %>%
        summarize(
          score = sum(rank * trend * extent, na.rm = TRUE) / (sum(extent * rank, na.rm =
                                                                    TRUE)),
          dimension = 'trend'
        )
    )
  } else {
    # if no trend score, assign NA
    scores_CP <- dplyr::bind_rows(scores_CP,
                                  d %>%
                                    group_by(rgn_id) %>%
                                    summarize(score = NA,
                                              dimension = 'trend'))
  }

  ## finalize scores_CP
  scores_CP <- scores_CP %>%
    mutate(goal = 'CP') %>%
    select(region_id, goal, dimension, score)


  ## create weights file for pressures/resilience calculations

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

  # return scores
  return(scores_CP)

}

TR <- function(layers) {
  ## formula:
  ##  E   = Ep                         # Ep: % of direct tourism jobs. tr_jobs_pct_tourism.csv
  ##  S   = (S_score - 1) / (7 - 1)    # S_score: raw TTCI score, not normalized (1-7). tr_sustainability.csv
  ##  Xtr = E * S

  pct_ref <- 90

  scen_year <- layers$data$scenario_year


  ## read in layers

  tourism <-
    AlignDataYears(layer_nm = "tr_jobs_pct_tourism", layers_obj = layers) %>%
    select(-layer_name)
  sustain <-
    AlignDataYears(layer_nm = "tr_sustainability", layers_obj = layers) %>%
    select(-layer_name)

  tr_data  <-
    full_join(tourism, sustain, by = c('rgn_id', 'scenario_year'))

  tr_model <- tr_data %>%
    mutate(E   = Ep,
           S   = (S_score - 1) / (7 - 1),
           # scale score from 1 to 7.
           Xtr = E * S)


  # regions with Travel Warnings
  rgn_travel_warnings <-
    AlignDataYears(layer_nm = "tr_travelwarnings", layers_obj = layers) %>%
    select(-layer_name)

  ## incorporate Travel Warnings
  tr_model <- tr_model %>%
    left_join(rgn_travel_warnings, by = c('rgn_id', 'scenario_year')) %>%
    mutate(Xtr = ifelse(!is.na(multiplier), multiplier * Xtr, Xtr)) %>%
    select(-multiplier)


  ### Calculate status based on quantile reference (see function call for pct_ref)
  tr_model <- tr_model %>%
    group_by(scenario_year) %>%
    mutate(Xtr_q = quantile(Xtr, probs = pct_ref / 100, na.rm = TRUE)) %>%
    mutate(status  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) %>% # rescale to qth percentile, cap at 1
    ungroup()

  # get status
  tr_status <- tr_model %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, score = status) %>%
    mutate(score = score * 100) %>%
    mutate(dimension = 'status')


  # calculate trend

  trend_data <- tr_model %>%
    filter(!is.na(status))

  trend_years <- (scen_year - 4):(scen_year)

  tr_trend <-
    CalculateTrend(status_data = trend_data, trend_years = trend_years)


  # bind status and trend by rows
  tr_score <- bind_rows(tr_status, tr_trend) %>%
    mutate(goal = 'TR')


  # assign NA for uninhabitated islands
  if (conf$config$layer_region_labels == 'rgn_global') {
    unpopulated = layers$data$le_popn %>%
      group_by(rgn_id) %>%
      filter(count == 0) %>%
      select(region_id = rgn_id)
    tr_score$score = ifelse(tr_score$region_id %in% unpopulated$region_id,
                            NA,
                            tr_score$score)
  }

  # return final scores
  scores <- tr_score %>%
    select(region_id, goal, dimension, score)

  return(scores)
}

LIV <- function(layers){

  ## read in all data: gdp, wages, jobs and workforce_size data

  le_wages = SelectLayersData(layers, layers='le_wage_sector_year') %>%
    dplyr::select(rgn_id = id_num, year, sector = category, wage_usd = val_num)

  le_jobs  = SelectLayersData(layers, layers='le_jobs_sector_year') %>%
    dplyr::select(rgn_id = id_num, year, sector = category, jobs = val_num)

  le_workforce_size = SelectLayersData(layers, layers='le_workforcesize_adj') %>%
    dplyr::select(rgn_id = id_num, year, jobs_all = val_num)

  le_unemployment = SelectLayersData(layers, layers='le_unemployment') %>%
    dplyr::select(rgn_id = id_num, year, pct_unemployed = val_num)


  ## multipliers from Table S10 (Halpern et al 2012 SOM)
  multipliers_jobs = data.frame('sector' = c('tour','cf', 'mmw', 'wte','mar'),
                                'multiplier' = c(1, 1.582, 1.915, 1.88, 2.7))
  ## multipler not listed for tour (=1)

  # calculate employment counts
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('rgn_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) / 100,
           employed            = jobs_all * proportion_employed)

  liv =
    # adjust jobs
    le_jobs %>%
    left_join(multipliers_jobs, by = 'sector') %>%
    mutate(jobs_mult = jobs * multiplier) %>%  # adjust jobs by multipliers
    left_join(le_employed, by= c('rgn_id', 'year')) %>%
    mutate(jobs_adj = jobs_mult * proportion_employed) %>% # adjust jobs by proportion employed
    left_join(le_wages, by=c('rgn_id','year','sector')) %>%
    arrange(year, sector, rgn_id)

  # LIV calculations ----

  # LIV status
  liv_status = liv %>%
    filter(!is.na(jobs_adj) & !is.na(wage_usd))
  if (nrow(liv_status)==0){
    liv_status = liv %>%
      dplyr::select(region_id=rgn_id) %>%
      group_by(region_id) %>%
      summarize(
        goal      = 'LIV',
        dimension = 'status',
        score     = NA)
    liv_trend = liv %>%
      dplyr::select(region_id=rgn_id) %>%
      group_by(region_id) %>%
      summarize(
        goal      = 'LIV',
        dimension = 'trend',
        score     = NA)
  } else {
    liv_status = liv_status %>%
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      arrange(rgn_id, year, sector) %>%
      # summarize across sectors
      group_by(rgn_id, year) %>%
      summarize(
        # across sectors, jobs are summed
        jobs_sum  = sum(jobs_adj, na.rm=T),
        # across sectors, wages are averaged
        wages_avg = mean(wage_usd, na.rm=T)) %>%
      group_by(rgn_id) %>%
      arrange(rgn_id, year) %>%
      mutate(
        # reference for jobs [j]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        jobs_sum_first  = first(jobs_sum),                     # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
        # original reference for wages [w]: target value for average annual wages is the highest value observed across all reporting units
        # new reference for wages [w]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        wages_avg_first = first(wages_avg)) %>% # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
      # calculate final scores
      ungroup() %>%
      mutate(
        x_jobs  = pmax(-1, pmin(1,  jobs_sum / jobs_sum_first)),
        x_wages = pmax(-1, pmin(1, wages_avg / wages_avg_first)),
        score   = mean(c(x_jobs, x_wages), na.rm=T) * 100) %>%
      # filter for most recent year
      filter(year == max(year, na.rm=T)) %>%
      # format
      dplyr::select(
        region_id = rgn_id,
        score) %>%
      mutate(
        goal      = 'LIV',
        dimension = 'status')

    ## LIV trend ----

    # get trend across years as slope of individual sectors for jobs and wages
    liv_trend = liv %>%
      filter(!is.na(jobs_adj) & !is.na(wage_usd)) %>%
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      # get sector weight as total jobs across years for given region
      arrange(rgn_id, year, sector) %>%
      group_by(rgn_id, sector) %>%
      mutate(
        weight = sum(jobs_adj, na.rm=T)) %>%
      # reshape into jobs and wages columns into single metric to get slope of both
      reshape2::melt(id=c('rgn_id','year','sector','weight'), variable='metric', value.name='value') %>%
      mutate(
        sector = as.character(sector),
        metric = as.character(metric)) %>%
      # get linear model coefficient per metric
      group_by(metric, rgn_id, sector, weight) %>%
      do(mdl = lm(value ~ year, data=.)) %>%
      summarize(
        metric = metric,
        weight = weight,
        rgn_id = rgn_id,
        sector = sector,
        sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
      arrange(rgn_id, metric, sector) %>%
      # get weighted mean across sectors per region-metric
      group_by(metric, rgn_id) %>%
      summarize(
        metric_trend = weighted.mean(sector_trend, weight, na.rm=T)) %>%
      # get mean trend across metrics (jobs, wages) per region
      group_by(rgn_id) %>%
      summarize(
        score = mean(metric_trend, na.rm=T)) %>%
      # format
      mutate(
        goal      = 'LIV',
        dimension = 'trend') %>%
      dplyr::select(
        goal, dimension,
        region_id = rgn_id,
        score)
  }

  ## create scores and rbind to other goal scores
  scores = rbind(liv_status, liv_trend) %>%
    dplyr::select(region_id,
                  score,
                  dimension,
                  goal)

  return(scores)

}

ECO <- function(layers){

  ## read in data layers
  le_gdp   = SelectLayersData(layers, layers='le_gdp')  %>%
    dplyr::select(rgn_id = id_num, year, gdp_usd = val_num)

  le_workforce_size = SelectLayersData(layers, layers='le_workforcesize_adj') %>%
    dplyr::select(rgn_id = id_num, year, jobs_all = val_num)

  le_unemployment = SelectLayersData(layers, layers='le_unemployment') %>%
    dplyr::select(rgn_id = id_num, year, pct_unemployed = val_num)


  # calculate employment counts
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('rgn_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) / 100,
           employed            = jobs_all * proportion_employed)

  # ECO calculations ----
  eco = le_gdp %>%
    mutate(
      rev_adj = gdp_usd,
      sector = 'gdp') %>%
    # adjust rev with national GDP rates if available. Example: (rev_adj = gdp_usd / ntl_gdp)
    dplyr::select(rgn_id, year, sector, rev_adj)

  # ECO status
  eco_status = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    # across sectors, revenue is summed
    group_by(rgn_id, year) %>%
    summarize(
      rev_sum  = sum(rev_adj, na.rm=T)) %>%
    # reference for revenue [e]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
    arrange(rgn_id, year) %>%
    group_by(rgn_id) %>%
    mutate(
      rev_sum_first  = first(rev_sum)) %>%
    # calculate final scores
    ungroup() %>%
    mutate(
      score  = pmin(rev_sum / rev_sum_first, 1) * 100) %>%
    # get most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'status') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)

  # ECO trend
  eco_trend = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4 ) %>% # 5 year trend
    # get sector weight as total revenue across years for given region
    arrange(rgn_id, year, sector) %>%
    group_by(rgn_id, sector) %>%
    mutate(
      weight = sum(rev_adj, na.rm=T)) %>%
    # get linear model coefficient per region-sector
    group_by(rgn_id, sector, weight) %>%
    do(mdl = lm(rev_adj ~ year, data=.)) %>%
    summarize(
      weight = weight,
      rgn_id = rgn_id,
      sector = sector,
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    # get weighted mean across sectors per region
    group_by(rgn_id) %>%
    summarize(
      score = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'trend') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)

  ## create scores and rbind to other goal scores
  scores = rbind(eco_status, eco_trend) %>%
    dplyr::select(region_id,
                  score,
                  dimension,
                  goal)

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
    select(region_id = rgn_id, sciname, iucn_cat=category, scenario_year, ico_spp_iucn_status_year) %>%
    mutate(iucn_cat = as.character(iucn_cat))

  # lookup for weights status
  #  LC <- "LOWER RISK/LEAST CONCERN (LR/LC)"
  #  NT <- "LOWER RISK/NEAR THREATENED (LR/NT)"
  #  T  <- "THREATENED (T)" treat as "EN"
  #  VU <- "VULNERABLE (V)"
  #  EN <- "ENDANGERED (E)"
  #  LR/CD <- "LOWER RISK/CONSERVATION DEPENDENT (LR/CD)" treat as between VU and NT
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
    group_by(region_id, sciname, scenario_year, ico_spp_iucn_status_year) %>%
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
    select(goal, dimension, region_id, score) %>%
    data.frame()
  return(scores)

}


LSP <- function(layers) {
  scen_year <- layers$data$scenario_year

  ref_pct_cmpa <- 30
  ref_pct_cp <- 30

  # select data
  total_area <-
    rbind(layers$data$rgn_area_inland1km,
          layers$data$rgn_area_offshore3nm) %>% #total offshore/inland areas
    select(region_id = rgn_id, area, layer) %>%
    spread(layer, area) %>%
    select(region_id,
           area_inland1km = rgn_area_inland1km,
           area_offshore3nm = rgn_area_offshore3nm)


  offshore <-
    AlignDataYears(layer_nm = "lsp_prot_area_offshore3nm", layers_obj = layers) %>%
    select(region_id = rgn_id,
           year = scenario_year,
           cmpa = a_prot_3nm)
  inland <-
    AlignDataYears(layer_nm = "lsp_prot_area_inland1km", layers_obj = layers) %>%
    select(region_id = rgn_id,
           year = scenario_year,
           cp = a_prot_1km)


  # ry_offshore <-  layers$data$lsp_prot_area_offshore3nm %>%
  #   select(region_id = rgn_id, year, cmpa = a_prot_3nm)
  # ry_inland <- layers$data$lsp_prot_area_inland1km %>%
  #   select(region_id = rgn_id, year, cp = a_prot_1km)
  #
  lsp_data <- full_join(offshore, inland, by = c("region_id", "year"))

  # fill in time series for all regions
  lsp_data_expand <-
    expand.grid(region_id = unique(lsp_data$region_id),
                year = unique(lsp_data$year)) %>%
    left_join(lsp_data, by = c('region_id', 'year')) %>%
    arrange(region_id, year) %>%
    mutate(cp = ifelse(is.na(cp), 0, cp),
           cmpa = ifelse(is.na(cmpa), 0, cmpa)) %>%
    mutate(pa     = cp + cmpa)


  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
  # and calculate status score
  status_data <- lsp_data_expand %>%
    full_join(total_area, by = "region_id") %>%
    mutate(
      pct_cp    = pmin(cp   / area_inland1km   * 100, 100),
      pct_cmpa  = pmin(cmpa / area_offshore3nm * 100, 100),
      status    = (pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1)) / 2
    ) %>%
    filter(!is.na(status))

  # extract status based on specified year

  r.status <- status_data %>%
    filter(year == scen_year) %>%
    mutate(score = status * 100) %>%
    select(region_id, score) %>%
    mutate(dimension = "status")

  # calculate trend

  trend_years <- (scen_year - 4):(scen_year)

  r.trend <-
    CalculateTrend(status_data = status_data, trend_years = trend_years)


  # return scores
  scores <- bind_rows(r.status, r.trend) %>%
    mutate(goal = "LSP")
  return(scores[, c('region_id', 'goal', 'dimension', 'score')])
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
  
  #cw_trash_trend
  
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
    dplyr::select(region_id = rgn_id, status = pesticides, scenario_year)
  
  pest_status <- pest %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score, dimension)
  
  trend_years <- (scen_year - 9):(scen_year)
  pest_trend <- CalculateTrend(status_data = pest, trend_years = trend_years)
  
  #inorganic run-off
  #update this section with built areas and monthly rainfall data
  run_off <- AlignDataYears(layer_nm = "cw_inorganic_run_off2", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = inorg_run_off, scenario_year)
  
  run_off_status <- run_off %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score, dimension)
  
  trend_years <- (scen_year - 9):(scen_year)
  run_off_trend <- CalculateTrend(status_data = run_off, trend_years = trend_years)

  #nutrients
  nut <- AlignDataYears(layer_nm = "cw_nutrients", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id, status = nutrients, scenario_year)
  
  nut_status <- nut %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score, dimension)
  
  trend_years <- (scen_year - 9):(scen_year)
  nut_trend <- CalculateTrend(status_data=nut, trend_years = trend_years)
  
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

  wq_status <- wq %>%
    filter(scenario_year == scen_year) %>%
    mutate(score = status * 100) %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score, dimension)
  
  trend_years <- (scen_year - 9):(scen_year)
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
    filter(scenario_year==scen_year)  %>%
    mutate(dimension = "status") %>%
    dplyr::select(region_id, score, dimension)

  #bring status and trend scores together
  status <- rbind (bw_status, nut_status, pest_status, run_off_status, trash_status, wq_status) 
  
  status <- status %>%
    group_by(region_id) %>%
    summarize(score = geometric.mean2 (score, na.rm=TRUE)) %>% # take geometric mean
    mutate(dimension = "status")%>%
    ungroup()
  
  trend <- rbind(bw_trend, nut_trend, pest_trend, run_off_trend, trash_trend, wq_trend)
  
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


  extent_lyrs <-
    c(
      'hab_mangrove_extent',
      'hab_seagrass_extent',
      'hab_saltmarsh_extent',
      'hab_coral_extent',
      'hab_seaice_extent',
      'hab_softbottom_extent'
    )
  health_lyrs <-
    c(
      'hab_mangrove_health',
      'hab_seagrass_health',
      'hab_saltmarsh_health',
      'hab_coral_health',
      'hab_seaice_health',
      'hab_softbottom_health'
    )
  trend_lyrs <-
    c(
      'hab_mangrove_trend',
      'hab_seagrass_trend',
      'hab_saltmarsh_trend',
      'hab_coral_trend',
      'hab_seaice_trend',
      'hab_softbottom_trend'
    )

  # get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, extent = km2) %>%
    mutate(habitat = as.character(habitat))

  health <- AlignManyDataYears(health_lyrs) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <- AlignManyDataYears(trend_lyrs) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id = rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))


  # join and limit to HAB habitats
  d <- health %>%
    full_join(trend, by = c('region_id', 'habitat')) %>%
    full_join(extent, by = c('region_id', 'habitat')) %>%
    filter(
      habitat %in% c(
        'coral',
        'mangrove',
        'saltmarsh',
        'seaice_edge',
        'seagrass',
        'soft_bottom'
      )
    ) %>%
    mutate(w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
    filter(!is.na(w))

  if (sum(d$w %in% 1 & is.na(d$trend)) > 0) {
    warning(
      "Some regions/habitats have extent data, but no trend data.  Consider estimating these values."
    )
  }

  if (sum(d$w %in% 1 & is.na(d$health)) > 0) {
    warning(
      "Some regions/habitats have extent data, but no health data.  Consider estimating these values."
    )
  }


  ## calculate scores
  status <- d %>%
    group_by(region_id) %>%
    filter(!is.na(health)) %>%
    summarize(score = pmin(1, sum(health) / sum(w)) * 100,
              dimension = 'status') %>%
    ungroup()

  trend <- d %>%
    group_by(region_id) %>%
    filter(!is.na(trend)) %>%
    summarize(score =  sum(trend) / sum(w),
              dimension = 'trend')  %>%
    ungroup()

  scores_HAB <- rbind(status, trend) %>%
    mutate(goal = "HAB") %>%
    select(region_id, goal, dimension, score)


  ## create weights file for pressures/resilience calculations

  weights <- extent %>%
    filter(
      habitat %in% c(
        'seagrass',
        'saltmarsh',
        'mangrove',
        'coral',
        'seaice_edge',
        'soft_bottom'
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


  # return scores
  return(scores_HAB)
}


SPP <- function(layers) {
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
