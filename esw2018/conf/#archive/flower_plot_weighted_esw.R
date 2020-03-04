
rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#-------------#
#generate N random positive integers that sum to M. 
#selected around a normal distribution whose mean is M/N, 
#with a small standard deviation 

rand_vect <- function(N, M, sd = 0.5, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M, digits = 1)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}

#generate table of positive weights with no zeros 
#TT <- TRUE
#while(length(TT) > 0){
  #v <- rand_vect(13, 9, pos.only = TRUE)
  #v[v == 0] <- NA
  #unique(v[is.na(v)])
  #TT <- is.na(unique(v[is.na(v)]))
#}

#weights <- v

#-------------#


## ggtheme_plot ----

library(ggplot2)

ggtheme_plot <- function(base_size = 9) {
  theme(axis.ticks = element_blank(),
        text             = element_text(family = '', color = 'gray30', size = base_size), #source: 'Helvetica'
        plot.title       = element_text(size = rel(2), hjust = 0.5, vjust=-2, face = 'bold'), #SKP source size = rel(3) hjust = 0.5
        #panel.background = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        #legend.position  = "right",
        legend.position = c(1.1, 0.5), #shift legend position (left/right) (up/down)
        plot.margin = margin(0, 4, 0, 0, unit = "cm"), #shift plot to left #turn off with no legend
        #legend.position = "none", #removes legend
        panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'grey90', size = .25),
        # panel.grid.major = element_blank(),
        legend.key       = element_rect(colour = NA, fill = NA),
        axis.line        = element_blank())# element_line(colour = "grey30", size = .5))
        #axis.line        = element_line(colour = "grey30", size = .5)) # draws x axis below plot
}

## adapted from PlotFlower.R from ohicore package, and from visualisation.R (github: OHI-Science/bhi)
## original function by Casey O'Hara, Julia Lowndes, Melanie Frazier

library(tidyverse)
library(stringr)
library(RColorBrewer)
library(rgdal)
#install.packages("circlize")
library(circlize) #for curved labels

#---------------#
setwd("C:/Folders/Dropbox/github/esw_copy/esw2018")
region_plot     = NA
year_plot       = NA
assessment_name = "South West England"
dir_fig_save    = "reports/figures_wt"
scale_fill = TRUE
gradient_fill = FALSE
#---------------#

## scores data
scores <- read.csv("scores.csv") %>%
  mutate(goal = as.character(goal))

## if there is no year variable in the data, the current year is assigned
if(sum(names(scores) == "year") == 0){
  scores$year <- substring(date(), 21, 24)
}

## if there are multiple years in the dataset and no year_plot argument,
## the most recent year of data is dplyr::selected
if(is.na(year_plot)){
  scores <- scores %>%
    filter(year == max(year))
}

## filters the region of interest, otherwise all goals are printed
if (!any(is.na(region_plot))){
  scores <- scores %>%
    filter(region_id %in% region_plot)
}

## filter only score dimension
#and remove centre mean values 'index'
scores <- scores %>%
  filter(dimension == 'score') 

## labeling:: Index score for center labeling before join with conf

score_index <- scores %>%
  filter(goal == "Index") %>%
  dplyr::select(region_id, score) %>%
  mutate(score = round(score))

## unique regions to plot
region_plots <- unique(scores$region_id)

## goals.csv configuration info
## read in conf/goals.csv, start dealing with supra goals
conf <-  readr::read_csv('conf/goals_wt1.csv')
goals_supra <- na.omit(unique(conf$parent))
supra_lookup <- conf %>%
  filter(goal %in% goals_supra) %>%
  dplyr::select(parent = goal, name_supra = name)

## extract conf info for labeling
conf <- conf %>%
  left_join(supra_lookup, by = 'parent') %>%
  filter(!(goal %in% goals_supra)) %>%
  dplyr::select(goal, order_color, order_hierarchy, order_calculate, order_leg,
                weight, name_supra, name, name_flower) %>%
  mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
  arrange(order_hierarchy)

#---------------#
#generate table of positive weights with no zeros 
TT <- TRUE
while(length(TT) > 0){
  v <- rand_vect(13, 9, pos.only = TRUE)
  v[v == 0] <- NA
  unique(v[is.na(v)])
  TT <- is.na(unique(v[is.na(v)]))
}
#v

#turn back on
weights <- v

#---------------#

#OR read in weights .csv here (from conf folder)

#---------------#

#read in weights, tidy decimal place, format and overwrite conf data
weights <- format(round(weights, 1), nsmall = 1)
weights <- as.numeric(weights)
conf$weight <- weights

## join scores and conf
score_df <- scores %>%
  inner_join(conf, by="goal") %>%
  arrange(order_color)

## set up positions for the bar centers:
## cumulative sum of weights (incl current) minus half the current weight
score_df <- score_df %>%
  group_by(region_id) %>%
  mutate(pos = sum(weight) - (cumsum(weight) - 0.5 * weight)) %>%
  mutate(pos_end = sum(weight)) %>%
  #mutate(pos_end = 9) %>%
  ungroup() %>%
  group_by(name_supra) %>%
  ## calculate position of supra goals before any unequal weighting (ie for FP)
  mutate(pos_supra  = ifelse(!is.na(name_supra), mean(pos), NA)) %>%
  ungroup() %>%
  filter(weight != 0) %>%
  ## set up for displaying NAs
  mutate(plot_NA = ifelse(is.na(score), 100, NA))

## create supra goal dataframe for position and labeling
#not used
supra <- score_df %>%
  mutate(name_supra = ifelse(is.na(name_supra), name_flower, name_supra)) %>%
  mutate(name_supra = paste0(name_supra, "\n"),
         name_supra  = gsub("Coastal", "", name_supra, fixed = TRUE)) %>%
  dplyr::select(name_supra, pos_supra) %>%
  unique() %>%
  as.data.frame()

## calculate arc: stackoverflow.com/questions/38207390/making-curved-text-on-coord-polar
#not used
supra_df <- supra %>%
  mutate(myAng = seq(-70, 250, length.out = dim(supra)[1])) %>%
  filter(!is.na(pos_supra))

## more labeling and parameters
goal_labels <- score_df %>%
  dplyr::select(goal, name_flower)

p_limits <- c(-0.001, score_df$pos_end[1]) # small neg min x axis needed to allow dodging of petals 
blank_circle_rad <- 42
light_line <- 'grey90'
white_fill <- 'white'
light_fill <- 'grey80'
med_line   <- 'grey50'
med_fill   <- 'grey52'
dark_line  <- 'grey20'
dark_fill  <- 'grey22'

#color palette for gradient fill
reds <- grDevices::colorRampPalette(
  c("#A50026", "#F46D43", "#FEE090"), space="Lab")(50) #source: #65 #c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090")
blues <- grDevices::colorRampPalette(
  c("#E0F3F8", "#74ADD1", "#313695"))(50) #source: #35 #c("#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695")
myPalette <- c(reds, blues)

#colours for scale fill
goals_pal <- tibble::tibble(goal = c("MAR","FIS","SPP","HAB","CW","LSP","ICO","ECO","LIV","TR","CP","CS","AO"),
                            color = c("#9A1A4D", "#D84B5B", "#F06352", "#F77753", "#F6955E", "#FFCC81", "#F3E48F", "#CAE297", "#A9D7A4", "#6AC2A7", "#51ACB1", "#3B8DBF", "#5D56A2"))
#goals_pal <- tibble::tibble(goal = c("MAR","FIS","SPP","HAB","CW","LSP","ICO","ECO","LIV","TR","CP","CS","AO"),
#color = c("#BB3153", "#BB3153", "#F06352", "#F06352", "#F77753", "#F6955E", "#F6955E", "#FFCC81", "#FFCC81", "#E4E999", "#88CCA5", "#51ACB1", "#4E70B7"))
goals_pal <- goals_pal %>%
  arrange(goal)

#add colour palette to score_df
score_df <- score_df %>%
  dplyr::left_join(goals_pal, by = "goal")

## filenaming for labeling and saving
## order regions to start with whole study_area
region_names_all <- bind_rows(
  tibble(region_id = 0, region_name = assessment_name),
  read_csv('spatial/regions_list.csv') %>%
    dplyr::select(region_id = rgn_id,
                  region_name = rgn_name)) %>%
  mutate(flower_png = sprintf('%s/flower_%s.png',
                              dir_fig_save,
                              str_replace_all(region_name, ' ', '')))
## write out filenames
readr::write_csv(region_names_all, 'reports/figures_wt/regions_figs.csv')

## filter only regions to plot
## in case region_id 0 was included in regions_list.csv
region_names <- region_names_all %>%
  filter(region_id %in% region_plots) %>%
  distinct()

## loop through to save flower plot for each region

for (region in region_plots) {
  #region = 1
  ## filter region info, setup to plot
  #regional data
  plot_df <- score_df %>%
    filter(region_id == region)
  
  #centre score (non-weighted mean)
  plot_score_index <- score_index %>%
    filter(region_id == region)
  
  #calculate centre score from weighted scores and overwrite
  plot_score_index$score <- round(weighted.mean(plot_df$score, plot_df$weight))
    
  #fig_name to save
  fig_save <- region_names$flower_png[region_names$region_id == region]
  
  #labeling:: region name for title
  region_name <- region_names %>%
    filter(region_id == region) %>%
    dplyr::select(region_name)
  #reverse plot labels  e.g. 3 Cornwall to Cornwall (3)
  #for regions 1 to 6
  txt <- gsub("[[:digit:]] ", "", region_name)
  int <- gsub("[^0-9.-]", "", region_name)
  if(int >= 1) {region_name <- paste(txt, " ", "(Rgn. ", int, ")",sep = "")} #SKP
  
  ## set up basic plot parameters
  
  #gradient fill
  if(isTRUE(gradient_fill)){
    plot_obj <- ggplot(data = plot_df,
                       aes(x = pos, y = score, fill = score, width = weight))
  }
  
  #scale fill
  if(isTRUE(scale_fill)){
    plot_obj <- ggplot(data = plot_df, aes(x = pos, y = score, fill = goal, width = weight))
  }
  
  ## sets up the background/borders to the external boundary (100%) of plot
  plot_obj <- plot_obj +
    geom_bar(aes(y = 100), stat = 'identity', color = light_line, fill = white_fill, size = .2) +
    geom_errorbar(aes(x = pos, ymin = 100, ymax = 100, width = weight), size = 0.5, color = light_line, show.legend = NA)
  
  ## lays any NA bars on top of background, with darker grey:
  if(any(!is.na(plot_df$plot_NA))) {
    plot_obj <- plot_obj +
      geom_bar(aes(x = pos, y = plot_NA), stat = 'identity', color = light_line, fill = light_fill, size = .2)
  }
  
  ## establish the basics of the flower plot
  col <- goals_pal$color
  
  #gradient fill
  if(isTRUE(gradient_fill)){
    plot_obj <- plot_obj +
      ## plot the actual scores on top of background/borders:
      geom_bar(stat = 'identity', color = dark_line, size = .5) + #petal sides - source size = .2 skp .5
      ## emphasize (outer) edge of petal
      geom_errorbar(aes(x = pos, ymin = score, ymax = score),
                    size = 0.5, color = dark_line, show.legend = NA) +
      ## plot zero as a baseline:
      geom_errorbar(aes(x = pos, ymin = 0, ymax = 0),
                    size = 0.5, color = dark_line, show.legend = NA) +
      ## turn linear bar chart into polar coordinates start at 90 degrees (pi*.5)
      coord_polar(start = pi * 0.5) +
      ## set petal colors to the red-yellow-blue color scale:
      scale_fill_gradientn(colours=myPalette, na.value="black",
                           limits = c(0, 100)) +
      ## use weights to assign widths to petals:
      scale_x_continuous(labels = plot_df$goal, breaks = plot_df$pos, limits = p_limits) +
      scale_y_continuous(limits = c(-blank_circle_rad,
                                    ifelse(first(goal_labels == TRUE) |
                                             is.data.frame(goal_labels),
                                           150, 100)))
  }
  
  #scale fill
  if(isTRUE(scale_fill)){
    plot_obj <- plot_obj +
      ## plot the actual scores on top of background/borders:
      geom_bar(stat = 'identity', color = dark_line, size = .5) + #petal sides - source size = .2 skp .5
      ## emphasize (outer) edge of petal
      geom_errorbar(aes(x = pos, ymin = score, ymax = score),
                    size = 0.5, color = dark_line, show.legend = NA) +
      ## plot zero as a baseline:
      geom_errorbar(aes(x = pos, ymin = 0, ymax = 0),
                    size = 0.5, color = dark_line, show.legend = NA) +
      ## turn linear bar chart into polar coordinates start at 90 degrees (pi*.5)
      coord_polar(start = pi * 0.5) +
      scale_fill_manual(values = col, na.value = "black") +
      ## use weights to assign widths to petals:
      scale_x_continuous(labels = plot_df$goal, breaks = plot_df$pos, limits = p_limits) +
      scale_y_continuous(limits = c(-blank_circle_rad,
                                    ifelse(first(goal_labels == TRUE) |
                                             is.data.frame(goal_labels),
                                           150, 100)))
  }
  
  ## add center number and title
  plot_obj <- plot_obj +
    geom_text(data = score_index,
              inherit.aes = FALSE,
              aes(label = plot_score_index$score),
              x = 0, y = -blank_circle_rad,
              hjust = .5, vjust = .5,
              size = 12,
              color = dark_line) +
    labs(title = str_replace_all(region_name, '-', ' - '))
  
  ### clean up the theme
  plot_obj <- plot_obj +
    ggtheme_plot() +
    theme(panel.grid.major = element_blank(),
          axis.line  = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank())
  
  ## add goal names
  plot_obj <- plot_obj +
    geom_text(aes(label = goal, x = pos, y = 125), #source y = 120 NB >125 does not plot #name_flower
              hjust = .5, vjust = 0.3, #source vjust = .5
              size = 2.9, #source code size = 3
              color = dark_line)

  #add goal scores
  #round score data
  plot_obj <- plot_obj +
    geom_text(aes(label = paste("(",round(score),")",sep=""), x = pos, y = 125),
              hjust = .5, vjust = 2,
              size = 2.5,
              color = dark_line)
  
  #---------------#
  #not used YET!!
  #read flower back in
  #plot <- magick::image_read(temp_plot)
  
  #read in temp labels
  #text <- magick::image_read(temp_labels)
  
  #scale images
  #plot <- magick::image_scale(plot, 800)
  #text <- magick::image_scale(magick::image_background(text, "none"), 600)
  
  #composite images
  #plot_obj <- magick::image_composite(text, plot, offset = "-100-10")
  #---------------#
  
  #---------------#
  #create temp legend and edit in paint
  #change legend title
  #and legend text
  #leg_lab <- plot_df %>% 
    #select(goal, name, color, order_leg) %>% 
    #arrange(order_leg) %>% 
    #mutate(goal_name = paste(goal, name, sep = " "))
  
  #plot_obj1 <- plot_obj +
  #labs(fill = "Goal") +
  #scale_fill_manual(labels = c("c", "c", "c", "c","c", "c", "c", "c","c","c", "c", "c", "c"), values = col)
  #scale_fill_manual(labels = leg_lab$goal_name, values = leg_lab$color)
  
  #print(plot_obj1)
  
  #temp_plot <- paste0("temp/legend.png")
  #ggplot2::ggsave(filename = temp_plot, plot = plot_obj1, device = "png", bg = "transparent",
                  #height = 6, width = 8, units = "in", dpi = 300)
  #---------------#
  
  #change legend title
  #and legend text
  
  leg_lab <- plot_df %>% 
  select(goal, name, color, weight) %>% 
  arrange(goal) %>% 
  mutate(goal_name = paste(goal, name, sep = " ")) %>% 
  mutate(goal_prop_wt = round((weight / sum(weight) * 100), 1)) %>% 
  mutate(goal_name_prop_wt = paste(goal_name, " (",goal_prop_wt,")", sep = "")) 
  
  plot_obj <- plot_obj +
  labs(fill = "        Goal (weighting %)") +
  scale_fill_manual(labels = leg_lab$goal_name_prop_wt, values = leg_lab$color)
  print(plot_obj)
  
  
  ### display/save options: print to graphics, save to file
  #print(plot_obj)
  
  #if(class(plot_obj) == "magick-image"){
    #magick::image_write(plot_obj, path = fig_save, format = "png")
  #} else {
  
    #ggplot2::ggsave(filename = fig_save, plot = plot_obj, device = "png",
                    #height = 6, width = 8, units = "in", dpi = 300)
  
     ggplot2::ggsave(filename = fig_save, plot = plot_obj, device = "png",
                      height = 6, width = 8, units = "in", dpi = 400)
     
  #}
}


#sum(weights)
#unique(score_df$pos_end)
#unique(score_df$pos)

#warnings()
#assign("last.warning", NULL, envir = baseenv())



