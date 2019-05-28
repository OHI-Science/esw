
#--------------#
#TO DELETE IN FINAL VERSION
setwd("C:/Folders/Dropbox/github/esw_copy/esw2018")
region_plot     = NA
year_plot       = NA
assessment_name = "OHI Assessment"
dir_fig_save    = "reports/figures"
#--------------#

## ggtheme_plot ----

ggtheme_plot <- function(base_size = 9) {
  theme(axis.ticks = element_blank(),
        text             = element_text(family = 'Helvetica', color = 'gray30', size = base_size),
        plot.title       = element_text(size = rel(2.5), hjust = 0.5, vjust=-2, face = 'bold'), #SKP source size = rel(3) hjust = 0.5
        panel.background = element_blank(),
        legend.position  = "right",
        #legend.position = c(1.1, 0.7), #SKP shift legend position up and right
        #plot.margin = margin(0, 4, 0, 0, unit = "cm"), #SKP shift plot to left
        #legend.position = "none", #removes legend
        panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'grey90', size = .25),
        # panel.grid.major = element_blank(),
        legend.key       = element_rect(colour = NA, fill = NA),
        axis.line        = element_blank()) # element_line(colour = "grey30", size = .5))
        #axis.line        = element_line(colour = "grey30", size = .5)) # draws x axis below plot
}

## adapted from PlotFlower.R from ohicore package
## and ....
## original function by Casey O'Hara, Julia Lowndes, Melanie Frazier

library(tidyverse)
library(stringr)
library(RColorBrewer)
library(rgdal)

PlotFlower <- function(region_plot     = NA,
                       year_plot       = NA,
                       assessment_name = "OHI Assessment",
                       dir_fig_save    = "reports/figures") {

  ## scores data ----
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
  conf <-  readr::read_csv('conf/goals.csv')
  goals_supra <- na.omit(unique(conf$parent))
  supra_lookup <- conf %>%
    filter(goal %in% goals_supra) %>%
    dplyr::select(parent = goal, name_supra = name)

  ## extract conf info for labeling
  conf <- conf %>%
    left_join(supra_lookup, by = 'parent') %>%
    filter(!(goal %in% goals_supra)) %>%
    dplyr::select(goal, order_color, order_hierarchy,
                  weight, name_supra, name_flower) %>%
    mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
    arrange(order_hierarchy)

  ## join scores and conf
  score_df <- scores %>%
    inner_join(conf, by="goal") %>%
    arrange(order_color)

  ## set up positions for the bar centers:
  ## cumulative sum of weights (incl current) minus half the current weight
  score_df <- score_df %>%
    group_by(region_id) %>%
    mutate(pos   = sum(weight) - (cumsum(weight) - 0.5 * weight)) %>%
    mutate(pos_end = sum(weight)) %>%
    ungroup() %>%
    group_by(name_supra) %>%
    ## calculate position of supra goals before any unequal weighting (ie for FP)
    mutate(pos_supra  = ifelse(!is.na(name_supra), mean(pos), NA)) %>%
    ungroup() %>%
    filter(weight != 0) %>%
    ## set up for displaying NAs
    mutate(plot_NA = ifelse(is.na(score), 100, NA))

#FP weights turned off
  ## read if file for weights for FIS vs. MAR
  #w_fn <- list.files(path="layers", pattern = "fp_wildcaught_weight", full.names = TRUE)
  #if( length(w_fn) > 0 ) {
    ## if there are 2 files, ignore global
    #if( length(w_fn) > 1 ) {
      #w_fn_gl <- list.files(path="layers", pattern = "fp_wildcaught_weight_gl", full.names = TRUE)
      #w_fn <- dplyr::setdiff(w_fn, w_fn_gl)
      #message(sprintf('Two weighting files found to plot FIS and MAR with unequal weighting...\nUsing %s, ignoring %s',w_fn, w_fn_gl))
    #} else {
      #message(sprintf('Using %s to plot FIS and MAR with unequal weighting', w_fn))
    #}
    ## read in weights
    #w <- read_csv(w_fn) %>%
      #dplyr::select(rgn_id, w_fis)
    #w <- rbind(w, data.frame(rgn_id = 0, w_fis = mean(w$w_fis))) %>%
      #arrange(rgn_id)
    ## make sure weight regions match regions_plot regions
    #if ( !any(w$rgn_id %in% region_plots) ) {
      #message('`layers/fp_wildcaught_weight*.csv` missing regions...plotting FIS and MAR with equal weighting\n')
      #w_fn = NULL
    #}
  #} else {
    #message('Cannot find `layers/fp_wildcaught_weight*.csv`...plotting FIS and MAR with equal weighting\n')
  #}

  ## create supra goal dataframe for position and labeling
  supra <- score_df %>%
    mutate(name_supra = ifelse(is.na(name_supra), name_flower, name_supra)) %>%
    mutate(name_supra = paste0(name_supra, "\n"),
           name_supra  = gsub("Coastal", "", name_supra, fixed = TRUE)) %>%
    dplyr::select(name_supra, pos_supra) %>%
    unique() %>%
    as.data.frame()

  ## calculate arc: stackoverflow.com/questions/38207390/making-curved-text-on-coord-polar
  supra_df <- supra %>%
    mutate(myAng = seq(-70, 250, length.out = dim(supra)[1])) %>%
    filter(!is.na(pos_supra))

  ## more labeling and parameters
  goal_labels <- score_df %>%
    dplyr::select(goal, name_flower)

  p_limits <- c(0, score_df$pos_end[1])
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
  goals_pal13 <- tibble::tibble(goal = c("MAR","FIS","SPP","HAB","CW","LSP","ICO","ECO","LIV","TR","CP","CS","AO"),
                               color = c("#9A1A4D", "#D84B5B", "#F06352", "#F77753", "#F6955E", "#FFCC81", "#F3E48F", "#CAE297", "#A9D7A4", "#6AC2A7", "#51ACB1", "#3B8DBF", "#5D56A2"))
  goals_pal13 <- goals_pal13 %>%
    arrange(goal)

  goals_pal9 <- tibble::tibble(goal = c("MAR","FIS","SPP","HAB","CW","LSP","ICO","ECO","LIV","TR","CP","CS","AO"),
                               color = c("#BB3153", "#BB3153", "#F06352", "#F06352", "#F77753", "#F6955E", "#F6955E", "#FFCC81", "#FFCC81", "#E4E999", "#88CCA5", "#51ACB1", "#4E70B7"))
  goals_pal9 <- goals_pal9 %>%
    arrange(goal)

  #add colour palette to score_df
#add if statement here for switching between palettes
  score_df <- score_df %>%
    dplyr::left_join(goals_pal13, by = "goal")

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
  readr::write_csv(region_names_all, 'reports/figures/regions_figs.csv')

  ## move into for loop only with region_names to plot
  ## filter only regions to plot
  ## in case region_id 0 was included in regions_list.csv
  region_names <- region_names_all %>%
    filter(region_id %in% region_plots) %>%
    distinct()

  ## loop through to save flower plot for each region ----

  for (region in region_plots) #{ #region = 1

    ## filter region info, setup to plot
    #regional data
    plot_df <- score_df %>%
      filter(region_id == region)
    #centre score
    plot_score_index <- score_index %>%
      filter(region_id == region)
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

    ## inject weights for FIS vs. MAR ----
#source code
    #if (length(w_fn) > 0) {
      ## inject FIS/MAR weights
      #plot_df$weight[plot_df$goal == "FIS"] <- w$w_fis[w$rgn_id == region]
      #plot_df$weight[plot_df$goal == "MAR"] <- 1 - w$w_fis[w$rgn_id == region]
      ## recalculate pos with injected weights arrange by pos for proper ordering
      #plot_df <- plot_df %>%
        #mutate(pos = sum(weight) - (cumsum(weight) - 0.5 * weight)) %>%
        #arrange(pos)
      #}

#SKP assign 90/10 split for graphical plot
      ## inject FIS/MAR weights
      plot_df$weight[plot_df$goal == "FIS"] <- 0.9
      plot_df$weight[plot_df$goal == "MAR"] <- 0.1

      ## recalculate pos with injected weights arrange by pos for proper ordering
      plot_df <- plot_df %>%
        mutate(pos = sum(weight) - (cumsum(weight) - 0.5 * weight)) %>%
        arrange(pos)

    ## set up basic plot parameters
#gradient fill
    #plot_obj <- ggplot(data = plot_df,
                       #aes(x = pos, y = score, fill = score, width = weight))
#scale fill
    plot_obj <- ggplot(data = plot_df,
                       aes(x = pos, y = score, fill = goal, width = weight))

    ## sets up the background/borders to the external boundary (100%) of plot
    plot_obj <- plot_obj +
      geom_bar(aes(y = 100),
               stat = 'identity', color = light_line, fill = white_fill, size = .2) +
      geom_errorbar(aes(x = pos, ymin = 100, ymax = 100, width = weight),
                    size = 0.5, color = light_line, show.legend = NA)

    ## lays any NA bars on top of background, with darker grey:
    if(any(!is.na(plot_df$plot_NA))) {
      plot_obj <- plot_obj +
        geom_bar(aes(x = pos, y = plot_NA),
                 stat = 'identity', color = light_line, fill = light_fill, size = .2)
    }

    ## establish the basics of the flower plot
#if statement here
    col <- goals_pal13$color

    plot_obj <- plot_obj +
      ## plot the actual scores on top of background/borders:
      geom_bar(stat = 'identity', color = dark_line, size = .5) + #SKP: petal sides - source size = .2
      ## emphasize (outer) edge of petal
      geom_errorbar(aes(x = pos, ymin = score, ymax = score),
                    size = 0.5, color = dark_line, show.legend = NA) +
      ## plot zero as a baseline:
      geom_errorbar(aes(x = pos, ymin = 0, ymax = 0),
                    size = 0.5, color = dark_line, show.legend = NA) +
      ## turn linear bar chart into polar coordinates start at 90 degrees (pi*.5)
      coord_polar(start = pi * 0.5) +

#gradient fill
      ## set petal colors to the red-yellow-blue color scale:
      #scale_fill_gradientn(colours=myPalette, na.value="black",
                           #limits = c(0, 100)) +
#scale fill
      scale_fill_manual(values = col, na.value = "black") +
      ## use weights to assign widths to petals:
      scale_x_continuous(labels = plot_df$goal, breaks = plot_df$pos, limits = p_limits) +
      scale_y_continuous(limits = c(-blank_circle_rad,
                                    ifelse(first(goal_labels == TRUE) |
                                             is.data.frame(goal_labels),
                                           150, 100)))

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
      geom_text(aes(label = name_flower, x = pos, y = 125), #SKP: source y = 120 NB >125 does not plot
                hjust = .5, vjust = .3, #SKP: source vjust = .5
                size = 2.9, #SKP: source code size = 3
                color = dark_line)

    #add goal scores
    #round score data
    plot_obj <- plot_obj +
      geom_text(aes(label = paste("(",round(score),")",sep=""), x = pos, y = 125),
                hjust = .5, vjust = 3,
                size = 2.5,
                color = dark_line)

    ## position supra arc and names. x is angle, y is distance from center
    supra_rad  <- 150  ## supra goal radius from center #SKP: source supra_rad  <- 145

    plot_obj <- plot_obj +
      ## add supragoal arcs
      geom_errorbar(data = supra_df, inherit.aes = FALSE,
                    aes(x = pos_supra, ymin = supra_rad, ymax = supra_rad),
                    size = 0.25, show.legend = NA) +
      geom_text(data = supra_df, inherit.aes = FALSE,
                aes(label = name_supra, x = pos_supra, y = supra_rad, angle = myAng),
                hjust = .5, vjust = .5,
                size = 3, #SKP: source size = 3
                color = dark_line)

    #change legend title
    plot_obj <- plot_obj +
      labs(fill = "Goal") + #change legend title




    ### display/save options: print to graphics, save to file
    print(plot_obj)





    ## save plot
    ggsave(filename = fig_save,
           plot = plot_obj,
           device = "png",
           height = 6, width = 8, units = 'in', dpi = 300)

    ### ...then return the plot object for further use
    # return(invisible(plot_obj)) ## can't return with this for loop
  }
}

