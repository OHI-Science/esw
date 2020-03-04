
#STATUS
#!!!plot data_year

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

setwd("C:/Folders/Dropbox/github/esw_copy/esw2018")

library(tidyverse)

#rescale data
n = 1 
#cat data
n = 2

if(n == 1) {f <- list.files("status_data/rescale", full.names = T)}
if(n == 2) {f <- list.files("status_data/cat", full.names = T)}

#colour palate for regions
library(RColorBrewer)
#display.brewer.all()
#display.brewer.pal(n = 6, "Set1")
#colours <- brewer.pal(n = 6, "Set1")
colours <- brewer.pal(n = 6, "Dark2")

#points
pch <- c(0, 2, 1, 4, 5, 6)

for(i in 1:length(f)){
  t <- read.csv(f[i])
  t <- t[t$data_year >= max(t$data_year) - 4 ,]
  if(isTRUE(str_detect(f[i], "MAR"))){
    region_id <- rep(4, 5) #no data for IOS (region 4)
    scenario_year <- c(2006:2010)
    data_year <- c(2006:2010)
    sum_sus_tonnes <- rep(0, 5)
    max_sum_sus_tonnes <- rep(0, 5)
    status <- rep(0, 5)
    dimension <- rep("status", 5)
    ios <- cbind(region_id, scenario_year, data_year, sum_sus_tonnes,max_sum_sus_tonnes, status, dimension)
    t <- rbind(t, ios)
  }
  t <- t %>% 
    arrange(region_id)
  #change na to 0
  t[is.na(t)] <- 0
  if(n == 1) {id <- gsub("status_data/rescale/", "", f[i])}
  if(n == 2) {id <- gsub("status_data/cat/", "", f[i])}
  id <- gsub("status.csv", "", id)
  jpeg(file = paste("C:/Folders/Dropbox/github/esw_copy/esw2018/reports/status/", id, "status.jpeg", sep = ""), res=300, width=1600, height=1200)
  par(mar = c(5,5,2,6))
  plot(NULL, 
       xlim = as.numeric(c(min(t$data_year), max(t$data_year))), 
       ylim = as.numeric(c(min(t$status, na.rm = T), max(t$status, na.rm = T))),
       ylab = "Status",
       xlab = "Year",
       cex.axis = 0.9
  )
  rgns <- unique(t$region_id)
  for(j in 1:length(rgns)){
    if(id == "AO_red_" | id == "CW_nut_" | id == "CW_pest_" | id == "ICO_"){points(t[t$region_id == rgns[j], "data_year"],
                                                                                   t[t$region_id == rgns[j], "status"],
                                                                                   pch = 16,
                                                                                   col = "black")
    }
    else {points(t[t$region_id == rgns[j], "data_year"],
                 t[t$region_id == rgns[j], "status"],
                 pch = pch[j],
                 col = colours[j])
    }
    if(id == "AO_red_" | id == "CW_nut_" | id == "CW_pest_" | id == "ICO_") {lines(t[t$region_id == rgns[j], "data_year"],
                                                                                   t[t$region_id == rgns[j], "status"],
                                                                                   lwd = 2,
                                                                                   type = "l",
                                                                                   col = "black")
    }
    else {lines(t[t$region_id == rgns[j], "data_year"],
                t[t$region_id == rgns[j], "status"],
                lwd = 2,
                type = "l",
                col = colours[j])
    }
    #alternative line plots
    #lines(t[t$region_id == rgns[j], "data_year"], t[t$region_id == rgns[j], "status"], lwd = 2, type = "l", col = colours[j])
    #lines(t[t$region_id == rgns[j], "data_year"], t[t$region_id == rgns[j], "status"], lwd = 2, type = "b", pch = pch[j], col = colours[j])
    #lines(t[t$region_id == rgns[j], "data_year"], t[t$region_id == rgns[j], "status"], lwd = 2, type = "c", col = colours[j])
    #lines(t[t$region_id == rgns[j], "data_year"], t[t$region_id == rgns[j], "status"], lwd = 2, type = "o", pch = pch[j], col = colours[j])
  }
  if(id == "AO_red_" | id == "CW_nut_" | id == "CW_pest_" | id == "ICO_") {legend(x = "topright",
                                                                                  legend = "All regions", 
                                                                                  col = "black",
                                                                                  pch = 16,
                                                                                  bty = "n",
                                                                                  pt.cex = 2,
                                                                                  text.col = "black",
                                                                                  xpd = T,
                                                                                  inset = c(-0.4,0.1))
  }
  else {legend(x = "topright", 
               legend = c("Rgn. 1", "Rgn. 2", "Rgn. 3", "Rgn. 4", "Rgn. 5", "Rgn. 6"), 
               col = colours,
               pch = pch,
               lwd = 2,
               pt.cex = 2,
               bty = "n",
               text.col = colours,
               horiz = F,
               xpd = T,
               inset = c(-0.4,0.1))
  }
  if(n == 1) {mtext("(% of max. data value by region)", side = 2, line = 2.2, cex = 0.8)}
  if(n == 2) {mtext("(classification 0-1)", side = 2, line = 2.2, cex = 0.8)}
  dev.off()
}


#hist data (no time series)

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

setwd("C:/Folders/Dropbox/github/esw_copy/esw2018")

f <- list.files("status_data/hist", full.names = T)

#colour palate for regions
library(RColorBrewer)
#display.brewer.all()
#display.brewer.pal(n = 6, "Set1")
#colours <- brewer.pal(n = 6, "Set1")
colours <- brewer.pal(n = 6, "Dark2")
rgns <- c("Rgn. 1", "Rgn. 2", "Rgn. 3", "Rgn. 4", "Rgn. 5", "Rgn. 6")

for(i in 1:length(f)){
  t <- read.csv(f[i])
  t <- t[t$data_year == max(t$data_year),]
  t <- t %>% 
    arrange(region_id)
  #change na to 0
  t[is.na(t)] <- 0
  id <- gsub("status_data/hist/", "", f[i])
  id <- gsub("status.csv", "", id)
  jpeg(file = paste("C:/Folders/Dropbox/github/esw_copy/esw2018/reports/status/", id, "status.jpeg", sep = ""), res=300, width=1600, height=1200)
  par(mar = c(5,5,2,2))
  barplot(t$status, 
          ylim = c(0, 100),
          ylab = "Status",
          xlab = "Region Id.",
          col = colours,
          names.arg = rgns,
          cex.names = 0.9,
          cex.axis = 0.9
  )
  if(id == "TR_rgn_sv_") {mtext("(% seaview within 1 km of land)", side = 2, line = 2.2, cex = 0.8)}
  if(id == "TR_rec_") {mtext("(recreational potential within 3 nm of coast)", side = 2, line = 2.2, cex = 0.8)}
  if(id == "SPP_") {mtext("(mean species extinction risk)", side = 2, line = 2.2, cex = 0.8)}
  if(id == "HAB_seagrass_") {mtext("(mean health classified 0-1)", side = 2, line = 2.2, cex = 0.8)}
  if(id == "CW_trash_") {mtext("(% of max. data value by region)", side = 2, line = 2.2, cex = 0.8)}
  if(id == "AO_access_") {mtext("(% of coast within 1 km of access point)", side = 2, line = 2.2, cex = 0.8)}
  dev.off()
}


########
########

#STATUS & TREND
#!!!plot data_year

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

#!!!plot data_year

setwd("C:/Folders/Dropbox/github/esw_copy/esw2018")

library(tidyverse)

#rescale data
#n = 1
#cat data
n = 2

if(n == 1) {f <- list.files("status_data/rescale", full.names = T)}
if(n == 2) {f <- list.files("status_data/cat", full.names = T)}

#colour palate for regions
library(RColorBrewer)
#display.brewer.all()
#display.brewer.pal(n = 6, "Set1")
#colours <- brewer.pal(n = 6, "Set1")
colours <- brewer.pal(n = 6, "Dark2")

#points
pch <- c(0, 2, 1, 4, 5, 6)

for(i in 1:length(f)){
  t <- read.csv(f[i])
  t <- t[t$data_year >= max(t$data_year) - 4 ,]
  if(isTRUE(str_detect(f[i], "MAR"))){
    region_id <- rep(4, 5) #no data for IOS (region 4)
    scenario_year <- c(2006:2010)
    data_year <- c(2006:2010)
    sum_sus_tonnes <- rep(0, 5)
    max_sum_sus_tonnes <- rep(0, 5)
    status <- rep(0, 5)
    dimension <- rep("status", 5)
    ios <- cbind(region_id, scenario_year, data_year, sum_sus_tonnes,max_sum_sus_tonnes, status, dimension)
    t <- rbind(t, ios)
  }
  t <- t %>% 
    arrange(region_id)
  #change na to 0
  t[is.na(t)] <- 0
  if(n == 1) {id <- gsub("status_data/rescale/", "", f[i])}
  if(n == 2) {id <- gsub("status_data/cat/", "", f[i])}
  id <- gsub("status.csv", "", id)
  jpeg(file = paste("C:/Folders/Dropbox/github/esw_copy/esw2018/reports/status_trend/", id, "status_trend.jpeg", sep = ""), res=300, width=1600, height=1200)
  par(mar = c(5,5,2,6))
  plot(NULL, 
       xlim = as.numeric(c(min(t$data_year), max(t$data_year))), 
       ylim = as.numeric(c(min(t$status, na.rm = T), max(t$status, na.rm = T))),
       ylab = "Status",
       xlab = "Year",
       cex.axis = 0.9
  )
  rgns <- unique(t$region_id)
  for(j in 1:length(rgns)){
    fit <- lm(t[t$region_id == rgns[j], "status"] ~ t[t$region_id == rgns[j], "data_year"])
    if(id == "AO_red_" | id == "CW_nut_" | id == "CW_pest_" | id == "ICO_"){points(t[t$region_id == rgns[j], "data_year"],
                                                                                   t[t$region_id == rgns[j], "status"],
                                                                                   pch = 16,
                                                                                   col = "black")
    }
    else {points(t[t$region_id == rgns[j], "data_year"],
                 t[t$region_id == rgns[j], "status"],
                 pch = pch[j],
                 col = colours[j],
                 lwd = 2)
    }
    if(id == "AO_red_" | id == "CW_nut_" | id == "CW_pest_" | id == "ICO_") {lines(t[t$region_id == rgns[j], "data_year"], 
                                                                                   fitted(fit), 
                                                                                   lwd = 2, 
                                                                                   col = "black")
    }
    else {lines(t[t$region_id == rgns[j], "data_year"], 
                fitted(fit), 
                lwd = 2,
                col = colours[j])
    }
  }
  if(id == "AO_red_" | id == "CW_nut_" | id == "CW_pest_" | id == "ICO_") {legend(x = "topright",
                                                                                  legend = "All regions", 
                                                                                  col = "black",
                                                                                  pch = 16,
                                                                                  bty = "n",
                                                                                  pt.cex = 2,
                                                                                  text.col = "black",
                                                                                  xpd = T,
                                                                                  inset = c(-0.4,0.1))
  }
  else {legend(x = "topright", 
               legend = c("Rgn. 1", "Rgn. 2", "Rgn. 3", "Rgn. 4", "Rgn. 5", "Rgn. 6"), 
               col = colours,
               pch = pch,
               lwd = 2,
               pt.cex = 2,
               bty = "n",
               text.col = colours,
               horiz = F,
               xpd = T,
               inset = c(-0.4,0.1))
  }
  if(n == 1) {mtext("(% of max. data value by region)", side = 2, line = 2.2, cex = 0.8)}
  if(n == 2) {mtext("(classification 0-1)", side = 2, line = 2.2, cex = 0.8)}
  dev.off()
}

#######


########
########

#ARCHIVE
#NOT RUN

#read in scores.csv
#plot current stauts (for 2018)
#and trend
#by region

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

setwd("C:/Folders/Dropbox/github/esw_copy/esw2018")

library(tidyverse)
#colour palate for regions
library(RColorBrewer)
#display.brewer.all()
#display.brewer.pal(n = 6, "Set1")
#colours <- brewer.pal(n = 6, "Set1")
colours <- brewer.pal(n = 6, "Dark2")

#points
pch <- c(0, 2, 1, 4, 5, 6)

t <- read.csv("scores.csv")
g <- unique(t$goal)
g <- as.data.frame(g)
g <- subset(g, g != "Index")
g <- g$g


region_id <- c(1:6)

for(i in 1:length(g)){
  jpeg(file = paste("C:/Folders/Dropbox/github/esw_copy/esw2018/reports/status_proj_trend/", g[i], "_current_status_proj_trend.jpeg", sep = ""), res=300, width=1600, height=1200)
  par(mar = c(5,5,2,6))
  plot(c(0,4), c(0,1), type = "n", xlab = "Year", ylab = "Status", xaxt = "n", yaxt = "n")
  axis(1, at=0:4, labels = c(2018:2022))
  axis(2, at= c(0, 0.20, 0.4, 0.6, 0.8, 1), labels = c(0, 20, 40, 60, 80, 100))
  
  for(j in 1:length(region_id)){
    abline(t[t$goal == g[i] & t$dimension == "status" & t$region_id == region_id[j], "score"] / 100, 
           b = t[t$goal == g[i] & t$dimension == "trend" & t$region_id == region_id[j], "score"],
           col = colours[j], lwd = 2)
    points(0 ,
           t[t$goal == g[i] & t$dimension == "status" & t$region_id == region_id[j], "score"] / 100,
           #t$status[j] / 100,
           pch = pch[j],
           col = colours[j],
           lwd = 2)
  }
  legend(x = "topright", 
         legend = c("Rgn. 1", "Rgn. 2", "Rgn. 3", "Rgn. 4", "Rgn. 5", "Rgn. 6"), 
         col = colours,
         pch = pch,
         lwd = 2,
         pt.cex = 2,
         bty = "n",
         text.col = colours,
         horiz = F,
         xpd = T,
         inset = c(-0.4,0.1))
  #mtext("(& projected trend)", side = 2, line = 2.2, cex = 0.8)
  dev.off()
}



########
########

#ARCHIVE
#NOT RUN

#CURRENT STATUS & TREND

#trend for
#SPP (population trend)
#TR rec (tourism and population)
#Seaview - none
#AO - none

rm(list=ls()) #clear environment
cat("\014") #clear plots and console

library(tidyverse)
library(RColorBrewer)
colours <- brewer.pal(n = 6, "Dark2")
#colours <- brewer.pal(n = 6, "Set1")
#points
pch <- c(0, 2, 1, 4, 5, 6)

setwd("C:/Folders/Dropbox/github/esw_copy/esw2018")

#######

#seagrass data - current status and trend

t1 <- read.csv("status_data/hist/HAB_seagrass_status.csv") #data 2018
t1[t1 == 0] <- -10 #no data for regions 1 ad 2
t2 <- read.csv("layers/hab_seagrass_trend_esw2018.csv")

jpeg(file = paste("C:/Folders/Dropbox/github/esw_copy/esw2018/reports/status_trend/HAB_seagrass_status_trend.jpeg", sep = ""), res=300, width=1600, height=1200)
par(mar = c(5,5,2,6))
plot(c(0,4), c(0,1), type = "n", xlab = "Year", ylab = "Status", xaxt = "n", yaxt = "n")
axis(1, at=0:4, labels = c(2014:2018))
axis(2, at= c(0, 0.20, 0.4, 0.6, 0.8, 1), labels = c(0, 20, 40, 60, 80, 100))

for(j in 1:length(t1$region_id)){
  if (t1$status[j] > 0) {abline(((t1$status[j] / 100)) + 0.28, b = unique(t2$trend), col = colours[j], lwd = 2)}
  points(4,
         t1$status[j] / 100,
         pch = pch[j],
         col = colours[j],
         lwd = 2)
}

legend(x = "topright", 
       legend = c("Rgn. 3", "Rgn. 4", "Rgn. 5", "Rgn. 6"), 
       col = colours,
       pch = pch,
       lwd = 2,
       pt.cex = 2,
       bty = "n",
       text.col = colours[3:6],
       horiz = F,
       xpd = T,
       inset = c(-0.4,0.1))
#mtext("(& projected trend)", side = 2, line = 2.2, cex = 0.8)
dev.off()


#######

#trash data - current status and projected trend

t1 <- read.csv("status_data/hist/CW_trash_status.csv") #data 2014
t2 <- read.csv("layers/cw_trash_trend_gl2017.csv") #invert trend
t2 <- t2 %>% 
   mutate(trend = -1 * trend)

jpeg(file = paste("C:/Folders/Dropbox/github/esw_copy/esw2018/reports/status_proj_trend/CW_trash_status_proj_trend.jpeg", sep = ""), res=300, width=1600, height=1200)
par(mar = c(5,5,2,6))
plot(c(1,5), c(0,1), type = "n", xlab = "Year", ylab = "Status", xaxt = "n", yaxt = "n")
axis(1, at=1:5, labels = c(2018:2022))
axis(2, at= c(0, 0.20, 0.4, 0.6, 0.8, 1), labels = c(0, 20, 40, 60, 80, 100))

for(j in 1:length(t1$region_id)){
  abline((t1$status[j] / 100) + 0.13, b = unique(t2$trend), col = colours[j], lwd = 2)
  points(1 ,
       t1$status[j] / 100,
       pch = pch[j],
       col = colours[j],
       lwd = 2)
}
legend(x = "topright", 
       legend = c("Rgn. 1", "Rgn. 2", "Rgn. 3", "Rgn. 4", "Rgn. 5", "Rgn. 6"), 
       col = colours,
       pch = pch,
       lwd = 2,
       pt.cex = 2,
       bty = "n",
       text.col = colours,
       horiz = F,
       xpd = T,
       inset = c(-0.4,0.1))
mtext("(& projected trend)", side = 2, line = 2.2, cex = 0.8)
dev.off()

#######

#seagrass data - current status and trend

t1 <- read.csv("status_data/hist/HAB_seagrass_status.csv") #data 2018
t1[t1 == 0] <- -10 #no data for regions 1 ad 2
t2 <- read.csv("layers/hab_seagrass_trend_esw2018.csv")

#jpeg(file = paste("C:/Folders/Dropbox/github/esw_copy/esw2018/reports/status_proj_trend/HAB_seagrass_status_proj_trend.jpeg", sep = ""), res=300, width=1600, height=1200)
par(mar = c(5,5,2,6))
plot(c(0,4), c(0,1), type = "n", xlab = "Year", ylab = "Status", xaxt = "n", yaxt = "n")
axis(1, at=0:4, labels = c(2014:2018))
axis(2, at= c(0, 0.20, 0.4, 0.6, 0.8, 1), labels = c(0, 20, 40, 60, 80, 100))

for(j in 1:length(t1$region_id)){
  if (t1$status[j] > 0) {abline(((t1$status[j] / 100)) - 0.28, b = unique(t2$trend * -1), col = colours[j], lwd = 2)}
  points(4,
       t1$status[j] / 100,
       pch = pch[j],
       col = colours[j],
       lwd = 2)
}


legend(x = "topright", 
       legend = c("Rgn. 3", "Rgn. 4", "Rgn. 5", "Rgn. 6"), 
       col = colours,
       pch = pch,
       lwd = 2,
       pt.cex = 2,
       bty = "n",
       text.col = colours[3:6],
       horiz = F,
       xpd = T,
       inset = c(-0.4,0.1))
mtext("(& projected trend)", side = 2, line = 2.2, cex = 0.8)
dev.off()

#######

###########

