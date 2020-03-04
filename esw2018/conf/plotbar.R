
#grouped bar chart

library(tidyverse)

#plotbar <- function(scores) {
  t <- read.csv(paste("esw2018/reports/figures","_",ref,"/scores.csv",sep = ""))
  
  t <- scores %>% 
    filter(dimension == "score") %>% 
    filter(region_id != 0) %>% 
    filter(goal!= "Index") %>% 
    select(-dimension) %>% 
    filter(goal == "AO" | goal == "MAR" | goal == "FIS"| goal == "SPP"| goal == "HAB"| goal == "CW"| goal == "SOC"
           | goal == "ECL"| goal == "ECO"| goal == "LIV"| goal == "TR"| goal == "CP"| goal == "CS")
  
  #ord <- tibble(goal = c("AO", "MAR", "FIS", "SPP", "HAB", "CW", "SOC", "ECL", "ECO", "LIV", "TR", "CP", "CS"))
  #t <- left_join(ord, t, by = "goal")
  
  tt <- t %>% 
    spread(goal, score)
  
  ttm <- tt[, c(2:14)]
  ttm <- as.matrix(ttm)
  ttm <- ttm[, c("AO", "MAR", "FIS", "SPP", "HAB", "CW", "SOC", "ECL", "ECO", "LIV", "TR", "CP", "CS")]
  
  colfunc <- colorRampPalette(c("black", "white"))
  colbrs <- colfunc(6)
  
  jpeg(file = paste("esw2018/reports/figures","_",ref,"/barplot_scores.jpeg",sep = "") ,res=400,width=4400,height=2000)
  #jpeg(file = paste("reports/figures","_",ref,"/barplot_scores.jpeg",sep = "") ,res=400,width=4400,height=2000)
  par(mar=c(5,4,4,5))
  
  barplot(ttm, main="",
          xlab = "", ylab = "Score",
          col = colbrs,
          legend = rownames(tt), 
          args.legend=list(x = 100,y = 100,cex = 1.1,bty="n"),
          beside = TRUE)
  
  dev.off()
#}

