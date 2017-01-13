
# Try to find cumulative rainfall and intensity
#   impacts on the elevation of Lake St. Clair

library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

source("./src/daily_merged_timeseries.R")
data.join <- ImportStClairData("month")

# Combinations of long term cumulative rainfall and 
#   intensity of rainfall
cum.months = seq(12*3, 12*6, by = 2)
intense.months = seq(12*7, 12*12, by = 2)

combinations <- expand.grid(cumulative = cum.months,
                            intensity = intense.months)

combinations$p.cum.log <- -1
combinations$p.intense.log <- -1
combinations$r.squared <- -1

# Loop through all the possible combinations of cumulative
#   precip and precip intensity values, and record stats
pb <- txtProgressBar(min = 0, max = nrow(combinations), style = 3)
i <- 0

while (i < nrow(combinations)) {
  i <- i + 1
  data.current <- data.join
  
  cum.month <- combinations[i, "cumulative"]
  intense.month <- combinations[i, "intensity"]
  
  series0 <- rollapplyr(data.current$precip.merge, 
                        width = 6, 
                        FUN = sum, 
                        fill = NA)
  series1 <- rollapplyr(data.current$precip.merge, 
                        width = cum.month, 
                        FUN = sum, 
                        fill = NA)
  series2 <- rollapplyr(data.current$precip.intensity, 
                        width = intense.month, 
                        FUN = sum, 
                        fill = NA)

  data.current$sum.precip.season <- series0
  data.current$sum.precip <- series1
  data.current$sum.intensity <- series2

  data.current.lm <- with(data.current, 
                          lm(elevation ~ sum.precip.season + sum.precip + sum.intensity))
  
  combinations$p.cum.log[i] <- log(summary(data.current.lm)$coefficients[3+12])
  combinations$p.intense.log[i] <- log(summary(data.current.lm)$coefficients[4+12])
  combinations$r.squared[i] <- summary(data.current.lm)$adj.r.squared
  
  setTxtProgressBar(pb, i)
  
  #summary(data.current.lm)
}

close(pb)

# Simple plot of r squared vs. months (for intensity)
plot(combinations$intensity, combinations$r.squared)
plot(combinations$cumulative, combinations$r.squared)

best.fit <- combinations[which.max(combinations$r.squared), ]



