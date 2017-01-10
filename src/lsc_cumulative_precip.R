
# Try to find short, medium, and long term cumulative rainfall
#   impacts on the elevation of Lake St. Clair

library(readr)
library(dplyr)
library(zoo)
library(ggplot2)

# Calculate three separate rolling-window cumulative sum series.
#   These cumulative sums are non-overlapping; values from shorter
#   series are subtracted from the longer series.
CumSumWindow <- function(series, short, long) {
  series.short <- rollapplyr(series, 
                             width = short, 
                             FUN = sum, 
                             fill = NA)
  series.long <- rollapplyr(series, 
                             width = long, 
                             FUN = sum, 
                             fill = NA)
  series.long <- series.long - series.short
  
  return(data.frame(short = series.short, 
                    long = series.long))
}

# Remove incomplete cases, and also make the number of samples
#   more even between highly-sampled windows (such as the daily
#   sampling from 2011-2014) and less-sampled windows (such as
#   the weekly or so sampling from 2014-2016).
TrimData <- function(df) {
  df.out <- df[complete.cases(df), ]
  df.out <- df.out %>% 
    mutate(years = year(days)) %>%
    group_by(years) %>%
    sample_n(25) %>%
    ungroup() %>%
    select(-years) %>%
    arrange(days)
}

source("./src/daily_merged_timeseries.R")
data.join <- ImportStClairData()

# Sequences of days for short & long cumulative precip series.
#   Short series goes by months
#   Long series goes by quarters
shorts <- seq(60, 274, by = 30)
longs <- seq(365, 2557, by = 91)

# We see a peak in r squared around 1200, so we'll increase the 
#   resolution right there to get a more accurate picture.
#longs2 <- seq(1150, 1250, by = 10)
#longs <- unique(c(longs, longs2)[order(c(longs, longs2))])

combinations <- expand.grid(short = shorts,
                            long = longs)

combinations$short.p <- -1
combinations$long.p <- -1
combinations$r.squared <- -1

# Loop through all the possible combinations of short & long
#   cumulative precipitation values, and run regressions
#   to pull out the p and adjusted r squared values
pb <- txtProgressBar(min = 0, max = nrow(combinations), style = 3)
i <- 0

while (i < nrow(combinations)) {
#while (i < 5) {
  i <- i + 1
  data.current <- data.join
  
  short <- combinations[i, "short"]
  long <- combinations[i, "long"]
  
  #print(combo$short)
  
  series <- CumSumWindow(data.current$precip.merge, short, long)
  data.current$short <- series$short
  data.current$long <- series$long
  
  data.current <- TrimData(data.current)

  data.current.lm <- with(data.current, lm(elevation ~ short + long))
  
  combinations$short.p[i] <- summary(data.current.lm)$coefficients[2+9]
  combinations$long.p[i] <- summary(data.current.lm)$coefficients[3+9]
  combinations$r.squared[i] <- summary(data.current.lm)$adj.r.squared
  
  setTxtProgressBar(pb, i)
  
  #summary(data.current.lm)
}

close(pb)

# Plots of the P values for every value of first the short 
#   part, then the long part
par(mfrow = c(2,1),
    mai = rep(1, 4))
plot(combinations$short, log(combinations$short.p))
plot(combinations$long, log(combinations$long.p))
dev.off()

# Plots of Adjusted R Squared vs. short & long vals
ggplot(combinations,
       aes(x = long, 
           y = r.squared,
           color = short)) + 
  geom_line(aes(group = short)) + 
  geom_point(aes(group = short)) + 
  guides(color = guide_legend(title = "Short (# days)")) +
  labs(title = "Correlation of Lake St. Clair Elevation and Cumulative Precip",
       subtitle = "Cumulative precipitation, divided into short and long parts",
       x = "Long (number of days)",
       y = "Adjusted R Squared") +
  theme_classic()

ggplot(combinations,
       aes(x = short, 
           y = r.squared,
           color = long)) + 
  geom_line(aes(group = long)) + 
  geom_point(aes(group = long)) + 
  guides(color = guide_legend(title = "Long (# days)")) +
  labs(title = "Correlation of Lake St. Clair Elevation and Cumulative Precip",
       subtitle = "Cumulative precipitation, divided into short and long parts",
       x = "Short (number of days)",
       y = "Adjusted R Squared") +
  theme_classic()

# Combined contour plot of r squared vs. short & long values
ggplot(combinations,
       aes(x = short,
           y = long)) +
  geom_raster(aes(fill = log(long.p))) + 
  theme_classic()

# Look at the best fit
m <- which.max(combinations$r.squared)
best.fit <- combinations[m,]

# Recreate the regression from the best fit
data.current <- data.join
short <- combinations[m, "short"]
long <- combinations[m, "long"]
series <- CumSumWindow(data.current$precip.merge, short, long)
data.current$short <- series$short
data.current$long <- series$long
data.current <- TrimData(data.current)
data.current.lm <- with(data.current, lm(elevation ~ short + long))

summary(data.current.lm)

data.current$elev.pred <- predict(data.current.lm, data.current)
plot(data.current$days, data.current$elevation, type = "l")
points(data.current$days, data.current$elev.pred, col = "red", type = "l")
points(data.current$days, data.current$elev.pred, col = "red", pch = 19)


