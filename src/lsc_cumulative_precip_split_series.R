
# Try to find short, medium, and long term cumulative rainfall
#   impacts on the elevation of Lake St. Clair

library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

source("./src/daily_merged_timeseries.R")
data.join <- ImportStClairData("month")

# Use pre-determined best values to create two cumulative
#   precipitation series, and add to the data frame
month1 <- 5
month2 <- 12*9

series1 <- rollapplyr(data.join$precip.merge, 
                     width = month1, 
                     FUN = sum, 
                     fill = NA)
#series2 <- rollapplyr(data.join$precip.merge, 
#                      width = month2, 
#                      FUN = sum, 
#                      fill = NA)
series2 <- rollapplyr(data.join$precip.intensity, 
                      width = month2, 
                      FUN = sum, 
                      fill = NA)
data.join$sum.precip.1 <- series1
data.join$sum.precip.2 <- series2

# Split the data in two and run separate regressions.  This
#   is based on the intuition (from looking at the regression
#   results in lsc_cumulative_precip_long_series.R) that the
#   relationship between cumulative precip and elevation
#   fundamentally changes somewhere around the 2005 mark

split.center <- 224

width <- 12*7
splits <- seq(split.center - as.integer(width/2),
              split.center + as.integer(width/2),
              by = 1)

# Record the p-statistics and r-squared values of each regression
splits.df <- data.frame(month = splits)
splits.df$pl.log <- -1
splits.df$pu.log <- -1
splits.df$r.squared.l <- -1
splits.df$r.squared.u <- -1

pb <- txtProgressBar(min = 0, max = nrow(splits.df), style = 3)
i <- min(splits)
while(i <= max(splits)) {
  i <- i + 1
  j <- i - min(splits)
  
  data.lower <- data.join[1:i,]
  data.upper <- data.join[i:nrow(data.join),]
  
  data.lower.lm <- with(data.lower, 
                          lm(elevation ~ sum.precip.1 + sum.precip.2))
  data.upper.lm <- with(data.upper, 
                          lm(elevation ~ sum.precip.1 + sum.precip.2))
  
  splits.df$pl.log[j] <- log(summary(data.lower.lm)$coefficients[2+6])
  splits.df$pu.log[j] <- log(summary(data.upper.lm)$coefficients[2+6])
  splits.df$r.squared.l[j] <- summary(data.upper.lm)$adj.r.squared
  splits.df$r.squared.u[j] <- summary(data.lower.lm)$adj.r.squared
  
  setTxtProgressBar(pb, j)
  
}

splits.df$r.squared.avg <- (splits.df$r.squared.l + splits.df$r.squared.u) / 2
splits.df$p.avg <- (splits.df$pl.log + splits.df$pu.log) / 2

# Plot the R squared for the splits at different months,
#   for the lower and upper series and their mean
splits.long <- splits.df %>%
  mutate(date = data.join$measure.mon[month]) %>%
  select(date,
         Lower = r.squared.l, 
         Upper = r.squared.u, 
         Mean = r.squared.avg) %>%
  gather(Split, R.Squared, -date)

r2.cols <- c("Mean" = "black", "Lower" = "#d8b365", "Upper" = "#5ab4ac")
ggplot(splits.long, aes(x = date,
                        y = R.Squared)) +
  geom_line(aes(group = Split,
                color = Split)) +
  scale_color_manual(values = r2.cols) +
  labs(title = "Lake St. Clair Correlations with Split Series",
       subtitle = paste0("Correlations between elevation and cumulative precip, ",
                         "split between upper and lower series"),
       x = "Split (date on which data were bifurcated)",
       y = "R Squared") + 
  theme_minimal()

ggsave(file = "./results/elevation_correlation_split_r2.png",
       width = 7,
       height = 4)

# Predict the elevation of Lake St. Clair using both optimized
#   regressions - upper and lower.  Display on single graph.
# Best split value per above appears to be 197, or May of 2004.

# Create regressions
best.month <- which.max(splits.df$r.squared.avg)
i <- splits.df$month[best.month]

data.lower <- data.join[1:i,]
data.upper <- data.join[i:nrow(data.join),]

data.lower.lm <- with(data.lower, 
                      lm(elevation ~ sum.precip.1 + sum.precip.2))
data.upper.lm <- with(data.upper, 
                      lm(elevation ~ sum.precip.1 + sum.precip.2))

data.join$predict.l <- predict(data.lower.lm, data.join)
data.join$predict.u <- predict(data.upper.lm, data.join)

# Select data and convert to long format for charting
data.predict.long <- data.join %>%
  select(date = measure.mon,
         elevation = elevation,
         predict.upper = predict.u,
         predict.lower = predict.l) %>%
  gather(type, elevation, -date)

# Chart
pred.cols <- c("elevation" = "black", 
               "predict.lower" = "#d8b365", 
               "predict.upper" = "#5ab4ac")
pred.labels <- c("elevation" = "Measured",
                 "predict.lower" = "Predicted (lower)",
                 "predict.upper" = "Predicted (upper)")
ggplot(data.predict.long, aes(x = date,
                        y = elevation)) +
  geom_line(aes(group = type,
                color = type)) +
  geom_line(data = subset(data.predict.long, type == "elevation"),
            aes(x = date,
                y = elevation,
                group = type,
                color = type)) +
  scale_color_manual(name = "Elevation",
                     values = pred.cols,
                     labels = pred.labels) +
  labs(title = "Lake St. Clair Predicted Elevations",
       subtitle = paste0("Actual elevations vs elevations predicted using ",
                         "the upper and lower series of data"),
       x = "Date",
       y = "Elevation (feet above mean sea level)") + 
  theme_minimal()

ggsave(file = "./results/elevation_correlation_split_predictions.png",
       width = 7,
       height = 4)

summary(data.lower.lm)
summary(data.upper.lm)

