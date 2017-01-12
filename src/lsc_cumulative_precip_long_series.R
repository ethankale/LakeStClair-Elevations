
# Try to find short, medium, and long term cumulative rainfall
#   impacts on the elevation of Lake St. Clair

library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

source("./src/daily_merged_timeseries.R")
data.join <- ImportStClairData("month")

# Total window for cumulative rainfall period
combinations <- data.frame(num.months = seq(1, 12*8, by = 1))

combinations$p.log <- -1
combinations$r.squared <- -1

# Loop through all the possible combinations of short & long
#   cumulative precipitation values, and run regressions
#   to pull out the p and adjusted r squared values
pb <- txtProgressBar(min = 0, max = nrow(combinations), style = 3)
i <- 0

while (i < nrow(combinations)) {
  i <- i + 1
  data.current <- data.join
  
  month <- combinations[i, "num.months"]
  
  series <- rollapplyr(data.current$precip.merge, 
                       width = month, 
                       FUN = sum, 
                       fill = NA)

  data.current$sum.precip <- series
  #data.current <- data.current[complete.cases(data.current), ]

  data.current.lm <- with(data.current, 
                          lm(elevation ~ sum.precip))
  
  combinations$p.log[i] <- log(summary(data.current.lm)$coefficients[2+6])
  combinations$r.squared[i] <- summary(data.current.lm)$adj.r.squared
  
  setTxtProgressBar(pb, i)
  
  #summary(data.current.lm)
}

close(pb)

# Plot the number of days vs. the rsquared
ggplot(combinations, aes(x = num.months,
                         y = r.squared)) +
  geom_line() +
  geom_point() +
  labs(title = "Lake St. Clair Elevation and Precipitation Correlation",
       subtitle = paste0("Correlation between cumulative ", 
                         "precipitation and lake level elevation; 1.0 ",
                         " = perfect prediction"),
       x = "Months of cumulative precipitation",
       y = "R Squared") +
  theme_minimal()

ggsave(file = "./results/elevation_correlation_single_r2.png",
       width = 7,
       height = 4)

# Recreate the regression from the best fit
month1 <- 5
month2 <- 12*4
#best.fit <- combinations[month,]

data.current <- data.join
series1 <- rollapplyr(data.current$precip.merge, 
                     width = month1, 
                     FUN = sum, 
                     fill = NA)
series2 <- rollapplyr(data.current$precip.merge, 
                      width = month2, 
                      FUN = sum, 
                      fill = NA)
data.current$sum.precip.1 <- series1
data.current$sum.precip.2 <- series2
data.current.lm <- with(data.current, 
                        lm(elevation ~ sum.precip.1 + sum.precip.2))
data.current$predict <- predict(data.current.lm, data.current)


# Look at elevation vs. cumulative rainfall
data.long <- data.current %>%
  select(Date = measure.mon, 
         `Precip\n(raw)` = precip.merge, 
         `Precip\n(cum 5 mon)` = sum.precip.1, 
         `Precip\n(cum 4 yr)` = sum.precip.2, 
         Elevation = elevation) %>%
  gather(type, measure, -Date)

ggplot(data.long, aes(x = Date,
                      y = measure)) + 
  geom_line() +
  labs(title = "Lake St. Clair Precipitation and Elevation",
       subtitle = paste0("Cumulative precipitation is the sum of raw precipitation"),
       x = "Date",
       y = "Precipitation (inches) and Elevation (feet)") +
  facet_grid(type ~ .,
             scales = "free_y")

ggsave(file = "./results/elevation_correlation_best_fit.png",
       width = 7,
       height = 4)

# Look at actual vs. predicted elevation values
data.predict <- data.current %>%
  select(Date = measure.mon,
         Elevation = elevation,
         `Predicted Elevation` = predict) %>%
  gather(type, elevation, -Date)

cols <- c("Elevation" = "black", "Predicted Elevation" = "red")
ggplot(data.predict, aes(x = Date,
                         y = elevation)) +
  geom_line(aes(group = type,
                color = type)) +
  scale_color_manual(name = "Type of Elevation",
                     values = cols) +
  labs(title = "Lake St. Clair Elevations",
       subtitle = "Actual elevation vs. elevation predicted by regression",
       x = "Date",
       y = "Elevation (feet above mean sea level)") +
  theme_minimal()
  
ggsave(file = "./results/elevation_correlation_predict.png",
       width = 7,
       height = 4)
