
# Try to find cumulative rainfall and intensity
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

  data.current.lm <- with(data.current, 
                          lm(elevation ~ sum.precip))
  
  combinations$p.log[i] <- log(summary(data.current.lm)$coefficients[2+6])
  combinations$r.squared[i] <- summary(data.current.lm)$adj.r.squared
  
  setTxtProgressBar(pb, i)
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
month1 <- 6
month2 <- 6 + 12*3
month3 <- 12*9

data.current <- data.join
series1 <- rollapplyr(data.current$precip.merge, 
                     width = month1, 
                     FUN = sum, 
                     fill = NA)
series2 <- rollapplyr(data.current$precip.merge, 
                      width = month2, 
                      FUN = sum, 
                      fill = NA)
series3 <- rollapplyr(data.current$precip.intensity, 
                      width = month3, 
                      FUN = sum, 
                      fill = NA)
data.current$sum.precip.1 <- series1
data.current$sum.precip.2 <- series2
data.current$sum.intensity <- series3
data.current.lm <- with(data.current, 
#                        lm(elevation ~ sum.precip.1 + sum.precip.2))
                        lm(elevation ~ sum.precip.1 + sum.precip.2 + sum.intensity))
#                        lm(elevation ~ sum.precip.2 + sum.intensity))
#                       lm(elevation ~ sum.precip.1 + sum.intensity))
data.current$predict <- predict(data.current.lm, data.current)


# Look at elevation vs. cumulative rainfall
data.long <- data.current %>%
  select(Date = measure.mon, 
         `D: Precipitation Intensity` = sum.intensity, 
         `B: Cumulative Precipitation (6 month)` = sum.precip.1, 
         `C: Cumulative Precipitation (42 month)` = sum.precip.2, 
         `A: Elevation` = elevation) %>%
  gather(type, measure, -Date)

ggplot(data.long, aes(x = Date,
                      y = measure)) + 
  geom_line() +
  labs(title = "Lake St. Clair Precipitation and Elevation",
       subtitle = paste0("6 month and 42 month cumulative precipitation; 9 year intensity"),
       x = "Date",
       y = "Precipitation (inches) and Elevation (feet)") +
  facet_wrap( ~ type,
             ncol = 1,
             scales = "free_y") +
  theme_minimal()

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

# Look at raw vs. cumulative precipitation values
data.precip.compare <- data.current %>%
  filter(measure.mon > as.yearmon('2010-01', format = "%Y-%m")) %>%
  select(Date = measure.mon,
         Raw = precip.merge,
         Cumulative = sum.precip.1) %>%
  gather(type, precip, -Date)

ggplot(data.precip.compare, aes(x = Date,
                         y = precip)) +
  geom_line() +
  facet_wrap( ~ type,
              ncol = 1) +
  labs(title = "Precipitation, Cumulative vs. Raw",
       subtitle = paste0("Cumulative precipitation in a ",
                          month1,
                          "-month moving window"),
       x = "Date",
       y = "Precipitation (inches)") +
  theme_minimal()
  
ggsave(file = "./results/elevation_correlation_precip_comparison.png",
       width = 7,
       height = 4)

# Look at raw vs. cumulative intensity values
data.intensity.compare <- data.current %>%
  filter(measure.mon > as.yearmon('2010-01', format = "%Y-%m")) %>%
  select(Date = measure.mon,
         Raw = precip.intensity,
         Cumulative = sum.intensity) %>%
  gather(type, intensity, -Date)

ggplot(data.intensity.compare, aes(x = Date,
                         y = intensity)) +
  geom_line() +
  facet_wrap( ~ type,
             ncol = 1,
             scales = "free_y") +
  labs(title = "Precipitation Intensity, Cumulative vs. Raw",
       subtitle = "Cumulative precipitation intensity in a 9-year moving window",
       x = "Date",
       y = "Intensity") +
  theme_minimal()
  
ggsave(file = "./results/elevation_correlation_intensity_comparison.png",
       width = 7,
       height = 4)

# Export precipitation data, raw & cumulative
data.export <- data.current %>%
  select(month = measure.mon,
         precip.in = precip.merge,
         precip.in.sum.6 = sum.precip.1,
         precip.in.sum.42 = sum.precip.2)

write_csv(data.export,
          "./results/eaton_precip_monthly_filled.csv")


# Make a graph of cumulative rainfall at various windows, compared
#   to lake elevation

windows <- c(6, 30, 54)
data.current <- data.join
for (window in windows) {

  series <- rollapplyr(data.current$precip.merge, 
                        width = window, 
                        FUN = sum, 
                        fill = NA)
  
  
  data.current[, paste0("sum.", window)] <- series
}

data.examples <- data.current %>%
  select(measure.mon, 
         Monthly = precip.merge, 
         `Sum\n0.5 Year` = sum.6, 
         `Sum\n2.5 Year` = sum.30, 
         `Sum\n4.5 Year` = sum.54) %>%
  gather(type, precip, -measure.mon)

ggplot(data.examples, aes(x = measure.mon, 
                          y = precip)) +
  geom_line() +
  facet_grid(type ~ .,
             scales = "free_y") +
  labs(title = "Cumulative Rainfall Over Different Periods",
       x = "Date",
       y = "Precipitation (inches)")

ggsave(file = "./results/elevation_correlation_sums_comparison.png",
       width = 7,
       height = 4)


