
# Decompose Lake St. Clair elevation data into seasonal, trend, residual
#   components.

library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(stlplus)
library(ggplot2)
library(tidyr)
library(dplyr)

s.window = "periodic"
t.window = 6 + 12*3

source("./src/daily_merged_timeseries.R")
data.join <- ImportStClairData(period = "month") %>%
  filter(measure.mon >= as.yearmon("1987-10", format = "%Y-%m"))

# Create time series
# First for lake st. clair
data.z <- zoo(data.join$elevation, data.join$measure.mon)
data.ts <- as.ts(data.z, frequency = 12)

data.ts.stl <- stlplus(data.ts,
                       s.window = s.window,
                       t.window = t.window)

data.ts.trend <- tibble(measure.mon = as.yearmon(data.ts.stl$time),
                        elevation.trend = data.ts.stl$data$trend)


# Now timeseries for eaton/10u
eaton.z <- zoo(data.join$precip.merge, data.join$measure.mon)
eaton.ts <- as.ts(eaton.z, frequency = t.window)

eaton.ts.stl <- stlplus(eaton.ts,
                       s.window = s.window,
                       t.window = t.window)

eaton.ts.trend <- tibble(measure.mon = as.yearmon(eaton.ts.stl$time),
                        precip.trend = eaton.ts.stl$data$trend)

# Now timeseries for rainfall intensity
intensity.z <- zoo(data.join$precip.intensity, data.join$measure.mon)
intensity.ts <- as.ts(intensity.z, frequency = 12)

intensity.ts.stl <- stlplus(intensity.ts,
                       s.window = s.window,
                       t.window = t.window)

intensity.ts.trend <- tibble(measure.mon = as.yearmon(intensity.ts.stl$time),
                        precip.trend = intensity.ts.stl$data$trend)


# Graph the elevation data.
data.ts.seasonal <- data.frame(date = data.ts.stl$time,
  elev.s = data.ts.stl$data$seasonal)

data.decompose.long <- data.ts.stl$data %>%
  select(`A: Raw` = raw, 
         `B: Seasonal` = seasonal, 
         `C: Trend (42 month weighted average)` = trend, 
         `D: Remainder` = remainder) %>%
  mutate(month = data.join$measure.mon) %>%
  gather(part, value, -month)

ggplot(data.decompose.long, aes(x = month,
                                y = value)) +
  geom_line() +
  facet_wrap( ~ part,
             ncol = 1,
             scales = "free_y") +
  labs(title = "Lake St. Clair Elevation",
       subtitle = "Raw = Seasonal + Trend + Remainder",
       x = "Date",
       y = "Elevation (feet)") +
  theme_minimal()

ggsave(file = "./results/decompose_lsc.png",
       width = 7,
       height = 4)

png("./results/decompose_lsc_trend.png",
    width = 700,
    height = 400,
    pointsize = 20)
plot(data.ts.stl$time, data.ts.stl$data$trend,
     type = "l",
     main = "Lake St. Clair TS Decomposition Trend")
dev.off()

# Graph the precipitation data.

eaton.decompose.long <- eaton.ts.stl$data %>%
  select(`A: Raw` = raw, 
         `B: Seasonal` = seasonal, 
         `C: Trend` = trend, 
         `D: Remainder` = remainder) %>%
  mutate(month = data.join$measure.mon) %>%
  gather(part, value, -month)

ggplot(eaton.decompose.long, aes(x = month,
                                y = value)) +
  geom_line() +
  facet_wrap( ~ part,
             ncol = 1,
             scales = "free_y") +
  labs(title = "Eaton Station Precipitation",
       subtitle = "Raw = Seasonal + Trend + Remainder",
       x = "Date",
       y = "Precipitation (inches)") +
  theme_minimal()

ggsave(file = "./results/decompose_eaton.png",
       width = 7,
       height = 4)


# Graph the precip data.
png("./results/decompose_eaton_trend.png",
    width = 700,
    height = 400,
    pointsize = 20)
plot(eaton.ts.stl$time, eaton.ts.stl$data$trend,
     type = "l",
     main = "Eaton TS Decomposition Trend")
dev.off()

# Graph the precip intensity data.
png("./results/decompose_eaton_intensity_trend.png",
    width = 700,
    height = 400,
    pointsize = 20)
plot(intensity.ts.stl$time, intensity.ts.stl$data$trend,
     type = "l",
     main = "Eaton Intensity Decomposition Trend")
dev.off()

png("./results/decompose_eaton_intensity.png",
    width = 700,
    height = 400,
    pointsize = 20)
plot(intensity.ts.stl,
     main = "Eaton (Extended) Intensity",
     ylab = "Rainfall Intensity (unitless)",
     xlab = "Year")
dev.off()

# Plot trends against each other
trends.df <- tibble(thedate = data.ts.stl$time,
                    Elevation = data.ts.stl$data$trend,
                    Precipitation = eaton.ts.stl$data$trend) %>%
  gather(type, value, -thedate)



ggplot(trends.df, aes(x = thedate,
                      y = value)) +
  geom_line() +
  facet_wrap( ~ type,
             ncol = 1,
             scales = "free_y") +
  labs(title = "Long-term Trends",
       subtitle = paste0(t.window, " Month Weighted Average"),
       x = "Date",
       y = "Elevation (feet) and Precipitation (inches)") +
  theme_minimal()

ggsave("./results/decompose_trend_comparison.png",
       width = 7,
       height = 4)
  




