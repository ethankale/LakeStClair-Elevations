
# Decompose Lake St. Clair elevation data into seasonal, trend, residual
#   components.

library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(stlplus)
library(ggplot2)

s.window = "periodic"
t.window = 12*2


source("./src/daily_merged_timeseries.R")
data.join <- ImportStClairData(period = "month")

# Create time series
# First for lake st. clair
data.z <- zoo(data.join$elevation, data.join$measure.mon)
data.ts <- as.ts(data.z, frequency = 12)

data.ts.stl <- stlplus(data.ts,
                       s.window = s.window,
                       t.window = t.window)

data.ts.trend <- tibble(measure.mon = as.yearmon(data.ts.stl$time),
                        elevation.trend = data.ts.stl$data$trend)


# Now timeseries for eaton
eaton.z <- zoo(data.join$precip.merge, data.join$measure.mon)
eaton.ts <- as.ts(eaton.z, frequency = t.window)

eaton.ts.stl <- stlplus(eaton.ts,
                       s.window = s.window,
                       t.window = t.window)

eaton.ts.trend <- tibble(measure.mon = as.yearmon(eaton.ts.stl$time),
                        precip.trend = eaton.ts.stl$data$trend)

# Offset the NOAA trend data by the appropriate amount, join, and perform
#   a linear regression.
# Precipitation trends should preceed lake level trends.  This is particularly true
#   if we're right that Lake St. Clair is more groundwater fed than surface water
#   fed.  Groundwater travels over years, rather than days & weeks, for watersheds
#   of the size we're looking at.

# Iterate through many potential months
offset.months <- seq(1, 12*11, by = 1)
fit.r2 <- c()

i <- 0
while (i < length(offset.months)) {
  i <- i+1
  eaton.ts.current <- eaton.ts.trend
  eaton.ts.current$measure.mon <- eaton.ts.current$measure.mon + (i/12)

  ts.dates.current <- eaton.ts.current %>%
    left_join(data.ts.trend)
  
  ts.dates.lm <- with(ts.dates.current, lm(elevation.trend ~ precip.trend))
  fit.r2 <- c(fit.r2, summary(ts.dates.lm)$adj.r.squared)
}

# Find the maximum fit
fit.df <- data.frame(offset = offset.months,
                     fit = fit.r2)

plot(fit.df,
     main = "R Squared of Eaton Creek Precip (10u) vs. Lake St. Clair",
     xlab = "Offset (months)",
     ylab = "R Squared")
offset.months.best <- which.max(fit.r2)

# Plot the NOAA trend, offset with the best value from above, against
#   the Lake St. Clair trend
eaton.ts.best <- eaton.ts.trend
eaton.ts.best$measure.mon <- eaton.ts.best$measure.mon + 
  (offset.months.best/12)

ts.dates.best <- eaton.ts.best %>%
  left_join(data.ts.trend)

with(ts.dates.best, plot(measure.mon, 
                         precip.trend, 
                         col = "red",
                         main = paste0("Eaton Precipitation, shifted forward ", 
                                       offset.months.best, 
                                       " months")))

with(ts.dates.best, plot(measure.mon,
                         elevation.trend,
                         main = "Lake St. Clair elevation"))

# Graph the seasonal elevation data only.
data.ts.seasonal <- data.frame(date = data.ts.stl$time,
  elev.s = data.ts.stl$data$seasonal)

png("./results/lsc_decompose.png",
    width = 800,
    height = 600,
    pointsize = 20)
plot(data.ts.stl,
     main = paste0("Lake St. Clair Water Height (", 
                   t.window, 
                   " month long-term trend)"),
     ylab = "Elevations (monthly mean)",
     xlab = "Year")
dev.off()

png("./results/eaton_decompose.png",
    width = 800,
    height = 600,
    pointsize = 20)
plot(eaton.ts.stl,
     main = "Eaton Rainfall",
     ylab = "Precipitation (inches)",
     xlab = "Year")
dev.off()

png("./results/lsc_decompose_trend.png",
    width = 800,
    height = 600,
    pointsize = 20)
plot(data.ts.stl$time, data.ts.stl$data$trend,
     type = "l",
     main = "Lake St. Clair TS Decomposition Remainder")
dev.off()

png("./results/eaton_decompose_trend.png",
    width = 800,
    height = 600,
    pointsize = 20)
plot(eaton.ts.stl$time, eaton.ts.stl$data$trend,
     type = "l",
     main = "Eaton TS Decomposition Remainder")
dev.off()

