
# Decompose Lake St. Clair elevation data into seasonal, trend, residual
#   components.

library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(stlplus)

s.window = "periodic"
t.window = 12*5


data.raw <- read_csv("./data/Lake StClair 1988-2016.csv")
noaa.raw <- read_csv("./data/11u_1988-2016_Day.csv",
                     col_types = "cccd")
# PEA1 is the early site id for what is now Eaton Creek - 10u
pea1.raw <- read_csv("./data/PEA1_daily.csv",
                     col_types = "Ddc")

# 10u is the code for Eaton Creek.  Using "tenu" because variables
#   that start with numbers are bad.
# There are a number of data files, one for each water year 2004-2016
tenu.years <- seq(2004, 2016, by = 1)
tenu.raw <- data.frame()

for (year in tenu.years) {
  
  tenu.current <- read_csv(paste0("./data/10u_", year, "_Day.csv"),
                               col_types = "cdccc")
  tenu.raw <- rbind(tenu.current, tenu.raw)
  
}

# Match up the format of pea1.raw and tenu.raw
tenu.reformat <- tenu.raw %>%
  mutate(days = as.Date(Day, format = "%m/%d/%Y"),
         total.precip = `Rain (inches)`,
         max.flag = Estimated) %>%
  select(days, total.precip, max.flag)
  

tenu.reformat$max.flag <- replace(tenu.reformat$max.flag,
                                  list = tenu.reformat$max.flag == "e",
                                  values = "FAIL")
tenu.reformat$max.flag <- replace(tenu.reformat$max.flag,
                                  list = is.na(tenu.reformat$max.flag),
                                  values = "Normal")

# Now mash together the pea1 data and the 10u data
eaton.raw <- rbind(pea1.raw, tenu.reformat)
eaton.raw <- eaton.raw[order(eaton.raw$days), ]


# Create "yearmon" fields - native date fields that store only the year and month
data.raw$measure.date <- as.yearmon(data.raw$`Mon Year`, format="%b %Y")
noaa.raw$measure.date <- as.yearmon(noaa.raw$DATE, format="%Y%m")

eaton.raw$days.char <- as.character(eaton.raw$days)
eaton.raw$measure.date <- as.yearmon(eaton.raw$days.char, format="%Y-%m")

# Set flagged values to NA.  These numbers didn't make sense,
#   but might be important in future analyses
data.raw$Average[data.raw$Flag] <- NA
eaton.raw$total.precip[eaton.raw$max.flag != "Normal"] <- NA

# Aggregate NOAA and Eaton data to month
noaa.ag <- aggregate(x = noaa.raw$PRCP,
                     by = list(measure.date = noaa.raw$measure.date),
                     FUN = sum)

eaton.ag <- aggregate(x = eaton.raw$total.precip,
                     by = list(measure.date = eaton.raw$measure.date),
                     FUN = sum)

# Join data together in a common dataframe.  This becomes important
#   later, when we want to shift the NOAA data relative to
#   the lake data; otherwise joining is tough
#data.join <- noaa.ag %>%
data.join <- eaton.ag %>%
  left_join(data.raw)

# Create time series
# First for lake st. clair
data.z <- zoo(data.join$Average, data.join$measure.date)
data.ts <- as.ts(data.z, frequency = 12)

data.ts.stl <- stlplus(data.ts,
                       s.window = s.window,
                       t.window = t.window)

data.ts.trend <- tibble(measure.date = as.yearmon(data.ts.stl$time),
                        elevation.trend = data.ts.stl$data$trend)


# Now timeseries for eaton
eaton.z <- zoo(data.join$x, data.join$measure.date)
eaton.ts <- as.ts(eaton.z, frequency = t.window)

eaton.ts.stl <- stlplus(eaton.ts,
                       s.window = s.window,
                       t.window = t.window)

eaton.ts.trend <- tibble(measure.date = as.yearmon(eaton.ts.stl$time),
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
  eaton.ts.current$measure.date <- eaton.ts.current$measure.date + (i/12)

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
eaton.ts.best$measure.date <- eaton.ts.best$measure.date + 
  (offset.months.best/12)

ts.dates.best <- eaton.ts.best %>%
  left_join(data.ts.trend)

with(ts.dates.best, plot(measure.date, 
                         precip.trend, 
                         col = "red",
                         main = paste0("Eaton Precipitation, shifted forward ", 
                                       offset.months.best, 
                                       " months")))

with(ts.dates.best, plot(measure.date,
                         elevation.trend,
                         main = "Lake St. Clair elevation"))

png("./results/lsc_decompose.png",
    width = 800,
    height = 600,
    pointsize = 20)
plot(data.ts.stl,
     main = "Lake St. Clair Water Height",
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

