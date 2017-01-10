
# Use Fourier transformation to see what kinds of
#   trends exist in the NOAA data

library(readr)
library(dplyr)
library(zoo)

# See http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html
plot.frequency.spectrum <- function(X.k,
                                    main = "Fourier Transform",
                                    xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, 
       t = "h", 
       lwd = 2, 
       main = main, 
       xlab = "Frequency (Hz)", 
       ylab = "Strength", 
       xlim = xlimits, 
       ylim = c(0,max(Mod(plot.data[,2]))))
}


noaa.raw <- read_csv("./data/11u_1988-2016_Day.csv",
                     col_types = "cccd")

# Create date and month columns
noaa.process <- noaa.raw %>%
  mutate(day = as.Date(DATE, format = "%Y%m%d"),
         yearmon = as.yearmon(day))

# There are only three days missing in the NOAA data.  We're going
#   to assume that the impact of those missing days on the analysis
#   is minimal, but we could use the below to fill them in using
#   zoo's na.approx() function.
#noaa.days <- seq(min(noaa.process$day), max(noaa.process$day), by = 1)

# Fourier transform
noaa.monthly <- noaa.process %>%
  group_by(yearmon) %>%
  summarise(precip = sum(PRCP))

noaa.monthly.fft <- fft(noaa.monthly$precip)

# Save some plots
png(file = "./results/noaa_monthly.png",
    width = 800,
    height = 600,
    pointsize = 20)
par(mfrow = c(1,1))
plot(noaa.monthly, 
     type = "l",
     main = "NOAA Monthly Total Precipitation",
     xlab = "Date",
     ylab = "Precipitation (inches)")
dev.off()

png(file = paste0("./results/noaa_fourier.png"),
    width = 800,
    height = 600,
    pointsize = 20)
par(mfrow = c(2,1))
plot.frequency.spectrum(noaa.monthly.fft,
                        main = "NOAA Monthly Rainfall, Fourier Signal Strength",
                        xlimits = c(0, 100))
plot.frequency.spectrum(noaa.monthly.fft,
                        main = "")
dev.off()


# Strongest signals
noaa.monthly.fft.strength  <- Mod(noaa.monthly.fft) * 2
cutoff <- quantile(noaa.monthly.fft.strength, .98)
noaa.monthly.fft.maxindex <- which(noaa.monthly.fft.strength > cutoff)

noaa.monthly.fft.max <- data.frame(hz = noaa.monthly.fft.maxindex - 1,
                                   strength = noaa.monthly.fft.strength[
                                     noaa.monthly.fft.maxindex
                                   ])
# Drop anything with an index of greater than half of the total time series;
#   those values don't help us understand the data at all (because they
#   don't repeat over the available time frame)
noaa.monthly.fft.max <- noaa.monthly.fft.max[noaa.monthly.fft.max[,1] <= nrow(noaa.monthly)*0.5,]



