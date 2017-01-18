
# Use Fourier transformation to see what kinds of
#   trends exist in the NOAA data

library(readr)
library(dplyr)
library(zoo)

# See http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html;
#  Better yet, http://stackoverflow.com/questions/4364823/how-do-i-obtain-the-frequencies-of-each-value-in-an-fft

# The index of the result of the FTT is *NOT* the same as the frequency.
#   To get frequency, create a vector, then scale: 
#   frequency <- c(1:length(data)-1) * (sample.frequency / length(data))
# The value of the created vector at i is the frequency of the result of the
#   fft() function at i; e.g., frequency[i] has strength fft(data)[i]

# I'm using a default sample frequency of 12 because it works for data
#   sampled on a monthly basis.  This will return the frequency in years.
plot.frequency.spectrum <- function(X.k,
                                    sample.frequency = 12,
                                    main = "Fourier Transform",
                                    xlab = "Frequency") {
  
  frequency <- c(1:length(X.k)-1) * (sample.frequency / length(X.k))
  strength <- Mod(X.k)
  plot.data  <- data.frame(frequency, strength)
  plot.data <- plot.data[2:(nrow(plot.data)/2), ]
  
  plot(plot.data, 
       t = "h", 
       lwd = 2, 
       main = main, 
       xlab = xlab, 
       ylab = "Strength")
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
par(mfrow = c(2,1))
plot(noaa.monthly, 
     type = "l",
     main = "NOAA Monthly Total Precipitation",
     xlab = "Date",
     ylab = "Precipitation (inches)")

plot.frequency.spectrum(noaa.monthly.fft,
                        main = "NOAA Monthly Rainfall, Fourier Signal Strength",
                        xlab = "Frequency (cycles per year)")
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



