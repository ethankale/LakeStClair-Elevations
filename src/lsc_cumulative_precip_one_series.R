
# Try to find short, medium, and long term cumulative rainfall
#   impacts on the elevation of Lake St. Clair

library(readr)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

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
data.join <- ImportStClairData("day")

# Total window for cumulative rainfall period
num.years <- seq(0.1, 6.5, by = .1)
combinations <- data.frame(num.years = num.years,
                           num.days = as.integer(num.years * 365.25))

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
  
  day <- combinations[i, "num.days"]
  
  #print(combo$short)
  
  series <- rollapplyr(data.current$precip.merge, 
                       width = day, 
                       FUN = sum, 
                       fill = NA)

  data.current$num.days <- series
  data.current <- TrimData(data.current)

  data.current.lm <- with(data.current, 
                          lm(elevation ~ num.days))
  
  combinations$p.log[i] <- log(summary(data.current.lm)$coefficients[2+6])
  combinations$r.squared[i] <- summary(data.current.lm)$adj.r.squared
  
  setTxtProgressBar(pb, i)
  
  #summary(data.current.lm)
}

close(pb)

# Plot the number of days vs. the rsquared
ggplot(combinations, aes(x = num.years,
                         y = r.squared)) +
  geom_line() +
  labs(title = "Lake St. Clair Elevation and Precipitation Correlation",
       subtitle = paste0("R squared is the correlation between cumulative ", 
                         "precipitation and lake level elevation, where 1.0 ",
                         " = perfect prediction"),
       x = "Years of cumulative precipitation",
       y = "R Squared") +
  theme_minimal()


