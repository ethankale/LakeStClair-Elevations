
# Generate a nice graph for a public factsheet for LSC
# Simple graph of LSC elevation over time

library(tidyverse)
library(stlplus)

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

data.ts.graph <- tibble(measure.mon = as.yearmon(data.ts.stl$time),
                        elevation = data.ts.stl$data$raw,
                        trend = data.ts.stl$data$trend) %>%
  gather(type, elevation, -measure.mon)

ggplot(data.ts.graph, aes(x = measure.mon,
                          y = elevation)) +
  labs(title = "Lake St. Clair Elevations",
       y = "Elevation (mean sea level)") +
  geom_line(aes(linetype = type)) +
  scale_linetype_manual(values = c("elevation" = 1,
                                "trend" = 2)) +
  guides(linetype = FALSE) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank())

ggsave(filename = "./results/factsheet_elevation_graph.png",
       width = 5,
       height = 1.5)
  
  

