
# Check for correlations between Eaton Creek discharge
#   and Lake St. Clair elevations

library(tidyverse)
library(lubridate)
library(zoo)
library(stlplus)

discharges <- read_csv("./data/eaton_flow_2008-2016.csv") %>%
  mutate(day = as.Date(date, format = "%m/%d/%y"),
         monthyear = as.yearmon(day),
         acrefeet = 0.504166660405 * mean_flow) %>%
  group_by(monthyear) %>%
  summarize(discharge.total = sum(acrefeet),
            discharge.mean = mean(acrefeet),
            discharge.max = max(acrefeet),
            count = n()) %>%
  filter(count > 26) %>%
  select(-count)

elevations <- read_csv("./data/Lake StClair 1988-2016.csv",
                          col_types = "cdc") %>%
  mutate(monthyear = as.yearmon(`Mon Year`, format="%b %Y")) %>%
  select(monthyear, elevation = Average) %>%
  filter(elevation < 75)

# Evapotranspiration data from the 23u station (Building 4)
et <- read_csv("./data/23u_all_records.csv") %>%
  mutate(day = as.Date(Date, format = "%m/%d/%y"),
         monthyear = as.yearmon(day)) %>%
  group_by(monthyear) %>%
  summarize(et.sum = sum(ET),
            et.mean = mean(ET))
  
# Start by comparing raw data
combined <- discharges %>%
  left_join(elevations) %>%
  left_join(et)

#with(combined, plot(discharge.total, elevation))
#with(combined, plot(et.sum, elevation))
#with(combined, plot(monthyear, et.sum, type = "l"))
#with(combined, plot(monthyear, discharge.total, type = "l"))

# Relationship between just discharge & elevation.  It's clear from
#   the plot that the relationship is logarithmic.  Also, adding
#   stage (stage + log(stage)) doesn't improve the r2 or t- or p-stats,
#   so don't use that.
lm.linear <- with(combined, lm(elevation ~ discharge.total))
lm.poly <- with(combined, lm(elevation ~ discharge.total + I(discharge.total^2)))
lm.log <- with(combined, lm(elevation ~ log(discharge.total)))
combined$pred.total.raw <- predict(lm.log, combined)

# Elevations over time, actual and predicted from discharge
ggplot(combined, aes(x = monthyear,
                     y = elevation)) +
  geom_point() +
  geom_line(aes(linetype = "Actual")) +
  geom_line(aes(x = monthyear,
                y = pred.total.raw,
                linetype = "Predicted")) +
  labs(title = "Lake St. Clair Elevations",
       subtitle = "Elevations over time, actual and predicted from Eaton Creek discharge",
       x = "Date",
       y = "Elevation (feet, NGVD 29)") +
  scale_linetype_manual(name = "",
    values = c("Actual" = 1, "Predicted" = 2)) +
  theme_minimal()

ggsave(filename = "./results/eaton_elevation_prediction.png",
       width = 7,
       height = 4)

# Elevations vs Eaton Creek discharge, showing logarithmic relationship
r2 <- round(summary(lm.log)$adj.r.squared, 3)

ggplot(combined, aes(x = discharge.total,
                     y = elevation)) +
  geom_point(aes(size = "Measured Value")) +
  geom_line(aes(x = discharge.total,
                y = pred.total.raw,
                linetype = "Best Fit Line")) +
  labs(title = "Lake St. Clair and Eaton Creek",
       subtitle = paste0("Elevations vs discharge, showing ",
                         "logarithmic relationship; ",
                         "R2 = ",
                         r2),
       x = "Discharge (acre feet)",
       y = "Elevation (feet, NGVD 29") +
  scale_linetype_manual(name = "",
                        values = c("Best Fit Line" = 2)) +
  scale_size_manual(name = "",
                    values = c("Measured Value" = 3)) +
  theme_minimal()

ggsave(filename = "./results/eaton_elevation_comparison.png",
       width = 7,
       height = 4)

# Eaton Creek discharge over time
ggplot(combined, aes(x = monthyear,
                     y = discharge.total)) +
  geom_line() +
  geom_point() +
  labs(title = "Eaton Creek Discharge",
       subtitle = "2008 to 2016",
       x = "Date",
       y = "Discharge (monthly sum, acre feet)") +
  theme_minimal()

ggsave(filename = "./results/eaton_discharge_over_time.png",
       width = 7,
       height = 4)

# Raw data looks very good; explains significant percentage of
#   variation.  Now try 6-month cumulative function.
         
combined$discharge.6mon <- rollapplyr(combined$discharge.total,
                                      width = 6,
                                      FUN = sum,
                                      fill = NA)

lm.log.6mon <- with(combined, lm(elevation ~ log(discharge.6mon)))

# Cumulative sum over a 6 month period is worse than raw data.

# Try a 36-month (3-year) cumulative function.
combined$discharge.36mon <- rollapplyr(combined$discharge.total,
                                      width = 36,
                                      FUN = sum,
                                      fill = NA)

lm.log.36mon <- with(combined, lm(elevation ~ log(discharge.36mon)))

# Cumulative sum over 36-month window is worse yet than raw.

# This is an interesting outcome, given that a similar cumulative function
#   over rainfall data results in a better, not worse, relationship with 
#   lake water elevation.  This strongly suggests that the cumulative
#   nature of rainfall is accounted for in the stream discharge.  It also
#   means that Eaton Creek discharges are the best predictor of lake level
#   so far, though with an important caveat - they *do not* extend earlier
#   than 2008.

# How about evapotranspiration?
lm.et.discharge <- with(combined, lm(elevation ~ et.sum + log(discharge.total)))
summary(lm.et.discharge)

# Adding raw ET makes the model worse.  Possibly due to fewer data points, but it
#   definitely doesn't improve the model.  What about cumulative 6-month ET?
combined$et.6mon <- rollapplyr(combined$et.sum,
                                      width = 6,
                                      FUN = sum,
                                      fill = NA)
with(combined, plot(monthyear, et.6mon, type = "l"))
with(combined, plot(et.6mon, elevation))

lm.et <- with(combined, lm(elevation ~ et.6mon))
lm.etcum.discharge <- with(combined, lm(elevation ~ et.6mon + log(discharge.total)))

summary(lm.et)
summary(lm.etcum.discharge)

# There's a relationship between et and elevation, but adding et
#   to the overall model decreases the explanatory value.  Probably et
#   is just showing seasonal changes in lake elevation, something already
#   explained by eaton creek discharge.
et.eaton.df <- combined %>%
  select(et = et.sum, discharge = discharge.total) %>%
  filter(complete.cases(.))

et.eaton.cov <- with(et.eaton.df, cov(et, discharge))

# We in fact see significant negative covariation between et and discharge.
#   This is expected, because et increases in summer while discharge
#   decreases.  Overall, it's best to leave ET out of the equation.

##################################
# Decompose Eaton Creek discharges
##################################

eaton.ts <- ts(combined$discharge.total,
               frequency = 12)

eaton.stl <- stlplus(eaton.ts,
                     s.window = "periodic",
                     t.window = 42)
plot(eaton.stl)

eaton.stl.plot <- eaton.stl$data %>%
  select(-weights, -sub.labels,
         a_raw = raw,
         b_season = seasonal,
         c_trend = trend,
         d_remain = remainder) %>%
  mutate(monthyear = as.Date(combined$monthyear)) %>%
  gather(type, discharge, -monthyear)

# Plot the breakdown
ggplot(eaton.stl.plot, aes(x = monthyear,
                           y = discharge)) +
  geom_line() +
  facet_wrap( ~ type,
              ncol = 1,
              scales = "free_y",
              labeller = as_labeller(c("a_raw" = "Raw",
                                       "b_season" = "Seasonal",
                                       "c_trend" = "Trend",
                                       "d_remain" = "Remainder"))) +
  labs(title = "Eaton Creek Discharge Breakdown",
       subtitle = "Raw = Seasonal + Trend + Remainder\nTrend smoothed over 42 months",
       x = "Date",
       y = "Discharge (acre feet per month)") +
  theme_minimal()

ggsave("./results/eaton_discharge_breakdown.png",
       width = 7,
       height = 4)



