
# Reusable code to import a joined dataset with
#   daily Lake St. Clair elevations and adjusted
#   10u rainfall records

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)

ImportStClairPrecip <- function() {
  noaa.raw <- read_csv("./data/11u_1955-2016_Day.csv",
#  noaa.raw <- read_csv("./data/11u_1988-2016_Day.csv",
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
  
  # set flagged values to NA
  eaton.raw$total.precip[eaton.raw$max.flag != "Normal"] <- NA
  
  # Give NOAA and lake elevation date fields
  noaa.raw$days <- as.Date(noaa.raw$DATE, format = "%Y%m%d")
  
  data.join <- noaa.raw %>%
    left_join(eaton.raw) %>%
    select(days, max.flag, total.precip, PRCP)
  
  # Build the relationship between precip records
  data.join.lm <- with(data.join, lm(total.precip ~ PRCP))
  
  # Create the merged rainfall record set
  data.join$precip.merge <- predict(data.join.lm, data.join)
  
  precip.not.na <- which(!is.na(data.join$total.precip))
  data.join$precip.merge <- replace(data.join$precip.merge,
                                    list = precip.not.na,
                                    values = data.join$total.precip[precip.not.na])
  
  return(data.join)
  
}

ImportStClairData <- function(period) {
  
  data.precip <- ImportStClairPrecip()
  
  if (period == "day") {
    data.raw <- read_csv("./data/Lake StClair Daily.csv",
                         col_types = "cd")
    
    data.elev <- data.raw %>%
      mutate(days = as.Date(Date, format = "%m/%d/%y"))

  } else if (period == "month") {
    data.raw <- read_csv("./data/Lake StClair 1988-2016.csv")
    
    data.elev <- data.raw %>%
      mutate(days = as.Date(paste0(`Mon Year`, "28"), format = "%b %Y%d"),
             elevation = Average) %>%
      filter(Flag == FALSE)
  }

  # Join the data
  data.join <- data.precip %>%
    left_join(data.elev) %>%
    mutate(measure.mon = as.yearmon(days)) %>%
    select(days, max.flag, measure.mon,
           total.precip, PRCP, precip.merge, elevation) %>%
    filter(days >= as.Date("1977-10-01", format = "%Y-%m-%d"))
  
  if (period == "month") {
    data.join <- data.join %>%
      group_by(measure.mon) %>%
      summarize(PRCP = sum(PRCP), 
                total.precip = sum(total.precip), 
                precip.max = max(precip.merge),
                precip.merge = sum(precip.merge), 
                precip.intensity = ifelse(is.na(precip.max / precip.merge),
                                          0,
                                          precip.max / precip.merge),
                elevation = mean(elevation, na.rm = TRUE), 
                max.flag = max(max.flag))
  } else if (period == "day") {
    data.join <- data.join %>%
      mutate(year.decimal = decimal_date(days))
  }
  
  return(data.join)
}

data.join <- ImportStClairData("month")

data.precip.long <- data.join %>%
  select(Date = measure.mon, 
         `C: Raw (NOAA)` = PRCP, 
         `B: Raw (Eaton)` = total.precip, 
         `A: Final` = precip.merge) %>%
  mutate(eaton.missing = is.na(`B: Raw (Eaton)`)) %>%
  gather(type, precip, -Date, -eaton.missing) %>%
  mutate(group = 1)

ggplot(data.precip.long, aes(x = Date,
                             y = precip)) +
  labs(title = "Extending and Filling Precipitation",
       x = "Date",
       y = "Precipitation (inches per month)") +
  geom_line(aes(color = eaton.missing,
                group = group)) +
  facet_wrap( ~ type,
              ncol = 1) +
  scale_color_manual(values = c("black", "red"),
                                guide = FALSE) +
  theme_minimal()

ggsave("./results/precipitation_comparison.png",
       width = 7,
       height = 4)



