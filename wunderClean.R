# Weather Underground basic data cleaning
#
library(data.table)
library(lubridate)

setwd("D:/data/taxi/nyctaxi//")

all <- lapply(list.files("./wunder_raw/", full.names = T),
              fread)

clean <- function(dt){
  dt[,precipitation := as.numeric(precipitation)]
  dt[,temperature := as.numeric(temperature)]
  dt[,windspeed := as.numeric(windspeed)]
  dt[,datetime := as.POSIXct(time, "%Y-%m-%d %I:%M %p", tz="EST")]
  dt[,hour := hour(datetime)]
}


lapply(all, clean) # side effects, no saving

# Bind/save
wunder <- rbindlist(all)

# Missing = 0 precip
wunder[is.na(precipitation), precipitation := 0]

# multiple measurements for some hours
cleaned = wunder[, list(temperature = mean(temperature, na.rm = T),
              precipitation = max(precipitation, na.rm = T),
              windspeed = mean(windspeed, na.rm = T)),
       by = list(year, month, day, hour)]

write.csv(cleaned, "./wunder/wunder.csv", row.names = F)
