# Weather Underground basic data cleaning
#
library(data.table)
library(lubridate)

# setwd("D:/data/taxi/")
all <- lapply(list.files("./wunder_raw/", full.names = T),
              fread)

# Cleaning steps
# 1. Examine
# lapply(all[[1]], class)

clean <- function(dt){
  dt[,precipitation := as.numeric(precipitation)]
  dt[,temperature := as.numeric(temperature)]
  dt[,windspeed := as.numeric(windspeed)]
  dt[,datetime := as.POSIXct(time, "%Y-%m-%d %I:%M %p", tz="EST")]
  dt[,hour := hour(datetime)]
}
# test = all[[1]]
# clean(test)

lapply(all, clean) # side effects, no saving

# Bind/save
wunder <- rbindlist(all)
write.csv(wunder, "./wunder/wunder.csv", row.names = F)
