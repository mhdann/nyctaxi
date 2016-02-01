# Michael Dann
# Mhdann@gmail.com
#
# Econometric regressions of weather impacts on taxi demand/passenger density
#
library(data.table)
library(RSQLite)


# Read in rasterized and hourly-binned observations from the NYC Taxi db
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "./nyc_DB/gridded.db")
park.binned = data.table(dbReadTable(con, "park_binned"))
dbDisconnect(con)
setnames(park.binned, "Month", "month")

# Read in weather underground temperature/precip/wind observations
wunder = fread("./wunder/wunder.csv")[year == 2015 & month %in% 1:6, list(year, month, day, hour, temperature, precipitation, windspeed)]
setnames(wunder, c("day", "hour"), c("pickup_day", "pickup_hour"))

# Cross with cells
unique(wunder[,list(month, pickup_day, pickup_hour)])
out = lapply(1:400, function(x){dt = copy(wunder); dt[, pickup_cell:=x]; return(dt)})
all.wunder = rbindlist(out)

# Bind
setkey(park.binned, pickup_cell, month, pickup_day, pickup_hour)
setkey(all.wunder , pickup_cell, month, pickup_day, pickup_hour)
data = all.wunder[park.binned]


###############################################################################
# Regressions
#
# Idea: Explain trip frequency as a function of weather, location, time
#
# log(rides) as an dependent variable
# "precip" t/f as independent variable
# 
data[, precip := 0]
data[!is.na(precipitation) & precipitation > 0, precip := 1]

data[, l_rides:= log(rides)]

fm = lm(data, formula = "l_rides ~ factor(month) + factor(Weekday) + temperature + factor(precip) + factor(pickup_cell)")
###############################################################################

