# Michael Dann
# Mhdann@gmail.com
#
# Econometric regressions of weather impacts on taxi demand/passenger density
#
# Database deleted as part of exploratory data purge, now defunct file
#
setwd("D:/data/taxi/nyctaxi/")

library(ggplot2)
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

data[is.na(precipitation), precipitation := 0]


###############################################################################
# Regressions
#
# Idea: Explain trip frequency as a function of weather, location, time
#
# log(rides) as an dependent variable
# "precip" t/f as independent variable
 
data[, l_rides:= log(rides)]

# fm.wo.precip = lm(data, formula = "l_rides ~ factor(month) + factor(Weekday) + factor(pickup_hour) + factor(pickup_cell) + temperature", na.action = na.exclude)
# sum.fm.wo.precip = summary(fm.wo.precip)
# backup = copy(data)

fm           = lm(data, formula = "rides ~ factor(month) + factor(Weekday) + factor(pickup_hour) + factor(pickup_cell) + temperature + precipitation", na.action = na.exclude)


# data[, l_rides_resid:= resid(fm.wo.precip)]
# data[, l_rides_hat:= predict(fm.wo.precip)]
# data[, rides_resid := exp(l_rides_resid)]
data[, rides_resid := resid(fm)]
# data[, rides_hat := exp(l_rides_hat)]
data[, rides_hat := predict(fm)]


# Take the spatial mean so we can look at the time series/scatter
data.hr  = data[, lapply(.SD, mean, na.rm=T),  by = list(year, month, Weekday, pickup_day, pickup_hour), .SDcols = c("rides", "rides_hat", "rides_resid", "precipitation", "temperature")]
data.day = data[, lapply(.SD, mean, na.rm=T),  by = list(year, month, Weekday, pickup_day), .SDcols = c("rides", "rides_hat", "rides_resid", "precipitation", "temperature")]

data.hr[ , datetime := ISOdatetime(year, month, day = pickup_day, hour = pickup_hour, min = 30, sec = 0)]
data.day[, datetime := ISOdatetime(year, month, day = pickup_day, hour = 12, min = 30, sec = 0)]


ggplot(data.hr, aes(x = datetime, y = rides-rides_hat)) + geom_line() + theme_bw() + labs(x = "Date", y = "Residual")

residuals = ggplot(data.day, aes(x = datetime, y = rides-rides_hat)) + geom_line() + theme_bw() + labs(x = "Date", y = "Residual", title = "Residuals: Park/Midtown")
ggsave(plot = residuals, "./images/residuals.park.midtown.png", width = 6, height = 6, dpi = 200)

ggplot(data.hr, aes(x = precipitation, y = rides_resid)) + geom_point() + theme_bw() + labs(x = "Precipitation", y = "Residual w/o precipitation")
ggplot(data.day, aes(x = precipitation, y = rides-rides_hat)) + geom_point() + theme_bw() + labs(x = "Precipitation", y = "Residual w/o precipitation")

#
prediction  = ggplot(data.hr, aes(x = rides_hat, y = rides)) + geom_point() + theme_bw() + labs(x = "Predicted Rides", y = "Rides", title = "Actual v Prediction for Midtown") + geom_abline(intercept = 0, slope = 1) + theme_bw()
ggsave(plot = prediction, "images/prediction.park.midtown.png", width = 6, height = 6, dpi = 200)


ggplot(data.day, aes(x = rides_hat, y = rides)) + geom_point() + theme_bw() + labs(x = "Predicted Rides", y = "Rides") + geom_abline(intercept = 0, slope = 1)

ggplot(data.hr, aes(y = rides, x = datetime)) + geom_smooth() + theme_bw() + labs(x = "Date", y = "Rides")
ggplot(data.day, aes(y = rides, x = datetime)) + geom_point() + geom_point(aes(y = rides_resid))  + theme_bw() + labs(x = "Date", y = "Rides")

# saveRDS(summary(fm.wo.precip), "reg1_rides.rds")
###############################################################################

