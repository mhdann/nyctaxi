# NYC Taxi Project
# Basic analysis by tract
#
#  Generates examples of stochastic 
# 

library(data.table)
library(ggplot2)

project.root.dir = "D:/data/taxi/nyctaxi/"

# Flat files by tract
# pickup.dir   = "F:/nyc_yellow_tract/pickup/"
# dropoff.dir  = "F:/nyc_yellow_tract/dropoff/"
# pickup.dirs  = list.files(pickup.dir)
# dropoff.dirs = list.files(pickup.dir)
net.dirs = "F:/nyc_yellow_tract/net/"

# Tract code files, neighborhood, borough etc.
tract_codes = fread("tract_codes.csv")

# Weather data
wunder <- fread(paste0(project.root.dir, "wunder/wunder.csv"))

# object 4 GB, recommend +10 GB of RAM
net    <- fread(paste0(net.dirs, "net.trips.csv")) 

names(net)     # 15 min resolution by tract
names(wunder)  # 1 hr resolution whole city

setkey(net, year, month, day, hour)
setkey(wunder, year, month, day, hour)

setnames(tract_codes, "id", "tract_id")
tract_codes[,tract_id := as.integer(tract_id)]
setkey(tract_codes, tract_id)

# Left Outter Join
all = wunder[net] # Equivalent: all = merge(net, wunder, all.x = T)

# Cleanup
rm(net, wunder); gc()

# Order for fast subsetting
setkey(all, tract_id, year, month, day, weekday, hour, quarter_hour)

# Add smallest key (RAM issues, append names back later)
all = tract_codes[,list(tract_id, BoroCode, NTACode)][all]

# Basic calcs, reduce to neighborhood
# MAYBE INCORRECT CALCS: need to parse sooner since "trip in" and "trip out" are defined on tract here, not on neighborhood or borough
neighborhood = all[,list(trips.out = sum(trips.out, na.rm = T),
                         trips.in = sum(trips.in, na.rm = T),
                         net.trips.in = sum(net.trips.in, na.rm = T),
                         temperature = mean(temperature, na.rm = T),
                         precipitation = mean(precipitation, na.rm = T),
                         windspeed = mean(windspeed, na.rm = T))
                   , by = list(BoroCode, NTACode, year, month, day, hour, weekday, quarter_hour)]
setkey(neighborhood, BoroCode, NTACode, year, month, day, weekday, hour, quarter_hour)

borough = all[,list(trips.out = sum(trips.out, na.rm = T),
                     trips.in = sum(trips.in, na.rm = T),
                     net.trips.in = sum(net.trips.in, na.rm = T),
                     temperature = mean(temperature, na.rm = T),
                     precipitation = mean(precipitation, na.rm = T),
                     windspeed = mean(windspeed, na.rm = T))
                , by = list(BoroCode, year, month, day, hour, weekday, quarter_hour)]

 setkey(borough, BoroCode, year, month, day, weekday, hour, quarter_hour)


all.week          =          all[, lapply(.SD, mean, na.rm = T), by = list(BoroCode, NTACode, tract_id, weekday, hour, quarter_hour), .SDcols = c("trips.out", "trips.in", "net.trips.in", "temperature", "precipitation", "windspeed")]
neighborhood.week = neighborhood[, lapply(.SD, mean, na.rm = T), by = list(BoroCode, NTACode,           weekday, hour, quarter_hour), .SDcols = c("trips.out", "trips.in", "net.trips.in", "temperature", "precipitation", "windspeed")]
borough.week     =     borough[, lapply(.SD, mean, na.rm = T), by = list(BoroCode,                    weekday, hour, quarter_hour), .SDcols = c("trips.out", "trips.in", "net.trips.in", "temperature", "precipitation", "windspeed")]

# generate a graphic for each 15 minute interval for each aggregation level for Monday
source(file = "./nycMapping.R") # ex_staten_island_map, nyc_map etc
library(ggmap)

# from todd again
title_with_subtitle = function(title, subtitle = "") {
  ggtitle(bquote(atop(bold(.(title)), atop(.(subtitle)))))
}



###############################################################################
# Net trips by census tract, over time (15 min interval highest res)
###############################################################################

# get range of net.trips.in for color scale
# max(abs(range(all.week[weekday == 2, net.trips.in]))) # 171
# plot(density(all.week[weekday == 2, net.trips.in])) # fat tail
# quantile(all.week[weekday == 2, net.trips.in], c(0.01, 0.99))

# call the range +-10
range.clip = 10

goog_map = get_googlemap(center = c(-73.90, 40.7425), zoom = 11, style = "element:labels|visibility:off")
for(hr in 0:23){
  for(qtr in 0:3){
    # Formatting for this plot borrowed partially from toddwschneider, https://github.com/toddwschneider/nyc-taxi-data/blob/master/analysis/analysis.R
    bbox = attr(goog_map, "bb")
    x = all.week[weekday == 2 & hour == hr & quarter_hour == qtr, list(tract_id, net.trips.in)]
    stitle = paste0("Monday ", sprintf("%02d", hr), ":", sprintf("%02d", qtr*15))
    setkey(x, tract_id)
    net.vec = x[.(1:2166), net.trips.in]
    net.vec[net.vec > range.clip] = range.clip
    net.vec[net.vec < -range.clip] = -range.clip
    p <- ggmap(goog_map, extend = "device") +
      geom_polygon(data = nyc_map, alpha = 0.9, color = NA,
                   aes(x = long, y = lat, group = group, fill = net.vec[as.integer(id)]),                  
                   size = 0) +
      scale_fill_gradient2(limits = c(-range.clip,range.clip), high = "blue", mid = "white", low = "red") +
      coord_map(xlim = bbox[c(2, 4)], ylim = bbox[c(1, 3)]) +
      title_with_subtitle("Net Taxis accumulating in each tract", stitle) +  
      theme(legend.position = "bottom",
            axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()) +
      labs(fill = "Net Trips Into the Tract")
    fn = paste0("./images/accumulation_Monday_", sprintf("%02d", hr), sprintf("%02d", 15*qtr), ".png")
    png(filename = fn, width = 480, height = 550, bg = "#ffffff")
    plot(p)
    dev.off()
  }
}

# Now by burrough
# max(abs(range(borough.week[weekday == 2, net.trips.in]))) # 599
# quantile(borough.week[weekday == 2, net.trips.in], c(0.01, 0.99)) 320
range.clip = 320

goog_map = get_googlemap(center = c(-73.90, 40.7425), zoom = 11, style = "element:labels|visibility:off")
for(hr in 0:23){
  for(qtr in 0:3){
    # Formatting for this plot borrowed partially from toddwschneider, https://github.com/toddwschneider/nyc-taxi-data/blob/master/analysis/analysis.R
    bbox = attr(goog_map, "bb")
    x = borough.week[weekday == 2 & hour == hr & quarter_hour == qtr, list(BoroCode, net.trips.in)]
    stitle = paste0("Monday ", sprintf("%02d", hr), ":", sprintf("%02d", qtr*15))
    setnames(x, "BoroCode", "bcode") # avoid name scoping in dt[] later
    setkey(x, bcode)
    x[net.trips.in > range.clip, net.trips.in := range.clip]
    x[net.trips.in < -range.clip, net.trips.in := -range.clip]
    net.vec = x[.(nyc_map$BoroCode), net.trips.in]
    p <- ggmap(goog_map, extend = "device") +
      geom_polygon(data = nyc_map, alpha = 0.9, color = NA,
                   aes(x = long, y = lat, group = group, fill = net.vec),                  
                   size = 0) +
      scale_fill_gradient2(limits = c(-range.clip,range.clip), high = "blue", mid = "white", low = "red",
                           breaks = c(-range.clip,0,range.clip)) +
      coord_map(xlim = bbox[c(2, 4)], ylim = bbox[c(1, 3)]) +
      title_with_subtitle("Net Taxis accumulating in each Borough", stitle) +  
      theme(legend.position = "bottom",
            axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()) +
      labs(fill = "Net Trips Into the borough")
    fn = paste0("./images/accumulation_boro_Monday_", sprintf("%02d", hr), sprintf("%02d", 15*qtr), ".png")
    png(filename = fn, width = 480, height = 550, bg = "#ffffff")
    plot(p)
    dev.off()
  }
}




#################### GIF MAKING CALL, IMAGEMAGICK ############
#  cd ./images
#  convert.exe -delay 25 -loop 0 accumulation_Monday*.png monday_accumulations.gif
#  convert.exe -delay 25 -loop 0 accumulation_boro_Monday*.png monday_accumulations_burough.gif



###############################################################################
# Other trips
###############################################################################


# Patterns for each day of the week
# daily = all[, lapply(.SD, mean), by = list(quarter_hour, hour, weekday), .SDcols = c("trips.out", "trips.in", "net.trips.in")]

# Patterns for each month
# monthly = all[, lapply(.SD, mean), by = list(day, month), .SDcols = c("trips.out", "trips.in", "net.trips.in")]

# Annual signal
# yearly = all[, lapply(.SD, mean), by = list(month, year), .SDcols = c("trips.out", "trips.in", "net.trips.in")]


# Plot daily net pattern
# ggplot(daily, aes(x = hour + quarter_hour/4, y = net.trips.in, group = factor(weekday), color = factor(weekday))) + geom_line() + theme_bw()

# Plot daily pickup/dropoff pattern
# ggplot(daily, aes(x = hour + quarter_hour/4, y = trips.out, group = factor(weekday), color = factor(weekday))) + geom_line() + theme_bw()
# ggplot(daily, aes(x = hour + quarter_hour/4, y = trips.in, group = factor(weekday), color = factor(weekday))) + geom_line() + theme_bw()



