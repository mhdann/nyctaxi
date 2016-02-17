# NYC Taxi Project
# Basic analysis by tract
#
#  Code fragments meant to be executed piece-meal
# 

library(data.table)
library(ggplot2)

project.root.dir = "D:/data/taxi/nyctaxi/"

# Flat files by tract
pickup.dir  = "F:/nyc_yellow_tract/pickup/"
dropoff.dir = "F:/nyc_yellow_tract/dropoff/"
net.dirs = "F:/nyc_yellow_tract/net/"

source(paste0(project.root.dir, "nycParseRaw.R"))

wunder <- fread(paste0(project.root.dir, "wunder/wunder.csv"))

pickup.dirs  <- list.files(pickup.dir)
dropoff.dirs <- list.files(pickup.dir)

tract_codes = fread("tract_codes.csv")


###############################################################################
# Net trips by census tract, over time (15 min interval highest res)
###############################################################################
  



# Patterns for each day of the week
# daily = net[, lapply(.SD, mean), by = list(quarter_hour, hour, weekday), .SDcols = c("trips.out", "trips.in", "net.trips.in")]

# Patterns for each month
# monthly = net[, lapply(.SD, mean), by = list(day, month), .SDcols = c("trips.out", "trips.in", "net.trips.in")]

# Annual signal
# yearly = net[, lapply(.SD, mean), by = list(month, year), .SDcols = c("trips.out", "trips.in", "net.trips.in")]


# Plot daily net pattern
ggplot(daily, aes(x = hour + quarter_hour/4, y = net.trips.in, group = factor(weekday), color = factor(weekday))) + geom_line() + theme_bw()

# Plot daily pickup/dropoff pattern
ggplot(daily, aes(x = hour + quarter_hour/4, y = trips.out, group = factor(weekday), color = factor(weekday))) + geom_line() + theme_bw()
ggplot(daily, aes(x = hour + quarter_hour/4, y = trips.in, group = factor(weekday), color = factor(weekday))) + geom_line() + theme_bw()



# dt.pickups[, list(trips = sum(.N)), by = list(pickup_tract_id, pickup_year, pickup_month, weekday, pickup_day, pickup_hour)]
# dt.pickups[, list(trips = sum(.N)), by = list(pickup_tract_id, weekday, pickup_hour)]

###############################################################################
# Regressions against weather underground and typical time patterns
###############################################################################

fm = lm(data = dt.hour, trips ~ factor(pickup_year) + factor(pickup_month) + factor(weekday) + factor(pickup_hour))
summary(fm)



goog_map = get_googlemap(center = c(-73.984, 40.7425), zoom = 13, style = "element:labels|visibility:off")
bbox = attr(goog_map, "bb")
dropoffs = table(bnt$dropoff_nyct2010_gid)

png(filename = "graphs/bridge_and_tunnel_tracts.png", width = 480, height = 550, bg = "#f4f4f4")
ggmap(goog_map, extend = "device") +
  geom_polygon(data = ex_staten_island_map,
               aes(x = long, y = lat, group = group, alpha = dropoffs[as.character(id)]),
               fill = "#ff0000",
               color = "#222222",
               size = 0) +
  scale_x_continuous(lim = range(nyc_map$long)) +
  scale_y_continuous(lim = range(nyc_map$lat)) +
  scale_alpha_continuous(range = c(0, 0.8)) +
  coord_map(xlim = bbox[c(2, 4)], ylim = bbox[c(1, 3)]) +
  title_with_subtitle("Bridge and Tunnel Destinations", "Drop offs for Saturday evening taxi rides originating at Penn Station") +
  theme_tws_map(base_size = 19) +
  theme(legend.position = "none")
add_credits()
dev.off()