# Michael Dann
# mhdann@gmail.com
#
# Rasterizing functions for the NYC cab dataset
#
# Idea: Bin the NYC cab dataset by time/grid
#
# Uses data read-in with taxi.R
#
# Create a grid using bounds from taxi.R
# xlims = c(-74.05,-73.75); ylims = c(40.6, 40.9)
# xlims[2]-xlims[1] # 0.3
# => Grid of 300x300 so cell values across => round to 3 decimals
# subset instead
library(RSQLite)
library(data.table)

setwd("D:/data/taxi/nyctaxi")

# xlims = c(-73.98,-73.96); ylims = c(40.75,40.77) # subset around the south end of the park. Parts of midtown, 
# ncol = (round(xlims[2],3)-round(xlims[1],3)) * 1000

# t2015[,pickup_cell := (round(pickup_longitude, 3) - xlims[1]) * 1000 + 
#        ncol * (round(pickup_latitude,  3) - ylims[1]) * 1000]
# Subset, spatial
# park = t2015[pickup_cell %in% 1:(ncol*ncol)]

# Save/read... just in case
# Set up database connection
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "./nyc_DB/gridded.db")
# dbWriteTable(con, "park", park, overwrite = T)
park = data.table(dbReadTable(con, "park"))
# park = as.data.table(dbGetQuery(con, "SELECT * FROM park"))
dbDisconnect(con)

lapply(park, class)
# Create a time-step bin: Hours (first attempt)
# Rasterize
park.binned = park[clean == T, list(rides = .N,
                                    passenger_count = sum(passenger_count, na.rm = T),
                                    fare_amount = mean(fare_amount, na.rm = T),
                                    fare_rev_per_hour = mean(fare_rev_per_hour, na.rm = T),
                                    trip_distance = mean(trip_distance, na.rm = T)),
                   
                   by = list(pickup_cell, Month, Weekday, pickup_day, pickup_hour)]

con <- dbConnect(drv, dbname = "./nyc_DB/gridded.db")
dbWriteTable(con, "park_binned", park.binned)
dbDisconnect(con)

