# Michael Dann
# mhdann@gmail.com
#
# A pipeline to take RAW csvs to processed csvs by census tract-yearmonth 
#
#

library(data.table)
library(ggplot2)
library(RPostgreSQL)

# Create your own root
setwd("D:/data/taxi/nyctaxi/")

###############################################################################
# Name management
###############################################################################

# rename()
source(file = "./nycRenaming.R")

###############################################################################
# Parsing Raw / Appending Time Variables
###############################################################################

# appendTimeVariables(), removeVars(), clean(), cleanAll()
source(file = "./nycParseRaw.R")

###############################################################################  
# Mapping files for pair tracts
###############################################################################  

# Loads: 
# tracts
# nyc_map
# ex_staten_island_map 
# rgridToTractMapN800
# 
source(file = "./nycMapping.R")

# Examine
# p = ggplot() +
#  geom_polygon(data = ex_staten_island_map,
#               aes(x = long, y = lat, group = group),
#               fill = "#FFFFFF", color = "#080808") +   coord_map(xlim = range(ex_staten_island_map$long), ylim = range(ex_staten_island_map$lat)) +
#  theme(legend.position = "none")
 p = ggplot() +
  geom_polygon(data = nyc_map,
               aes(x = long, y = lat, group = group),
               fill = "#FFFFFF", color = "#080808") +   coord_map(xlim = range(nyc_map$long), ylim = range(nyc_map$lat)) +
  theme(legend.position = "none")

plot(p)

###############################################################################  
# Files
###############################################################################  

files = list.files("D:/data/taxi/nyctaxi/nyc_yellow_raw/",pattern = "\\.csv$", full.names = T)

f2015 <- files[grep("2015", files)]
f2014 <- files[grep("2014", files)]
f2013 <- files[grep("2013", files)]
f2012 <- files[grep("2012", files)]
f2011 <- files[grep("2011", files)]
f2010 <- files[grep("2010", files)]
f2009 <- files[grep("2009", files)]


###############################################################################
# Full pipeline save to csvs
# Not quite a MapReduce(). It's a map without the reduce
###############################################################################

vars = c( "pickup_tract_id",   "pickup_datetime",  "pickup_latitude",  "pickup_longitude",
          "dropoff_tract_id", "dropoff_datetime", "dropoff_latitude", "dropoff_longitude",
          "pickup_year", "pickup_month", "weekday", "passenger_count", "fare_rev_per_hour", "fare_amount", "total_amount", "trip_time", "trip_distance")

yearFromFile <- function(file){
  n = nchar(file)
  substr(file, n - 10, n - 7)
}

monthFromFile <- function(file){
  n = nchar(file)
  substr(file, n - 5, n - 4)
}

setwd("F:/")
pipeline <- function(file, vars, con, tab, nyc_map, rgridToTractMapN800){
  
  year  = yearFromFile(file)
  month = monthFromFile(file)
  
  dt = fread(file)
  # dt = fread(f2009[6])  # test
  
  rename(dt, all.names)
  removeVars(dt)
  appendTimeVariables(dt, singleStamp = T, ym = T)
  clean(dt)
  
  # Map to census tracts ~ 100s
  appendTractIds(dt, nyc_map, map = rgridToTractMapN800)
  
  # Recode tract_id NAs as 0
  dt[is.na(pickup_tract_id), pickup_tract_id := 0]
  dt[is.na(dropoff_tract_id), dropoff_tract_id := 0]

  cl = dt[(clean), vars, with = F]
  
  {# Save sorted on pickup_tract_id
    setkey(cl, pickup_tract_id, pickup_datetime, pickup_latitude, pickup_longitude, dropoff_tract_id, dropoff_datetime, dropoff_latitude, dropoff_longitude)  
  
    for(tract_id in unique(cl$pickup_tract_id)){
      str.tract = sprintf("%04d", tract_id)
      dir.name = paste0("./nyc_yellow_tract/pickup",
                        str.tract,
                        "/")
      dir.create(file.path(dir.name), showWarnings = FALSE)
      file.name = paste0(str.tract, "_",
                         yearFromFile(file), "_",
                         monthFromFile(file), ".csv")
      write.csv(cl[pickup_tract_id == tract_id],
                file = paste0(dir.name, file.name), row.names = F)
    }
  }
  
  {# Save sorted on dropoff_tract_id
    
    setkey(cl, dropoff_tract_id, dropoff_datetime, dropoff_latitude, dropoff_longitude, pickup_tract_id, pickup_datetime, pickup_latitude, pickup_longitude)  
    
    for(tract_id in unique(cl$dropoff_tract_id)){
      str.tract = sprintf("%04d", tract_id)
      dir.name = paste0("./nyc_yellow_tract/dropoff/",
                        str.tract,
                        "/")
      dir.create(file.path(dir.name), showWarnings = FALSE)
      file.name = paste0(str.tract, "_",
                         yearFromFile(file), "_",
                         monthFromFile(file), ".csv")
      write.csv(cl[dropoff_tract_id == tract_id],
                file = paste0(dir.name, file.name), row.names = F)
    }
  }
  
  
  print(paste0("Wrote ", file, " to tract_year_month.csv"))
}

lapply(f2009, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map, rgridToTractMapN800 = rgridToTractMapN800)
lapply(f2010, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map, rgridToTractMapN800 = rgridToTractMapN800)
lapply(f2011, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map, rgridToTractMapN800 = rgridToTractMapN800)
lapply(f2012, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map, rgridToTractMapN800 = rgridToTractMapN800)
lapply(f2013, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map, rgridToTractMapN800 = rgridToTractMapN800)
lapply(f2014, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map, rgridToTractMapN800 = rgridToTractMapN800)
lapply(f2015, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map, rgridToTractMapN800 = rgridToTractMapN800)


###############################################################################
# Pipeline testing
###############################################################################

# Read ~ 90s
# system.time(dt201009 <- fread(f2010[9])) # 75s

# Screening ~ 20s w/all time fields or 5s w/string time
# system.time(rename(dt201009, all.names)) # 0s
# system.time(removeVars(dt201009))
# system.time(appendTimeVariables(dt201009, singleStamp = T, ym = T))
# system.time(clean(dt201009))

# Map to census tracts ~ 100s
# system.time(appendTractIds(dt201009, nyc_map, map = rgridToTractMapN800))

# Recode tract_id NAs as 0
# dt201009[is.na(pickup_tract_id), pickup_tract_id := 0]
# dt201009[is.na(dropoff_tract_id), dropoff_tract_id := 0]

# Plot the tracts/ points to verify mapping
# observation: most pickup points are along tract edges since boundaries
# are the roads themselves
# plotPickupInTract(dt200901, tracts)

# Set a key
# system.time(setkey(dt201009, pickup_tract_id, pickup_datetime, pickup_latitude, pickup_longitude, dropoff_tract_id, dropoff_datetime, dropoff_latitude, dropoff_longitude)) # 5s
# system.time(setkey(cl, pickup_tract_id, pickup_datetime, pickup_latitude, pickup_longitude, dropoff_tract_id, dropoff_datetime, dropoff_latitude, dropoff_longitude)) # 5s

# Duplicate checking
# cl = dt201009[(clean), vars, with = F]
# setkeyv(cl, vars)
# dupes = (1:cl[,.N])[duplicated(cl)]
# sort(cl[sort(c(dupes, dupes - 1)),])

# Count max pickups by tract, to guestimate slice query across the location
# dt200901[!is.na(pickup_tract_id),list(.N, pickup_tract_id), by = pickup_tract_id][N==max(N)]
# ~ 330k for 1 month => 24m for 72 months. Fits in RAM

