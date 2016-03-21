# Michael Dann
# mhdann@gmail.com
#
# Rectangular grid version of nycPipeLineCsv
# A pipeline to take RAW csvs to processed csvs by grid-yearmonth 
#
#

library(data.table)
library(ggplot2)
library(RPostgreSQL)

# Create your own root
src_dir = "C:/Users/mhdan_000/Dropbox/Incubator/nyctaxi/"
raw_dir = "D:/nyc_yellow_raw"
grid_dir = "D:/nyc_yellow_grid"


###############################################################################
# Name management
###############################################################################

# To be run in the raw csv directory
setwd(raw_dir)
source(file = paste0(src_dir, "nycRenaming.R"))

###############################################################################
# Parsing Raw / Appending Time Variables
###############################################################################

# appendTimeVariables(), removeVars(), clean(), cleanAll()
source(file = paste0(src_dir, "nycParseRaw.R"))

###############################################################################  
# Mapping files for the grid
###############################################################################  

setwd(src_dir)
source(file = paste0(src_dir, "nycMapping.R"))
# Loads: 
# tracts
# nyc_map
# ex_staten_island_map 
# rgridToTractMapN800
# roughingGrid(dt, var = "pickup",  nyc_map = nyc_map, n = n)


###############################################################################  
# List of Raw files
###############################################################################  

files = list.files(raw_dir ,pattern = "\\.csv$", full.names = T)

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

vars = c( "pickup_grid",   "pickup_datetime",  "pickup_latitude",  "pickup_longitude",
          "dropoff_grid", "dropoff_datetime", "dropoff_latitude", "dropoff_longitude",
          "passenger_count", "fare_rev_per_hour", "fare_amount", "total_amount", "trip_time", "trip_distance")

yearFromFile <- function(file){
  n = nchar(file)
  substr(file, n - 10, n - 7)
}

monthFromFile <- function(file){
  n = nchar(file)
  substr(file, n - 5, n - 4)
}

pipeline <- function(file, vars, con, tab, nyc_map){
  
  # file = f2009[[1]] #test  
  
  year  = yearFromFile(file)
  month = monthFromFile(file)
  
  dt = fread(file)  
  # dt = copy(dt.bak)
  
  rename(dt, all.names)
  removeVars(dt)
  appendTimeVariablesSimple(dt)
  clean(dt)
  
  roughingGrid(dt, var = "pickup", nyc_map = nyc_map, n = 100)
  roughingGrid(dt, var = "dropoff", nyc_map = nyc_map, n = 100)
  
  # Recode grid NAs as 0
  dt[is.na(pickup_grid), pickup_grid := 0]
  dt[is.na(dropoff_grid), dropoff_grid := 0]

  dt = dt[(clean), vars, with = F]
  
  {# Save sorted on pickup_grid
    setkey(dt, pickup_grid, pickup_datetime, pickup_latitude, pickup_longitude, dropoff_grid, dropoff_datetime, dropoff_latitude, dropoff_longitude)  
  
    for(grid in unique(dt$pickup_grid)){
      str.grid = sprintf("%05d", grid)
      dir.name = paste0(grid_dir,"/pickup/",
                        str.grid,
                        "/")
      dir.create(file.path(dir.name), showWarnings = FALSE)
      file.name = paste0(str.grid, "_",
                         yearFromFile(file), "_",
                         monthFromFile(file), ".csv")
      write.csv(dt[pickup_grid == grid],
                file = paste0(dir.name, file.name), row.names = F)
    }
  }
  
  {# Save sorted on dropoff_grid
    
    setkey(dt, dropoff_grid, dropoff_datetime, dropoff_latitude, dropoff_longitude, pickup_grid, pickup_datetime, pickup_latitude, pickup_longitude)  
    
    for(grid in unique(dt$dropoff_grid)){
      str.grid = sprintf("%05d", grid)
      dir.name = paste0(grid_dir, "/dropoff/",
                        str.grid,
                        "/")
      dir.create(file.path(dir.name), showWarnings = FALSE)
      file.name = paste0(str.grid, "_",
                         yearFromFile(file), "_",
                         monthFromFile(file), ".csv")
      write.csv(dt[dropoff_grid == grid],
                file = paste0(dir.name, file.name), row.names = F)
    }
  }
  
  print(paste0("Wrote ", file, " to grid_year_month.csv"))
}

# lapply(f2009, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map)
# lapply(f2010, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map)
# lapply(f2011, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map)
# lapply(f2012, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map)
# lapply(f2013, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map)
# lapply(f2014, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map)
# lapply(f2015, pipeline, vars = vars, con = con , tab = "nyc_yellow_test", nyc_map = nyc_map)

#2009-2010, desktop
#2011-2012, 2015 laptop
#2013-2014, desktop, rerun on laptop
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

