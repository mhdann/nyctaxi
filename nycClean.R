# Michael Dann
# mhdann@gmail.com
#
# NYC Taxi Commission database exploratory analysis and local database setup
#
#

library(data.table)
library(ggplot2)
library(lubridate)
library(RPostgreSQL)
library(fasttime)

# Create your own root
setwd("D:/data/taxi/nyctaxi/")

###############################################################################
# Name management
###############################################################################
# Load from files locally, check names

files = list.files("nyc_yellow_raw/", full.names = T)

# Compare names
dts       <- lapply(files, fread, nrows=1000)
dts.names <- lapply(dts, names)
uni.names <- unique(dts.names)

# Notes on names: 2009-2014 share the same variables in the same order,
# if different names. 2015 has different variables with different names

# The same variables remain through "fare amount" or row 13
# The "extra" variable appears in 2015. This may be equivalent to "surcharge" in 2014
# 
#           2014 v 2015
# row 14 surcharge v extra
# row 15 mta_tax v mta_tax
# row 16 tip_amount v tip_amount
# row 17 tolls_amount v tolls_amount
# row 18 total_amount v improvement_surcharge
# row 19 NULL        v total_amount


# Idea: create name sets corresponding to specific variables
uni <- function(...){
  # Traverses each ... in parallel and returns unique names across them
  mapply(function(...){unique(c(...))}, ...)
}

n1.13  = lapply(uni.names, function(x)x[1:13])
n15.17 = lapply(uni.names, function(x)x[15:17])

# A set of sets. Subsets are the unique names for each variable
all.names = c(
  do.call(uni, n1.13), 
  do.call(uni, n15.17), 
  tot = list(unique(c(uni.names[[1]][18], uni.names[[2]][18], uni.names[[3]][19])))
)
rm(n1.13, n15.17)

# Give them formal names
names(all.names) <- c("vendor_id",
                      "pickup_datetime",
                      "dropoff_datetime",
                      "passenger_count",
                      "trip_distance",
                      "pickup_longitude",
                      "pickup_latitude",
                      "rate_code",
                      "store_and_fwd_flag",
                      "dropoff_longitude",
                      "dropoff_latitude",
                      "payment_type",
                      "fare_amount",
                      "mta_tax",
                      "tip_amount",
                      "tolls_amount",
                      "total_amount")

# Rename all for consistency: note, "surchage" and "extra" ignored due to ambiguity
rename <- function(dt, all.names = all.names){
  # Given a dt and the namelist set,
  # rename dt variable in the subsets, to the subset name
  for(n in names(all.names)){
    dt.n = intersect(names(dt), all.names[[n]])
    setnames(dt, dt.n, n)
  }  
}


# Cleanup
rm(dts, dts.names, uni.names, uni)

# Check
# lapply(dts, rename, all.names = all.names) # Evaluated for side effects, results "null"
# lapply(dts, names)
# Check it, good. 
# Names are now consistent

###############################################################################
# Cleaning, indexing
###############################################################################

removeVars <- function(dt, vars = c("vendor_id", "store_and_fwd_flag", "surcharge", "extra", "mta_tax", "tip_amount", "tolls_amount", "improvement_surcharge")){
  # Removes variables not currently under analysis
  dt[, (vars) := NULL] # () added so that it gets seen as an expression
  print(paste0("Removed: ", paste(vars, collapse=", ")))
}

appendTimeVariables <- function(dt){
  # Replaces previous functions, fast
  library(lubridate)
  library(fasttime)
  # p_datetime  = as.POSIXct(dt$pickup_datetime,  format="%Y-%m-%d %H:%M:%S")
  # d_datetime = as.POSIXct(dt$dropoff_datetime, format="%Y-%m-%d %H:%M:%S")  
  p_datetime  = fastPOSIXct(dt$pickup_datetime, tz = "GMT")
  d_datetime = fastPOSIXct(dt$dropoff_datetime, tz = "GMT")
  # hackish tz treatment but verifiably working.
  # I never save the p/d_datetime, they are intermediary
  print("Processed datetime from string")
  # dt[, pickup_date  := as.Date(p_datetime)]
  # dt[, dropoff_date := as.Date(d_datetime)]
  # print("Added date")
  dt[, pickup_year  := year(p_datetime)]
  dt[, dropoff_year := year(d_datetime)]
  print("Added year")
  dt[, pickup_month  := month(p_datetime)]
  dt[, dropoff_month := month(d_datetime)]
  print("Added month")
  dt[, pickup_day := mday(p_datetime)]
  dt[,dropoff_day := mday(d_datetime)]
  print("Added day")
  dt[,pickup_hour  := hour(p_datetime)]
  dt[,dropoff_hour := hour(d_datetime)]
  print("Added hour")
  dt[,pickup_min  := minute(p_datetime)]
  dt[,dropoff_min := minute(d_datetime)]
  print("Added min")
  dt[,pickup_quarter_hour  := pickup_min  %/% 15]
  dt[,dropoff_quarter_hour := dropoff_min %/% 15]
  print("Added quarter_hour")
  dt[, trip_time := as.numeric(difftime(d_datetime, p_datetime, units = "hour"))]
  dt[, fare_rev_per_hour := fare_amount/trip_time]
  print("Calculated fare_rev_per_hour")
  # Day of the week
  dt[, weekday := wday(p_datetime)]
  print("Added weekday")
}

clean <- function(dt){
  # Clean the raw data of unrealistic distances and trip costs per mile
  # append "clean" field using side-effects of data.table references
  dt[, clean := F]
  dt[trip_distance < 100 & 
       fare_amount/trip_distance < 20 & 
       fare_amount/trip_distance > 1 & 
       pickup_longitude < -72 &
       pickup_longitude > -75 &
       pickup_latitude < 41.5 &
       pickup_latitude > 40,
     clean := T]
  
  # Ballpark the fare-density
  # plot(density(dt[clean == T & trip_time > 1/60 & fare_rev_per_hour < 1000 & pickup_hour == 6, fare_rev_per_hour]))
  
  # Looks log-normal
  # Looks like an upper cutoff around 200-250
  # looks like a lower cutoff around 25-30
  
  # Clean more
  dt[clean == T & !(trip_time > 1/60 & fare_rev_per_hour < 1000), clean := F]  
  print("Added 'clean' field to indicate valid data")
  
}

cleanAll <- function(dt, ...){
  removeVars(dt, ...)
  appendTimeVariables(dt)
  clean(dt)
}

# Testing
# blah = fastPOSIXct(head(dt200901$pickup_datetime), tz = "GMT")
# df = data.frame(pickup_datetime = head(dt200901$pickup_datetime), blah)
# df$blah

# Testing
# df = data.frame(head(dt200901$pickup_datetime, 100000), as.POSIXct(head(dt200901$pickup_datetime, 100000),  format="%Y-%m-%d %H:%M:%S"))
# names(df) <- c("String", "Time")
# df$hour = hour(df$Time)

###############################################################################  
# mapping, not reducing yet
###############################################################################  
library(rgdal)
library(maptools)
library(dplyr)
library(ggplot2)
library(RInside)
library(mapproj)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit() # 

# Copied 5 lines from toddwschneider
tracts = spTransform(readOGR("./nyct2010_15b", layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))
tracts@data$id = as.character(as.numeric(rownames(tracts@data)) + 1)
tracts.points = fortify(tracts, region = "id")
tracts.map = inner_join(tracts.points, tracts@data, by = "id")
nyc_map = tracts.map
ex_staten_island_map = filter(tracts.map, BoroName != "Staten Island")



roughingGrid <- function(lon, lat, nyc_map, n = 10, offset = 0.00001){
  # Returns the cell number corresponding to a lat/lon
  # in a roughing grid (n x n), within the extents of the nyc_map
    
  # Construct extremes of nyc_map (for use in roughing bounds)
  lat.min = min(nyc_map$lat) - offset # eliminates edge case (1,0] in roughingGrid()
  lat.max = max(nyc_map$lat)
  
  lon.min = min(nyc_map$lon) - offset # eliminates edge case (1,0] in roughingGrid() 
  lon.max = max(nyc_map$lon)
  
  lat.width = (lat.max - lat.min) / n 
  lon.width = (lon.max - lon.min) / n
  
  library(data.table)
  dt = data.table(lon, lat)
  # For a point, returning the roughingGrid cell number
  # (top to bottom, *RIGHT TO LEFT*)
  
  dt[, v := ((lat.max - lat) %/% lat.width) * n +
  (lon.max - lon) %/% lon.width + 1]
  
  # Clip the box
  dt[lon > lon.max, v := NA]
  dt[lat > lat.max, v := NA]
  dt[lon < lon.min, v := NA]
  dt[lat < lat.min, v := NA]
  
  dt$v
}


# Check corners
#  roughingGrid(max(nyc_map$lon), max(nyc_map$lat), nyc_map, n = 20) # 1
#  roughingGrid(min(nyc_map$lon), max(nyc_map$lat), nyc_map, n = 20) # n
#  roughingGrid(min(nyc_map$lon), min(nyc_map$lat), nyc_map, n = 20) # n*n
#  roughingGrid(max(nyc_map$lon), min(nyc_map$lat), nyc_map, n = 20) # n*(n-1) + 1

# Time it: 0.6s with boundary checking, 0.35s w/o
# system.time(dt200901[,rgid := roughingGrid(pickup_longitude, pickup_latitude)])
# sum(is.na(dt200901$rgid)) # 247k outside the grid

roughingGridPolys <- function(nyc_map, n = 10, offset = 0.00001){
  
  library(rgeos)
  
  lat.min = min(nyc_map$lat) - offset # eliminates edge case (1,0] in roughingGrid()
  lat.max = max(nyc_map$lat)
  
  lon.min = min(nyc_map$lon) - offset # eliminates edge case (1,0] in roughingGrid() 
  lon.max = max(nyc_map$lon)
  
  lat.width = (lat.max - lat.min) / n 
  lon.width = (lon.max - lon.min) / n
  
  n.grids = n*n
  grids = 1:n.grids
  
  # generate origin of upper-right corners
  row    = (grids - 1) %/% n
  column = (grids - row * n) - 1
  
  origins = data.table(lon = lon.max - column * lon.width, lat = lat.max - row * lat.width, id = grids)
  
  # Generate polygon for every grid
  Mxy <- function(x,y){
    o = c(x,y)
    matrix(c(o, o + c(0, -lat.width), o + c(-lon.width, -lat.width), o + c(-lon.width, 0), o),ncol = 2, byrow = T)  
  }
  
  sp = origins[, list(rects = list(Mxy(lon, lat))), by = list(id)]
  
  # Create SP
  polys <- SpatialPolygons(mapply(function(rect, id) {
    Polygons(list(Polygon(rect, hole = F)), ID=id)
  }, as.list(sp$rects), as.list(sp$id)))
  
  # Verify plotting from upper right
  # plot(polys[c(1:10, 86:100)])
  
  proj4string(polys) <- CRS("+proj=longlat +datum=WGS84")
  return(polys )
}

roughingGridToTracts <- function(tracts, rgrid.polys){
  # For each grid id in the roughing grid (corresponding to a rect)
  # List the ids of the corresponding tracts
  
  # For each polygon in polys, get list of tract ids
  # over(polys[1], tracts, returnlist = T)
  L <- list()
  for(i in 1:length(polys)){    
    intersect = over(rgrid.polys[i], tracts, returnList = T)
    L[[i]] <- as.vector(sapply(intersect, function(x) as.integer(x$id)))
  }  
  return(L)
}

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

# over() with tracts is extremely slow
# sp = SpatialPoints(dt200901[clean == T][,list(pickup_longitude, pickup_latitude)], CRS("+proj=longlat +datum=WGS84"))
# sp = SpatialPoints(dt200901[clean == T][1:10,list(pickup_longitude, pickup_latitude)], CRS("+proj=longlat +datum=WGS84"))
# system.time(int <- over(x = sp, y = tracts)) # 15-18s per million

# Improve over() time by subsetting (divide and conquer, binary sortish)

# 1) Do once: Map rough grid boxes n x n to subset of census tracts.
# O(n2), n = 50 is 90s, n = 100 is 6.5 minutes, n = 200 is 28 min, 
# system.time(rgridToTractMapN50 <- roughingGridToTracts(tracts, nyc_map, n = 50))
# saveRDS(rgridToTractMapN50, file="./rgidToTractN50.RDS")
# system.time(rgridToTractMapN100 <- roughingGridToTracts(tracts, nyc_map, n = 100))
# saveRDS(rgridToTractMapN100, file="./rgidToTractN100.RDS")
# system.time(rgridToTractMapN200 <- roughingGridToTracts(tracts, nyc_map, n = 200))
# saveRDS(rgridToTractMapN200, file="./rgidToTractN200.RDS")
# polys = roughingGridPolys(nyc_map, n = 800, offset = 0.00001)
# system.time(rgridToTractMapN800 <- roughingGridToTracts(tracts, rgrid.polys = polys))
# saveRDS(rgridToTractMapN800, file="./rgidToTractN800.RDS")

# rgridToTractMapN200 = readRDS("./rgidToTractN200.RDS")
# rgridToTractMapN400 = readRDS("./rgidToTractN400.RDS")
rgridToTractMapN800 = readRDS("./rgidToTractN800.RDS")

# system.time(rgridToTractMapN400 <- roughingGridToTracts(tracts, nyc_map, n = 400))
# saveRDS(rgridToTractMapN400, file="./rgidToTractN400.RDS")

# 2) Divide pickup/dropoff into rough grid boxes (0.7s, constant time)
dt200901[, pickup_rgid  := roughingGrid(pickup_longitude, pickup_latitude, n = 800, nyc_map = nyc_map)]
dt200901[, dropoff_rgid := roughingGrid(dropoff_longitude, dropoff_latitude, n = 800, nyc_map = nyc_map)]

# 3) Map through: pickup/dropoff -> tractid
# the over() call can be reduced to the appropriate tract subset

setTractId <- function(dt, map, var = "pickup"){
  # Quick-sets the easy to find tract ids based on the roughing grid
  # "0" is the explicit ID indicating that finer-spatial analysis is required
  
  keys <- function(ids){
    if(length(ids)==0){
      tract.id = as.integer(NA)     
    } 
    if(length(ids)==1){
      tract.id = ids[1]
    } 
    if (length(ids) > 1){
      # Multiple tract-ids per roughing id => need a closer look
      tract.id = 0
    }
    return(tract.id)
  }
  
  N = length(map)
  dt.key = data.table(rgid = c(NA, 1:N), tract_id = c(NA, sapply(lapply(map,unlist), keys)))  
  
  
  setnames(dt, paste0(var, "_rgid"), "rgid")  
  setkey(dt,     rgid)  # 3s
  setkey(dt.key, rgid)  # 0s
  
  dt[,list(rgid)][dt.key]$tract_id
  
  dt[, merge(dt, dt.key, by = "rgid")$tract_id]
  # START HERE
    
  setnames(dt, "rgid",     paste0(var, "_rgid"))
  setnames(dt, "tract_id", paste0(var, "_tract_id"))
  
  return(temp)
}


# Vectorized version of roughing, meant to by run for each rgid
idOfTract <- function(lat, lon, rgid){  
  ids = unlist(rgridToTractMapN800[[unique(rgid)[1]]])
  sp = SpatialPoints(data.frame(lon, lat), CRS("+proj=longlat +datum=WGS84"))
  out = over(sp, tracts[ids, ])
  # print(length(ids)) # tracing
  as.numeric(out$id)
}


sub = copy(dt200901)

# O(n): 4s n = 50, 8s n = 100, 17s & n = 200
setTractId(sub, rgridToTractMapN800, var = "pickup")
setTractId(sub, rgridToTractMapN800, var = "dropoff")

# sum(sub$pickup_tract_id==0, na.rm=T) # how many left, 13.5 m for 100, 12.9 m for 200

# Timing: 123s for n = 30, 130s for n = 100, 120s for n = 200
system.time(sub[ (pickup_tract_id == 0),  pickup_tract_id := idOfTract( pickup_latitude,  pickup_longitude,  pickup_rgid), by = pickup_rgid])
system.time(sub[(dropoff_tract_id == 0), dropoff_tract_id := idOfTract(dropoff_latitude, dropoff_longitude, dropoff_rgid), by = dropoff_rgid])

# testing
# idOfTract(dt200901[1,pickup_latitude], dt200901[1,pickup_longitude],  dt200901[1,pickup_rgid])
# idOfTract(dt200901[1,dropoff_latitude], dt200901[1,dropoff_longitude],  dt200901[1,dropoff_rgid])

###############################################################################  
# Pipeline test, MapReduce()
###############################################################################  

f2015 <- files[grep("2015", files)]
f2014 <- files[grep("2014", files)]
f2013 <- files[grep("2013", files)]
f2012 <- files[grep("2012", files)]
f2011 <- files[grep("2011", files)]
f2010 <- files[grep("2010", files)]
f2009 <- files[grep("2009", files)]

# For Testing, read is 80-100s per file
# dt201001 <- fread(f2010[1])
# dt201101 <- fread(f2011[1])
# dt201201 <- fread(f2012[1])
# dt201301 <- fread(f2013[1])
# dt201401 <- fread(f2014[1])
# dt201501 <- fread(f2015[1])


# reading 34:04 - 24:53 = 550s for 6 ~ 90s each
# system.time((taxis2015 = lapply(f2015, fread)))

# Read
system.time(dt200901 <- fread(f2009[1])) # 75s

# Rename
system.time(rename(dt200901, all.names)) # 0s

# Screening
system.time(cleanAll(dt200901)) # 16s with fastPOSIXct() and 285s with as.POSIXct()

# Map Reduce, either 1) calculate summary states by cell or 2) append cell
# system.time(appendCellMap1(dt200901))

# Saving, intermediary prior to DB integration
dir.create(file.path(".", "nyc_rds"), showWarnings = FALSE)
saveRDS(dt200901, "./nyc_rds/yellow_2009_01.RDS")


###############################################################################
# Save to DB
###############################################################################

library(RSQLite)
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "./nyc_DB/nyc.db")
# dbRemoveTable(con, "nyc_yellow")

vars = c( "pickup_tract_id",  "pickup_latitude",  "pickup_longitude",  "pickup_year",  "pickup_month",  "pickup_day",  "pickup_hour",  "pickup_quarter_hour",  "pickup_min",
         "dropoff_tract_id", "dropoff_latitude", "dropoff_longitude", "dropoff_year", "dropoff_month", "dropoff_day", "dropoff_hour", "dropoff_quarter_hour", "dropoff_min",
         "weekday", "passenger_count", "fare_rev_per_hour", "fare_amount", "total_amount", "trip_time", "trip_distance", "clean")

dbWriteTable(con, "nyc_yellow", dt200901[, vars, with = F], append = T)
dbDisconnect(con)  

library(RPostgreSQL)
library(sqldf)

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "new_user_password"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "nyc",
                 host = "localhost", port = 5433,
                 user = "mhdann", password = "")


# check for the cartable
dbExistsTable(con, "postgres")
dbListTables(con)
dbWriteTable(con, "nyc_yellow", dt200901, append = T)
# TRUE



###############################################################################

plot(density(t2015[clean == T & fare_rev_per_hour < 200 & fare_rev_per_hour > 3 & Weekday == "Monday" & pickup_hour == 6, log(fare_rev_per_hour)]))

# Plot fare density 2d, log-normal scale
xlims = c(-74.05,-73.75); ylims = c(40.6, 40.9)

my_theme = theme(panel.background = element_rect(fill = "black"),
                 panel.border = element_rect(colour = "black", fill=NA, size=2),
                 axis.title = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.ticks = element_blank(), 
                 axis.text = element_blank(), 
                 legend.position="bottom") 
n = 800
breaks = list(x = seq(xlims[1], xlims[2], length.out = n),
              y = seq(ylims[1], ylims[2], length.out = n))

monday = t2015[clean == T & trip_time > 1/60 & fare_rev_per_hour < 201 & fare_rev_per_hour > 29 & Weekday == "Monday", list(pickup_hour, pickup_longitude, pickup_latitude, fare_rev_per_hour, passenger_count)]
# Pick-up fare_rev_per_hour density, time sensitive for ride sharing

for(h in 0:23){
  p <- ggplot(data = monday[pickup_hour == h,],
              aes(x = pickup_longitude, y = pickup_latitude, z = fare_rev_per_hour)) + 
    stat_summary2d(fun = mean, breaks = breaks) + my_theme +  
    xlim(xlims) + ylim(ylims) + 
    scale_fill_gradient(name = "Fare Revenue per Hour", 
                        trans= "log", 
                        labels = c("50", "100"),
                        breaks = c(50, 100),
                        low = "black", high = "white") 
  # + coord_map(projection = "ortho", orientation = c(mean(ylims), mean(xlims), 0))
  p <- p + annotate(geom="text", x=breaks$x[n/2], y=breaks$y[n-20], label=paste0("Fare Revenue per Hour on Monday at ", sprintf("%02d:00",h)),
                    color="white")
  ggsave(filename = paste0("images/density_fare_rev_2015_Monday_",sprintf("%02d00",h),".png"),
         plot = p,
         width = 6, height = 5, dpi = 300)
}

#################### GIF MAKING CALL, IMAGEMAGICK ############
#  cd ./images
#  convert.exe -delay 50 -loop 0 *.png monday.gif
#################### GIF MAKING CALL, IMAGEMAGICK ############


# Time-series of the weekly pattern in prices
weekly = t2015[clean == T & trip_time > 1/60 & fare_rev_per_hour < 201 & fare_rev_per_hour > 29,
               list(fare_rev_per_hour = mean(fare_rev_per_hour)),
               by = list(Weekday, pickup_hour)]

w = ggplot(data = weekly, aes(x = pickup_hour, y = fare_rev_per_hour, group = Weekday, color = Weekday)) +
  geom_line() + theme_bw() + 
  labs(x = "Hour", y = "Fare Revenue per Hour")

ggsave(filename = paste0("ts_fare_rev_2015_weekday.png"),
       plot = w,
       width = 6, height = 5, dpi = 300)


# Monthly patterns 
monthly = t2015[clean == T & trip_time > 1/60 & fare_rev_per_hour < 201 & fare_rev_per_hour > 29,
                list(fare_rev_per_hour = mean(fare_rev_per_hour)),
                by = list(Month, pickup_hour)]




#
# smoothScatter(t2015[clean == T & pickup_day == 1 , list(pickup_longitude, pickup_latitude)][1:1000000,],
#              xlim = xlims, ylim = ylims)

# Check Passenger density
plot(density(t2015[clean == T & trip_time > 1/60 & fare_rev_per_hour < 200 & Weekday == "Monday" & pickup_hour == 6, passenger_count]))

for(h in 0:23){
  p <- ggplot(data = monday[pickup_hour == h,],
              aes(x = pickup_longitude, y = pickup_latitude, z = passenger_count)) + 
    stat_summary2d(fun = sum, breaks = breaks) + my_theme +  
    xlim(xlims) + ylim(ylims) + 
    scale_fill_gradient(name = "Passenger Density", 
                        trans= "log", 
                        low = "black", high = "white") 
  p <- p + annotate(geom="text", x=breaks$x[n/2], y=breaks$y[n-20], label=paste0("Passengers Coming/Arriving on Monday at ", sprintf("%02d:00",h)),
                    color="white")
  ggsave(filename = paste0("images/density_fare_rev_2015_Monday_",sprintf("%02d00",h),".png"),
         plot = p,
         width = 6, height = 5, dpi = 300)
}


# Plotting pickup/dropoff density
plotCity <- function(dt, title, alpha = 0.01, size = 0.001){
  ggplot(data = dt) + 
    geom_point(aes(x = pickup_longitude, y = pickup_latitude), 
               color = "white", size = size, alpha = alpha, ) + 
    geom_point(aes(x = dropoff_longitude, y = dropoff_latitude), 
               color = "white", size = size, alpha = alpha, ) + 
    my_theme +
    xlim(xlims) + ylim(ylims) + labs(title = title)
}

# monday = t2015[clean == T & trip_time > 1/60 & fare_rev_per_hour < 201 & fare_rev_per_hour > 29 & Weekday == "Monday", list(pickup_hour, pickup_longitude, pickup_latitude, pickup_hour, dropoff_longitude, dropoff_latitude)]
# plotCity(monday, title = "Monday Pickup & Dropoff Density")
