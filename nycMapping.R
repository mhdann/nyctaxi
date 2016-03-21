# nycMapping library
#
#

library(rgdal)
library(maptools)
library(dplyr)
library(ggplot2)
library(RInside)
library(mapproj)

# Requires RTools and RTools dir in windows path!!!!!
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit() # 

# Copied 6 lines from toddwschneider
tracts = spTransform(readOGR("./nyct2010_15b", layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))
tracts@data$id = as.character(as.numeric(rownames(tracts@data)) + 1)
tracts.points = fortify(tracts, region = "id")
tracts.map = inner_join(tracts.points, tracts@data, by = "id")
nyc_map = tracts.map
ex_staten_island_map = filter(tracts.map, BoroName != "Staten Island")

saveCodeMap <- function(nyc_map){
  write.csv(row.names = F, file = "tract_codes.csv", x = unique(nyc_map[,c("id", "CTLabel", "BoroName", "BoroCode", "CT2010", "BoroCT2010", "CDEligibil", "NTACode", "NTAName")]))
}

roughingGrid <- function(dt, var="pickup",  nyc_map, n = 800, offset = 0.00001){  
  # Returns the cell number corresponding to a lat/lon
  # in a roughing grid (n x n), within the extents of the nyc_map.
  # Vectorized
  
  # Construct extremes of nyc_map (for use in roughing bounds)
  lat.min = min(nyc_map$lat) - offset # eliminates edge case (1,0] in roughingGrid()
  lat.max = max(nyc_map$lat)
  
  lon.min = min(nyc_map$lon) - offset # eliminates edge case (1,0] in roughingGrid() 
  lon.max = max(nyc_map$lon)
  
  lat.width = (lat.max - lat.min) / n 
  lon.width = (lon.max - lon.min) / n
  
  setnames(dt, paste0(var,"_latitude"), "lat")
  setnames(dt, paste0(var,"_longitude"), "lon")
  
  # For a point, returning the roughingGrid cell number
  # (top to bottom, *RIGHT TO LEFT*)
  
  dt[, grid := ((lat.max - lat) %/% lat.width) * n +
       (lon.max - lon) %/% lon.width + 1]
  
  # Clip the box
  dt[lon > lon.max, grid := NA]
  dt[lat > lat.max, grid := NA]
  dt[lon < lon.min, grid := NA]
  dt[lat < lat.min, grid := NA]
  
  setnames(dt, c("lat", "lon", "grid"),
           paste0(var, c("_latitude", "_longitude", "_grid")))
  
  
  # Testing
  # Check corners
  #  roughingGrid(max(nyc_map$lon), max(nyc_map$lat), nyc_map, n = 20) # 1
  #  roughingGrid(min(nyc_map$lon), max(nyc_map$lat), nyc_map, n = 20) # n
  #  roughingGrid(min(nyc_map$lon), min(nyc_map$lat), nyc_map, n = 20) # n*n
  #  roughingGrid(max(nyc_map$lon), min(nyc_map$lat), nyc_map, n = 20) # n*(n-1) + 1
  
  # Time it: 0.6s with boundary checking, 0.35s w/o
  # system.time(dt200901[,grid := roughingGrid(pickup_longitude, pickup_latitude)])
  # sum(is.na(dt200901$grid)) # 247k outside the grid
  
}

roughingGridPolys <- function(nyc_map, n = 800, offset = 0.00001){
  # Returns spatialpolygonsdataframe corresponding to the roughing grid
  
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

# over() with tracts is extremely slow
# sp = SpatialPoints(dt200901[clean == T][,list(pickup_longitude, pickup_latitude)], CRS("+proj=longlat +datum=WGS84"))
# sp = SpatialPoints(dt200901[clean == T][1:10,list(pickup_longitude, pickup_latitude)], CRS("+proj=longlat +datum=WGS84"))
# system.time(int <- over(x = sp, y = tracts)) # 15-18s per million

# Improve over() time by subsetting (divide and conquer, binary sortish)

# 1) Do once: Map rough grid boxes n x n to subset of census tracts.
# O(n2), n = 50 is 90s, n = 100 is 6.5 minutes, n = 200 is 28 min, 
# system.time(rgridToTractMapN50 <- roughingGridToTracts(tracts, nyc_map, n = 50))
# saveRDS(rgridToTractMapN50, file="./gridToTractN50.RDS")
# system.time(rgridToTractMapN100 <- roughingGridToTracts(tracts, nyc_map, n = 100))
# saveRDS(rgridToTractMapN100, file="./gridToTractN100.RDS")
# system.time(rgridToTractMapN200 <- roughingGridToTracts(tracts, nyc_map, n = 200))
# saveRDS(rgridToTractMapN200, file="./gridToTractN200.RDS")
# polys = roughingGridPolys(nyc_map, n = 800, offset = 0.00001)
# system.time(rgridToTractMapN800 <- roughingGridToTracts(tracts, rgrid.polys = polys))
# saveRDS(rgridToTractMapN800, file="./gridToTractN800.RDS")

# rgridToTractMapN200 = readRDS("./gridToTractN200.RDS")
# rgridToTractMapN400 = readRDS("./gridToTractN400.RDS")
rgridToTractMapN800 = readRDS("./gridToTractN800.RDS")

# system.time(rgridToTractMapN400 <- roughingGridToTracts(tracts, nyc_map, n = 400))
# saveRDS(rgridToTractMapN400, file="./gridToTractN400.RDS")

quickSetTractId <- function(dt, map, var){
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
  dt.key = data.table(grid = c(NA, 1:N), tract_id = c(NA, sapply(lapply(map,unlist), keys)))  
  
  setnames(dt, paste0(var, "_grid"), "grid")  
  setkey(dt,     grid)  # 3s
  setkey(dt.key, grid)  # 0s
  
  # Fast-merge
  dt[, tract_id := dt.key[dt[,list(grid)]]$tract_id]
  
  setnames(dt, "grid",     paste0(var, "_grid"))
  setnames(dt, "tract_id", paste0(var, "_tract_id"))
  return(NULL)
}

# Vectorized version of roughing, meant to be run for each grid or groups of grid
idOfTract <- function(lat, lon, grid){
  ids = unique(unlist(rgridToTractMapN800[unique(grid)]))
  sp = SpatialPoints(data.frame(lon, lat), CRS("+proj=longlat +datum=WGS84"))
  out = over(sp, tracts[ids, ])
  # print(length(ids)) # tracing
  as.numeric(out$id)
}


appendTractIds <- function(dt, nyc_map, n = 800, map = rgridToTractMapN800){  
  # 2) Divide pickup/dropoff into rough grid boxes (0.7s, constant time)
  roughingGrid(dt, var = "pickup",  nyc_map = nyc_map, n = n)
  roughingGrid(dt, var = "dropoff", nyc_map = nyc_map, n = n)
  
  # 3) Map through: pickup/dropoff -> tractid
  quickSetTractId(dt, map = map, var = "dropoff") # 8s
  quickSetTractId(dt, map = map, var = "pickup")  # 8s
  
  # slow - spatial pairing of tract_ids for remaining points
  # 50s - 90s
  dt[ (pickup_tract_id == 0),  pickup_tract_id := idOfTract( pickup_latitude,  pickup_longitude,  pickup_grid), by =  pickup_grid %/% (n/2)]
  dt[(dropoff_tract_id == 0), dropoff_tract_id := idOfTract(dropoff_latitude, dropoff_longitude, dropoff_grid), by = dropoff_grid %/% (n/2)]  
  
  print(paste0("Appended tract_id"))
}



# the over() call can be reduced to the appropriate tract subset






# sub = copy(dt200901)

# O(n): 4s n = 50, 8s n = 100, 17s & n = 200
# setTractId(sub, rgridToTractMapN800, var = "pickup")
# setTractId(sub, rgridToTractMapN800, var = "dropoff")

# sum(sub$pickup_tract_id==0, na.rm=T) # how many left, 13.5 m for 100, 12.9 m for 200

# Timing: 123s for n = 30, 130s for n = 100, 120s for n = 200
# system.time(sub[ (pickup_tract_id == 0),  pickup_tract_id := idOfTract( pickup_latitude,  pickup_longitude,  pickup_grid), by = pickup_grid])
# system.time(sub[(dropoff_tract_id == 0), dropoff_tract_id := idOfTract(dropoff_latitude, dropoff_longitude, dropoff_grid), by = dropoff_grid])

# testing
# idOfTract(dt200901[1,pickup_latitude], dt200901[1,pickup_longitude],  dt200901[1,pickup_grid])
# idOfTract(dt200901[1,dropoff_latitude], dt200901[1,dropoff_longitude],  dt200901[1,dropoff_grid])


# Sanity check for spatial mapping
plotPickupInTract <- function(dt, tracts){
  samp.row = dt[!is.na(pickup_tract_id),][sample(.N, 1), ]
  sp = SpatialPoints(samp.row[,list(pickup_longitude, pickup_latitude)])
  plot(tracts[samp.row[, pickup_tract_id], ])
  plot(sp, add = T)  
}