#
# Library for developing derivatives and plots from gridded data
#

# Grid extents

# CellNumbers(GridExtents, N)

# Testing:
# setwd("C:/Users/mhdan_000/Dropbox/Incubator/nyctaxi/")

# From census map extent (nycMapping.R)

# min.x = min(nyc_map$lon)
# max.x = max(nyc_map$lon)
# min.y = min(nyc_map$lat)
# max.y = max(nyc_map$lat)

# Testing
# setwd("C:/Users/mhdan_000/Dropbox/Incubator/nyctaxi/")

censusExtent <- c(-74.25559,-73.70001, 40.49612, 40.9153) # c(min.x, max.x, min.y, max.y)
  
latLonToGrid <- function(lat, lon, n = 100, offset = 0.00001, leftToRight = T){  
  # Returns the cell number corresponding to a lat/lon
  # in a roughing grid (n x n), within the extents of the nyc_map.
  # Vectorized
  
  # Construct extremes of nyc_map (for use in roughing bounds)
  lat.min = censusExtent[3] - offset # eliminates edge case (1,0] in roughingGrid()
  lat.max = censusExtent[4]
  
  lon.min = censusExtent[1] - offset # eliminates edge case (1,0] in roughingGrid() 
  lon.max = censusExtent[2];'.;;;;;;;;;;;;;;;'
  
  lat.width = (lat.max - lat.min) / n 
  lon.width = (lon.max - lon.min) / n
  
  # For a point, returning the grid cell number
  # (top to bottom, *LEFT TO RIGHT*) ro
  # NOTE: Prior versions were RIGHT TO LEFT, such as nycMapping.roughingGrid(). this has been amended
  if(leftToRight){
    grid_id = (lat.max - lat) %/% lat.width * n  +
              (lon - lon.min) %/% lon.width  + 1
  } else { 
    grid_id = (lat.max - lat) %/% lat.width * n +
              (lon.max - lon) %/% lon.width + 1
  }
  
  # Clipping index
  na_index = lon > lon.max | 
             lat > lat.max |
             lon < lon.min |
             lat < lat.min

  grid_id[na_index] = NA # Not in place... could be problematic for larger vectors  
  return(grid_id)
  # Testing
  # Check corners
  # latLonToGrid(lat = nyc_map$lat, lon = nyc_map$lon, n = 20)
  # Time it: 0.6s with boundary checking, 0.35s w/o
  # system.time(dt200901[,grid := roughingGrid(pickup_longitude, pickup_latitude)])
  # sum(is.na(dt200901$grid)) # 247k outside the grid
}

gridHFlip <- function(grid_id, n){
  # Fixes prior grid assignments by flipping horizontally the raster grid_ids
  # Mispecifying n will mess up the assignment.
  
  # Get colNum (RIGHT to LEFT, but shall be swapped, so irrelevant)
  colNum = (grid_id - 1) %% n + 1
  
  # Reflect grid about center by adding the reflector vector indexed on col#
  reflector = seq((n-1), (1-n), by = -2)
  
  return(grid_id + reflector[colNum])
  # Testing
  # gridHFlip(18:1, n = 9)
  # gridHFlip(1:20, n = 9)
  # matrix(gridHFlip(1:9, 3), ncol = 3,byrow = T)
  # matrix(gridHFlip(1:16, 4), ncol = 4,byrow = T)
}

gridReduce <- function(grid_id, n = 100, smaller_n = 25){
  # Grid downsize to finer mesh
  # Takes a grid_id and linear grid number
  # Outputs a grid_id for a new new, coarser mesh defined by "smaller_n" the linear size
  # Vectorized
  # 
  # Idea:
  # Get col/row of fine mesh
  # downsize to col/row of new mesh by fine/coarse factor
  # Assign new id
  #
  reduceFactor = n / smaller_n
  if(reduceFactor != floor(reduceFactor)){
    print(paste(n, "is not an integer multiple of", smaller_n))
    return(NULL)
  }
  
  # Zero indexed
  fineCol = (grid_id - 1) %% n
  fineRow = grid_id %/% n 
  coarseCol = fineCol %/% reduceFactor
  coarseRow = fineRow %/% reduceFactor
  
  # One indexed
  coarse_grid_id = 1 + coarseCol + coarseRow * smaller_n 
  return(coarse_grid_id)
}

getCentroidGrid <- function(grid_id, n, offset = 0.00001, leftToRight=T){
 # Given grid_ids and linear grid size
 # return centroids of grid points
 
 # Zero Indexed row/col:
 row = (grid_id - 1) %/% n 
 col = (grid_id - 1) %% n
 
 lat.width = (offset + censusExtent[4] - censusExtent[3]) / n
 lon.width = (offset + censusExtent[2] - censusExtent[1]) / n 
 
 # centroid of Upper-left origin
 # for standard top-bottom, left-to-right raster cell ordering
 # x, y
 # lon, lat
 origin = c(censusExtent[1], censusExtent[4]) + 1/2 * c(lon.width, -lat.width)
 
 centroid.lat = origin[2] - row * lat.width
 centroid.lon = origin[1] + col * lon.width
 df = data.frame(lon = centroid.lon, lat = centroid.lat, grid_id = grid_id)
 coordinates(df) = ~ lon + lat
 proj4string(df) = CRS("+proj=longlat +datum=WGS84")
 
 return(df)
 # Test: getCentroidGrid(1, 100)
}
 
gridToGraphOld <- function(grid_id,n){
  library(plyr)
  connected <- function(grid_id_1, grid_id_2, n){
    if(abs((grid_id_1 - 1) %% n - (grid_id_2 - 1) %% n) == 1 &
         (grid_id_1 - 1) %/% n == (grid_id_2 - 1) %/% n){
      return(1)
    }
    
    if(abs((grid_id_1 - 1) %/% n - (grid_id_2 - 1) %/% n) == 1 & 
         (grid_id_1 - 1) %% n == (grid_id_2 - 1) %% n){
      return(1)
    }
    return(0)
  }
  # Cartesian product: grid_id X grid_id
  df = expand.grid(gid_1 = 1:n^2, gid_2 = 1:n^2)
  
  # For each combo, check connected(), symmetry => matrix fill direction irrelevant
  M = matrix(mapply(connected, df$gid_1, df$gid_2, n), ncol = n^2, nrow = n^2)
  
  # For grids not in the provided grid_ids, delete adjacency
  del_grids = setdiff(1:n^2, unique(grid_id))
  
  M[del_grids, ] = 0
  M[, del_grids] = 0
  
}
 
gridToGraph <- function(grid_id, n){
  # Given a vector of grid_ids present in an (n x n) grid,
  # return the adjacency matrix for the undirected graph
  
  # Idea 1: generate adjacency for square matrix,
  #         wipe out missing (non-physical) cells
  #
  subdiag <- function(vec, size, offset=0){ 
    # diag(x,k) matlab equivalent
    # https://stackoverflow.com/questions/7745363/r-equivalent-to-diagx-k-in-matlab
    M <- matrix(0, size, size)
    M[row(M)-offset == col(M)] <- vec
    return(M)
  }
  grid_length = n^2
  M = subdiag(1, grid_length, 1) +
      subdiag(1, grid_length, -1) +
      subdiag(1, grid_length, n) + 
      subdiag(1, grid_length, -n)
  M[row(M) %% n == 0 & col(M) %% n == 1] = 0 
  M[row(M) %% n == 1 & col(M) %% n == 0] = 0

  # For grids not in the provided grid_ids, delete adjacency
  del_grids = setdiff(1:n^2, unique(grid_id))
  
  M[del_grids, ] = 0
  M[, del_grids] = 0

  return(M)
  # Test: gridToGraph(grid_id = 1:9, n = 3)
} 
# M =  gridToGraph(grid_id = 1:10000, n = 100)

 
graphWater <- function(graph){
 # Using the extent, the square graph, and a map, 
 # null out the graph connections to water/uninhabitable areas
 
 # Idea:
 # For each point associated with the graph, check if it's in NY or NJ.
 # If not, zero out the graph connections for that row/column
 library(rgdal)
 library(maptools)
 nytracts = spTransform(readOGR("./nyct2010_15b", layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))
 njtracts = spTransform(readOGR("./Census2010Tr2012", layer = "Govt_TIGER2012_tract2010"), CRS("+proj=longlat +datum=WGS84"))
 boroughs = unionSpatialPolygons(nytracts, nytracts$BoroCode)
 nj = unionSpatialPolygons(njtracts, njtracts$COUNTYFP)
 n = sqrt(dim(graph)[1])
 # Test 
 # centroids = getCentroidGrid(1:100, 10) 
 centroids = getCentroidGrid(1:n^2, n) 
 remove = is.na(over(centroids, nj)) & is.na(over(centroids, boroughs))
 
 graph[remove,] = 0
 graph[,remove] = 0
 return(graph)
}

 
graphConnect <- function(graph, gids1, gids2){
 # Connect the given graph ids on the graph
 # operation is copy, not in-place
 # mapply(function(gid1,gid2){
  # graph[gid1, gid2] = 1
  # graph[gid2, gid1] = 1   
 #}, gids1, gids2)
 for(i in 1:length(gids1)){
   graph[gids1[i], gids2[i]] = 1
   graph[gids2[i], gids1[i]] = 1
 }
 return(graph)
}

getBridgesAndTunnels <- function(){
 # Read bridges and tunnels file
 # Return df.bridges = data.frame(w_lat, w_lon, e_lat, e_lon, name) row pairs in DF
 # Must be in project src dir
 return(read.csv('bridgesAndTunnels.csv'))
}

graphBridgesAndTunnels <- function(graph){
 # Take bridgesAndTunnels,
 # append graph according to entrances/exists of bridges and tunnels
 bAndT = getBridgesAndTunnels()
 n = sqrt(dim(graph)[1])
 bAndT$w_grid_id = latLonToGrid(lat = bAndT$w_lat, lon = bAndT$w_lon, n = n)
 bAndT$e_grid_id = latLonToGrid(lat = bAndT$e_lat, lon = bAndT$e_lon, n = n) 
 bAndT$connect = bAndT$w_grid_id != bAndT$e_grid_id
 print(bAndT)
 return(graphConnect(graph, gids1 = bAndT$e_grid_id[bAndT$connect], gids2= bAndT$w_grid_id[bAndT$connect]))
}


plotGraph <- function(graph){
 # Given a graph, plot the connections as lines
 
 library(plyr)
 n = sqrt(dim(graph)[1]) # assumes graph represents square grid
 
 # Get list of connected graph ids, plot the connected nodes in the upper triangular (symmetry)
 line_grid_ids = which(graph * upper.tri(graph) == 1, arr.ind = T)
 
 # Make data frame of pairs of points
 c1 = getCentroidGrid(line_grid_ids[,1], n)
 c2 = getCentroidGrid(line_grid_ids[,2], n)
 
 df = as.data.frame(cbind(coordinates(c1), coordinates(c2)))  
 names(df) <- c("p1_lon", "p1_lat", "p2_lon", "p2_lat")
 
 # plot lines 
 plot(1, main="Transportation Graph",type="n", xlab="", ylab="", xlim = censusExtent[1:2], ylim = censusExtent[3:4])
 m_ply(.data = df, .fun = function(p1_lon, p1_lat, p2_lon, p2_lat){
   lines(c(p1_lon, p2_lon), c(p1_lat, p2_lat))
 })
}

 
# testGraph  = gridToGraph(1:25, 5) 
# testGraph2 = graphWater(testGraph)

# N =  gridToGraph(grid_id = 1:2500, n = 50)
# N.wat = graphWater(N) 
# N.bat = graphBridgesAndTunnels(N.wat)
# plotGraph(N.bat)
 
M =  gridToGraph(grid_id = 1:10000, n = 100)
M.wat = graphWater(M)
A = graphBridgesAndTunnels(M.wat) # Adjacency matrix A

saveAdjacencyMatrix <- function(A, filename = "./"){
  
}


saveGraphPlot <- function(graph){
  png("./images/transport_graph.png", width = 800, height = 800)
  plotGraph(A)
  dev.off()
}

# saveGraphPlot(A)

connectivity = colSums(A) # Number of connected grid points

diffuse <- function(V, delta=0){
  averages = colSums(V * A)/connectivity
  averages[is.na(averages)] = 0
    del.V = (averages-V) + delta
  return(V + del.V)
}
  
getGridPolys <- function(n = 100, offset = 0.00001){
 # Returns spatialpolygonsdataframe corresponding to the grid
 # Grid ID order is raster standard: Top to bottom outter, Left to Right inner
 
 library(rgeos)
 library(data.table)
 
 lat.min = censusExtent[3] - offset # eliminates edge case (1,0] in roughingGrid()
 lat.max = censusExtent[4]
 
 lon.min = censusExtent[1] - offset # eliminates edge case (1,0] in roughingGrid() 
 lon.max = censusExtent[2]
 
 lat.width = (lat.max - lat.min) / n 
 lon.width = (lon.max - lon.min) / n
 
 n.grids = n*n
 grids = 1:n.grids
 
 # generate origin of upper-left corners
 # Zero indexed:
 row    = (grids - 1) %/% n   
 column = (grids - row * n) - 1
 
 origins = data.table(lon = lon.min + column * lon.width, lat = lat.max - row * lat.width, id = grids)
 
 # Generate polygon for every grid
 Mxy <- function(x,y){
   o = c(x,y)
   matrix(c(o, o + c(0, -lat.width), o + c(lon.width, -lat.width), o + c(lon.width, 0), o),ncol = 2, byrow = T)  
 }
 
 sp = origins[, list(rects = list(Mxy(lon, lat))), by = list(id)]
 
 # Create SP
 polys <- SpatialPolygons(mapply(function(rect, id) {
   Polygons(list(Polygon(rect, hole = F)), ID=id)
 }, as.list(sp$rects), as.list(sp$id)))
 
 # Verify plotting from upper left
 # plot(getGridPolys[c(1:10, 86:100)])
 
 proj4string(polys) <- CRS("+proj=longlat +datum=WGS84")
 return(polys )
}

ps = getGridPolys()
library(ggplot2)
fps = fortify(ps)

plotGridData <- function(fortified_polys, V, title="", range.clip=10){
  # Plots data in the grid, must be ordered by grid ID,
  # must be nxn or length(V) == n^2 

  library(ggplot2)
  p <-ggplot() + geom_polygon(data = fortified_polys, alpha = 0.9, color = NA,
               aes(x = long, y = lat, group = id, fill = V[as.integer(id)]),                  
               size = 0) + ggtitle(title) + 
    scale_fill_gradient2(limits = c(-range.clip,range.clip), high = "blue", mid = "white", low = "red") +
    coord_map(xlim = censusExtent[1:2], ylim = censusExtent[3:4]) +
    theme(legend.position = "bottom",
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    labs(fill = "Density")
  plot(p)
}
 


###############################################################################
# Diffusion model, proof of concept
###############################################################################

V <- as.vector(matrix(c(rep(10, 5000), rep(-10,5000)), ncol=100, nrow=100, byrow = T))
L <- list(V)
for(i in 2:100){
  L[[i]] <- diffuse(L[[i-1]])
}

png("./images/transport_example_002.png", width = 800, height = 800)
plotGridData(fps, L[[2]], title = "Time 002") 
dev.off()

png("./images/transport_example_100.png", width = 800, height = 800)
plotGridData(fps, L[[100]], title = "Time 100") 
dev.off()
