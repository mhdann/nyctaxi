# NYC Taxi Project
#
#  Processing tract identified trips to net transfer matricies and net trips
# 

library(data.table)
library(ggplot2)

project.root.dir = "C:/Users/mhdan_000/Dropbox/Incubator/nyctaxi/"

# Flat files by tract
pickup.dir   = "F:/nyc_yellow_grid/pickup/"
dropoff.dir  = "F:/nyc_yellow_grid/dropoff/"
# pickup.dirs  = list.files(pickup.dir)
# dropoff.dirs = list.files(pickup.dir)
pickup.dirs = formatC(3550:10000, width=5, flag="0")
dropoff.dirs = formatC(3550:10000, width=5, flag="0")
# Necessary parity check: is there a pickup tract dir for every dropoff?
identical(pickup.dirs, dropoff.dirs)



net.dirs = "F:/nyc_yellow_grid/net/"


source(paste0(project.root.dir, "nycParseRaw.R"))

for(grid in setdiff(pickup.dirs, "00000")){
   #grid = "00290" # Testing
  dt.pickups  <- rbindlist(lapply(list.files(paste0(pickup.dir,  grid), full.names = T), fread))
  dt.dropoffs <- rbindlist(lapply(list.files(paste0(dropoff.dir, grid), full.names = T), fread))
  
  cleanV1(dt.pickups)
  appendTimeVariablesCsv(dt.pickups)
  appendTimeVariablesCsv(dt.dropoffs)
    
  grid_names = c("pickup_grid","dropoff_grid")
  setnames(dt.pickups, grid_names, paste0(grid_names,"_id"))
  setnames(dt.dropoffs, grid_names, paste0(grid_names,"_id"))
  
  
  # To get Net Transfer Matrices rows
  
  # Trips out of the grid
  transfer.out  = dt.pickups[,list(trips = .N) , by = list(dropoff_grid_id, pickup_weekday, pickup_hour, pickup_quarter_hour)]
  
  # Trips into the grid
  transfer.into = dt.dropoffs[,list(trips = .N) , by = list(pickup_grid_id, dropoff_weekday, dropoff_hour, dropoff_quarter_hour)]
  
  
  # Trips in, trips out, by grid_id
  into = dt.dropoffs[,list(trips.in = .N) , by = list(dropoff_year, dropoff_month, dropoff_day, dropoff_weekday, dropoff_hour, dropoff_quarter_hour)]
  out  = dt.pickups[,list(trips.out = .N, mean_fare_rev_per_hour = mean(fare_rev_per_hour, na.rm=T)) , by = list(pickup_year, pickup_month, pickup_day, pickup_weekday, pickup_hour, pickup_quarter_hour)]
  
  # Join all, take difference.
  setkey(out, pickup_year, pickup_month, pickup_day, pickup_weekday, pickup_hour, pickup_quarter_hour)
  setkey(into, dropoff_year, dropoff_month, dropoff_day, dropoff_weekday, dropoff_hour, dropoff_quarter_hour)
  
  setnames(out, c("pickup_year", "pickup_month", "pickup_day", "pickup_weekday", "pickup_hour", "pickup_quarter_hour"),
           c("year", "month", "day", "weekday", "hour", "quarter_hour"))
  setnames(into, c("dropoff_year", "dropoff_month", "dropoff_day", "dropoff_weekday", "dropoff_hour", "dropoff_quarter_hour"),
           c("year", "month", "day", "weekday", "hour", "quarter_hour"))
  
  net = merge(out, into, all.x = T, all.y = T)
  net[is.na(trips.in), trips.in := 0]
  net[is.na(trips.out), trips.out := 0]
  net[, net.trips.in := trips.in - trips.out]
  
  dir.name = paste0(net.dirs, grid)
  dir.create(file.path(dir.name), showWarnings = FALSE)
  
  write.csv(row.names = F, x = net, paste0(dir.name, "/net.csv"))
  write.csv(row.names = F, x = transfer.into, paste0(dir.name, "/into.csv"))
  write.csv(row.names = F, x = transfer.out, paste0(dir.name, "/out.csv"))
  file_size = round(sum(file.size(list.files(dir.name, full.names = T)))/10^6, 2)
  output_msg = paste0("Processed grid ", grid, ", wrote ", file_size, "MB")  
  print(output_msg) 
}

#
# Concatenate to one file each
#
grid_codes = fread("grid_codes.csv") # To slice by neighborhood or borrough

  <- list()
L.into <- list()
L.out  <- list()

readNet <- function(filename){
  library(stringr)
  # filename = "C:\blah\bleep1\boop321\\0013\net.csv" # Double slash nec to escape
  dt <- fread(filename)
  print(paste0("Read in ", filename))
  str_grid <- str_extract(string = filename, "[0-9]{4}")
  if(!is.na(str_grid)){
    grid = as.integer(str_grid)
  } else {
    print(paste0("Error on ", filename))
    return(NULL)
  }
  dt[, grid_id := grid]
  return(dt)
}

net.dirs = "F:/nyc_yellow_grid/net/"

L.net = lapply(setdiff(pickup.dirs, "00000"), function(grid){
  file.name = paste0(net.dirs, grid,"/net.csv")
  readNet(file.name)
})

dt.net = rbindlist(L.net)

write.csv(row.names = F, x = dt.net, paste0(net.dirs,"net.trips.csv"))
