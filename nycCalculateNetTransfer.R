# NYC Taxi Project
#
#  Processing tract identified trips to transfer matricies and net trips
# 

library(data.table)
library(ggplot2)

project.root.dir = "D:/data/taxi/nyctaxi/"

# Flat files by tract
pickup.dir   = "F:/nyc_yellow_tract/pickup/"
dropoff.dir  = "F:/nyc_yellow_tract/dropoff/"
pickup.dirs  = list.files(pickup.dir)
dropoff.dirs = list.files(pickup.dir)

# Necessary parity check: is there a pickup tract dir for every dropoff?
identical(pickup.dirs, dropoff.dirs)

net.dirs = "F:/nyc_yellow_tract/net//"

source(paste0(project.root.dir, "nycParseRaw.R"))

# Weather data, for later
# wunder <- fread(paste0(project.root.dir, "wunder/wunder.csv"))

# Tract code files, neighborhood, borrough etc.
tract_codes = fread("tract_codes.csv")
setnames(tract_codes, "id", "tract_id")
tract_codes[,tract_id := as.integer(tract_id)]
setkey(tract_codes, tract_id)



for(tract in setdiff(pickup.dirs, "0000")){
  # tract = "0029" # Testing
  dt.pickups  <- rbindlist(lapply(list.files(paste0(pickup.dir,  tract), full.names = T), fread))
  dt.dropoffs <- rbindlist(lapply(list.files(paste0(dropoff.dir, tract), full.names = T), fread))
  
  cleanV1(dt.pickups)
  appendTimeVariablesCsv(dt.pickups)
  appendTimeVariablesCsv(dt.dropoffs)
  
  # Work in progress: track pickup/dropoffs by NTACode and BoroCode
  # setkey(dt.pickups, tract_id)
  # setkey(dt.dropoffs, tract_id)
  # (tract_codes[dt.pickups])[, list(BoroCode, NTACode)]
  
  # Transfer Matricies rows
  
  # Trips out of the tract
  transfer.out  = dt.pickups[,list(trips = .N) , by = list(dropoff_tract_id, pickup_weekday, pickup_hour, pickup_quarter_hour)]
  
  # Trips into the tract
  transfer.into = dt.dropoffs[,list(trips = .N) , by = list(pickup_tract_id, dropoff_weekday, dropoff_hour, dropoff_quarter_hour)]
  
  
  # Trips in, trips out, by tract_id
  into = dt.dropoffs[,list(trips.in = .N) , by = list(dropoff_year, dropoff_month, dropoff_day, dropoff_weekday, dropoff_hour, dropoff_quarter_hour)]
  out  = dt.pickups[,list(trips.out = .N) , by = list(pickup_year, pickup_month, pickup_day, pickup_weekday, pickup_hour, pickup_quarter_hour)]
  
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
  
  dir.name = paste0(net.dirs, tract)
  dir.create(file.path(dir.name), showWarnings = FALSE)
  
  write.csv(row.names = F, x = net, paste0(dir.name, "/net.csv"))
  write.csv(row.names = F, x = transfer.into, paste0(dir.name, "/into.csv"))
  write.csv(row.names = F, x = transfer.out, paste0(dir.name, "/out.csv"))
  file_size = round(sum(file.size(list.files(dir.name, full.names = T)))/10^6, 1)
  output_msg = paste0("Processed tract ", tract, ", wrote ", file_size, "MB")  
  print(output_msg) 
}

#
# Concatenate to one file each
#
tract_codes = fread("tract_codes.csv") # To slice by neighborhood or borrough

  <- list()
L.into <- list()
L.out  <- list()

readNet <- function(filename){
  library(stringr)
  # filename = "C:\blah\bleep1\boop321\\0013\net.csv" # Double slash nec to escape
  dt <- fread(filename)
  print(paste0("Read in ", filename))
  str_tract <- str_extract(string = filename, "[0-9]{4}")
  if(!is.na(str_tract)){
    tract = as.integer(str_tract)
  } else {
    print(paste0("Error on ", filename))
    return(NULL)
  }
  dt[, tract_id := tract]
  return(dt)
}

net.dirs = "F:/nyc_yellow_tract/net/"

L.net = lapply(setdiff(pickup.dirs, "0000"), function(tract){
  file.name = paste0(net.dirs, tract,"/net.csv")
  readNet(file.name)
})

dt.net = rbindlist(L.net)

write.csv(row.names = F, x = dt.net, paste0(net.dirs,"net.trips.csv"))
