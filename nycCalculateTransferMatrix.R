# NYC Taxi Project
#
#  Processing tract identified trips to transfer matricies and net trips
# 

library(data.table)
library(ggplot2)

project.root.dir = "C:/Users/mm/Dropbox/Incubator/nyctaxi/"

# Flat files by tract
pickup.dir   = "F:/nyc_yellow_tract/pickup/"
dropoff.dir  = "F:/nyc_yellow_tract/dropoff/"
pickup.dirs  = list.files(pickup.dir)
dropoff.dirs = list.files(pickup.dir)

# Necessary parity check: is there a pickup tract dir for every dropoff?
identical(pickup.dirs, dropoff.dirs)

net.dirs = "F:/nyc_yellow_tract/net//"

setwd(project.root.dir)
source( "nycParseRaw.R")

# Weather data, for later
# wunder <- fread(paste0(project.root.dir, "wunder/wunder.csv"))

# Tract code files, neighborhood, borrough etc.
tract_codes = fread("tract_codes.csv")

setnames(tract_codes, "id", "tract_id")
tract_codes[,tract_id := as.integer(tract_id)]
setkey(tract_codes, tract_id)

L = list()

for(tract in setdiff(pickup.dirs, "0000")){
  # Roughly 2GB object
  
  # tract = "0029" # Testing
  dt.pickups  <- rbindlist(lapply(list.files(paste0(pickup.dir,  tract), full.names = T), fread))
  
  cleanV1(dt.pickups)
  appendTimeVariablesCsv(dt.pickups)
  
  L[[tract]] <- dt.pickups[,.N, by=list(dropoff_tract_id, pickup_weekday, pickup_hour, pickup_quarter_hour)]
}

lapply(names(L), function(x){dt = L[[x]][,pickup_tract := as.integer(x)]})
out <- rbindlist(L)

write.csv(out, "F:/nyc_yellow_tract/tracts_transfer_matrix.csv", row.names = F)

# Idea:
# If no observations exist for a destination/time, out lacks the row
# This means the matrix needs filling with zeros for the cross product of the unique identifiers
# But the space requirements are large. 2k * 2k * 7 * 96 ~ billions of rows
# => Collapse first on weekday => Then expand only on file load => hundreds of millions of rows

