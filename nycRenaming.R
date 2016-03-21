# nycRenaming.R
#
# generate name mappings to regularize raw data names
# 

library(data.table)

files = list.files(full.names = T)

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

rm(dts, dts.names, uni.names, uni)


# Cleanup

# Check
# lapply(dts, rename, all.names = all.names) # Evaluated for side effects, results "null"
# lapply(dts, names)
# Check it, good. 
# Names are now consistent
