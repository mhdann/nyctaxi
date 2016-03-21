#
#  Cleaning functions for raw data
# 

# Notes: should exclude anything longer than 2 hrs

library(data.table)
library(fasttime)
library(lubridate)

cleanV1 <- function(dt){
  if("V1" %in% names(dt)){dt[, V1 := NULL]}  
}


removeVars <- function(dt, vars = c("vendor_id", "store_and_fwd_flag", "surcharge", "extra", "mta_tax", "tip_amount", "tolls_amount", "improvement_surcharge")){
  # Removes variables not currently under analysis
  dt[, (vars) := NULL] # () added so that it gets seen as an expression
  print(paste0("Removed: ", paste(vars, collapse=", ")))
}

appendTimeVariables <- function(dt, singleStamp = F, ym = F){
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
  dt[, trip_time := as.numeric(difftime(d_datetime, p_datetime, units = "hour"))]
  dt[, fare_rev_per_hour := fare_amount/trip_time]
  print("Calculated fare_rev_per_hour")
  # Day of the week
  dt[, weekday := wday(p_datetime)]
  print("Added weekday")
  
  if(ym == T){
    dt[, pickup_year  := year(p_datetime)]
    dt[, pickup_month  := month(p_datetime)]  
    print("Appended pickup_year, pickup_month")
  }
  
  
  if(!singleStamp){
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
    dt[, (c("pickup_datetime", "dropoff_datetime")) := NULL]
    print("Removed string datetime")
  }
}

appendTimeVariablesSimple <- function(dt){
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
  dt[, trip_time := as.numeric(difftime(d_datetime, p_datetime, units = "hour"))]
  dt[, fare_rev_per_hour := fare_amount/trip_time]
  print("Calculated fare_rev_per_hour")

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


appendTimeVariablesCsv <- function(dt, singleStamp = F, ym = F, rm_datetime = T){
  # Replaces previous functions, fast
  library(lubridate)
  library(fasttime)
  p_datetime  = fastPOSIXct(dt$pickup_datetime, tz = "GMT")
  d_datetime = fastPOSIXct(dt$dropoff_datetime, tz = "GMT")
  # hackish tz treatment but verifiably working.
  # I never save the p/d_datetime, they are intermediary
  print("Processed datetime from string")
  # Day of the week  
  if(!singleStamp){
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
    dt[, pickup_weekday := wday(p_datetime)]
    dt[, dropoff_weekday := wday(d_datetime)]
    print("Added weekday")
    dt[,pickup_hour  := hour(p_datetime)]
    dt[,dropoff_hour := hour(d_datetime)]
    print("Added hour")
    dt[,pickup_min  := minute(p_datetime)]
    dt[,dropoff_min := minute(d_datetime)]
    print("Added min")
    dt[,pickup_quarter_hour  := pickup_min  %/% 15]
    dt[,dropoff_quarter_hour := dropoff_min %/% 15]
    print("Added quarter_hour")
    if (rm_datetime){
      dt[, (c("pickup_datetime", "dropoff_datetime")) := NULL]
      print("Removed string datetime")  
    }
  }
}



# Testing
# blah = fastPOSIXct(head(dt200901$pickup_datetime), tz = "GMT")
# df = data.frame(pickup_datetime = head(dt200901$pickup_datetime), blah)
# df$blah

# Testing
# df = data.frame(head(dt200901$pickup_datetime, 100000), as.POSIXct(head(dt200901$pickup_datetime, 100000),  format="%Y-%m-%d %H:%M:%S"))
# names(df) <- c("String", "Time")
# df$hour = hour(df$Time)