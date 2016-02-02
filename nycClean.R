# Michael Dann
# mhdann@gmail.com
#
# NYC Taxi Commission database exploratory analysis and local database setup
#
#

library(data.table)
library(ggplot2)
library(lubridate)

setwd("D:/data/taxi/nyctaxi/")

###############################################################################
# Download the RAW data
###############################################################################

urls = fread("urlsYellow.csv")[,x]

urls2015 = urls[grep("2015", urls)]

library(RCurl)
bdown=function(url, file){
  # https://stackoverflow.com/questions/14426359/downloading-large-files-with-r-rcurl-efficiently
  library('RCurl')
  f = CFILE(file, mode="wb")
  a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
  close(f)
  return(a)
}

fnames = paste0("./nyc_yellow_raw/", 
                sapply(strsplit(urls2015, "/"), function(x){x[6]}))

# Warning! 20 minutes @ 200 mbps
mapply(bdown, urls2015, fnames)  

###############################################################################
# Load from files locally

files = list.files("nyc_yellow/", full.names = T)
f2015 <- files[grep("2015", files)]

###############################################################################
# Cleaning, indexing
###############################################################################

removeVars <- function(dt, vars = c("VendorID", "store_and_fwd_flag", "extra", "mta_tax", "tip_amount", "tolls_amount", "improvement_surcharge")){
  # Removes variables not currently under analysis
  dt[, (vars) := NULL] # () added so that it gets seen as an expression
  print(paste0("Removed: ", paste(vars, collapse=", ")))
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
  print("Added 'clean' field to indicate valid data")
}

appendDateTime <- function(dt){
  # Append POSIX time variables, remove raw
  # Side-effects herein, reader beware
  dt[,pickup_datetime:= as.POSIXct(tpep_pickup_datetime, format="%Y-%m-%d %H:%M:%S")]
  dt[,dropoff_datetime:= as.POSIXct(tpep_dropoff_datetime, format="%Y-%m-%d %H:%M:%S")]
  dt[,tpep_pickup_datetime:= NULL]
  dt[,tpep_dropoff_datetime:= NULL]  
  
}

appendDate <- function(dt){
  # Append Date using POSIX time field
  dt[,pickup_date := as.Date(pickup_datetime)]
  dt[,dropoff_date := as.Date(dropoff_datetime)]
}

appendMonth <- function(dt){
  # Append Date using POSIX time field
  dt[,pickup_month := month(pickup_datetime)]
  dt[,dropoff_month := month(dropoff_datetime)]
  print("Added month")
}

appendDay <- function(dt){
  # Append Date using POSIX time field
  dt[,pickup_day := mday(pickup_datetime)]
  dt[,dropoff_day := mday(dropoff_datetime)]
  print("Added day")
}

appendHour <- function(dt){
  # Append Date using POSIX time field
  dt[,pickup_hour  := hour(pickup_datetime)]
  dt[,dropoff_hour := hour(dropoff_datetime)]
  print("Added hour")
}

appendMin <- function(dt){
  # Append Date using POSIX time field
  dt[,pickup_min  := minute(pickup_datetime)]
  dt[,dropoff_min := minute(dropoff_datetime)]
  print("Added min")
}

cleanAll <- function(dt, ...){
  removeVars(dt, ...)
  clean(dt)
  appendDateTime(dt)
  appendDay(dt)
  appendHour(dt)
}

###############################################################################  

# reading
taxis2015 = lapply(f2015, fread)

# Screening
lapply(taxis2015, cleanAll)

# Binding (RAM WARNING > 20 GB!)
t2015 = rbindlist(taxis2015)
rm(taxis2015); gc()


###############################################################################  
# Values of Interest
###############################################################################  

# A cabbie or uber dispatch may be interested in the density of high-value
# fares. One way to examine this is to calculate the fare/trip_time, and
# graph the resulting density map.

# Calculate fare_rev_per_hour = fare_amount/trip_time
t2015[clean == T, trip_time := as.numeric(difftime(dropoff_datetime, pickup_datetime, units = "hour"))]
t2015[clean == T, fare_rev_per_hour := fare_amount/trip_time]

# Day of the week
t2015[, Weekday := factor(weekdays(pickup_datetime), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))]

# month
t2015[, Month := month(pickup_datetime)]


###############################################################################  
# First - Plotting
###############################################################################  

# Ballpark the fare-density
plot(density(t2015[clean == T & trip_time > 1/60 & fare_rev_per_hour < 1000 & Weekday == "Monday" & pickup_hour == 6, fare_rev_per_hour]))

# Looks log-normal
# Looks like an upper cutoff around 200-250
# looks like a lower cutoff around 25-30

# Clean more
t2015[clean == T & !(trip_time > 1/60 & fare_rev_per_hour < 1000), clean := F]

###############################################################################
# Save to DB
library(RSQLite)
drv <- dbDriver("SQLite")
for(m in 1:6){
  # Chunked because SQLite appears to be crashing
  con <- dbConnect(drv, dbname = "./nyc_DB/nyc.db")
  dbWriteTable(con, "nyc_yellow", t2015[Month==m,],append = T)
  dbDisconnect(con)  
}

# Test
con <- dbConnect(drv, dbname = "./nyc_DB/nyc.db")
test = data.table(dbReadTable(con, "nyc_yellow"))
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
