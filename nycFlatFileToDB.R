# nycFlatFileToDB.R
#
# To take the CSV flat file mapReduced() data and put them in an sqlite DB
#

library(RSQLite)
library(data.table)

project.root.dir = "C:/Users/mm/Dropbox/Incubator/"
sql.dir          = "F:/nyc_yellow_tract/"
sql.dir          = "C:/Users/mhdan_000/Desktop/"
net.dirs         = "F:/nyc_yellow_tract/net/"


setwd(net.dirs)
net <- fread("net.trips.csv")

runsql <- function(sql, dbname="mydatabase.db"){
  # http://conjugateprior.org/2013/01/querying-an-sqlite-database-from-r/
  library(RSQLite)
  driver <- dbDriver("SQLite")
  connect <- dbConnect(driver, dbname=dbname);
  closeup <- function(){
    sqliteCloseConnection(connect)
    sqliteCloseDriver(driver)
  }
  dd <- tryCatch(dbGetQuery(connect, sql), finally=closeup)
  return(dd)
}

# Test read
res <- runsql("SELECT * FROM taxi_net WHERE weekday = 1;", "taxi.sqlite")


# Open DB
setwd(sql.dir)
drv <- dbDriver("SQLite")
con = dbConnect(drv = drv, dbname = "taxi.sqlite")
# dbDisconnect(con)

# Write DB
dbSendQuery(con,
            "CREATE TABLE taxi_net (
            tract_id INTEGER,
            year INTEGER,
            month INTEGER,
            day INTEGER,
            weekday INTEGER,
            hour INTEGER,
            quarter_hour INTEGER,
            trips_out INTEGER,
            trips_in INTEGER,
            net_trips_in INTEGER,
            CONSTRAINT pk_taxi PRIMARY KEY (tract_id, year, month, day, weekday, hour, quarter_hour)
);")

# Name conformity
names(net)
setnames(net, c("trips.out", "trips.in", "net.trips.in"), 
              c("trips_out", "trips_in", "net_trips_in"))

setcolorder(net, c("tract_id", "year", "month", "day", "weekday", "hour", "quarter_hour", "trips_out", "trips_in", "net_trips_in"))

# Approximately 2 minutes
dbWriteTable(conn = con, name = "taxi_net", net, append = T)
dbDisconnect(con)

net_test <- data.table(dbReadTable(con, "taxi_net"))


