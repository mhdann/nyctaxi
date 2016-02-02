# Michael Dann
# mhdann@gmail.com
#
# Basic graphs of the taxi data
#
#
setwd("D:/data/taxi/nyctaxi")

library(ggplot2)
library(RSQLite)
library(data.table)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "./nyc_DB/nyc.db")
dbListTables(con)
t2015 = data.table(dbReadTable(con, "nyc_yellow"))
dbDisconnect(con)
setnames(park.binned, "Month", "month")