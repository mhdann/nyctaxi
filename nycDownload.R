# Mhdann@gmail.com
# see bottom for note on cleaning the raw files


library(data.table)
library(RCurl)

# Create your own root
setwd("D:/data/taxi/nyctaxi/")

###############################################################################
# Download the RAW data
###############################################################################

dir.create(file.path(".", "nyc_yellow_raw/"), showWarnings = FALSE)

urls = fread("urlsYellow.csv", sep= ",", header = F)$V1

# urls2015 = urls[grep("2015", urls)]
# urlsOther = urls[-c(grep("2015", urls), grep("2014", urls))]

bdown=function(url, file){
  # https://stackoverflow.com/questions/14426359/downloading-large-files-with-r-rcurl-efficiently
  library('RCurl')
  f = CFILE(file, mode="wb")
  a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
  close(f)
  return(a)
}

#fnames = paste0("./nyc_yellow_raw/", 
#                sapply(strsplit(urls2015, "/"), function(x){x[6]}))

fnames = paste0("./nyc_yellow_raw/", 
                sapply(strsplit(urls, "/"), function(x){x[6]}))


# Warning! 20 minutes @ 200 mbps for 6 months of 2015,
# hours for whole set
mapply(bdown, urls, fnames)


###############################################################################
# Run low level cleaning next.

