#!/bin/bash
# Low-level cleaning, execute in cygwin or *nix
#
# Data contains extra line after the first. Remove: 
# TEST head yellow_tripdata_2014-01.csv  | sed '/^\s*$/d'
# TEST sed '/^\s*$/d' yellow_tripdata_2014-01.csv > test.csv
sed -i.bak '/^\s*$/d' ./nyc_yellow_raw*.*


# See also extra field in yellow_tripdata_2010-02.csv
mv ./nyc_yellow_raw/yellow_tripdata_2010-02.csv ./nyc_yellow_raw/yellow_tripdata_2010-02.csv.ef.bak
mv ./nyc_yellow_raw/yellow_tripdata_2010-03.csv ./nyc_yellow_raw/yellow_tripdata_2010-03.csv.ef.bak
mv ./nyc_yellow_raw/yellow_tripdata_2010-04.csv ./nyc_yellow_raw/yellow_tripdata_2010-04.csv.ef.bak

awk -F',' 'NF==18' ./nyc_yellow_raw/yellow_tripdata_2010-02.csv.ef.bak > ./nyc_yellow_raw/yellow_tripdata_2010-02.csv
awk -F',' 'NF==18' ./nyc_yellow_raw/yellow_tripdata_2010-03.csv.ef.bak > ./nyc_yellow_raw/yellow_tripdata_2010-03.csv
awk -F',' 'NF==18' ./nyc_yellow_raw/yellow_tripdata_2010-04.csv.ef.bak > ./nyc_yellow_raw/yellow_tripdata_2010-04.csv
