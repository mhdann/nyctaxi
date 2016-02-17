# NYC Taxi Data Analysis
***
This is a process for acquiring and analyzing the NYC Taxi & Limousine Commission data on cab trips between 2009 and mid 2015. Trip pickup and dropoff locations are paired to census tract and processed to flat files to tract to support a variety of calculations.

Graphics and intermediary objects are generated for a basic-web R-Shiny web app (coming.)

####1. Download the raw files for the Yellow Cab trips

`./nycDownload.R`

####2. Clean the raw files of mislaid columns and characters.

`./nycRawClean.sh`

####3. Pair the raw files census tract number and save to csv flat files by pickup/dropoff tract directories.

`./nycPipelineCsv.R`

####4. Generate figures and regressions by tract

`./nycAnalysis.R`

###To Do List

1. Simple Web App
    + R-Shiny transfer matrix browser
        + Tract level maps, shading/intensity to indicate cost or probability of going to destination tract
        + Demonstrate effect of weather on net rides (coefficients or conditional means)
          + Binned coefficients
    + Demonstrate conditional means of regular temporal patterns
    + Time-series plot on various margins
    + Demonstrate any gross trends at low frequency

2. Deferred
    * Cleanup data processing
        * Remove row.names from pickup tract csvs
        * Remove redundant time columns
    * Data integrity
        * 10% of the 2010-09 yellow taxi data appears to contain duplicates on lat/lon, time, but not passenger details.
        * A large portion of 2010 contains apparent duplicates
3. Data Scope
    * Add covariates
        * "events" (snow etc) from wunder
        * Add Green cab data

