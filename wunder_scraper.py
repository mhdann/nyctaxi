# -*- coding: utf-8 -*-
"""
@Project: Taxi demand modeling, Dead simple weather underground scraper
@author: Michael Dann

Recent notes: 
    Missing: 2012/01/29 and 2012/01/30
    First run: succeeds 2015
    Second run: 2009-2012 failing on 2012/01/29 
    Third run: 2014-2012 backwards, fails on 1/30        
"""

from bs4 import BeautifulSoup
import csv
import urllib2
import os
import time
import re

# Project path
path = "D:\\data\\taxi\\nyctaxi"
os.chdir(path)
os.listdir("./")




def readRow(row):
    # Column notes, 1 time, 2 temp, 9 wind speed,11 precip
    cells = row.find_all("td")
    timeTxt = cells[0].get_text()
    temp = re.sub(r"[^0-9\.]+", '', cells[1].get_text())
    windspeed = re.sub(r"[^0-9\.]+", '', cells[8].get_text())
    precip = re.sub(r"[^0-9\.]+", '', cells[10].get_text())
    return([timeTxt, temp, precip, windspeed])



# See 'wunderUrls.csv" for 2015
# And wunderUrls2.csv" for 2009-2014
# And wunderUrls3.csv" for 2012-01-28 through 2014

urls = []
with open('wunderUrls3.csv', 'rb') as f:
    reader = csv.reader(f)
    for row in reader:
        urls.append(row[0])
  
for url in urls[::-1]:
    print(url)
    
    # Flood prevention
    time.sleep(0.2)    
    
    # 1. Open a url
    # 2. Identify the correct table
    # 3. Read from table
    # 4. Write to CSV

   # File name parser
    part = url[49:60]
    parts = part.split("/")
    year = int(parts[0])
    month = int(parts[1])
    day = int(parts[2])
    date = [year, month, day]
    strdate = format(year,'4') + "-" + format(month, '02') + "-" + format(day, '02')
    fname = 'wunder_' + format(year,'4') + format(month, '02') + format(day, '02') + '.csv'
        
    # Open
    response = urllib2.urlopen(url)
    html = response.read()
    soup = BeautifulSoup(html)
    
    # Locate
    table = soup.find('table', id="obsTable")
    body = table.find('tbody')
    rows = body.find_all('tr')
    
    # Read in
    allrows = []
    for row in rows:
        # Idea: iterate over rows, grab appropriate columns    
        outputRow = readRow(row)
        outputRow[0] = strdate + " " + outputRow[0]
        allrows.append(date + outputRow)

    
    
    # Save to file    
    with open('./wunder_raw/' + fname, 'wb') as f:
        writer = csv.writer(f)
        writer.writerow(["year", "month", "day", "time", "temperature", "precipitation", "windspeed"])
        writer.writerows(allrows)