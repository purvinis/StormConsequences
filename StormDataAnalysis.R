library(dplyr)
library(tidyr)
library(ggplot2)
library(R.utils)

print(Sys.info())
print(Sys.info()['sysname'])
print(Sys.info()['machine'])
print(Sys.info()['release'])
print(Sys.info()['version'])
print(R.Version()['version.string'])  # "R version 4.0.2 (2020-06-22)"

t1 <- Sys.time()
bunzip2("repdata_data_StormData.csv.bz2","StormData.csv", remove = FALSE, skip = TRUE)
t2 <- Sys.time()
print(t2-t1)  #Time difference of 19.25788 secs

#Load raw data into R
t3 <- Sys.time()
StormData <- read.csv("StormData.csv",sep = ",")
t4 <- Sys.time()
print(t4-t3)   #Time difference of 20.29467 secs

## PROCESSING
# info about the original variables can be found here: 
# https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Export-Format.pdf
# The data we are given is a subset, with renamed and buggered up variables?
# Which event types (EVTYPE) are most harmful wrt population health?
# Which has greatest economic consequences?

print(str(StormData, give.length = TRUE))
# Missing data often shown as "" rather than NAs. 
# There are 985 unique events listed, but the documentation lists 48 Storm Events
print(unique(StormData$EVTYPE))  # returns list of 985 events

sd1 <-na_if(StormData,"")   #this is slow. Not sure I had to yet
plot(StormData$EVTYPE,StormData$FATALITIES,)
