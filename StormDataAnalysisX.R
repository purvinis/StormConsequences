install.packages("R.utils")

library(dplyr)
library(tidyr)
library(ggplot2)
library(R.utils)
library(lubridate)
library(lattice)

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
print(t4-t3)   #Time difference of 20.29467 secs -24.37243 secs

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

sd1 <-na_if(StormData[,],"")   #this is slow. Not sure I had to yet
sd1$BGN_DATE <- date(mdy_hms(sd1$BGN_DATE))
print(length(unique(sd1$EVTYPE)))
print(str(sd1))

# FATALITIES

p1 <- plot.default(factor(sd1$EVTYPE),y = sd1$FATALITIES,ylim = c(0,200))
png('plot1.png',width=480,height=480,units="px",bg = "transparent")
print(p1) #make prettier, color the ranges
dev.off()

# list the event types corresponding to fatalities between 15, 105
Fatalpk1  <- filter(sd1,FATALITIES >15 & FATALITIES < 105)  # 87 observations
Fatalpk2 <- filter(sd1,FATALITIES >105 & FATALITIES < 500)   # 3 tornado observations
# Make sure all versions of Tornado are counted
Fatalpk1$EVTYPE[agrep("TORNADO",Fatalpk1$EVTYPE,ignore.case = TRUE)] <- "Tornado"
Fatalpk1Tornado <-Fatalpk1 %>%
                       filter(EVTYPE == "Tornado") %>%
                       select(EVTYPE) 
print(summary(factor(Fatalpk1$EVTYPE)))

maxFatalIndex  <- which.max(sd1$FATALITIES)
eventMaxFatal <- StormData$EVTYPE[maxFatalIndex]   # "heat" or "HEAT
dateMaxFatal <- mdy_hms(StormData$BGN_DATE)[maxFatalIndex]  #1995-07-12
remarksMaxFatal <- StormData$REMARKS[maxFatalIndex] "...july 12 - 16"
# Data seems to indicate heat or tornado related events both cause fatalities.
# Find totals from tornados and heat (grep heat)
# REPORT ON THIS
# --------------------------------------------------------------------------
#--------------------------------------------------------------------------
# Repeat process for injuries
p2 <- plot.default(factor(sd1$EVTYPE),y = sd1$INJURIES)
png('plot2.png',width=480,height=480,units="px",bg = "transparent")
print(p2) #make prettier, color the ranges
dev.off()

sdinj <- sd1 %>% select(c(INJURIES, EVTYPE,REFNUM)) %>% filter(INJURIES > 50)
print(summary (sdinj))   #421 observations

hist(sdinj$REFNUM,breaks=48)
print(sdinj$EVTYPE[1:100])  #99 Tornadoes and 1 TSTM WIND
print(sdinj$EVTYPE[101:200]) #100 Tornados
print(sdinj$EVTYPE[201:300])  #mixed: w tornadoes, heat, flood, ...
print(sdinj$EVTYPE[301:421])  #mixed: "...

print (sdinj %>% count(EVTYPE, sort = TRUE))  #305 tornadoes, 38 excessive heat, etc
# Combine: heat (extreme, excessive)

maxInjIndex  <- which.max(sd1$INJURIES)
eventMaxInj <- StormData$EVTYPE[maxInjIndex]   # "Tornado
dateMaxInj <- mdy_hms(StormData$BGN_DATE)[maxInjIndex]  #1979-04-10
#remarksMaxInj <- StormData$REMARKS[maxInjIndex]

#sd1$EVTYPE[agrep("TORNAD | GUSTNA | FUNNEL ",
#sd1$EVTYPE,ignore.case = TRUE)] <- "Tornado"  # did not change result
#===========================================================================
# So likely to be tornadoes or heat.
# Clean up data set and combine similar event types

sd2 <- sd1 %>% select(c(FATALITIES,INJURIES,EVTYPE,REFNUM,BGN_DATE))

# Want the event type to fit in one of the 48 official categories
sd2$EVTYPE[agrep("torn | funnel",
                 sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Tornado"
sd2$EVTYPE[grep("HAIL",sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Hail"


sd2$EVTYPE[agrep("THUNDER | TSTM | TURBU | BURST ",
                 sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Thunderstorm"
sd2$EVTYPE[agrep("HURRICANE | TROPICAL",sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Hurricane"
sd2$EVTYPE[grep("FLOOD | FLD",sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Flood"
sd2$EVTYPE[agrep("DROUGHT | DRY",sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Drought"
sd2$EVTYPE[grep("HEAT | HOT | WARM | HIGH TEMP",sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Heat"
sd2$EVTYPE[grep("DUST",sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Dust"
sd2$EVTYPE[grep("RAIN | PRECIP | SUMM | Summ",sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Rain"
sd2$EVTYPE[grep("SUMMARY | OTHER | '?' ",sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "tbd"
sd2$EVTYPE[grep("FIRE | SMOKE",sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Fire"
sd2$EVTYPE[grep("MUD",sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Mud"
sd2$EVTYPE[grep("VOLCAN | VOG ",sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Volcanic"

sd2$EVTYPE[grep("SNOW | ICE | WINT | COLD | FROST |
                 SLEET | FREEZ ",sd2$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Winter"

# https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html
print(unique(sd2$EVTYPE))
