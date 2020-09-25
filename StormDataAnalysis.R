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
#png('plot1.png',width=480,height=480,units="px",bg = "transparent")
print(p2) #make prettier, color the ranges
#dev.off()

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


#===========================================================================
print(quantile(sd1$INJURIES))

human <-as.data.frame(cbind(mdy_hms(sd1$BGN_DATE),sd1$EVTYPE,sd1$FATALITIES,sd1$INJURIES))
length(unique(sd1$BGN_DATE))  #16225 unique days

plot(human$V1,human$V3)
# Want the event type to fit in one of the 48 official categories
sd1$EVTYPE[agrep("TORNAD | GUSTNA | FUNNEL | SPOUT",
                 sd1$EVTYPE,ignore.case = TRUE)] <- "Tornado"
sd1$EVTYPE[agrep("HAIL",sd1$EVTYPE,ignore.case = TRUE)] <- "Hail"
sd1$EVTYPE[agrep("SNOW | ICE | WINTER | WINTRY | COLD | FROST |
                 SLEET | FREEZING ",sd1$EVTYPE,ignore.case = TRUE)] <- "Winter"
sd1$EVTYPE[agrep("THUNDERSTORM | TSTM | TURBU | BURST ",
                 sd1$EVTYPE,ignore.case = TRUE)] <- "Thunderstorm"
sd1$EVTYPE[agrep("HURRICANE | TROPICAL",sd1$EVTYPE,ignore.case = TRUE)] <- "Hurricane"
sd1$EVTYPE[agrep("FLOOD | FLD",sd1$EVTYPE,ignore.case = TRUE)] <- "Flood"
sd1$EVTYPE[agrep("DROUGHT | DRY",sd1$EVTYPE,ignore.case = TRUE)] <- "Drought"
sd1$EVTYPE[agrep("HEAT | HOT | WARM | HIGH TEMP",sd1$EVTYPE,ignore.case = TRUE)] <- "Heat"
sd1$EVTYPE[agrep("DUST",sd1$EVTYPE,ignore.case = TRUE)] <- "Dust"
sd1$EVTYPE[agrep("RAIN | PRECIP | SUMM | Summ",sd1$EVTYPE,ignore.case = TRUE)] <- "Rain"
sd1$EVTYPE[agrep("SUMMARY | OTHER | ? ",sd1$EVTYPE,ignore.case = TRUE)] <- "Other"
sd1$EVTYPE[agrep("FIRE | SMOKE",sd1$EVTYPE,ignore.case = TRUE)] <- "Fire"
sd1$EVTYPE[agrep("MUD",sd1$EVTYPE,ignore.case = TRUE)] <- "Mud"
sd1$EVTYPE[agrep("VOLCAN",sd1$EVTYPE,ignore.case = TRUE)] <- "Volcanic"
