install.packages("R.utils")
install.packages ("cowplot")
library(cowplot)
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
print(t2-t1)  #Time difference of 19.25788 secs on my machine

#Load raw data into R
t3 <- Sys.time()
StormData <- read.csv("StormData.csv",sep = ",")
t4 <- Sys.time()
print(t4-t3)   #Time difference of 24.37243 secs on my machine

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

sd1 <-na_if(StormData[,],"")   #this is slow.
sd1$BGN_DATE <- date(mdy_hms(sd1$BGN_DATE))
print(length(unique(sd1$EVTYPE)))
print(str(sd1))

# FATALITIES
png('plot1.png',width=480,height=480,units="px",bg = "transparent")
p1 <- plot.default(factor(sd1$EVTYPE),y = sd1$FATALITIES,ylim = c(0,200))
print(p1) #
dev.off()

# Repeat process for injuries
png('plot2.png',width=480,height=480,units="px",bg = "transparent")
p2 <- plot.default(factor(sd1$EVTYPE),y = sd1$INJURIES)
print(p2) #make prettier, color the ranges
dev.off()

#FATALITIES AND INJURIES plotted Together
p3 <-ggplot(data =sd1,
            aes(EVTYPE))+
  ylab("Fatalities and Injuries")+
  xlab("985 event types") +
  geom_point(aes(y = INJURIES, colour = 'Injuries'))+
  geom_point(aes(y = FATALITIES, colour = 'Fatalities'))+
  theme(axis.text.x = element_blank())+
  labs(title = "Raw data scatter plot")
png('plot3.png',width=720,height=360,units="px",bg = "transparent")
print(p3)
dev.off()

sd3 <- sd1
#These matching criteria were iteratively determined
sd3$EVTYPE[grep("TORN|SPOUT|WHIRL|GUSTNA|FUNNE|ROTAT",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Tornado"
sd3$EVTYPE[grep("HAIL",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Hail"
sd3$EVTYPE[grep("HEAT|HOT|WARM|HIGH TEMP",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Heat"
sd3$EVTYPE[grep("WINT|BLIZZARD|SNOW|COLD|SLEET|FREEZ",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "snow"
sd3$EVTYPE[grep("ICE|MIX|GLAZE|ICY",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Ice"
sd3$EVTYPE[grep("THUNDER|TSTM|LIG|BURST|TURBU",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Tstorm"
sd3$EVTYPE[grep("HURRIC|TROPICAL|TYPHO",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Hurricane "
sd3$EVTYPE[grep("FLOOD|FLD|DAM BR",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Flood "
sd3$EVTYPE[grep("SUMMARY|OTHER|[:?:]|NONE",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "tbd"
sd3$EVTYPE[grep("FIRE|SMOKE",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Fire"
sd3$EVTYPE[grep("MUD|LANDS|ROCK",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Mud"
sd3$EVTYPE[grep("VOLCAN|VOG",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Volcanic"
sd3$EVTYPE[grep("GLAZE|BLACK ICE",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Black_Ice"
sd3$EVTYPE[grep("DROUGHT|DRY|DRIE",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Drought"
sd3$EVTYPE[grep("WIND|WND",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Winds"  #includes some TSTM
sd3$EVTYPE[grep("FROST|COOL",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Cool"  #includes frost
sd3$EVTYPE[grep("SURF|SEA|TIDE|CURRENT|MARINE|TSUN|BEACH|WAVE|SWELL|SURGE|COASTAL",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Marine"
sd3$EVTYPE[grep("RAIN|WET|PRECIP|SHOWER",sd3$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Rain"

#add consolidated events to sd1 for nice facet or dual plot
sd4 <-data.frame(cbind(FATALITIES=sd1$FATALITIES,
            INJURIES= sd1$INJURIES,
            EVTYPE=sd1$EVTYPE,EVTYPEsub = sd3$EVTYPE))

print(unique(sd4$EVTYPEsub))  #66 event categories

#Try to get all the pre and post data reduction together
p5 <-ggplot(data =sd4,
            aes(EVTYPEsub,INJURIES))+
  ylab("Fatalities and Injuries")+
  xlab("66 event catagories") +
  geom_point(aes(y = as.numeric(INJURIES), colour = 'Injuries'))+
  geom_point(aes(y = as.numeric(FATALITIES), colour = 'Fatalities'))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Weather events consolidated")
png('plot5.png',width=1080,height=480,units="px",bg = "transparent")
print(p5) #make prettier, color the ranges
dev.off()

p35 <-cowplot::plot_grid(p3,p5,nrow = 2, ncol = 1)
png('plot35.png',width=1080,height=480,units="px",bg = "transparent")
print(p35) #make prettier, color the ranges
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

# filter data to find events with high injuries (corresponds to peaks in plots)
sdinj <- sd1 %>% select(c(INJURIES, EVTYPE,REFNUM)) %>% filter(INJURIES > 50)
sdfat <- sd1 %>% select(c(FATALITIES, EVTYPE,REFNUM)) %>% filter(FATALITIES > 15)
print(summary (sdinj))   #421 observations
print(summary (sdfat))  #91 observations

hist(sdinj$REFNUM,breaks=48)
print(sdinj$EVTYPE[1:100])  #99 Tornadoes and 1 TSTM WIND
print(sdinj$EVTYPE[101:200]) #100 Tornados
print(sdinj$EVTYPE[201:300])  #mixed: w tornadoes, heat, flood, ...
print(sdinj$EVTYPE[301:421])  #mixed: "...

print (sdinj %>% count(EVTYPE, sort = TRUE))  #305 tornadoes, 38 excessive heat, etc
print (sdfat %>% count(EVTYPE, sort = TRUE)) # 55 tornadoes, with high heat events 2nd
# Do same with consolidated data:
sdinjc <- sd3 %>% select(c(INJURIES, EVTYPE,REFNUM)) %>% filter(INJURIES > 50)
sdfatc <- sd3 %>% select(c(FATALITIES, EVTYPE,REFNUM)) %>% filter(FATALITIES > 15)

print (sdinjc %>% count(EVTYPE, sort = TRUE))  #1 - tornado, 2- heat 
print (sdfatc %>% count(EVTYPE, sort = TRUE))

#Find max of single events:
maxInjIndex  <- which.max(sd1$INJURIES)
eventMaxInj <- sd1$EVTYPE[maxInjIndex]   # "Tornado
dateMaxInj <- mdy_hms(StormData$BGN_DATE)[maxInjIndex]  #1979-04-10
#remarksMaxInj <- StormData$REMARKS[maxInjIndex]

maxFatIndex  <- which.max(sd1$FATALITIES)
eventMaxFat <- sd1$EVTYPE[maxFatIndex]   # "Heat
dateMaxFat <- mdy_hms(StormData$BGN_DATE)[maxFatIndex]  #1995-07-12
#remarksMaxInj <- StormData$REMARKS[maxInjIndex]

#Quantify by summing top 1000 events with most injuries or fatalities.
#This determines event type with overall worst injury and fatality record
#===========================================================================
# So likely to be tornadoes or heat.
# Clean up data set and combine similar event types

sd2 <- sd1 %>% select(c(FATALITIES,INJURIES,EVTYPE,REFNUM,BGN_DATE))
#Look at top 1000 injury and fatality count events
injOrderInd <-order(sd2$INJURIES,decreasing = TRUE)  # gets index of max inj and sorts
injOrderFat <-order(sd2$FATALITIES,decreasing = TRUE)
top1000Injdf <- sd2[injOrderInd[1:1000],]
top1000Fatdf <- sd2[injOrderFat[1:1000],]
#print(tail(cbind(top1000Injdf$INJURIES,top1000Injdf$EVTYPE),n  = 20))
injUniq <- unique(top1000Injdf$EVTYPE) #45 evtypes returned
fatUniq <- unique(top1000Fatdf$EVTYPE) #71

qplot(factor(EVTYPE),INJURIES,data =top1000Injdf)
qplot(factor(EVTYPE),FATALITIES,data =top1000Fatdf)

# Want the event types to be grouped 
top1000Injdf$EVTYPE[grep("TORNA|SPOUT",top1000Injdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Tornado"
top1000Injdf$EVTYPE[grep("HEAT",top1000Injdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Heat"
top1000Injdf$EVTYPE[grep("WINTER|BLIZZARD|SNOW",top1000Injdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "snow"
top1000Injdf$EVTYPE[grep("ICE|MIX|GLAZE",top1000Injdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Ice"
top1000Injdf$EVTYPE[grep("THUNDER|TSTM|LIGHTN",top1000Injdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Tstorm"
top1000Injdf$EVTYPE[grep("HURRIC|TROPICAL",top1000Injdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Hurricane "
top1000Injdf$EVTYPE[grep("FLOOD",top1000Injdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Flood "
#22 categories now

top1000Fatdf$EVTYPE[grep("TORNA|SPOUT",top1000Fatdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Tornado"
top1000Fatdf$EVTYPE[grep("HEAT",top1000Fatdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Heat"
top1000Fatdf$EVTYPE[grep("WINTER|BLIZZARD|SNOW",top1000Fatdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "snow"
top1000Fatdf$EVTYPE[grep("ICE|MIX|GLAZE",top1000Fatdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Ice"
top1000Fatdf$EVTYPE[grep("THUNDER|TSTM|LIGHTN",top1000Fatdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Tstorm"
top1000Fatdf$EVTYPE[grep("HURRIC|TROPICAL",top1000Fatdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Hurricane "
top1000Fatdf$EVTYPE[grep("FLOOD",top1000Fatdf$EVTYPE,ignore.case = TRUE,fixed = FALSE)] <- "Flood "
# 42 categories now

p6 <-ggplot(data =top1000Injdf,
            aes(EVTYPE,INJURIES))+
            geom_point(aes(y = INJURIES), colour = 'red')+
  geom_point(aes(y = FATALITIES), colour = 'blue')+
  theme(axis.text.x = element_text(angle = 90))

lessInjCats <-unique(top1000Injdf$EVTYPE)

InjSums <-top1000Injdf %>% group_by(EVTYPE) %>%
  summarise(totinj = sum(INJURIES)) %>% arrange(desc(totinj),by_group = TRUE)
print(InjSums[1:10,])   #Tornado, Heat, Flood, snow...
#Interesting that the single max death event did not appear in this list
#Look again at total data with groupings. Call this dataset sd3

FatSums <-top1000Fatdf %>% group_by(EVTYPE) %>%
  summarise(totFat = sum(FATALITIES)) %>% arrange(desc(totFat),by_group = TRUE)
print(FatSums[1:10,]) 

#For both injuries and fatalites, order is : Tornado, Heat, Flood
#-----------------------------------------------------------------------------
# good grep example
#----------------------------------------------------
# https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html
#----------------------------------------------------------------------------------
# Look at damage data:
str(sd1)
summary(sd1$PROPDMG)
unique(sd1$PROPDMGEXP)
print (sd1 %>% count(PROPDMGEXP, sort = TRUE))  # NAs are significant
# need to look at the low count other values to be sure what they meant

sd1 %>% filter(!is.na(PROPDMGEXP)) %>%
  filter(PROPDMGEXP != 'K') %>% 
  filter(PROPDMGEXP != 'M') %>% 
  filter(PROPDMGEXP != '0') %>%
  filter(PROPDMGEXP != 'B') %>% 
  filter(PROPDMGEXP != 'K') %>% 
  select(c(EVTYPE,PROPDMG,PROPDMGEXP)) %>% print

sd1 %>% filter(is.na(PROPDMGEXP)) %>% select(PROPDMG) %>% summary  #mean 0.00113, max 75

summary(sd1$CROPDMG)
unique(sd1$CROPDMGEXP)
print (sd1 %>% count(CROPDMGEXP, sort = TRUE))  # NAs are significant
# need to look at the low count other values to be sure what they meant

sd1 %>% filter(!is.na(CROPDMGEXP)) %>%
  filter(CROPDMGEXP != 'K') %>% 
  filter(CROPDMGEXP != 'M') %>% 
  filter(CROPDMGEXP != '0') %>%
  filter(CROPDMGEXP != 'B') %>% 
  filter(CROPDMGEXP != 'K') %>% 
  select(c(EVTYPE,CROPDMG,CROPDMGEXP)) %>% print

sd1 %>% filter(is.na(CROPDMGEXP)) %>% select(CROPDMG) %>% summary

#(B or b = Billion, M or m = Million, K or k = Thousand, H or h = Hundred).
#The number from one to ten represent the power of ten (10^The number). 
# The symbols "-", "+" and "?" refers to less than, greater than and low certainty. 
# Ignore low certainty

## Approach: sum damage dollars by year, apply inflation, pick worst year, determine events

sdd1 <- sd1 %>% 
  filter(!is.na(CROPDMGEXP)) %>% 
  filter(!is.na(PROPDMGEXP)) %>%
  filter(!is.na(CROPDMG)) %>%
  filter(!is.na(PROPDMG)) %>%
  filter(CROPDMGEXP != '+') %>%
  filter(PROPDMGEXP != '+') %>%
  filter(CROPDMGEXP != '-') %>%
  filter(PROPDMGEXP != '-') %>%
  filter(CROPDMGEXP != '?') %>%
  filter(PROPDMGEXP != '?') %>%
  select(c(BGN_DATE,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP,EVTYPE,REFNUM))  #279561 obs

sdd1$PROPDMGEXP[grep("B|b",sdd1$PROPDMGEXP)] <- 9
sdd1$PROPDMGEXP[grep("M|m",sdd1$PROPDMGEXP)] <- 6
sdd1$PROPDMGEXP[grep("K|k",sdd1$PROPDMGEXP)] <- 3
sdd1$PROPDMGEXP[grep("H|h",sdd1$PROPDMGEXP)] <- 2
sdd1$CROPDMGEXP[grep("B|b",sdd1$CROPDMGEXP)] <- 9
sdd1$CROPDMGEXP[grep("M|m",sdd1$CROPDMGEXP)] <- 6
sdd1$CROPDMGEXP[grep("K|k",sdd1$CROPDMGEXP)] <- 3
sdd1$CROPDMGEXP[grep("H|h",sdd1$CROPDMGEXP)] <- 2

sdd2 <- sdd1 %>%
  mutate(PROPERTY = PROPDMG * 10^(as.numeric(PROPDMGEXP))) %>%
  mutate(CROPS = CROPDMG * 10^(as.numeric(CROPDMGEXP))) %>%
  mutate(totDamage = PROPERTY + CROPS) %>%
  select(c(BGN_DATE,PROPERTY,CROPS,totDamage,EVTYPE,REFNUM))

sdd2$BGN_DATE <- year(sdd2$BGN_DATE)

pd1 <- qplot(BGN_DATE,totDamage,data = sdd2,
             xlab = "year",
             ylab = "total damages, $")
print(pd1)

sdd3DamInd <-order(sdd2$totDamage,decreasing = TRUE)  # gets index of max $ and sorts
top5Dam <- sdd2[sdd3DamInd[1:5],]
print(top5Dam)