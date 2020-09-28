---
title: "Weather Event Consequences"
author: "GP"
date: "9/26/2020"
output: 
  html_document: 
    fig_width: 10
    keep_md: yes
---
*NOTE:  rmarkdown::render('Weather_Consequences.Rmd', clean = FALSE)* to knit



# SYNOPSIS
This analysis summarizes storms and severe weather events that have impacted human health or caused great economic impact. The data set was derived from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database and covers records from the year 1950 to November 2011. The goal of the analysis was to find which types of events are most harmful to population health and which have the greatest economic consequences.

The analysis indicates that the most harmful weather events to humans, in terms of fatalities and injuries, are overwhelming tornadoes, followed by heat and then flood related events. Tornado events (including similar rotational phenomena) accounted for more than 60,000 injuries and 4000 deaths during the time period of interest.

The analysis also indicates that the weather events that cause the greatest economic damage to crops and property are overwhelming flooding events, which includes river flooding, hurricane and storm surges. Flooding accounts for more than 100 billion dollars across the years of interest. 

# DATA PROCESSING
A little research found that information about the about the original variables can be found here: 
[https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Export-Format.pdf]
The data we are given for this analysis is a subset, with renamed and possibly compromised formatting. That being said, sometimes data is messy, so here is how this particular dataset is processed to answer the questions. Disclaimer: no public government or municipality should rely on this report.

The analysis was conducted using the following computer and software:

```r
print(Sys.info()[c('sysname','machine','release')])
```

```
##   sysname   machine   release 
## "Windows"  "x86-64"  "10 x64"
```

```r
print(R.Version()['version.string'])
```

```
## $version.string
## [1] "R version 4.0.2 (2020-06-22)"
```

### Import raw data and extract. Install needed libraries. Pre-Process data.
Importing and extracting the large data set uses significant time.This only needs to be done once however, and further analyses will run faster with the data extracted.

Install needed libraries:


```r
library(cowplot)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyr)
library(ggplot2)
library(R.utils)
```

```
## Loading required package: R.oo
```

```
## Loading required package: R.methodsS3
```

```
## R.methodsS3 v1.8.0 (2020-02-14 07:10:20 UTC) successfully loaded. See ?R.methodsS3 for help.
```

```
## R.oo v1.23.0 successfully loaded. See ?R.oo for help.
```

```
## 
## Attaching package: 'R.oo'
```

```
## The following object is masked from 'package:R.methodsS3':
## 
##     throw
```

```
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
```

```
## The following objects are masked from 'package:base':
## 
##     attach, detach, load, save
```

```
## R.utils v2.9.2 successfully loaded. See ?R.utils for help.
```

```
## 
## Attaching package: 'R.utils'
```

```
## The following object is masked from 'package:tidyr':
## 
##     extract
```

```
## The following object is masked from 'package:utils':
## 
##     timestamp
```

```
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, nullfile, parse,
##     warnings
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:cowplot':
## 
##     stamp
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(lattice)
library(knitr)
```


```r
t1 <- Sys.time()
bunzip2("repdata_data_StormData.csv.bz2","StormData.csv", remove = FALSE, skip = TRUE)
```

```
## [1] "StormData.csv"
## attr(,"temporary")
## [1] FALSE
```

```r
t2 <- Sys.time()
print(t2-t1)  #Time difference of 19.25788 secs on my machine first pass
```

```
## Time difference of 0.003997087 secs
```

```r
#Load raw data into R
t3 <- Sys.time()
StormData <- read.csv("StormData.csv",sep = ",")
t4 <- Sys.time()
print(t4-t3)   #Time difference of 16.83699 secs on my machine, but varies
```

```
## Time difference of 16.42219 secs
```

The raw data, a dataframe called StormData, is now in R. The data looks like:

```r
print(str(StormData, give.length = TRUE))
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num [1:902297] 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : chr [1:902297] "4/18/1950 0:00:00" "4/18/1950 0:00:00" "2/20/1951 0:00:00" "6/8/1951 0:00:00" ...
##  $ BGN_TIME  : chr [1:902297] "0130" "0145" "1600" "0900" ...
##  $ TIME_ZONE : chr [1:902297] "CST" "CST" "CST" "CST" ...
##  $ COUNTY    : num [1:902297] 97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: chr [1:902297] "MOBILE" "BALDWIN" "FAYETTE" "MADISON" ...
##  $ STATE     : chr [1:902297] "AL" "AL" "AL" "AL" ...
##  $ EVTYPE    : chr [1:902297] "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
##  $ BGN_RANGE : num [1:902297] 0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : chr [1:902297] "" "" "" "" ...
##  $ BGN_LOCATI: chr [1:902297] "" "" "" "" ...
##  $ END_DATE  : chr [1:902297] "" "" "" "" ...
##  $ END_TIME  : chr [1:902297] "" "" "" "" ...
##  $ COUNTY_END: num [1:902297] 0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi [1:902297] NA NA NA NA NA NA ...
##  $ END_RANGE : num [1:902297] 0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : chr [1:902297] "" "" "" "" ...
##  $ END_LOCATI: chr [1:902297] "" "" "" "" ...
##  $ LENGTH    : num [1:902297] 14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num [1:902297] 100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int [1:902297] 3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num [1:902297] 0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num [1:902297] 0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num [1:902297] 15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num [1:902297] 25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: chr [1:902297] "K" "K" "K" "K" ...
##  $ CROPDMG   : num [1:902297] 0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: chr [1:902297] "" "" "" "" ...
##  $ WFO       : chr [1:902297] "" "" "" "" ...
##  $ STATEOFFIC: chr [1:902297] "" "" "" "" ...
##  $ ZONENAMES : chr [1:902297] "" "" "" "" ...
##  $ LATITUDE  : num [1:902297] 3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num [1:902297] 8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num [1:902297] 3051 0 0 0 0 ...
##  $ LONGITUDE_: num [1:902297] 8806 0 0 0 0 ...
##  $ REMARKS   : chr [1:902297] "" "" "" "" ...
##  $ REFNUM    : num [1:902297] 1 2 3 4 5 6 7 8 9 10 ...
## NULL
```

```r
# Missing data often shown as "" rather than NAs. 
# There are 985 unique events listed, but the documentation lists 48 Storm Events
# print(unique(StormData$EVTYPE))  # returns list of 985 events (too big to show here)
```
The "" is replaced with NA, and the date format is cast into a useful format. New dataframe is 'sd1'. Now the analysis can begin.

```r
sd1 <-na_if(StormData[,],"")   #this is slow.
sd1$BGN_DATE <- date(mdy_hms(sd1$BGN_DATE))
```

## Across the United States, which types of events (EVTYPE variable) are most harmful with respect to population health?
Events (EVTYPE variable) that cause deaths or injuries are harmful. The analysis approach is to find the single worst event in terms of injuries and in terms of death. Then the analysis will group similar EVTYPE names, for example 'TORNADO' and 'FUNNEL' and find the total injuries and deaths across all the years. The results will then be summarized.

### Initial scatter plot of the raw data INJURIES, FATALITIES versus EVTYPE
The initial look at the data using a scatter plot indicated clusters and peaks that have high harm. The problem is that many event types have similar, ambiguous, or poorly worded values (such as '?', or 'none', or 'torndao', etc.), which results in the 985 unique event types when there really should be only 48 according to the documentation. These problems will need to be addressed. The initial plot and the determination of the maximum values is programmed with this code: 


```r
#FATALITIES AND INJURIES plotted Together
p3 <-ggplot(data =sd1,
            aes(EVTYPE))+
  ylab("Fatalities and Injuries")+
  xlab("985 event types") +
  geom_point(aes(y = INJURIES, colour = 'Injuries'))+
  geom_point(aes(y = FATALITIES, colour = 'Fatalities'))+
  theme(axis.text.x = element_blank())+
  labs(title = "Raw data scatter plot")
print(p3)
```

![Raw Storm Data Scatter Plot](Weather_Consequences_files/figure-html/rawScatterAndPeaks-1.png)

```r
#Find max of single events:
maxInjIndex  <- which.max(sd1$INJURIES)
maxInj <- sd1$INJURIES[maxInjIndex]
eventMaxInj <- sd1$EVTYPE[maxInjIndex]   # "Tornado
dateMaxInj <- mdy_hms(StormData$BGN_DATE)[maxInjIndex]  #1979-04-10
remarksMaxInj <- StormData$REMARKS[maxInjIndex]

maxFatIndex  <- which.max(sd1$FATALITIES)
maxFat <- sd1$FATALITIES[maxFatIndex]
eventMaxFat <- sd1$EVTYPE[maxFatIndex]   # "Heat
dateMaxFat <- mdy_hms(StormData$BGN_DATE)[maxFatIndex]  #1995-07-12
remarksMaxFat <- StormData$REMARKS[maxFatIndex]
```
The single highest day of injury was from a TORNADO which occurred on 1979-04-10 and caused 1700 injuries. 

The single highest day of fatalities was from a HEAT which occurred on 1995-07-12 and caused 583 injuries. 

A synopsis of the extreme fatality event can be found in the REMARKS variable. A clip is shown below. The fatality event stands out, as it does not correspond to a cluster of fatality events on the raw data scatter plot; so perhaps another event type causes more fatalities overall. However, max injuries from tornadoes does correspond to many such tornado events. Further investigation is warranted. Note: no remarks were submitted for the Injury event.


```r
print(substr(remarksMaxFat,1,542))
```

```
## [1] "An intense heat wave affected northern Illinois from Wednesday July 12 through Sunday July 16. The heat wave tied or broke several temperature records at Rockford and Chicago. But what set this heat wave apart from others was the extremely high humidities. Dew point temperatures peaked in the lower 80s late Wednesday the 12th and Thursday the 13th and were generally in the middle and upper 70s through the rest of the hot spell. The combined and cumulative effects of several days of high temperatures, high humidity, intense July sunshine"
```


```r
print(substr(remarksMaxInj,1,542))
```

```
## [1] ""
```
### Further analysis, consolidating events

Data was quantified by summing top 1000 events with the most injuries or fatalities. This determines the event types with overall worst injury and fatality record. The steps are to get the top 1000, then look at the EVTYPEs, then consolidate similar names. This was an iterative process initially. The final step will be to apply the consolidation to the original data (to reduce the 985 EVTYPES), and confirm that the peaks in the original data are the same, and that no significant other peaks have appeared.

Fortunately, the top 1000 did not include many of the outliers, i.e. events that occurred relatively few times, did not have high injuries and fatalities, and had poor naming conventions. These events can get lumped together. Some of these negligible events can be grouped with the high impact events when the REMARKS variable is read, if desired, but are not likely to change the overall outcome. 


```r
sd2 <- sd1 %>% select(c(FATALITIES,INJURIES,EVTYPE,REFNUM,BGN_DATE))
#Look at top 1000 injury and fatality count events
#Create subset of top 1000
injOrderInd <-order(sd2$INJURIES,decreasing = TRUE)  # gets index of max inj and sorts
injOrderFat <-order(sd2$FATALITIES,decreasing = TRUE)# gets index of max Fat and sorts

top1000Injdf <- sd2[injOrderInd[1:1000],]
top1000Fatdf <- sd2[injOrderFat[1:1000],]

injUniq <- unique(top1000Injdf$EVTYPE) #45 evtypes returned
fatUniq <- unique(top1000Fatdf$EVTYPE) #71

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
```
At this point a few plots will show that this reduced data set will make a scatter plot similar to the original, but with less points, indicating a reasonable grouping of events. (not shown). Next, find the sum of injuries and fatalities as a function of the events in the reduced data set. Recall we are neglecting outliers.


```r
InjSums <-top1000Injdf %>% group_by(EVTYPE) %>%
  summarise(totInjuries = sum(INJURIES)) %>% arrange(desc(totInjuries),by_group = TRUE)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
print(InjSums[1:10,])   #Tornado, Heat, Flood, snow...
```

```
## # A tibble: 10 x 2
##    EVTYPE       totInjuries
##    <chr>              <dbl>
##  1 "Tornado"          62401
##  2 "Heat"              8319
##  3 "Flood "            7200
##  4 "snow"              2086
##  5 "Ice"               2076
##  6 "Hurricane "        1537
##  7 "Tstorm"            1488
##  8 "HAIL"               481
##  9 "FOG"                388
## 10 "WILDFIRE"           254
```

```r
FatSums <-top1000Fatdf %>% group_by(EVTYPE) %>%
  summarise(totFatalities = sum(FATALITIES)) %>% arrange(desc(totFatalities),by_group = TRUE)
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
print(FatSums[1:10,])
```

```
## # A tibble: 10 x 2
##    EVTYPE                    totFatalities
##    <chr>                             <dbl>
##  1 "Tornado"                          4297
##  2 "Heat"                             2409
##  3 "Flood "                            461
##  4 "snow"                              176
##  5 "Hurricane "                        138
##  6 "Tstorm"                            135
##  7 "EXTREME COLD"                       56
##  8 "HIGH WIND"                          54
##  9 "WILDFIRE"                           47
## 10 "EXTREME COLD/WIND CHILL"            43
```

```r
#For both injuries and fatalities, order is : Tornado, Heat, Flood
```

Now apply a similar filter to the original data set and confirm groupings are the same to validate the sums obtained above are indeed indicative of events that cause the most harm and injuries. Some additional terms for the grep function were added to help reduce outliers. This will be illustrated with a plot.

Create new plot that will be combined with the first plot to validate event consolidation.Note that the new plot has a horizontal scale slightly stretched (less categories), but peak matching can still be observed.


```r
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

#print(unique(sd4$EVTYPEsub))  #66 event categories

#Try to get all the pre and post data reduction together
#Note p5 will be shown in the 'before and after' plot of p35.
p5 <-ggplot(data =sd4,
            aes(EVTYPEsub,INJURIES))+
  ylab("Fatalities and Injuries")+
  xlab("66 event catagories") +
  geom_point(aes(y = as.numeric(INJURIES), colour = 'Injuries'))+
  geom_point(aes(y = as.numeric(FATALITIES), colour = 'Fatalities'))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Weather events consolidated")


p35 <-cowplot::plot_grid(p3,p5,nrow = 2, ncol = 1)
print(p35) 
```

![Comparison of raw data scatter plot with consolidated events scatter plot](Weather_Consequences_files/figure-html/consolidatedInjFatPlot-1.png)

## Across the United States, which types of events have the greatest economic consequences?

The approach taken for this inquiry was to sum damage dollars by year, pick the worst years in terms of monetary damages to property and crops, then determine causing events.

Variables of interest are:

  *   date - BGN_DATE

  *   PROPDMG - numeric dollar amount base number for property damage

  *   PROPDMGEXP - The multiplier for the base number in PROPDMG.(B or b = Billion, M or m = Million, K or k = Thousand, H or h = Hundred).The number from one to ten represent the power of ten (10^The number). The symbols "-", "+" and "?" refers to less than, greater than and low certainty.

  *   CROPDMG - numeric dollar amount base number for property damage

  *   CROPDMGEXP - The multiplier for the base number in CROPDMG.(B or b = Billion, M or m = Million, K or k = Thousand, H or h = Hundred).The number from one to ten represent the power of ten (10^The number). The symbols "-", "+" and "?" refers to less than, greater than and low certainty.

  *   EVTYPE - type of weather event

Initial observations of the data set indicate high number of NAs in the crop and property damage values, which makes for good reduction of the data set. Also, the uncertain damage amounts were small, so can be neglected. The reduced data set is filtered with this code:


```r
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
```

Next the exponential data is processed so that 'B' is replaced with '9', for 10^9, and etc for 'M', 'k', 'h', and correction for inconsistent use of capitalization:


```r
sdd1$PROPDMGEXP[grep("B|b",sdd1$PROPDMGEXP)] <- 9
sdd1$PROPDMGEXP[grep("M|m",sdd1$PROPDMGEXP)] <- 6
sdd1$PROPDMGEXP[grep("K|k",sdd1$PROPDMGEXP)] <- 3
sdd1$PROPDMGEXP[grep("H|h",sdd1$PROPDMGEXP)] <- 2
sdd1$CROPDMGEXP[grep("B|b",sdd1$CROPDMGEXP)] <- 9
sdd1$CROPDMGEXP[grep("M|m",sdd1$CROPDMGEXP)] <- 6
sdd1$CROPDMGEXP[grep("K|k",sdd1$CROPDMGEXP)] <- 3
sdd1$CROPDMGEXP[grep("H|h",sdd1$CROPDMGEXP)] <- 2
```

The real cost of an event is calculated and a new column with the dollar amount is added to the dataset for both crop and property damage. Also, the damages are totaled for each event. We are interested in plotting by year, so the year of the date variable is extracted. The new dataset, reduced, is call sdd2.


```r
sdd2 <- sdd1 %>%
  mutate(PROPERTY = PROPDMG * 10^(as.numeric(PROPDMGEXP))) %>%
  mutate(CROPS = CROPDMG * 10^(as.numeric(CROPDMGEXP))) %>%
  mutate(totDamage = PROPERTY + CROPS) %>%
  select(c(BGN_DATE,PROPERTY,CROPS,totDamage,EVTYPE,REFNUM))

sdd2$BGN_DATE <- year(sdd2$BGN_DATE)
```

The data needs to be plotted to identify where are the events with high economic damages. We are interested in the peaks of plot. Note that the plot is color coded, but there are roughly 279k points, so much overlap exists. It is the peaks we want.


```r
pd1 <- ggplot(sdd2,aes(x=BGN_DATE))+
  geom_point(aes(y = log10(PROPERTY),color = 'Property'))+
  geom_point(aes(y = log10(CROPS),color = 'Crops'))+
  geom_point(aes(y = log10(totDamage),color = 'Total$'))+
  labs(x = "year",y = "log $",color = 'Damages',
       title = "Economic Damages from Weather Events by year")

print(pd1)
```

![Economic damages by year](Weather_Consequences_files/figure-html/damagesPlot-1.png)

From the plot, peaks are observed. Note that the vertical scale is log to the base 10 of the dollar amount, so that the points could stretch vertically. (Otherwise they mostly cluster at the bottom of the chart on top of each other). We are interested in the the top 10 or 20 events, and observing what the damages are and what events caused them. This code will provide the information:


```r
sdd3DamInd <-order(sdd2$totDamage,decreasing = TRUE)  # gets index of max $ and sorts
top5Dam <- sdd2[sdd3DamInd[1:10],]
print(top5Dam)
```

```
##        BGN_DATE PROPERTY    CROPS    totDamage            EVTYPE REFNUM
## 18900      2006 1.15e+11 3.25e+07 115032500000             FLOOD 605943
## 174        1993 5.00e+09 5.00e+09  10000000000       RIVER FLOOD 198375
## 18401      2005 5.88e+09 1.51e+09   7390000000 HURRICANE/TYPHOON 581537
## 16318      2004 5.42e+09 2.85e+08   5705000000 HURRICANE/TYPHOON 529299
## 1717       1994 5.00e+05 5.00e+09   5000500000         ICE STORM 211887
## 16321      2004 4.83e+09 9.32e+07   4923200000 HURRICANE/TYPHOON 529384
## 16323      2004 4.00e+09 2.50e+07   4025000000 HURRICANE/TYPHOON 529446
## 116840     2008 4.00e+09 0.00e+00   4000000000  STORM SURGE/TIDE 739515
## 9230       1999 3.00e+09 5.00e+08   3500000000         HURRICANE 366653
## 239898     2011 2.80e+09 0.00e+00   2800000000           TORNADO 862563
```
# Results

The analysis began with the raw data, then created a subset of data consisting of the most injurious or fatal events, to extract the overall events most harmful to population health. The analysis subsequently verified consolidating the untidy list of event types was a valid method of data consolidation. The table below shows that tornados are the extreme weather event that caused both the most injuries and the most fatalities between 1950 and November 2011. This is followed by heat related events. The single highest day of injury was from a TORNADO which occurred on 1979-04-10 and caused 1700 injuries. The single highest day of fatalities was from a HEAT which occurred on 1995-07-12 and caused 583 injuries. 

Summing injuries and fatalities from similar events results in tornados being overall the most harmful weather event to the population health. The table below shows the top 10 events.


```r
kable(cbind(InjSums[1:10,],FatSums[1:10,]), 
      caption = "Table showing top 10 event types for total injuries and fatalities")
```



Table: Table showing top 10 event types for total injuries and fatalities

|EVTYPE    | totInjuries|EVTYPE                  | totFatalities|
|:---------|-----------:|:-----------------------|-------------:|
|Tornado   |       62401|Tornado                 |          4297|
|Heat      |        8319|Heat                    |          2409|
|Flood     |        7200|Flood                   |           461|
|snow      |        2086|snow                    |           176|
|Ice       |        2076|Hurricane               |           138|
|Hurricane |        1537|Tstorm                  |           135|
|Tstorm    |        1488|EXTREME COLD            |            56|
|HAIL      |         481|HIGH WIND               |            54|
|FOG       |         388|WILDFIRE                |            47|
|WILDFIRE  |         254|EXTREME COLD/WIND CHILL |            43|

Economic damages were analyzed by year. The top most costly event was a flood in 2006. This event was 10 times higher in cost then the second event, which was also a flood. One can conclude that floods are the most economically damaging event, and can come from rivers, hurricanes, and storm surges. Because flooding events were overwhelming more costly, no attempt to account for inflation has been made. The table below shows the costs and events.


```r
kable(sdd2[sdd3DamInd[1:10],2:5],caption = "Table showing to economic damages, in dollars.",row.names = FALSE)
```



Table: Table showing to economic damages, in dollars.

| PROPERTY|    CROPS|    totDamage|EVTYPE            |
|--------:|--------:|------------:|:-----------------|
| 1.15e+11| 3.25e+07| 115032500000|FLOOD             |
| 5.00e+09| 5.00e+09|  10000000000|RIVER FLOOD       |
| 5.88e+09| 1.51e+09|   7390000000|HURRICANE/TYPHOON |
| 5.42e+09| 2.85e+08|   5705000000|HURRICANE/TYPHOON |
| 5.00e+05| 5.00e+09|   5000500000|ICE STORM         |
| 4.83e+09| 9.32e+07|   4923200000|HURRICANE/TYPHOON |
| 4.00e+09| 2.50e+07|   4025000000|HURRICANE/TYPHOON |
| 4.00e+09| 0.00e+00|   4000000000|STORM SURGE/TIDE  |
| 3.00e+09| 5.00e+08|   3500000000|HURRICANE         |
| 2.80e+09| 0.00e+00|   2800000000|TORNADO           |
