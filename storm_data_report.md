# Consequences of severe weather events on public health and economy in the U.S between 1950 and 2011


## Synopsis

This report aims to study the consequences of the storms and severe weather events on the 
economy and public health in the United States between 1950 and 2011.

This project involves exploring the U.S. National Oceanic and Atmospheric
Administration's (NOAA) storm database. This database tracks characteristics of
major storms and weather events in the United States, including when and where
they occur, as well as estimates of any fatalities, injuries, and property
damage.

**TODO: Complete the synopsis**  


## Data processing

### Loading and preprocessing of the raw data

We load the Storm data from a CSV file downloaded from the U.S National Oceanic
and Atmospheric Administration's (NOAA) storm database.


```r
storm <- read.csv("data/repdata-data-StormData.csv")
dim(storm)
```

```
## [1] 902297     37
```

```r
head(storm, 3)
```

```
##   STATE__          BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1 4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1 4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1 2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0                                               0
## 2 TORNADO         0                                               0
## 3 TORNADO         0                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0                      14.0   100 3   0          0
## 2         NA         0                       2.0   150 2   0          0
## 3         NA         0                       0.1   123 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15    25.0          K       0                                    
## 2        0     2.5          K       0                                    
## 3        2    25.0          K       0                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806              1
## 2     3042      8755          0          0              2
## 3     3340      8742          0          0              3
```

The the Storm data contains 902297 rows and 37 columns.

* Dates

We format properly the columns `BGN_DATE` and `END_DATE`:


```r
bgn_dates <- as.Date(as.character(storm$BGN_DATE), "%m/%d/%Y")
end_dates <- as.Date(as.character(storm$END_DATE), "%m/%d/%Y")
```


* Event types

A quick look at the event types revealed many inconsistencies in the naming. 
This can be explained by the fact that the dataset covers a wide period (61 years)
and has been updated by different people with different conventions.

For example we find values like `'THUNDERSTORM'`, `'THUNDERSTORMS'` or 
`'TSTM WIND'`, `'THUNDERSTORM WIND'`, `'THUNDERSTORM WIND 59'`
and `'THUNDERSTORM WIND 59 MPH.'` (and even `'THUNDERSTORM WINS'`). 
For the sake of simplicity, in this analysis, we make the assumption that singular
and plural terms identical. 


```r
endswith <- function(x, pattern) {
    grepl(paste0(pattern, "$"), x)
}

singularize <- function(name) {
    name <- as.character(name)
    if (endswith(tolower(name), "s")) {
        singular <- substr(name, 1, nchar(name)-1)
    }
    else {
        singular <- name
    }
    return(singular)
}

storm$EVTYPE <- sapply(storm$EVTYPE, singularize)
```

## Results

### The most harmful events with respect to population health

In this section we focus on the consequences of the weather events on the
population health. In particular we'll set a ranking of the most harmful events
designated by the variable `EVTYPE`. 

In order to have an idea of what types of events are recorded here are the 
10 first most frequent event types:


```r
head(rev(sort(table(storm$EVTYPE))), 10)
```

```
## 
##              HAIL         TSTM WIND THUNDERSTORM WIND           TORNADO 
##            288661            219946            103406             60653 
##       FLASH FLOOD             FLOOD         HIGH WIND         LIGHTNING 
##             54309             25329             21745             15754 
##        HEAVY SNOW        HEAVY RAIN 
##             15708             11749
```

The variables of interest to determine which types of events are most harmful
with respect to health population are the `INJURIES` and `FATALITIES`. 
The following code computes the total injuries and fatalities grouped by event 
types.


```r
suppressMessages(library(dplyr))

injuries <- storm %>% group_by(EVTYPE) %>% summarise(total=sum(INJURIES)) %>% arrange(-total)
fatalities <- storm %>% group_by(EVTYPE) %>% summarise(total=sum(FATALITIES)) %>% arrange(-total)
```

We plot the 10 event types that respectively caused the most injuries and 
fatalities across the U.S:


```r
par(las=2, mfcol = c(1, 2), mar=c(6.1, 4.1, 4.1, 2.1))
barplot(head(injuries$total, 10), names.arg = head(injuries$EVTYPE, 10), 
        horiz = FALSE, cex.names = 0.8, col="seagreen", main="Total number of injuries",
        xlab="Event types")
barplot(head(fatalities$total, 10), names.arg = head(fatalities$EVTYPE, 10), 
        horiz = FALSE, cex.names = 0.8, col="seagreen", main="Total number of fatalities",
        xlab="Event types")
```

![plot of chunk plot_of_most_harmful_event_types](figure/plot_of_most_harmful_event_types.png) 

### The types of events that had the greatest economic consequences
