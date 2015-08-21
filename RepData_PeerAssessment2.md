# Reproducible Research: Peer Assessment 2
August 21, 2015  
##U.S. Storms: The Human and Financial Harm (1950 - 2011)

##Synopsis
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.  In this study, we strive to answer the following questions:

1. Which types of events are most harmful with respect to population health?

2. Which types of events have the greatest economic consequences?

##Data Processing



```r
## Load the appropriate libraries
library("dplyr")
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library("ggplot2")
library("knitr")

## Read in NoAA data - note that read.csv can read directly from compressed files
noaaDB <- read.csv("repdata-data-StormData.csv.bz2")

## For dplyr, tbl_df is easier to use
noaaDB <- tbl_df(noaaDB)

glimpse(noaaDB)
```

```
## Observations: 902297
## Variables:
## $ STATE__    (dbl) 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
## $ BGN_DATE   (fctr) 4/18/1950 0:00:00, 4/18/1950 0:00:00, 2/20/1951 0:...
## $ BGN_TIME   (fctr) 0130, 0145, 1600, 0900, 1500, 2000, 0100, 0900, 20...
## $ TIME_ZONE  (fctr) CST, CST, CST, CST, CST, CST, CST, CST, CST, CST, ...
## $ COUNTY     (dbl) 97, 3, 57, 89, 43, 77, 9, 123, 125, 57, 43, 9, 73, ...
## $ COUNTYNAME (fctr) MOBILE, BALDWIN, FAYETTE, MADISON, CULLMAN, LAUDER...
## $ STATE      (fctr) AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL, AL...
## $ EVTYPE     (fctr) TORNADO, TORNADO, TORNADO, TORNADO, TORNADO, TORNA...
## $ BGN_RANGE  (dbl) 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ BGN_AZI    (fctr) , , , , , , , , , , , , , , , , , , , , , , , , 
## $ BGN_LOCATI (fctr) , , , , , , , , , , , , , , , , , , , , , , , , 
## $ END_DATE   (fctr) , , , , , , , , , , , , , , , , , , , , , , , , 
## $ END_TIME   (fctr) , , , , , , , , , , , , , , , , , , , , , , , , 
## $ COUNTY_END (dbl) 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ COUNTYENDN (lgl) NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,...
## $ END_RANGE  (dbl) 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ END_AZI    (fctr) , , , , , , , , , , , , , , , , , , , , , , , , 
## $ END_LOCATI (fctr) , , , , , , , , , , , , , , , , , , , , , , , , 
## $ LENGTH     (dbl) 14.0, 2.0, 0.1, 0.0, 0.0, 1.5, 1.5, 0.0, 3.3, 2.3, ...
## $ WIDTH      (dbl) 100, 150, 123, 100, 150, 177, 33, 33, 100, 100, 400...
## $ F          (int) 3, 2, 2, 2, 2, 2, 2, 1, 3, 3, 1, 1, 3, 3, 3, 4, 1, ...
## $ MAG        (dbl) 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ FATALITIES (dbl) 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 4, 0, ...
## $ INJURIES   (dbl) 15, 0, 2, 2, 2, 6, 1, 0, 14, 0, 3, 3, 26, 12, 6, 50...
## $ PROPDMG    (dbl) 25.0, 2.5, 25.0, 2.5, 2.5, 2.5, 2.5, 2.5, 25.0, 25....
## $ PROPDMGEXP (fctr) K, K, K, K, K, K, K, K, K, K, M, M, K, K, K, K, K,...
## $ CROPDMG    (dbl) 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ CROPDMGEXP (fctr) , , , , , , , , , , , , , , , , , , , , , , , , 
## $ WFO        (fctr) , , , , , , , , , , , , , , , , , , , , , , , , 
## $ STATEOFFIC (fctr) , , , , , , , , , , , , , , , , , , , , , , , , 
## $ ZONENAMES  (fctr) , , , , , , , , , , , , , , , , , , , , , , , , 
## $ LATITUDE   (dbl) 3040, 3042, 3340, 3458, 3412, 3450, 3405, 3255, 333...
## $ LONGITUDE  (dbl) 8812, 8755, 8742, 8626, 8642, 8748, 8631, 8558, 874...
## $ LATITUDE_E (dbl) 3051, 0, 0, 0, 0, 0, 0, 0, 3336, 3337, 3402, 3404, ...
## $ LONGITUDE_ (dbl) 8806, 0, 0, 0, 0, 0, 0, 0, 8738, 8737, 8644, 8640, ...
## $ REMARKS    (fctr) , , , , , , , , , , , , , , , , , , , , , , , , 
## $ REFNUM     (dbl) 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, ...
```

```r
## Rank Event Types by cost to human health
dfDamage <- noaaDB %>%
    group_by(EVTYPE) %>%
    summarise(p_Fatal = sum(FATALITIES, na.rm = TRUE), p_Injured = sum(INJURIES, na.rm = TRUE)) %>%
    mutate(rank = rank(p_Fatal)) %>%
    arrange(desc(rank), desc(p_Injured))


## Rank Event Types by cost to economic health
dfDamageEcon <- noaaDB %>%
    group_by(EVTYPE) %>%
    summarise(p_PropDmg = sum(PROPDMG, na.rm = TRUE), p_CropDmg = sum(CROPDMG, na.rm = TRUE)) %>%
    mutate(rank = rank(p_PropDmg)) %>%
    arrange(desc(rank), desc(p_CropDmg))


totImpace <- full_join(dfDamage, dfDamageEcon, by = "EVTYPE")
```



##Results



```r
head(dfDamage)
```

```
## Source: local data frame [6 x 4]
## 
##           EVTYPE p_Fatal p_Injured rank
## 1        TORNADO    5633     91346  985
## 2 EXCESSIVE HEAT    1903      6525  984
## 3    FLASH FLOOD     978      1777  983
## 4           HEAT     937      2100  982
## 5      LIGHTNING     816      5230  981
## 6      TSTM WIND     504      6957  980
```

According to the data, the highest overall cost to human health between 1950 and November 2011 as measured in fatalaties and injuries is caused by Tornadoes.  Deaths from Tornadoes are almost three times higher than the next highest cause, Excesssive Heat.  While a fatality is the ultimate cost, the real difference in cost to human health is the number of injuries.  Tornadoes cause well over 10 times more injuries that its counterparts, TSTM Wind, Flood, or Excessive Heat.


```r
head(dfDamageEcon)
```

```
## Source: local data frame [6 x 4]
## 
##              EVTYPE p_PropDmg p_CropDmg rank
## 1           TORNADO 3212258.2 100018.52  985
## 2       FLASH FLOOD 1420124.6 179200.46  984
## 3         TSTM WIND 1335965.6 109202.60  983
## 4             FLOOD  899938.5 168037.88  982
## 5 THUNDERSTORM WIND  876844.2  66791.45  981
## 6              HAIL  688693.4 579596.28  980
```

From an economic perspective, the data reveals a somewhat different hierarchy of costs, but not completely different than those that particularly affect human health.  Tornadoes still rank the hightest in terms of economic cost in property damage, but are certainly not the most damaging to agricultural endeavors.

Crop damages top economic costs result from Hail (which is almost three times higher than its next neighbor), Flash Floods, and Floods.  While the financial cost to crops is less than that of property damage, the overall cost to society cannot be understated.


```r
head(totImpace)
```

```
## Source: local data frame [6 x 7]
## 
##           EVTYPE p_Fatal p_Injured rank.x p_PropDmg p_CropDmg rank.y
## 1        TORNADO    5633     91346    985 3212258.2 100018.52    985
## 2 EXCESSIVE HEAT    1903      6525    984    1460.0    494.40    925
## 3    FLASH FLOOD     978      1777    983 1420124.6 179200.46    984
## 4           HEAT     937      2100    982     298.5    662.70    839
## 5      LIGHTNING     816      5230    981  603351.8   3580.61    979
## 6      TSTM WIND     504      6957    980 1335965.6 109202.60    983
```










