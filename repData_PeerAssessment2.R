## R Script for Peer Assessment #2 - Reproducible Research
##

## Load the appropriate libraries
library("dplyr")
library("ggplot2")
library("knitr")

## Read in NoAA data - note that read.csv can read directly from compressed files
noaaDB <- read.csv("repdata-data-StormData.csv.bz2")

## For dplyr, tbl_df is easier to use
noaaDB <- tbl_df(noaaDB)

glimpse(noaaDB)

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

library(lattice)
xyplot(steps ~ interval | factor(weekend.indicator),
       layout = c(1, 2),
       xlab="Year",
       ylab="Damage",
       type="l",
       lty=1,
       data=wk_df)


