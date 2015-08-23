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
    arrange(desc(rank), desc(p_Injured)) %>%
    mutate(totMan = p_Fatal + p_Injured) %>%
    head(5) %>%
    arrange(desc(totMan))



## Rank Event Types by cost to economic health
dfDamageEcon <- noaaDB %>%
    group_by(EVTYPE) %>%
    summarise(p_PropDmg = sum(PROPDMG, na.rm = TRUE), p_CropDmg = sum(CROPDMG, na.rm = TRUE)) %>%
    mutate(rank = rank(p_PropDmg)) %>%
    arrange(desc(rank), desc(p_CropDmg)) %>%
    mutate(tot = p_PropDmg + p_CropDmg) %>%
    head(5) %>%
    arrange(desc(tot))

barplot(dfDamage$totMan, main = "Top 5 Event Types Affecting Health", 
        xlab = "Event Types", 
        ylab = "No. Affected",
        names.arg = dfDamage$EVTYPE, cex.names = .75, las = 2)
barplot(dfDamageEcon$tot, main = "Top 5 Event Types Affecting Economy", 
        xlab = "Event Type", 
        ylab = "Damage",
        names.arg = dfDamageEcon$EVTYPE, cex.names = .75, las = 2)

