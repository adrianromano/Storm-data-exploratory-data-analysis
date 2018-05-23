if (!file.exists("Courseradata")) {
    dir.create("Courseradata")
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile = "/Users/adrianromano/Downloads/Courseradata/StormData.csv.bz2", method = "curl")

stormData <- read.csv(bzfile("/Users/adrianromano/Downloads/Courseradata/StormData.csv.bz2"))
str(stormData)

## Across the United States, which types of events are most harmful with respect to population health?
library(dplyr)
fatalities <- stormData %>%
    select(EVTYPE, FATALITIES) %>%
    group_by(EVTYPE) %>%
    summarize(sum = sum(FATALITIES)) %>%
    arrange(-sum)

injuries <- stormData %>%
    select(EVTYPE, INJURIES) %>%
    group_by(EVTYPE) %>%
    summarize(sum = sum(INJURIES)) %>%
    arrange(-sum)

library(ggplot2)
ggplot(fatalities[1:8, ], aes(x = reorder(EVTYPE, sum), y = sum, fill = EVTYPE, alpha = 0.1)) +
    geom_bar(stat = "identity", col = "black") +
    xlab("Event Type") +
    ylab("Number of Fatalities") +
    ggtitle("Top 8 Events with Highest Total Fatalities") +
    coord_flip() +
    guides(fill = FALSE, alpha = FALSE)

ggplot(injuries[1:8, ], aes(x = reorder(EVTYPE, sum), y = sum, fill = EVTYPE, alpha = 0.1)) +
    geom_bar(stat = "identity", col = "black") +
    xlab("Event Type") +
    ylab("Number of Injuries") +
    ggtitle("Top 8 Events with Highest Total Injuries") +
    coord_flip() +
    guides(fill = FALSE, alpha = FALSE)

## Across the United States, which types of events have the greatest economic consequences?
unique(stormData$PROPDMGEXP)
unique(stormData$CROPDMGEXP)
stormData$PROPDMGEXP <- toupper(stormData$PROPDMGEXP)
stormData$CROPDMGEXP <- toupper(stormData$CROPDMGEXP)

stormData[stormData$PROPDMGEXP %in% c("", "+", "-", "?"), "PROPDMGEXP"] <- "0"
stormData[stormData$CROPDMGEXP %in% c("", "?"), "CROPDMGEXP"] <- "0"
stormData[stormData$PROPDMGEXP == "K", "PROPDMGEXP"] <- "3"
stormData[stormData$PROPDMGEXP == "M", "PROPDMGEXP"] <- "6"
stormData[stormData$PROPDMGEXP == "B", "PROPDMGEXP"] <- "9"
stormData[stormData$PROPDMGEXP == "H", "PROPDMGEXP"] <- "2"
stormData[stormData$CROPDMGEXP == "M", "CROPDMGEXP"] <- "6"
stormData[stormData$CROPDMGEXP == "K", "CROPDMGEXP"] <- "3"
stormData[stormData$CROPDMGEXP == "B", "CROPDMGEXP"] <- "9"
unique(stormData$PROPDMGEXP)
unique(stormData$CROPDMGEXP)

stormData$PROPDMGEXP <- as.integer(stormData$PROPDMGEXP)
stormData$CROPDMGEXP <- as.integer(stormData$CROPDMGEXP)

library(dplyr)
totalDamage <- stormData %>%
    mutate(PROPDMGEXP2 = (10 ** PROPDMGEXP)) %>%
    mutate(CROPDMGEXP2 = (10 ** CROPDMGEXP)) %>%
    mutate(PROPDMG2 = PROPDMG * PROPDMGEXP2) %>%
    mutate(CROPDMG2 = CROPDMG * CROPDMGEXP2) %>%
    mutate(TOTALDMG = PROPDMG2 + CROPDMG2) 

totalDamage2 <- totalDamage %>%
    select(EVTYPE, TOTALDMG) %>%
    group_by(EVTYPE) %>%
    summarize(sum = sum(TOTALDMG)) %>%
    arrange(-sum)

library(ggplot2)
ggplot(totalDamage2[1:8, ], aes(x = reorder(EVTYPE, sum), y = sum, fill = EVTYPE, alpha = 0.1)) +
    geom_bar(stat = "identity", col = "black") +
    xlab("Event Type") +
    ylab("Total Damages in USD") +
    ggtitle("Top 8 Events with Highest Total Economic Impact") +
    coord_flip() +
    guides(fill = FALSE, alpha = FALSE)