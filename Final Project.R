#Install the relevant libraries - do this one time
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("formattable")
# install.packages("tidyr")
# install.packages('R.utils')
# Load the libraries
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(R.utils)
library(plyr)
customRed = "#ff7f7f"

# Load data

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "StormData.csv.bz2")
library(R.utils)
bunzip2("StormData.csv.bz2", "StormData.csv")
df <- read.csv("StormData.csv")


# Across the United States, which types of events (as indicated in the EVTYPE variable) 
# are most harmful with respect to population health?
# across the United States, which types of events have the greatest economic consequences?

# counting the number of occurrences
number_of_occurrences <- count(df, "EVTYPE")
# sorting by most frequent
number_of_occurrences_sorted <- number_of_occurrences[order(-number_of_occurrences$freq),]


# counting the number of fatalities
fatalities_by_type <- aggregate(FATALITIES ~ EVTYPE, df, sum)
# sorting by most frequent
fatalities_by_type_sorted <- fatalities_by_type[order(-fatalities_by_type$FATALITIES),]

# merging occurences and fatalities into one data frame to get frequency of death by event type
merged <- merge(number_of_occurrences, fatalities_by_type, by = "EVTYPE")

merged_sorted <- merged[order(-merged$FATALITIES),]

merged_sorted$DEATH_FREQ <- (merged_sorted$FATALITIES/merged_sorted$freq)
# Chaning column name from freq to count
colnames(merged_sorted)[colnames(merged_sorted)=="freq"] <- "COUNT"
# Decreasing number of decimals
merged_sorted <- merged_sorted %>% 
  mutate_if(is.numeric, round, digits = 2)

plot1 <- head(merged_sorted, 10)

p1 <- barplot(plot1$FATALITIES, las = 3, names.arg = plot1$EVTYPE, 
        main = "Weather Events With\n The Top 10 Highest Fatalities", ylab = "Number of Fatalities", col = "grey")

# top ten similar to bar chart, but displaying frequency of death
formattable(head(formattable(merged_sorted), 10), 
            align =c("l","l","c","c","r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")),
              `DEATH_FREQ` = color_bar(customRed)))
#-----------------#


injuries_by_type <- aggregate(INJURIES ~ EVTYPE, df, sum) 
# sorting by most frequent
injuries_by_type_sorted <- injuries_by_type[order(-injuries_by_type$INJURIES),]
# merging occurences and fatalities into one data frame to get frequency of death by event type
merged_injuries <- merge(number_of_occurrences, injuries_by_type, by = "EVTYPE")

merged_injuries_sorted <- merged_injuries[order(-merged_injuries$INJURIES),]

merged_injuries_sorted$INJURY_FREQ <- (merged_injuries_sorted$INJURIES/merged_injuries_sorted$freq)
colnames(merged_injuries_sorted)[colnames(merged_injuries_sorted)=="freq"] <- "COUNT"

merged_injuries_sorted <- merged_injuries_sorted %>% 
  mutate_if(is.numeric, round, digits = 2)

plot2 <- head(merged_injuries_sorted, 10)

p2 <- barplot(plot2$INJURIES, las = 3, names.arg = plot2$EVTYPE, 
              main = "Weather Events With\n The Top 10 Highest Injuries", ylab = "Number of Injuries", col = "grey")

formattable(head(formattable(merged_injuries_sorted), 10), 
            align =c("l","l","c","c","r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")),
              `INJURY_FREQ` = color_bar(customRed)))
#-----------------#

property_damage_by_type <- aggregate(PROPDMG ~ EVTYPE, df, sum)
property_damage_merged <- merge(number_of_occurrences, property_damage_by_type, by = "EVTYPE")
property_damage_merged_sorted <- property_damage_merged[order(-property_damage_merged$PROPDMG),] 
# Cost per event
property_damage_merged_sorted$COST_PER_EVENT <- (property_damage_merged$PROPDMG/property_damage_merged$freq)
colnames(property_damage_merged_sorted)[colnames(property_damage_merged_sorted)=="freq"] <- "COUNT"

plot3 <- head(property_damage_merged_sorted, 10)

p3 <- barplot(plot3$COST_PER_EVENT, las = 3, names.arg = plot3$EVTYPE, 
              main = "Most Expensive Weather Events", ylab = "Cost per Event", col = "grey")

row.names(property_damage_merged_sorted) <- NULL
formattable(head(formattable(property_damage_merged_sorted), 10), 
            align =c("l","l","c","c","r"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")),
              `COST_PER_EVENT` = color_bar(customRed)))
#-----------------#

crop_damage_by_type <- aggregate(CROPDMG ~ EVTYPE, df, sum)
crop_damage_by_type_merged <- merge(number_of_occurrences, crop_damage_by_type, by = "EVTYPE")
crop_damage_by_type_merged_sorted <- crop_damage_by_type_merged[order(-crop_damage_by_type_merged$CROPDMG),]
# Crop damage per event
crop_damage_by_type_merged_sorted$CROP_PER_EVENT <- (crop_damage_by_type_merged_sorted$CROPDMG/crop_damage_by_type_merged_sorted$freq)
colnames(crop_damage_by_type_merged_sorted)[colnames(crop_damage_by_type_merged_sorted)=="freq"] <- "COUNT"

plot4 <- head(crop_damage_by_type_merged_sorted, 10)
p4 <- barplot(plot4$CROP_PER_EVENT, las = 3, names.arg = plot4$EVTYPE, 
               main = "Events that cause the most damage to crops", ylab = "Crop Damage", col = "grey")
row.names(crop_damage_by_type_merged_sorted) <- NULL
formattable(head(formattable(crop_damage_by_type_merged_sorted), 10), 
            align =c("l","l","c","c","c"), 
            list(`Indicator Name` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")),
              `CROP_PER_EVENT` = color_bar(customRed)))
#-----------------#
