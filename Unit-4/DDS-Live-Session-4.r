
# Packages
library(httr)
library(jsonlite)
library(RJSONIO)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyverse)
library(GGally)
library(devtools)
library(rjson)
library(bit64)
library(WDI)
library(XML)
library(rvest)
library(RCurl)
library(httpgd)

# Part 1
## XML restaurant data from Baltimore
data <- getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")

### Generate R structure
doc <- xmlParse(data)

### Assign variable values and names
names <- xpathSApply(doc, "//name", xmlValue)
zipcodes <- xpathSApply(doc, "//zipcode", xmlValue)
council_districts <- xpathSApply(doc, "//councildistrict", xmlValue)

### Constuct data frame from variables
restaurant_df <- data.frame(names, zipcodes, council_districts)

### Estimate number of Sushi restaurants
sum(grepl("SUSHI", restaurant_df$names, ignore.case = TRUE))

### Sushi restaurants in downtown (District 11)
downtown_sushi <- restaurant_df %>% filter(council_districts == "11")
sum(grepl("SUSHI", downtown_sushi$names))

### Barplot of estimated number of restaurants in each council
downtown_bar <-
  restaurant_df %>%
  filter(grepl("BAR", restaurant_df$names)) %>%
  group_by(council_districts) %>%
  summarise(count = n())
downtown_bar

downtown_bar %>%
  ggplot(aes(x = reorder(council_districts, -count), y = count, fill = council_districts)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Council District") +
  ylab("Count") +
  ggtitle("Estimated Number of Bars by Council District") +
  theme_minimal()


# Part 2

results <- as.data.frame(WDIsearch("water"))

## GDP
gdp_data <-
  WDI(country = "all",
      indicator = "NY.GDP.MKTP.CD",
      start = 1960, end = 2020,
      language = "en")
names(gdp_data) <- c("country_code", "country", "gdp", "year")

head(gdp_data)

gdp_data %>%
  ggplot(aes(x = year, y = gdp)) +
  geom_bar(stat = "identity")

gdp_data %>%
  filter(year == 2017, country_code == "US") %>%
  ggplot(aes(x = country_code, y = gdp)) +
  geom_bar(stat = "identity")



## Water Productivity - GDP / (total fresh water withdrawl)
total_water_productivity <-
  WDI(country = "all",
      indicator = "ER.GDP.FWTL.M3.KD",
      start = 1960, end = 2020,
      language = "en")

names(total_water_productivity) <- c("country_code", "country", "water_productivity", "year")

total_water_productivity %>%
  ggplot(aes(x = year, y = water_productivity)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Water Productivity") +
  ggtitle("Historical Water Productivity") +
  theme_minimal()

### Top 20 countries by water productivity
total_water_productivity %>%
  filter(year >= 2017, !is.na(water_productivity) & !is.na(year)) %>%
  group_by(country) %>%
  summarise(total_wp = sum(water_productivity)) %>%
  slice_max(total_wp, n = 20) %>%
  ggplot(aes(x = reorder(country, -total_wp), y = total_wp)) +
  geom_bar(stat = "identity", fill = "#2c438f") +
  xlab("Country") +
  ylab("Water Productivity") +
  ggtitle("Top 20 Countries by Water Productivity") +
  theme_minimal()

### Bottom 20 countries by water productivity
total_water_productivity %>%
  filter(year >= 2017, !is.na(water_productivity) & !is.na(year),
        country != "IDA blend", country_code != "T5") %>%
  group_by(country) %>%
  summarise(total_wp = sum(water_productivity)) %>%
  slice_min(total_wp, n = 20) %>%
  ggplot(aes(x = reorder(country, -total_wp), y = total_wp)) +
  geom_bar(stat = "identity", fill = "#ce5e5e") +
  xlab("Country") +
  ylab("Water Productivity") +
  ggtitle("Bottom 20 Countries by Water Productivity") +
  theme_minimal()

## Annual Fresh Water Withdrawls - 2017
total_fresh_water <-
  WDI(country = c("Z4", "8S", "Z7", "XU", "ZJ", "ZQ", "ZG", "B8"),
      indicator = "ER.H2O.FWTL.K3",
      start = 2017, end = 2017,
      language = "en")
names(total_fresh_water) <- c("region_code", "region", "annual_water", "year")
head(total_fresh_water)

total_fresh_water %>%
  ggplot(aes(x = reorder(region, -annual_water), y = annual_water, fill = region)) +
  geom_bar(stat = "identity")

## GDP - 2017
gdp_data <-
  WDI(country = c("Z4", "8S", "Z7", "XU", "ZJ", "ZQ", "ZG", "B8"),
      indicator = "NY.GDP.MKTP.CD",
      start = 2017, end = 2017,
      language = "en")
names(gdp_data) <- c("region_code", "region", "gdp", "year")

gdp_water <- merge(total_fresh_water, gdp_data, c("region_code", "region", "year"))

head(gdp_water)

## GDP vs. Water coorelation
gdp_water %>%
  ggplot(aes(x = gdp, y = annual_water, color = region)) +
  geom_point(size = 5) +
  xlab("Annual GDP") +
  ylab("Fresh Water Withdrawls") +
  ggtitle("GDP vs. Fresh Water Withdrawls by Region (2017)") +
  theme_minimal()


## Annual Water Total (% of total resources)
total_fresh_water <-
  WDI(country = c("Z4", "8S", "Z7", "XU", "ZJ", "ZQ", "ZG", "B8"),
      indicator = "ER.H2O.FWTL.ZS",
      start = 2017, end = 2017,
      language = "en")

names(total_fresh_water) <- c("region_code", "region", "annual_water", "year")
head(total_fresh_water)

total_fresh_water %>%
  slice_min(annual_water, n = 20) %>%
  ggplot(aes(x = reorder(region, -annual_water), y = annual_water)) +
  geom_bar(stat = "identity", aes(fill = region)) +
  xlab("Region") +
  ylab("Fresh Water Withdrawls") +
  ggtitle("Fresh Water Withdrawls(% of total resources) by Region (2017)") +
  theme_minimal()

