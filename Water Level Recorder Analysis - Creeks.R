#Project:Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 2 - Analysis of Tidal Hydrology Regime of the Creek Water Level Recorders

#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire



#General Explanation of Code:
# The code is largely a product of the VulnToolkit package created by Troy Hill at the EPA. He devised a fantastic 
# method to analyze the duration of inundation and frequency of inundation for a given elevation. Additionally,
# the VulnToolkit calculates the elevation of the average and maximum high and low tides. 

# The overall purpose of the code is to analyze water level elevation data to accurately describe and summarize
# the tidal hydrology regime for salt marsh systems and groundwater regimes at individual water level recorders

#The second part of the code is to analyze the tidal hydrology regime of the creek water level recorders for a given site
# The creek water level recorders provide a baseline for the broader salt marsh site, while the individual water 
# level recorders provide more granular views of the groundwater or pool hydrology

# The second part of the code will calculate the low tide, high tide, higher high tide, and spring tide of the
# creek water level recorders


#Chapter 1: Library of packages for necessary work

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)
library(purrr)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)

#VulnToolkit is a special package that does all the meat and potatoes for this R-script
library(VulnToolkit)

#Chapter 2: Import the formatted water level elevation time series dataset

# The code removes the pesky "X" column created to label row names

wlr <- read.csv("Formatted Datasets\\WLR Formatted Dataset.csv") %>%
  select(-X)

glimpse(wlr)


#Chapter 3: Reformat Dataset to prepare for analysis

# Code selects only the Date.Time and any columns with "Creek" in the name,
# Reformats the Date.Time column,
# Creates a long datset to allow for dplyr for future analysis
wlr_format <- wlr %>%
  select(Date.Time, starts_with("Creek")) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  gather(key = WLR, value = elev, 
         #To make the code universal, the code gathers all columns that are "Creek..."
         colnames(select(., starts_with("Creek")))) %>%
  arrange(WLR, Date.Time)


#Chapter 4: Calculate the High and Low Tide for every 12.5 hours (every full tidal cycle)

wlr_tides <- wlr_format %>%
  group_by(WLR) %>%
  nest() %>%
  mutate(tides = HL(level = elev, time = Date.Time, period = 12.5)) %>%
  unnest(tides) %>%
  ungroup() %>%
  select(-data)


glimpse(wlr_tides)

write.csv(wlr_tides,
          paste("Formatted Datasets\\", Site_Name, "Creek Tidal Hydrology Dataset.csv", 
                collapse = ""))


#Chapter 5: Calculate the mean and standard error for hydrology metrics

#First, calculate the mean and standard error for the daily low and high tides

wlr_tides_stats <- wlr_tides %>%
  group_by(WLR, tide) %>%
  summarise(
    avg_elev = mean(level),
    se_elev = sd(level)/sqrt(n()),
    count = n()) %>%
  ungroup() %>%
  mutate(across(avg_elev:se_elev, ~round(., 3)))

#Second, calculate the mean and standard higher high tide for the daily tides

wlr_tides_stats2 <- wlr_tides %>%
  filter(tide2 == "HH") %>%
  group_by(WLR) %>%
  summarise(
    avg_hh = mean(level),
    se_hh = sd(level/sqrt(n())),
    count = n()) %>%
  ungroup() %>%
  mutate(across(avg_hh:se_hh, ~round(., 3)))


#Third, calculate the max tide observed throughout the dataset

wlr_tides_max <- wlr_format %>%
  group_by(WLR) %>%
  summarise(
    max_tide = max(elev)) %>%
  ungroup() %>%
  mutate(max_tide = round(max_tide, 3))



#Chapter 6: Wrap up all of the stats into one coherent data frame

creek_stats <- data.frame(matrix(nrow = length(unique(wlr_format$WLR)),
                                 ncol = 8)) %>%
  SetNames(c("Creek_WLR", "Mean_LT", "SE_LT", "Mean_HT", "SE_HT", "Mean_HHT", "SE_HHT", "Max_Tide")) %>%
  mutate(
    Creek_WLR = unique(wlr_format$WLR),
    
    Mean_LT = wlr_tides_stats$avg_elev[wlr_tides_stats$tide == "L"],
    SE_LT = wlr_tides_stats$se_elev[wlr_tides_stats$tide == "L"],
    
    Mean_HT = wlr_tides_stats$avg_elev[wlr_tides_stats$tide == "H"],
    SE_HT = wlr_tides_stats$se_elev[wlr_tides_stats$tide == "H"],
    
    Mean_HHT = wlr_tides_stats2$avg_hh,
    SE_HHT = wlr_tides_stats2$se_hh,
    
    Max_Tide = wlr_tides_max$max_tide)


write.csv(creek_stats,
          paste("Output Stats\\", Site_Name, "Creek Hydrology Stats.csv", 
                collapse = ""))


#Continue onto the Groundwater and Pool water level recorder analysis R code



