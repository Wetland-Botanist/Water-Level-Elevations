#Project:Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 3 - Analysis of Tidal Hydrology Regime of the Groundwater and Pool Water Level Recorders

#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire



#General Explanation of Code:
# The code is largely a product of the VulnToolkit package created by Troy Hill at the EPA. He devised a fantastic 
# method to analyze the duration of inundation and frequency of inundation for a given elevation. Additionally,
# the VulnToolkit caluclates the elevation of the average and maximum high and low tides. 

# The overall purpose of the code is to analyze water level elevation data to accurately describe and summarise
# the tidal hydrology regime for salt marsh systems and groundwater regimes at individual water level recorders

#The third part of the code is to analyze the tidal hydrology regime of the creek water level recoders for a given site
# The creek water level recoders provide a baseline for the broader salt marsh site, while the individaul water 
# level recoders provide more granular views of the groundwater or pool hydrology

# The second part of the code will calculate the low tide, high tide, higher high tide, and spring tide of the
# individual groundwater and pool water level recorders. Additionally, metrics of flooding frequency, flooding duration,
# and low tide drainage will be calculated for the marsh platform and root zone elevations. 


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

#First, input the 
# The code removes the pesky "X" column created to label row names, then selects
# only the Date.Time column and the non-creek water level recorder columns

wlr <- read.csv("Formatted Datasets\\WLR Formatted Dataset.csv") %>%
  select(-X) 

glimpse(wlr)


#The second input is a dataframe that contains each groundwater/pool water level recorder
# with associated marsh platform elevation, root zone elevation, and associated creeks


elevs <- read.csv("Input Data\\Essex\\Essex_2023_WLR_Elevations.csv")

glimpse(elevs)



#Chapter 3: Format the water level elevation time series dataset

# Code selects only the Date.Time and any columns with "Creek" in the name,
# Reformats the Date.Time column,
# Creates a long datset to allow for dplyr for future analysis


wlr_format <- wlr %>%
  select(Date.Time, !starts_with("Creek")) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  gather(key = WLR, value = elev, colnames(.)[2]:colnames(.[ncol(.)])) %>%
  arrange(WLR, Date.Time)


#Chapter 4: Calculate the High and Low Tide for every 12.5 hours (every full tidal cycle)

wlr_tides <- wlr_format %>%
  group_by(WLR) %>%
  nest() %>%
  mutate(tides = map(.x = data,
                     ~HL(level = .x$elev, time = .x$Date.Time, period = 12.5))) %>%
  unnest(tides) %>%
  ungroup() %>%
  select(-data) %>%
  rename(Date.Time = time) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S'))


glimpse(wlr_tides)


#One of the issues with analyzing tidal hydrology for groundwater and pools is that the VulnToolkit
#package has a hard time differentiating high tides when the water level is relatively flat over a long
# period of time (say groundwater during low tide). So, we have to manually correct it


#Calculate the max high tide for every 12 hours, which is differentiated for every AM (12AM - 12 PM) and PM
wlr_tides_high <- wlr_tides %>%
  filter(tide == "H") %>%
  mutate(
    month = month(Date.Time),
    day = day(Date.Time),
    daylight = ifelse(am(Date.Time), "AM", "PM")) %>%
  group_by(WLR, month, day, daylight, tide) %>%
  summarise(
    level = max(level),
    rank = which.max(level),
    time = Date.Time[rank],
    tide = tide[rank]) %>%
  ungroup()

#Calculate the minimum low tide for every 12 hours, which is differentiated for every AM (12AM - 12PM)
wlr_tides_low <- wlr_tides %>%
  filter(tide == "L") %>%
  mutate(
    month = month(Date.Time),
    day = day(Date.Time),
    daylight = ifelse(am(Date.Time), "AM", "PM")) %>%
  group_by(WLR, month, day, daylight, tide) %>%
  summarise(
    level = min(level),
    rank = which.min(level),
    time = Date.Time[rank],
    tide = tide[rank]) %>%
  ungroup()


#Combine the datasets for the full, more accurate dataset of low and high tides

wlr_tides_compiled <- rbind(wlr_tides_high, wlr_tides_low) %>%
  arrange(WLR, time)

#Lastly, calculate the mean and standard error for the daily low and high tides

wlr_tides_stats <- wlr_tides_compiled %>%
  group_by(WLR, tide) %>%
  summarise(
    avg_elev = mean(level),
    se_elev = sd(level)/sqrt(n()),
    count = n()) %>%
  ungroup() %>%
  mutate(across(avg_elev:se_elev, ~round(., 3)))



#Chapter 6: Calculate the mean and standard error of higher high tide for daily tides, Maximum tide

# The process of determining the Higher High tide for each 24 hour day is similar to the high and low tides
# Except, instead of every 12 hours, we are interested in the full 24 hours!

wlr_tides_stats2 <- wlr_tides %>%
  filter(tide2 == "HH") %>%
  group_by(WLR) %>%
  summarise(
    avg_hh = mean(level),
    se_hh = sd(level/sqrt(n())),
    count = n()) %>%
  ungroup() %>%
  mutate(across(avg_hh:se_hh, ~round(., 3)))

#Calculates the maximum observed water level elevation from the original wlr_format dataset

wlr_tides_max <- wlr_format %>%
  group_by(WLR) %>%
  summarise(
    max_tide = max(elev)) %>%
  ungroup() %>%
  mutate(max_tide = round(max_tide, 3))



#Chapter 7: Wrap up all of the stats into one coherent data frame

#All of the groundwater and pool water level recorders should be arranged alphabetically throughout
# the wlr_tides_compiled, wlr_tides_high, and wlr_tides_max datasets, making joining columns together
# super easy!

groundwater_stats <- data.frame(matrix(nrow = length(unique(wlr_format$WLR)),
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
          "Output Stats\\Groundwater Hydrology Stats.csv")



#Chapter 8: Assess the Flooding Duration and Flooding Duration of the Marsh Platform Elevation

#First, assess the flooding duration of the marsh platform elevation

wlr_flood <- wlr_format %>%
  merge(select(elevs, WLR, Marsh_Elevation, Rootzone_Elevation),  by = "WLR") %>%
  group_by(WLR) %>%
  nest() %>%
  mutate(Flooding = map(.x = data,
                        ~fld.dur(z = unique(.x$Marsh_Elevation[.x$WLR == WLR]),
                                level = .x$elev)))




























