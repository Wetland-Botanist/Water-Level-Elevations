#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 4 - Analysis of Tidal Flooding on Constructed Sparrow Islands

#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire



#General Explanation of Code:
# The code is largely a product of the VulnToolkit package created by Troy Hill at the EPA. He devised a fantastic 
# method to analyze the duration of inundation and frequency of inundation for a given elevation. Additionally,
# the VulnToolkit calculates the elevation of the average and maximum high and low tides. 

# The overall purpose of the code is to analyze water level elevation data to accurately describe and summarise
# the tidal hydrology regime for salt marsh systems and groundwater regimes at individual water level recorders

#The fourth part of the code is to analyze the flooding conditions of the constructed sparrow islands. Sparrow islands
# are piles of excavated peat from restoration activities designed to provide refuge to nesting saltmarsh sparrows
# above the Mean Higher High Water elevation of the local creek

# Essentially, the code calculates the flooding frequency, flooding duration, elevation above mean high higher water, 
# and maximum time between flooding events (or a maximum dry period) to gauge island success. Most of the code is 
# similar to the groundwater level recorder analysis code with the functions used. Additionally, note that the 
# hydrology analysis is performed with the creek water level recorders, not the groundwater water level recorders. 


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
# The code removes the pesky "X" column created to label row names, then 
# formats the Date.Time column to a POSIXct format

wlr <- read.csv(paste("Formatted Datasets\\", Site_Name, "WLR Formatted Dataset.csv", 
                      collapse = "")) %>%
  select(-X) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S'))

glimpse(wlr)


#The second input is a dataframe that contains each groundwater/pool water level recorder
# with associated marsh platform elevation, root zone elevation, and associated creeks

#USER INPUT NEEDED:

sparrow <- read.csv("Input Data\\Example Sparrow Island Elevations.csv")

glimpse(sparrow)



#The third input is the tidal hydrology (low tide, high tide, higher high tide) dataset of the creek water level 
# recorders. The creek tidal hydrology dataset will be used to calculate the high tide flooding frequency of the
# the sparrow islands.

tides <- read.csv(paste("Formatted Datasets\\", Site_Name, "Creek Tidal Hydrology Dataset.csv", 
                                  collapse = "")) %>%
  select(-X)

glimpse(tides)




#Chapter 3: Reduce formatted water level dataset to the Creeks, assign each sparrow island to a creek water level recorder

wlr_format <- wlr %>%
  #Select only the creek water level recorders
  select(Date.Time, starts_with("Creek")) %>%
  gather(key = WLR, value = elev, 
         colnames(select(., starts_with("Creek")))) %>%
  #Combines the sparrow island dataset with the formatted creek WLR dataset, so now
  #each sparrow island is associated with a full water level elevation dataset in the monitoring period
  merge(select(sparrow, Island, WLR, Elevation), by = "WLR") %>%
  select(Island, Elevation, Date.Time, WLR, elev) %>%
  rename(sparrow_elev = "Elevation") %>%
  arrange(Island, Date.Time)


#Chapter 4: Calculate hydrology metrics for the sparrow island elevations

#Metric 1: Flooding Duration (%)

#Calculate flooding duration of each sparrow island using summarise(), fld.dur functions

sparrow_flood <- wlr_format %>%
  group_by(Island) %>%
  summarise(
         island_flood = fld.dur(z = sparrow_elev[1],
                                    level = elev) * 100,
         island_elev = sparrow_elev[1]) %>%
  ungroup() %>%
  mutate(island_flood = as.numeric(island_flood),
           island_flood = round(island_flood, 1))


#Metric 2: High Tide Flooding Frequency

#Reduce the tides dataset to only the high tides ("H" in the tides column)

tides <- filter(tides, tide == "H")


#Calculate high tide flooding frequency of the sparrow islands with summarise(), fld.frq() functions
sparrow_freq <- tides %>%
  merge(select(sparrow, Island, Elevation, WLR), by = "WLR") %>%
  group_by(Island) %>%
  summarise(island_freq = fld.frq(z = Elevation[1], 
                                  ht = tides$level),
            island_elevation = Elevation[1]) %>%
  ungroup() %>%
  mutate(island_freq = round(island_freq, 2) * 100)


#Metric 3: Maximum Time Period Between Island Flooding Events (Maxium Dry Period)

# One of the main metrics of the sparrow construction is remaining dry (or not flooding) for at least several weeks
# to allow for sparrow fledglings to hatch and retreat from flooding

#Calculate the max dry period with the diff.date() function, which calculates the difference in two time periods
# and can be used as a substitute for a for loop when processing through an entire column.
# We want to calculate the maximum difference between two chronological time periods when the island was flooded

sparrow_dry <- wlr_format %>%
  group_by(Island) %>%
  #Sets the first and last water elevation to 2.5 NAVD88 m to artificially bound the analysis
  mutate(elev = ifelse(row_number() == 1, 2.5, 
                       ifelse(row_number() == nrow(.), 2.5, elev))) %>%
  #Keeps only the times that the island was flooded
  filter(elev > sparrow_elev) %>%
  #Calculates maximum time difference through the entire dataset
  summarise(island_dry = max(diff.Date(Date.Time))/ddays(1)) %>%
  mutate(island_dry = as.numeric(island_dry),
         island_dry = round(island_dry, 2)) %>%
  ungroup()




#Chapter 4: Wrap up the Sparrow Island Stats into one useful table
sparrow_stats <- sparrow %>%
  #Series of merging tables to the original sparrow island table,
  # For ease of reading, I broke up the code for each hydrology metric
  merge(select(sparrow_flood, Island, island_flood), by = "Island") %>%
  merge(select(sparrow_freq, Island, island_freq), by = "Island") %>%
  merge(select(sparrow_dry, Island, island_dry), by = "Island") %>%
  rename(Flooding_Duration_Percent = island_flood,
         HT_Frequency_Percent = island_freq,
         Max_Dry_Period_days = island_dry) %>%
  select(Island, Groundwater_WLR, WLR, Elevation,
         Flooding_Duration_Percent, HT_Frequency_Percent, Max_Dry_Period_days)

write.csv(sparrow_stats,
          paste("Output Stats\\", Site_Name, "Sparrow Island Hydrology Stats.csv",
                collapse = ""))
















