#Analysis of an individual water level recorder for salt marsh monitoring

#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire

#General Explanation of Code:
# The code is largely a product of the VulnToolkit package created by Troy Hill at the EPA. He devised a fantastic 
# method to analyze the duration of inundation and frequency of inundation for a given elevation. Additionally,
# the VulnToolkit caluclates the elevation of the average and maximum high and low tides. 

#Jenny Gibson and Grant McKown added specifications to adapt the code to the realities of monitoring 
# relatively stagnant tidal conditions of mega-pools and saturated pannes. The code analyzes the hydrology
# within only a given spring tidal cycle (~ 30 days) of monitoring, but this can easily be changed. 

# Lastly, all of the calculated hydrology statistics for a given water level recorder at a certain 
# elevation are combined into one CSV file. The graphs of the tidal cycle for the water level recorder 
# and the flooding frequency at a certain elevation are created. 


#Required Inputs: 
#      (1) CSV of post-processed HOBO data files that include the following columns:
#         (a) Date - Time with the format: Month\Day\Year Hour:Min (Military Time)
#                  - Column name should be "Date.Time"
#         (b) Water elevation - elevation of the tidal waters (reference to a datum like NAVD 88m)
#                             - Column name should be the site name
#     (2) Elevation of the Marsh Platform (or pool) for analysis



#Organization of Code:
# Step 1: Required library packages and user inputs
# Step 2: Import and format water level recorder data
# Step 3: Subset water level recorder data set to 30 day tidal cycle (Optional) 
# Step 4: Calculate low and high tide statistics, Graph tidal hydrology
# Step 5: Calculate flood duration and flood frequency of a given elevation, graph flood frequency
# Step 6: Compile tidal hydrology statistics into a single CSV for export


#Step 1: Required library packages and user inputs

#Input the Library required for the R-script. 
#If you don't have the script, install the packages

#Clears everything from your environment. Just good to do...
rm(list = ls())

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)

#patchwork and ggplot are great plotting packages (making graphs...)
library(patchwork)
library(gridExtra)
library(ggfortify)
library(ggplot2)

#VulnToolkit is a special package that does all the meat and potatoes for this R-script
library(VulnToolkit)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)



#Since the code only runs one water level recorder at a time, we figured it might be easier for the user
# to plug all of the required inputs in at the beginning of the code


# Input 1: Water level recorder data set
# The water level recorder data set can be composed of either a single water level recorder or a compilation
# of numerous water level recorders of the same time period

WLR <- read.csv("Input Data\\Ipswich_WLR_Area2_Compiled_2023.csv")

colnames(WLR)


#Input 4: Sparrow Nesting Island Elevations 

Sparrow_Islands <- read.csv("Input Data\\Ipswich_SparrowIsland_Elevations.csv")

#Input 8: File path of Sparrow Island Hydrology Statistics

Sparrow_Stats_Output <- "Output Data\\Ipswich_SparrowIsland_Stats_Area2.csv"



#Before we move on, the Date.Time column is not in the correct format. 
#For the VulnToolkit package to work, we need to convert the format from "Factor" to "Posix", which is the standard date and time
#We convert it using the as.posixct() function
#In the format subargument, we tell the function what the format of our date is.

WLR$Date.Time <- as.POSIXct(WLR$Date.Time, format = '%m/%d/%Y %H:%M')

glimpse(WLR)


# Sparrow Island Flooding Analysis


# The purpose of this section of code is to calculate:
# (1) Flooding Duration & Frequency of Flooding for the Elevation of the Sparrow Island
# (2) Longest Duration the Sparrow Island is not flooded

island_stats <- data.frame(matrix(ncol = 5, nrow = nrow(Sparrow_Islands)))

colnames(island_stats) <- c("Island", "Elevation", "Flood_Frequency", "Flood_Duration", "Dry_Period")

island_stats$Island <- Sparrow_Islands$Island

island_stats$Elevation <- round(Sparrow_Islands$Elevation, 3)

glimpse(island_stats)

# Create the flooding sequence of the Creek Water Level Recorder

creek.HL <- HL(level = WLR$Creek, time = WLR$Date.Time, period = 12.5) %>%
  filter(tide == "H") %>%
  select(level)

creek.seq <- seq(min(WLR$Creek), max(WLR$Creek), 0.005)

creek.seq <- data.frame(creek.seq) %>%
  rename(water_elev = creek.seq)

creek.seq$frq <- fld.frq(z = creek.seq$water_elev, ht = creek.HL$level)


# Calculate the Flooding Frequency and Flooding Duration for the Sparrow Elevations

island_stats <- island_stats %>%
  group_by(Island) %>%
  mutate(Flood_Frequency = creek.seq$frq[creek.seq$water_elev == Closest(creek.seq$water_elev, Elevation)] * 100,
         Flood_Duration = fld.dur(z = Elevation, WLR$Creek) * 100) %>%
  mutate(Flood_Frequency = round(Flood_Frequency, 2),
         Flood_Duration = round(Flood_Duration, 1)) %>%
  ungroup()

glimpse(island_stats)


# Calculate the longest period the sparrow island was dry (aka not flooded)


WLR$Creek[1] <- 2.5

island_stats <- island_stats %>%
  group_by(Island) %>%
  mutate(Dry_Period = max(diff.Date(WLR$Date.Time[WLR$Creek > Elevation]))/1440) %>%
  mutate(Dry_Period = as.numeric(Dry_Period)) %>%
  ungroup()

glimpse(island_stats)


write.csv(island_stats,
          Sparrow_Stats_Output)

