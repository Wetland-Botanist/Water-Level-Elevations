#Project:Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 1 - Format the Water Level Recorder Dataset


#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire



#General Explanation of Code:
# The code is largely a product of the VulnToolkit package created by Troy Hill at the EPA. He devised a fantastic 
# method to analyze the duration of inundation and frequency of inundation for a given elevation. Additionally,
# the VulnToolkit caluclates the elevation of the average and maximum high and low tides. 

# The overall purpose of the code is to analyze water level elevation data to accurately describe and summarise
# the tidal hydrology regime for salt marsh systems and groundwater regimes at individual water level recorders

#The first part of the code is to properly format the code to a single lunar cycle


#Chapter 1: Library of packages for necessary work

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)


#Chapter 2: Import the water level elevation time series dataset

wlr <- read.csv("Input Data\\Essex\\Essex_2023_Compiled.csv")

glimpse(wlr)


WLR_Site <- "Essex"


#Chapter 3: Format the Dataset

#Necessary steps to format the dataset

# Convert the 'Date.Time' column to a POSIT data type to make data calculations in 'lubridate' package easier
# Reduce the entire dataset to 30 day lunar tidal cycle

#Step 1: Conversion of Date.Time

wlr <- wlr %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%m/%d/%Y %H:%M'))

  glimpse(wlr)

#Step 2: Reduce the dataset to the 30 day lunar tidal cycle

#For research methods for our lab at University of New Hampshire, we normally deploy the WLRs for 
# roughly 30 - 40 days. We usually are not able to collect the WLRs perfectly at the end of the 
# lunar tidal cycle. We decided to subset the WLR data set to roughly a spring tidal cycle (~30 days). 
# We calculate the exact middle date in the monitoring period and +/- 15 days for it. 

lunar_cycle <- data.frame(matrix(nrow = 1, ncol = 6)) %>%
  setNames(c("Site", 'Year', 'Start_Date', 'End_Date', 'Middle_Date', 'Deployment_Time')) %>%
  mutate( Site = WLR_Site,
          Year = year(wlr$Date.Time[1]),
    
    #Deployment time is calculated as the total duration of the water level elevation dataset
    #Deployment time is calculated with the as.duration() function that converts the entire dataset to the number of seconds
    #Deployment time is then divided by the number of days to calculate the depoyment time in days
        Deployment_Time = as.duration(wlr$Date.Time[1] %--% wlr$Date.Time[nrow(wlr)])/ddays(1),
        Deployment_Time = round(Deployment_Time, 2),
    
    #Middle, Start, and End dates are calculated with the lubridate() package with quick mathematics of dates
    #Currently, start and end dates are calculated as +/- 15 days from the middle date
         Middle_Date = Deployment_Time + wlr$Date.Time[1],
         Start_Date = Middle_Date - days(15),
         End_Date = Middle_Date + days(15))

write.csv(lunar_cycle,
          "Formatted Datasets\\WLR Deployment Metadata.csv")


#Next filter the wlr dataset within the parameteres of the start and end dates

wlr_format <- wlr %>%
  filter(Date.Time > lunar_cycle$Start_Date,
         Date.Time < lunar_cycle$End_Date)


write.csv(wlr_format,
          "Formatted Datasets\\WLR Formatted Dataset.csv")


#Continue onto the Creek Hydrology Analysis code to describe the tidal regime for each creek WLR

