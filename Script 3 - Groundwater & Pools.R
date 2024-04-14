#Project:Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 3 - Analysis of Tidal Hydrology Regime of the Groundwater and Pool Water Level Recorders

#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire



#General Explanation of Code:
# The code is largely a product of the VulnToolkit package created by Troy Hill at the EPA. He devised a fantastic 
# method to analyze the duration of inundation and frequency of inundation for a given elevation. Additionally,
# the VulnToolkit calculates the elevation of the average and maximum high and low tides. 

# The overall purpose of the code is to analyze water level elevation data to accurately describe and summarise
# the tidal hydrology regime for salt marsh systems and groundwater regimes at individual water level recorders

#The third part of the code is to analyze the tidal hydrology regime of the creek water level recorders for a given site
# The creek water level recorders provide a baseline for the broader salt marsh site, while the individual water 
# level recorders provide more granular views of the groundwater or pool hydrology

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
# The code removes the pesky "X" column created to label row names

wlr <- read.csv(paste("Formatted Datasets\\", Site_Name, "WLR Formatted Dataset.csv", 
                      collapse = "")) %>%
  select(-X) 

glimpse(wlr)


#The second input is a dataframe that contains each groundwater/pool water level recorder
# with associated marsh platform elevation, root zone elevation, and associated creeks

#USER INPUT NEEDED:

elevs <- read.csv("Input Data\\Example Marsh and Root Zone Elevations.csv")

glimpse(elevs)


#The third input is the high and low water elevations and times for each creek WLR

tides_creek <- read.csv(paste("Formatted Datasets\\", Site_Name, "Creek Tidal Hydrology Dataset.csv", 
                              collapse = "")) %>%
  select(-X) %>%
  mutate(Date.Time = as.POSIXct(time, format = '%Y-%m-%d %H:%M:%S'))


#Chapter 3: Format the water level elevation time series dataset

# Code selects only the Date.Time and any columns with "Creek" in the name,
# re-formats the Date.Time column, and creates a long dataset to allow for dplyr for future analysis

wlr_format <- wlr %>%
  select(Date.Time, !starts_with("Creek")) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S')) %>%
  gather(key = WLR, value = elev, 
         #To make the code universal, the code gathers all columns that are not "Creek..." or "Date.Time"
         colnames(select(., -starts_with("Creek"), -Date.Time))) %>%
  arrange(WLR, Date.Time)


#Chapter 4: Calculate the High and Low Tide for every 12.5 hours (every full tidal cycle)

# One of the conundrums with using the VulnToolKit package for groundwater and pool water elevations, 
# is that it struggles to determine when and when low and high tides take place. At times, 
# the package does not locate a single high or low tide during a 12.5 hour period.

# To get around this, we will calculate the high and low tides based on the timing of the tides
# from the creek WLRs. The dates and times for each low and high tide will be extracted from the 
# dataset created in the previous R script. The dates and times will then be used to determine
# the low and high tides for each groundwater and pool water level recorder. 

# Due to small timing differences between high and low tides between the creek and groundwater
# loggers, there is a 1 hour window before and after the creek low/high tide to calculate the
# low/high tide for the groundwater loggers. 

#Step 1: Determine and Extract the Date - Times of tides from Creek

tides_creek_seq <- tides_creek %>%
  #Calculate the time 1 hr before and after each low/high tide
  mutate(seq_low = Date.Time - hours(2),
         seq_high = Date.Time + hours(2)) %>%
  #Group and nest each Creek WLR and tide type (low, high)
  group_by(Date.Time, WLR, tide) %>%
  nest() %>%
  #Create a 1 hour buffer (10 min intervals) before and after each low/high tide (2 hour total buffer),
  #Use the map function to create sequence independently for each WLR, tide type
  summarise(tide_seq = map(.x = data,
                           ~seq(from = .x$seq_low, to = .x$seq_high, by = '10 mins'))) %>%
  unnest(tide_seq) %>%
  ungroup() %>%
  #Rename columns for merging 
  rename(Associated_Creek = "WLR",
         Date_Group = "Date.Time",
         Date.Time = "tide_seq")


#Step 2: Using the 1 hour sequences from the Creek tides, extract low and high tides of groundwater WLRs

wlr_tides <- wlr_format %>%
  # Merge the Associated Creek column of the elevations dataset to the water elevations dataset
  merge(select(elevs, WLR, Associated_Creek), by = "WLR") %>%
  group_by(WLR) %>%
  #Merge the 1 hour high/low tide sequences with the groundwater elevations dataset
  # based on the Associated_Creek and Date-Time columns
  merge(tides_creek_seq, by = c("Date.Time", "Associated_Creek")) %>%
  ungroup() %>%
  group_by(WLR, Date_Group, tide) %>%
  #Calculate the high tide as the maximum water elevation in the time buffer,
  #Calculate the low tide as teh minimum water elevation in the time buffer
  mutate(tide_elev = ifelse(tide == "H", max(elev),
                            ifelse(tide == "L", min(elev), elev))) %>%
  arrange(WLR, Date.Time) %>%
  filter(elev == tide_elev) %>%
  ungroup() %>%
  #Remove duplicates from the dataset
  distinct(WLR, elev, tide, Date_Group, .keep_all = TRUE) %>%
  select(WLR, Date.Time, tide, elev)

glimpse(wlr_tides)


#Step 3: calculate the mean and standard error for the daily low and high tides

wlr_tides_stats <- wlr_tides %>%
  group_by(WLR, tide) %>%
  summarise(
    avg_elev = mean(elev),
    se_elev = sd(elev)/sqrt(n()),
    count = n()) %>%
  ungroup() %>%
  mutate(across(avg_elev:se_elev, ~round(., 3)))



#Chapter 6: Calculate the mean and standard error of higher high tide for daily tides, Maximum tide

# The process of determining the Higher High tide for each 24 hour day is similar to the high and low tides
# Except, instead of every 12 hours, we are interested in the full 24 hours!

wlr_tides_hh <- wlr_tides %>%
  #Removes low tides from consideration
  filter(tide == "H") %>%
  mutate(month = months(Date.Time),
         day = day(Date.Time)) %>%
    group_by(WLR, month, day) %>%
  #Keeps only the maximum high tide elevation (of the 2 high tides) per day for each WLR
      filter(elev == max(elev)) %>%
        ungroup() %>%
  group_by(WLR) %>%
  #Calculates mean and standard error for higher high tide
    summarise(avg_hh = mean(elev, na.rm = TRUE),
            se_hh = sd(elev, na.rm = TRUE)/sqrt(n())) %>%
ungroup()




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
  SetNames(c("WLR", "Mean_LT", "SE_LT", "Mean_HT", "SE_HT", "Mean_HHT", "SE_HHT", "Max_Tide")) %>%
  mutate(
    WLR = unique(wlr_format$WLR),
    #Add the mean and standard error of low tide elevation
    Mean_LT = wlr_tides_stats$avg_elev[wlr_tides_stats$tide == "L"],
    SE_LT = wlr_tides_stats$se_elev[wlr_tides_stats$tide == "L"],
    #Add the mean and standard error of high tide elevation
    Mean_HT = wlr_tides_stats$avg_elev[wlr_tides_stats$tide == "H"],
    SE_HT = wlr_tides_stats$se_elev[wlr_tides_stats$tide == "H"],
    #add the mean and standard error of higher high tide elevation
    Mean_HHT = wlr_tides_hh$avg_hh,
    SE_HHT = wlr_tides_hh$se_hh,
    #Add the maximum tide elevation
    Max_Tide = wlr_tides_max$max_tide)


write.csv(groundwater_stats,
          "Output Stats\\Groundwater Hydrology Stats.csv")





#Chapter 8: Assess the Flooding Duration and Flooding Duration of the Marsh Platform Elevation

#First, assess the flooding duration of the marsh platform and root zone elevation

#To accomplish using the fld.dur() function, which returns the flooding duration (as a percent of monitoring time)
#Additionally, we will create two new columns that "pulls" the marsh platform and root zone elevations from
#the 'elevs' dataframe.

wlr_flood <- wlr_format %>%
  merge(select(elevs, WLR, Marsh_Elevation, Rootzone_Elevation),  by = "WLR") %>%
  group_by(WLR) %>%
  summarise(marsh_flood = fld.dur(z = Marsh_Elevation[1],
                                    level = elev) * 100,
            root_flood = fld.dur(z = Rootzone_Elevation[1],
                                      level = elev) * 100,
            
            marsh_elev = Marsh_Elevation[1],
            
            
            root_elev = Rootzone_Elevation[1]) %>%
ungroup() %>%
  mutate(across(marsh_flood:root_flood, ~round(., 1)))



# Reformat the flooding duration for easier input into the overall statistics table later on (Chapter 9)

wlr_flood_format <- wlr_flood %>%
  select(-marsh_elev, -root_elev) %>%
  gather(key = zone, value = Flood_Duration_Percent, marsh_flood, root_flood) %>%
  mutate(zone = ifelse(zone == "marsh_flood", "Marsh Platform", "Root Zone")) %>%
  arrange(WLR, zone)


#Second, assess the high tide flooding frequency of the marsh platform and root zone elevations

#We calculate the high tide flooding frequency (%) with the fld.frq() function
# The function requires two inputs:   
# (1) z = tidal hydrology (water elevations) from the data set
# (2) ht = list of high tides

# We will pull the ht (list of high tides) from the wlr_tides dataset we created earlier in the code

wlr_freq <- wlr_tides %>%
  filter(tide == "H") %>%
  select(-tide2, -Date.Time) %>%
  merge(select(elevs, WLR, Marsh_Elevation, Rootzone_Elevation),  by = "WLR") %>%
  group_by(WLR) %>%
  summarise(marsh_freq = fld.frq(z = Marsh_Elevation[1], ht = level) *100,
         
         root_freq = fld.frq(z = Rootzone_Elevation[1], ht = level) * 100,
         
         marsh_elev = Marsh_Elevation[1],
         
         root_elev = Rootzone_Elevation[1]) %>%
  
  ungroup() %>%
  #Flooding frequency columns are in a 'list' format, reformat to double and round to 2 decimal points
  mutate(across(c(marsh_freq, root_freq), ~as.numeric(.)),
         across(c(marsh_freq, root_freq), ~round(., 1)))

glimpse(wlr_freq)


# Reformat the high tide flooding frequency for easier input into the overall statistics table later on (Chapter 9)
wlr_freq_format <- wlr_freq %>%
  select(-marsh_elev, -root_elev) %>%
  gather(key = zone, value = HT_Frequency_Percent, marsh_freq, root_freq) %>%
  mutate(zone = ifelse(zone == "marsh_freq", "Marsh Platform", "Root Zone")) %>%
  arrange(WLR, zone)



#Chapter 9: Wrap all of the data analysis into one nice table, calculate mean drainage metric

#Reformat the elevs dataset to make it easier to cbind() to the flooding duraiton and frequency dataframes
elevs_format <- elevs %>%
  select(WLR, Marsh_Elevation, Rootzone_Elevation) %>%
  gather(key = zone, value = elevation, Marsh_Elevation, Rootzone_Elevation) %>%
  mutate(zone = ifelse(zone == "Marsh_Elevation", "Marsh Platform", "Root Zone")) %>%
  arrange(WLR, zone)
  


#Wrap up all the stats into one nice dataset

wlr_stats <- cbind(elevs_format,
                   select(wlr_flood_format, Flood_Duration_Percent),
                   select(wlr_freq_format, HT_Frequency_Percent)) %>%
  #Pull in the mean low tide from the groundwater_stats table through merge()
  merge(select(groundwater_stats, WLR, Mean_LT), by = "WLR") %>%
  #Calculate the mean drainage below the marsh platform and root zone elevations
  mutate(Drainage = elevation - Mean_LT)


write.csv(wlr_stats,
          paste("Output Stats\\", Site_Name, "Groundwater WLR Hydrology Elevation Stats.csv", 
                collapse = ""))

#If applicable to the project, move onto the Sparrow Island Analysis R Code. If constructed sparrow islands
# are not a part of the project, move onto the Graphing R Code. 

