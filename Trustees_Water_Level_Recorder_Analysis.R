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

WLR <- read.csv("Input Data\\Fairhill\\Fairhill_WLR_Combined_R_csv.csv")

colnames(WLR)

Elevation <- read.csv("Input Data\\Fairhill\\Fairhill_WLR_Elevations.csv")

unique(Elevation$WLR)

#Input 2: Column name of the water level recorder that will be analyzed

WLR_name <- "Southern_Drainage"

WLR_GraphName <- "Southern_Drainage"

# Input 3: Elevation of the marsh platform that will be used for inundation and flooding frequency stats
  # Elevation of the marsh platform should be given in NAVD88 m
  # Elevation of the marsh platform is collected from elevation surveys of the marsh surface
  # Additionally, the same statistics will be calculated on the root zone, estimated as 5 cm below the 
  # marsh platform

#Input 3: Elevations for Water Level Recorder Analysis

PlatformElev <- Elevation$Marsh_Elevation[Elevation$WLR == WLR_name]
  
  
RootElev <- (PlatformElev - 0.05)

#Input 4: Sparrow Nesting Island Elevations (OPTIONAL)

#IslandElev1 <- 1.503

#IslandElev2 <- 1.65

#IslandElev3 <- 1.486


#Input 4: File path of the exported CSV file of the tidal hydrology statistics

Output <- "Output Data\\Fairhill_SouthernDrainage_stats.csv"
  
#Input 5: File path of the graph of the tidal hydrology over time

Tidegraph_output <- "Output Figures\\Fairhill_SouthernDrainage_Figure.jpg"

#Input 6: File path of the graph of flooding frequency of the water level recorder at the marsh platform elevation

Frequencygraph_output <- "Output Figures\\Fairhill_SouthernDrainage_FrequencyFigure.jpg"







#Step 2: Format water level recorder data

#Before we move on again, we are going to do one thing that will make our lives so much easier! 
#Instead of having to switch throughout the code the name of the column with our water level elevations,
#we are going to rename the column to "elev" at the very beginning of this code. 

WLR <- rename(WLR, 
       elev = WLR_name)

glimpse(WLR)

#Before we move on, the Date.Time column is not in the correct format. 
    #For the VulnToolkit package to work, we need to convert the format from "Factor" to "Posix", which is the standard date and time
    #We convert it using the as.posixct() function
    #In the format sub-argument, we tell the function what the format of our date is.

WLR$Date.Time <- as.POSIXct(WLR$Date.Time, format = '%m/%d/%Y %H:%M')

glimpse(WLR)



# Step 3: Subset water level recorder data set to 30 day tidal cycle (Optional)

# For research methods for our lab at University of New Hampshire, we normally deploy the WLRs for 
  # roughly 30 - 40 days. We usually are not able to collect the WLRs perfectly at the end of the 
  # lunar tidal cycle. We decided to subset the WLR data set to roughly a spring tidal cycle (~30 days). 
  # We calculate the exact middle date in the monitoring period and +/- 15 days for it. 

# This step is completely optional for a user if they wish to analyze their entire data set, especially 
# water level data that was intentionally deployed for an entire season or designed to capture king tide
# events

#First, we determine the middle date and then calculate the beginning and ending date and times of the
  # subsetted lunar tidal cycle data set

DeploymentTime <- as.duration(WLR$Date.Time[1] %--% WLR$Date.Time[nrow(WLR)])/ddays(1)

DeploymentTime

round(DeploymentTime / 2, 0)

Time.Middle <- WLR$Date.Time[1] + days(round(DeploymentTime / 2, 0))

glimpse(Time.Middle)

Time.Begin <- Time.Middle - days(15)

Time.End <- Time.Middle + days(15)

glimpse(Time.Begin)

glimpse(Time.End)


#Second, we filter out all water level recordings that are not within the Lunar Cycle Timeframe

WLR.filter <- WLR %>%
  #filter((Date.Time > Time.Begin) & (Date.Time < Time.End)) %>%
  select(Date.Time, elev)

MonitoringTime <- as.duration(WLR.filter$Date.Time[1] %--% WLR.filter$Date.Time[nrow(WLR.filter)])/ddays(1)

MonitoringTime

#Great! We now have ~roughly~ the true lunar tidal cycle to calculate our flood frequency, water elevations, and more!



# Step 4: Graph the Tidal Cycle with the subsetted water level recorder data frame
# The water elevation is shown in blue, marsh platform elevation in black, and root zone elevation in green

WLR.graph <- WLR.filter

colnames(WLR.graph)[2] <- WLR_GraphName

#colnames(WLR.graph)[2] <- "Ditches Creek"

WLR.graph <- WLR.graph %>%
  gather(key = "WLR", "Elevation", -Date.Time)

#WLR.graph$WLR <- factor(WLR.graph$WLR, levels = c("Ditches Creek", WLR_GraphName))


glimpse(WLR.graph)

Tidegraph <- ggplot(WLR.graph, aes(x = Date.Time, y = Elevation, colour = WLR)) + 
  geom_hline(yintercept = PlatformElev, linetype="dashed", color = "black", size = 1) +
  geom_hline(yintercept = RootElev, linetype = "dashed", color = "green", size = 1) +
# geom_hline(yintercept = IslandElev1, linetype = "dashed", colour = "brown", size = 1) +
#  geom_hline(yintercept = IslandElev2, linetype = "dashed", colour = "brown", size = 1) + 
  #geom_hline(yintercept = IslandElev3, linetype = "dashed", colour = "brown", size = 1) + 
  geom_line(size = 1) + 
  scale_color_manual(values = c("blue", "orange")) + 
  labs(x = "", y = "Water Elevation (NAVD88 m)") +
  scale_x_datetime(limits = c(min(WLR.graph$Date.Time), max(WLR.graph$Date.Time)),
                   breaks = "3 days",
                   date_labels = "%m-%d") +
  scale_y_continuous(breaks = seq(0.8, 
                                  round(max(WLR.graph$Elevation, na.rm = TRUE) + 0.1, 1),
                                  0.1)) +
  coord_cartesian(ylim = c(0.8, max(WLR.graph$Elevation) + 0.1)) +
  theme_bw() +
  theme(
    legend.position = c(0.125, 0.95),
    legend.text = element_text(size = 18),
    legend.title = element_blank(),
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0, size = 16, colour = "black"),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16, colour = "black"))

Tidegraph

ggsave(Tidegraph_output,
       Tidegraph, height = 806, width = 1536, 
       units = "px", dpi = 125)


rm(WLR.graph, Tidegraph)



# Step 4: Calculate low and high tide statistics, Graph tidal hydrology

#First, we want to figure out when each high and low tide is.
#We use the HL() function of the VulnToolkit to do this. 
# All we need to do is supply the function with two pieces of information:
    # level = water elevation column,
    # time = Date - Time column
# The function will determine the high/low tide for every 12.5 hours (full tidal cycle) 

WLR.HL <- HL(level = WLR.filter$elev, time = WLR.filter$Date.Time, period = 12.5)

# One of the issues with the HL() function is that is has difficult discerning high and low tides
# in systems that do not experience regular low and high tide flooding like pools and pannes. 

# We accomplished this with additional code that : 
    # (1) Identifies two consecutive High/Low Tides
    # (2) Keeps the highest High Tide or Lowest low tide and deletes the other


#Dplyr code to investigate the High Tides: 
    #(1) Filter out only the High Tides with the filter() function
    #(2) Create new columns that pull out the month, day, and "AM"/"PM" of the day for each High Tide
    #(3) Group the next few functions by Month, Day, and "AM"/"PM"
    #(4) Create a new dataframe with:
        #(a) Maximum water elevation within the same group of Month, Day, and "AM"/"PM"
        #(b) Out of hte pair between the two water levels, which one was the maximum?
        #(c) Pull out the time of the maximum water elevation based on the ranking
        #(d) Pull out the "H" for High Tide


WLR.HL.H <- WLR.HL %>%
  filter(tide == "H") %>%
  mutate(
    month = month(time),
    day = day(time),
    daylight = ifelse(am(time), "AM", "PM") ) %>%
  group_by(month, day, daylight) %>%
  summarise(
    level = max(level),
    rank = which.max(level),
    time = time[rank],
    tide = tide[rank]) %>%
  ungroup(month, day, daylight)

#This was recreated for the Low Tide
WLR.HL.L <- WLR.HL %>%
  filter(tide == "L") %>%
  mutate(
    month = month(time),
    day = day(time),
    daylight = ifelse(am(time), "AM", "PM")) %>%
  group_by(month, day, daylight) %>%
  summarise(
    level = max(level),
    rank = which.max(level),
    time = time[rank],
    tide = tide[rank]) %>%
  ungroup(month, day, daylight)

#Paste together the two resulting cleaned up High and Low Tide Dataframes
WLR.HL <- union(WLR.HL.H, WLR.HL.L)

#Arrange the dataframe by Time
WLR.HL <- arrange(WLR.HL, time)

WLR.HL$level <- as.numeric(WLR.HL$level)

#Next, we calculate the average high and low tide elevation during our time frame with dplyr functions 

WLR.Tides <- WLR.HL %>%
  group_by(tide) %>%
  summarise(
    avg_elev = mean(level),
    se_elev = sd(level)/sqrt(n()),
    n = n())

#Number of High Tides recorded. Good to check.
WLR.Tides$n[1]


#Calculate the Mean Higher Water for Each Day
#Essentially, we want to do the same thing as before with the MAXIMUM High Tide per DAY
#I am going to reduce the code above to just look for the maximum per 24 hours
#Since the WLR.HL.H contains the highest high tide per 12 hours, this code essentially reduces it to one per 24 hours

WLR.TidesHH <- WLR.HL %>%
  filter(tide == "H") %>%
  group_by(month, day) %>%
  summarise(
    level = max(level),
    rank = which.max(level),
    time = time[rank],
    tide = tide[rank]) %>%
  ungroup(month, day) %>%
group_by(tide) %>%
  summarise(
    avg_elev = mean(level),
    se_elev = sd(level)/sqrt(n()),
    n = n()) %>%
  ungroup()

glimpse(WLR.TidesHH)

#Calculation of Max Spring Tide Elevation

flood.max <- max(WLR$elev)

flood.max



#STEP 4: CALCULATION AND GRAPHING OF FLOODING FREQUENCY FOR TIDAL CYCLE

#Next we want to calculate the flooding frequency of a water level recorder for a given elevation
#We will accomplish this in two steps:
    #(1) Creating a very small incremental sequence of elevations from the marsh elevation to the max flood elevation
    #(2) fld.frq() function from 'VulnToolkit' calculates the flooding frequency (% of Tides)

#Create a dataframe called "flood.seq" with a sequence from the min to max flood elevation at an interval of 0.0005 m
elev <- seq(min(WLR.filter$elev, na.rm = TRUE), max(WLR.filter$elev, na.rm = TRUE), 0.005)

flood.seq <- data.frame(elev)


#The fld.frq() function requires the following arguments: 
  # (1) z = tidal hydrology (water elevations) from the data set
  # (2) ht = list of high tides

  #Create a list of the high tides
HighTides <- WLR.HL %>%
  filter(tide == "H") %>%
  select(level)

glimpse(HighTides)

flood.seq$frq <- fld.frq(z = flood.seq$elev, ht = HighTides$level)

glimpse(flood.seq)

#Graph the Flooding Frequency using ggplot to estimate the flooding frequency

Frequencygraph <- ggplot(filter(flood.seq, elev >= 0), aes(x = elev, y = frq)) +
                  geom_point() +
                  geom_vline(xintercept = PlatformElev, linetype = "dashed", color = "black", size = 1) +
                  geom_vline(xintercept = RootElev, linetype = "dashed", color = "green", size = 1) +
                  labs(x = "Elevation (m)", y = "Flooding Frequency") +
                  scale_x_continuous(breaks = seq(0, max(flood.seq$elev) + 0.1, 0.1)) +
                  theme_bw() +
  theme(axis.text = element_text(size = 16, colour = "black"),
        axis.title = element_text(size = 16, colour = "black"))

Frequencygraph

ggsave(Frequencygraph_output,
       Frequencygraph, height = 806, width = 1536, units = "px", dpi = 125)


#Lastly, we want to calculate the duration of time water remained above the salt marsh surface
#We accomplish this by the flood.dur() function. 
#All you have to do is supply:
# (1) z = Salt Marsh Platform Elevation
# (2) second argument = water level elevations over time

# Flood duration of the marsh platform
flood.durPlatform <- fld.dur(z = PlatformElev, WLR.filter$elev)

# Flood duration of the root zone

flood.durRoot <- fld.dur(z = RootElev, WLR.filter$elev)

#Conversion of Flood Duration (Percentage) into Hours

flood.timePlatform <- flood.durPlatform *(length(WLR.filter[,1]) / 6)

glimpse(flood.timePlatform)

flood.timeRoot <- flood.durRoot * (length(WLR.filter[,1]) / 6)

glimpse(flood.timeRoot)


                    
# Step 6: Compile tidal hydrology statistics into a single CSV for export
  # The code is rather clunky in Step 6, so please no judgement

#Create a blank data frame with the the following columns:

Tidal.Stats <- data.frame(matrix(ncol = 13, nrow = 2))
colnames(Tidal.Stats) <- c("Zone", "Elevation", "Flood_Frequency", "Flood_Duration_Percent", 
                           "Flood_Duration_Hour", "Avg_HT", "SE_HT", "Avg_LT", "SE_LT", 
                           "Avg_HH", "SE_HH", "Max_Tide","MonitoringTime")

Tidal.Stats$Elevation <- c(PlatformElev, RootElev)
Tidal.Stats$Zone <- c("Platform", "Root Zone")

#Column that has the Percent Flood Duration at the Salt Marsh Elevation

Tidal.Stats$Flood_Duration_Percent[1] <- (flood.durPlatform * 100)

Tidal.Stats$Flood_Duration_Percent[2] <- (flood.durRoot * 100)

#Column that has the Hourly Flood Duration at the Salt Marsh Elevation

Tidal.Stats$Flood_Duration_Hour[1] <- flood.timePlatform

Tidal.Stats$Flood_Duration_Hour[2] <- flood.timeRoot

#Column that has the Flood Frequency for the Salt Marsh Elevation

flood.frq <- flood.seq %>%
  filter(elev == Closest(flood.seq$elev, PlatformElev))

Tidal.Stats$Flood_Frequency[1] <- (flood.frq[1,2] * 100)


flood.frq <- flood.seq %>%
  filter(elev == Closest(flood.seq$elev, RootElev))

Tidal.Stats$Flood_Frequency[2] <- (flood.frq[1,2] * 100)

#Column that has the Average and Standard Error for the High and Low Tides

Tidal.Stats$Avg_HT[1] <- WLR.Tides$avg_elev[1]

Tidal.Stats$SE_HT[1] <- WLR.Tides$se_elev[1]

Tidal.Stats$Avg_LT[1] <- WLR.Tides$avg_elev[2]

Tidal.Stats$SE_LT[1] <- WLR.Tides$se_elev[2]

#Column that has the Average Higher High Tide and Standard Error

Tidal.Stats$Avg_HH[1] <- WLR.TidesHH$avg_elev[1]

Tidal.Stats$SE_HH[1] <- WLR.TidesHH$se_elev[1]

#Column that has the maximum Spring Flood Tide (m)

Tidal.Stats$Max_Tide[1] <- flood.max

#Column that has the monitoring time for the analysis

Tidal.Stats$MonitoringTime <- MonitoringTime

#Need to describe the name of the CSV
write.csv(Tidal.Stats, Output)


rm(WLR.HL, WLR.HL.H, WLR.HL.L, WLR.Tides, WLR.TidesHH)



