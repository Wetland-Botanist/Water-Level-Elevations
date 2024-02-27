#Project: Analysis of an individual water level recorder for salt marsh monitoring

#Code: Part 5 - Graphing Tidal Water Elevations

#Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

#Organization: Jackson Estuarine Laboratory, University of New Hampshire


#General Explanation of Code:
# The code is largely a product of the VulnToolkit package created by Troy Hill at the EPA. He devised a fantastic 
# method to analyze the duration of inundation and frequency of inundation for a given elevation. Additionally,
# the VulnToolkit calculates the elevation of the average and maximum high and low tides. 

# The overall purpose of the code is to analyze water level elevation data to accurately describe and summarise
# the tidal hydrology regime for salt marsh systems and groundwater regimes at individual water level recorders

#The fifth part of the code is designed to easily graph a single water level recorder with its associated creek and 
# sparrow islands elevations (optional). The graphs provide an in-depth look at each water level recorder and allow for
# easy and quick comparison to the broader hydrology of the salt marsh system 


#Chapter 1: Library of packages for necessary work

#Tidyr and dplyr are great data organization packages
library(tidyr)
library(dplyr)
library(pillar)

#Lubridate is a special package that allows us to do some cool stuff with dates and time arithmetic
library(lubridate)

#DescTools is a special package that allows for some cool functions
library(DescTools)

#VulnToolkit is a special package that does all the meat and potatoes for this R-script
library(ggplot2)


#Chapter 2: Import Datasets

#To graph, we will need to import all three datasets:
# 1) Formatted water level elevation dataset
# 2) Individual water level recorder and elevation dataset
# 3) Sparrow island elevation dataset

#Import formatted water level elevation dataset


#First input is the formatted water level elevations from the Reformat R Code
# The code removes the pesky "X" column created to label row names, then 
# formats the Date.Time column to a POSIXct format

wlr <- read.csv("Formatted Datasets\\WLR Formatted Dataset.csv") %>%
  select(-X) %>%
  mutate(Date.Time = as.POSIXct(Date.Time, format = '%Y-%m-%d %H:%M:%S'))

glimpse(wlr)


#Second input is the marsh platform and root zone elevations

#USER INPUT NEEDED:
elevs <- read.csv("Input Data\\KentsIsland_Elevations_2023.csv")

glimpse(elevs)


#The second input is a dataframe that contains each groundwater/pool water level recorder
# with associated marsh platform elevation, root zone elevation, and associated creeks

#USER INPUT NEEDED:
sparrow <- read.csv("Input Data\\KentsIsland_SparrowIsland_csv.csv")

glimpse(sparrow)



#Chapter 2: Subsetting the datasets to a specific water level recorder

#The code is designed for the user to input the name of the individual water level recorder and the
# code will automatically subset the datasets to the WLR and its associated creek WLR. 

wlr_name <- "RUN1"

creek_name <- elevs$Associated_Creek[which(elevs$WLR == wlr_name)]

# First, subset the formatted water level elevation datset to the specific water level recorder and 
# the creek water level recorder

wlr_subset <- wlr %>%
  gather(key = WLR, value = elev, Creek:length(wlr)) %>%
  filter(WLR == wlr_name | WLR == creek_name)

wlr_min <- min(wlr_subset$elev[which(wlr_subset$WLR == wlr_name)])

wlr_max <- max(wlr_subset$elev)

#Second, subset the marsh platform and root zone elevation dataset to the specific WLR

elevs_subset <- elevs %>%
  filter(WLR == wlr_name)

platform_elev <- elevs_subset$Marsh_Elevation

rootzone_elev <- elevs_subset$Rootzone_Elevation

#Third, subset the sparrow island elevaiton dataset to the specific WLR/Treatment

sparrow_subset <- sparrow %>%
  filter(WLR == wlr_name)

sparrow_elev <- sparrow_subset$Elevation

#Chapter 4: Graph the tidal water elevations


Tidegraph <- ggplot(wlr_subset, 
                    aes(x = Date.Time, y = elev, 
                        colour = WLR)) + 
  geom_hline(yintercept = platform_elev, linetype = "dashed", 
             color = "black", size = 1.25) +
  geom_hline(yintercept = rootzone_elev, linetype = "dashed", 
             color = "green", size = 1.25) +
  geom_hline(yintercept = sparrow_elev, 
   linetype = "dashed", colour = "brown", size = 1.25) +
  geom_line(size = 1.25) + 
  scale_color_manual(values = c("blue", "orange")) + 
  labs(x = "", y = "Water Elevation (NAVD88 m)") +
  scale_x_datetime(limits = c(min(wlr_subset$Date.Time), 
                              max(wlr_subset$Date.Time)),
                   breaks = "3 days",
                   date_labels = "%m-%d") +
  scale_y_continuous(breaks = seq(round(wlr_min, 1) - 0.20, 
                                  round(wlr_max, 1) + 0.1, 0.10)) + 
  coord_cartesian(ylim = c( round(wlr_min, 1) - 0.20, 
                           round(wlr_max, 1) + 0.1)) +
  theme_bw() +
  theme(
    legend.position = c(0.125, 0.95),
    legend.text = element_text(size = 18),
    legend.title = element_blank(),
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 0, size = 18, colour = "black"),
    axis.text.y = element_text(size = 18, colour = "black"),
    axis.title.y = element_text(size = 18, colour = "black"))

Tidegraph



#Save the graph to the 'Figures' Folder of the project

ggsave(Tidegraph,
       path = paste("Figures\\", wlr_name, " Water Level Elevations Graph.jpg", collapse = ""), 
       height = 806, width = 1536, 
       units = "px", dpi = 125)
