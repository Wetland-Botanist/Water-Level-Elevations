# Analysis of Tidal Water Elevations for Salt Marsh Monitoring and Restoration
Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

Organization: Coastal Habitat Restoration Team, Jackson Estuarine Laboratory, University of New Hampshire

Purpose:

The R package was created to expedite the analysis of tidal water elevation - time series data by caluclating common tidal hydrology metrics (mean low tide, high tide, etc.) and flooding parameters for given elevations of the salt marsh surface (flooding duration, high tide fooding frequency, etc.) across numerous water level recorders for a given site. Additionally, the code graphs the tidal hydrology for an individual water level recorder and its associated creek water level recorder.  

Any user is free (and encouraged!) to download and use the R project. Note of Warning - the code may be updated from time to time to ensure compatibility with R package updates and enhance capabilities. 

Monitoring Design Requirements:

The R code is specifically written to meet the hydrology monitoring protocols of the Coastal Habitat Restoration Team. In a given area of salt marsh, two types of water level recorders are deployed simultaneously: Creek and Groundwater/Pool WLRs. The creek WLRs are deployed in ditches or creeks to monitor the site-level tidal hydrology. The groundwater and pool WLRs are placed in PVC piezometers and set 50 - 60 cm belowground with the specific aim of monitoring the changes in groundwater at specific locations. The Coastal Habitat Restoration Team conducts monitoring of salt marsh restoration projects in a BACI design where there is one WLR in the creek, one in the impacted or restored marsh, one in a 'pristine' reference marsh, and one in a degraded no action marsh. The basic requirements for the monitoring design (and R Code) is to have at least one creek WLR and one groundwater/pool WLR. 

Project Overview:

A brief description of the five R code scripts is provided:

1) Format Dataset - R script formats the water level elevation - time series dataset to allow for use in the rest of the R scripts

2) Creeks - R script calculates mean low tide, high tide, higher high tide, and maximum tide elevation for each creek WLR in the formatted time series dataset

3) Groundwater & Pools - R script calculates tidal hydrology metrics (see Creeks R script) for each non-creek WLR. Additionally, the script calculates the flooding duration (% of monitoring time), high tide flooding frequency (%), and the mean drainage elevation for each low tide for given marsh platform and root zone elevations

4) Sparrow Islands - R script calculates the flooding metrics (see Groundwater & Pools script) for given elevations of constructed sparrow islands according to the nearest creek WLR. 

5) Graphing Tidal Elevations - R script graphs the water elevations of a single groundwater/pool WLR and its associated creek WLR. The elevations of the marsh platform, root zone, and associated constructed sparrow islands are also graphed.

Folders:

See individual README files in each folder for more in-depth details.

1) Input Data - folder the user of the R project will need to deposit the necessary datasets. See the folder README for in-depth details on the dataset needs for the R scripts. Example datasets are provided in the folder. R script is currently written to run with the example datasets. 
   
2) Dataset Templates - blank CSV files with appropriate column headers (and blank cells) for the needed input datasets
   
3) Formatted Datasets -  an intermediary folder that holds various water level elevation datasets that are created in one R script and utilized in the another R script'
   
4) Output Stats - destination of finalized tables for the descriptive statistics of tidal hydrology of creek, groundwater, and pool water level recorders and flooding for groundwater level recoders and constructed sparrow islands.
   
5) Figures - output of water level tidal elevation graphs in JPG format
