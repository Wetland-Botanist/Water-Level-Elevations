Project: Analysis of an individual water level recorder for salt marsh monitoring

Authors: Grant McKown (james.mckown@unh.edu), Jennifer Gibson (jennifer.gibson@unh.edu)

Organization: Coastal Habitat Restoration Team, Jackson Estuarine Laboratory, University of New Hampshire

Input Data README:

- The 'Input Data' folder is the only folder the user of the R project will need to deposit the necessary datasets. The datasets needed for the full code are:

1) Water Level Elevations Time Series
2) Marsh Platform and Root Zone Elevations of Individual Pool and Groundwater WLRs
3) Constructed Sparrow Island Elevations

- Each dataset has specific requirements on column names and column data including specific columns that must match across all three datasets.
- See the 'Input Data Template' Folder for blank templates of each dataset to ensure that the R code functions properly



Water Level Elevation Time Series

Column Names and Explanation:

Date.Time - Time series dataset for all of the water level recorders of a given site. Time series data should be in the format of "Month/Day/Year Hour:Minute". For example, "12/25/2021 05:55" 

The remaining column names should be the names of individual water level recoders. The site should have at least one water level recorder representing the tidal hydrology of the given site, typically installed in a ditch or creek. That water level recoder must start with "Creek" but can be further named "Creek1", "Creek_North", etc. The R code specifically subsets and selects the creek column based on the column name STARTING WTIH "Creek". The user can have multiple creek water level recoders as columns, representing the tidal hydrology of different portions of a salt marsh. The remaining column names can have any name to the user's liking, but must be the same forthe Marsh Platform and Root Zone Elevation.

The data of the remaining columns is water level elevations at time series in units of NAVD8 meters. 



Marsh Platform and Root Zone Elevations of Indiidual Pool and Groundwater WLRs

Column Names and Explanation:

WLR - names of the groundwater and pool water level recorders of a given site. The exact same spelling of each WLR from the Water Level Elevation Time Series data must be used. The exact number of groundwater and pool water level recorders must be used as well between the two datasets. 

Associated_Creek - the name of the creek water level recorder that represents the tidal hydrology the area of marsh the groundwater/pool water level recorder resides. Spelling of the creek water level recorder name must be exact to the Water Level Elevation Time Series datset. 

Marsh_Elevation - elevation of the marsh platform surrounding the groundwater/pool water level recoder. Units in NAVD88 meters. 

Rootzone_Elevation - elevation of the root zone surrounding the groundwater/pool water level recoder. Typically, the root zone elevation is considered to be 5 cm below the marsh platform elevation, but can be changed based on user needs. Units in NAVD88 meters. 



Constructed Sparrow Island Elevations 

Column Names and Explanation: 

Island = name or code of the constructed sparrow island

Groundwater_WLR = name of the groundwater or pool water level recorder closest to the sparrow island. The column is only used for graphing purposes.

WLR = name of the creek water level recoder measuring the tidal hydrology that impacts the sparrow island. The column is used for data analysis and hydrology metrics calculations.

Elevation - Elevation of the top of the sparrow island. Units in NAVD88 meters. 

