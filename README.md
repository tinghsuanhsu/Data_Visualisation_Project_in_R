# Data_Visualisation_Project_in_R

This R Shiny visualisation project aims to communicate analysis and insights of over 1.4 million trip data that were recorded by Indego bike share system from 2016 to 2017.
Analysis includes ridership, travel patterns, factors behind station popularity after combining trip data with historical weather, geographical information, and local events. 
Text-based analysis is provided for each topic and supported by interactive visualisations.


## Data Source

- Indego Trip Data 2016 - 2017 and Bike Station Coordinates by Indego : https://bit.ly/2vxi5ym
- Weather Data of Philadelphia 2016 - 2017 by Franklin Institute : https://bit.ly/2HBj2KW 
- Attractions Locations of Phiadelphi by Visit Philly : https://vstphl.ly/2qM0tu4 
- Public Transit Station Location by Southeastern Pennsylvania Transportation Authority (SEPTA) : https://bit.ly/2HPfdzl

## Application Structure

- helper.r : is the source script of ui.r and server.r which loads datasets, functions, and pre-
defined plots. This method separates interface and server from data and helper functions
for web deployment.
- ui.r : contains codes for the Shiny user interface (load “source(helper.r)” at the start).
- server.r : contains codes for the Shiny server that listens for inputs and outputs.
