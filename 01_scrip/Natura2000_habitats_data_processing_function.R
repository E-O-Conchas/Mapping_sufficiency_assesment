################################################################################
# 
# Author: no67wuwu
# Natura 2000 Habitat data Processing
# 
# Script: Natura2000_habitats_data_processing.R
# 
# This script processes Natura 2000 site data for a MS , including information 
# on habitats. It reads a shapefile containing the spatial data 
# for Natura 2000 sites in the MS and joins it with tabular data on habitats. 
# The script then performs data transformations and exports a 
# GeoPackage with the processed data.
#
################################################################################

# Clean the environment
rm(list=ls())
gc()


# Setting working directory
setwd("I:Set your work directory/")
getwd()

# Importing library
library(sf)
library(tidyverse)
library(dplyr)
library(terra)n


################################### HABITATS ###################################
# 
# Function to process habitats in Natura 2000 data
# 
# Description:
# The function performs data transformations and exports 
# shapefiles for the sites with habitats and a GeoPackage containing the processed 
# data for further analysis.
#
# Arguments:
# n2k_MS_shp_path: Path to the shapefile containing habitats Natura 2000 site data for the MS
# hab_n2k_tab_path: Path to the tabular data file for Natura 2000 habitats
# output_path: Path to the output folder where shapefiles and GeoPackage will be saved
#
################################################################################
## Check (free code)


# Function 
process_hab_natura2000_data <- function(n2k_MS_shp_path, hab_n2k_tab_path, output_path) {
  
  # Load the spatial data 
  n2k_MS_shp <- st_read(n2k_MS_shp_path)
  
  # Convert to a data frame
  n2k_MS_shp <- as.data.frame(n2k_MS_shp)
  
  # Load the tabular Natura 2000 habitats
  hab_n2k_tab <- read.table(hab_n2k_tab_path, 
                            header = TRUE, 
                            sep = ",", 
                            encoding = "UTF-8")
  
  # Join the n2k_MS_shp and hab_n2k_tab
  left_join_data <- left_join(n2k_MS_shp,
                              hab_n2k_tab,
                              by = "SITECODE",
                              multiple = "all")
  
  # Filter data based on the value of all_sites parameter
  if (all_sites) {
    habitats <- left_join_data
    file_name_suffix <- "_all_sites"
  } else {
    habitats <- left_join_data[!is.na(left_join_data$HABITATCODE),]
    file_name_suffix <- ""
  }
  
  # set new name 
  hab_n2k_ms <- habitats # don´n run this line if you want to obtain all the natura 2000 sites
  
  # Add a new column to indicate "D" for rows with "-" in RELSURFACE column
  hab_n2k_ms$RELSURFACE_CATEGORIES <- ifelse(hab_n2k_ms$RELSURFACE == "-", "D", 
                                             hab_n2k_ms$RELSURFACE)
  
  # Convert to sf object
  hab_n2k_ms_sf <- st_as_sf(hab_n2k_ms)
  
  # Extract the country code from the data frame
  country_code <- unique(hab_n2k_ms$MS)
  
  # Create the GeoPackage file path
  geopackage_path <- file.path(output_path, paste0("natura_2000_", country_code, "_habitats", file_name_suffix, "_ETRS89.gpkg"))
  
  # Create the GeoPackage and write the sf object
  layer_name <- paste0("natura_2000_", country_code, "_habitats", file_name_suffix, "_ETRS89")
  st_write(hab_n2k_ms_sf, geopackage_path, layer = layer_name, driver = "GPKG")
  
  # Create shapefiles for each habitat code in the GeoPackage
  habitats_codes <- unique(hab_n2k_ms$HABITATCODE)
  print("Habitat codes created")
  
  for (code in habitats_codes) {
    # Subset the data for the current habitat code
    habitat_data <- hab_n2k_ms[hab_n2k_ms$HABITATCODE == code,] 
    
    # Create the layer name for the habitat in the GeoPackage
    layer_name <- paste0("habitat_", code)
    
    # Write the sf object to the GeoPackage
    st_write(habitat_data, geopackage_path, layer = layer_name, driver = "GPKG", append = TRUE)
    
    print("The files have been created and saved in the output path.")
  }
}


# Set inputs and run the function
# Define the paths 
n2k_MS_shp_path <- 'Set your natura 2000 shapefile for the country to asses'
hab_n2k_tab_path <- 'Set your habitats tabular data'
output_path <- 'Set your output folder'

# Run function
process_hab_natura2000_data(n2k_MS_shp_path, hab_n2k_tab_path, output_path, all_sites = FALSE)



























