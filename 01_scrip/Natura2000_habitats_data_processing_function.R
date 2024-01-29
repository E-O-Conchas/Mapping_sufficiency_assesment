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
setwd("I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/outputs")
getwd()

# Importing library
library(sf)
library(tidyverse)
library(dplyr)
library(terra)


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
n2k_MS_shp_path <- "CY/Natura_2000_sites_v2023/Natura_2000_sites/N2000_Cyprus_ETRS.shp"
natura2000 <- st_read(n2k_MS_shp_path)
natura2000 <- natura2000 %>% 
  select(natura2000[1:-9])


names(natura2000)


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
  
  # Filter data for sites with habitats, because not all the habitas has an site attached
  #habitats <- left_join_data[!is.na(left_join_data$HABITATCODE),] #don't run this line for the changes analysis
  hab_n2k_ms <- left_join_data # run this line if you want to obtain all the natura 2000 sites
  
  # set new name 
  #hab_n2k_ms <- habitats # don´n run this line if you want to obtain all the natura 2000 sites
  
  # Add a new column to indicate "D" for rows with "-" in RELSURFACE column
  hab_n2k_ms$RELSURFACE_CATEGORIES <- ifelse(hab_n2k_ms$RELSURFACE == "-", "D", 
                                             hab_n2k_ms$RELSURFACE)
  
  # Convert to sf object
  hab_n2k_ms_sf <- st_as_sf(hab_n2k_ms)
  
  # Extract the country code from the data frame
  country_code <- unique(hab_n2k_ms$MS)
  
  # Create the GeoPackage file path
  geopackage_path <- file.path(output_path, paste0("natura_2000_", country_code, "_habitats_all_sites_ETRS89.gpkg"))
  #geopackage_path <- file.path(output_path, paste0("natura_2000_", country_code, "_habitats_ETRS89.gpkg"))
  
  
  # Create the GeoPackage and write the sf object
  layer_name <- paste0("natura_2000_", country_code, "_habitats_ETRS89")
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
n2k_MS_shp_path <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/outputs/LT/Natura_2000_sites_v2017/Natura_2000_sites_Lithuania/natura_2000_LT_ETRS89.shp"
hab_n2k_tab_path <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/outputs/LT/Natura_2000_sites_v2017/Natura_2000_sites_Lithuania/HABITATS.txt"
output_path <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/outputs/LT/Natura_2000_sites_v2017/habitats_new"

# Import tabular data to have a look
hab_n2k_tab <- read.table(hab_n2k_tab_path, 
                          header = TRUE, 
                          sep = ",", 
                          encoding = "UTF-8")


# Run function
process_hab_natura2000_data(n2k_MS_shp_path, hab_n2k_tab_path, output_path)

