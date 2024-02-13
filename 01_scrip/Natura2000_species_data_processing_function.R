################################################################################
# 
# Author: no67wuwu
# Natura 2000 Species data Processing
#
# Script: Natura2000_species_data_processing.R
# 
# This script processes Natura 2000 site data for a MS , including information 
# on species. It reads a shapefile containing the spatial data 
# for Natura 2000 sites in the MS and joins it with tabular data on 
# species. The script then performs data transformations and exports
# a GeoPackage with the processed data.
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
library(terra)


################################### SPECIES ####################################
#
# Function to process species in Natura 2000 data
#
# Description:
# The function performs data transformations and exports
# shapefiles for the sites with species and a GeoPackage containing the processed
# data for further analysis.
#
# Arguments:
# n2k_MS_shp_path: Path to species Natura 2000 site data for the MS
# spe_n2k_tab_path: Path to the tabular data file for Natura 2000 species
# output_path: Path to the output folder where shapefiles and GeoPackage will be saved
#
################################################################################
## Check (free code)



# Function to process species within Natura 2000 sites
process_spe_natura2000_data <- function(n2k_MS_shp_path, spe_n2k_tab_path, output_path, all_sites = FALSE) {
  
  # Load the spatial data
  n2k_MS_shp <- st_read(n2k_MS_shp_path)
  
  # Convert to a data frame
  n2k_MS_shp <- as.data.frame(n2k_MS_shp)
  
  # Load the tabular Natura 2000 species
  spe_n2k_tab <- read.table(spe_n2k_tab_path, 
                            header = TRUE, 
                            sep = ",", 
                            encoding = "UTF-8")
  
  # Join the n2k_MS_shp and spe_n2k_tab
  left_join_data <- left_join(n2k_MS_shp, 
                              spe_n2k_tab, 
                              by = "SITECODE", 
                              multiple = "all")
  
  
  # Filter data based on the value of all_sites parameter
  if (all_sites) {
    species <- left_join_data
    file_name_suffix <- "_all_sites"
  } else {
    species <- left_join_data[!is.na(left_join_data$SPECIESCODE),]
    file_name_suffix <- ""
  }
  
  # Rename the joined data frame
  spe_n2k_ms <- species
  
  
  # Add a new column to indicate "Data deficient" for rows with "D" in POPULATION column
  spe_n2k_ms$POPULATION_CATEGORIES <- ifelse(spe_n2k_ms$POPULATION == "-", "D", 
                                             spe_n2k_ms$POPULATION)
  
  spe_n2k_ms$POPULATION_CATEGORIES <- ifelse(spe_n2k_ms$POPULATION == "", "no data", 
                                             spe_n2k_ms$POPULATION)
  
  # Convert to sf object
  spe_n2k_ms_sf <- st_as_sf(spe_n2k_ms)
  
  # Extract the country code from the data frame
  country_code <- na.omit(unique(spe_n2k_ms$MS))
  
  # Create the GeoPackage file path
  geopackage_path <- file.path(output_path, paste0("natura_2000_", country_code, "_species", file_name_suffix ,"_ETRS89.gpkg"))
  
  # Create the GeoPackage and write the sf object
  layer_name <- paste0("natura_2000_", country_code, "_species", file_name_suffix ,"_ETRS89")
  st_write(spe_n2k_ms_sf, geopackage_path, layer = layer_name, driver = "GPKG")
  
  # Create shapefiles for each species code in the GeoPackage
  species_codes <- unique(spe_n2k_ms$SPECIESCODE)
  
  for (code in species_codes) {
    # Subset the data for the current species code
    species_data <- spe_n2k_ms[spe_n2k_ms$SPECIESCODE == code,]
    
    # Create the layer name for the species in the GeoPackage
    layer_name <- paste0("specie_", code)
    
    # Write the sf object to the GeoPackage
    st_write(species_data, geopackage_path, layer = layer_name, driver = "GPKG", append = TRUE)
    
    print("The files have been created and saved in the output path.")
  }
}


# Set the inputs and run the function
# Define the paths 
n2k_MS_shp_path <- 'Set your natura 2000 shapefile for the country to asses'
spe_n2k_tab_path <- 'Set your species tabular data'
output_path <- 'Set your output folder' 


# Run function
process_spe_natura2000_data(n2k_MS_shp_path, spe_n2k_tab_path, output_path, all_sites = TRUE)












