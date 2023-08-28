################################################################################
# Natura 2000 Data Processing
# Script: natura2000_processing.R
# 
# This script processes Natura 2000 site data for a MS , including information 
# on habitats and species. It reads a shapefile containing the spatial data 
# for Natura 2000 sites in the MS and joins it with tabular data on habitats and 
# species. The script then performs data transformations and exports shapefiles 
# and a GeoPackage with the processed data.
################################################################################

# Remove all objects from the environment
rm(list=ls())
gc()


# Set the working directory if needed
setwd()

# Load libraries
library(sf)
library(tidyverse)
library(dplyr)
library(terra)


# Habitats ----------------------------------------------------------------
# Function to process habitats in Natura 2000 data and export shapefiles and GeoPackage
# Description:
# This function processes habitats in Natura 2000 site data for MS. 
# It reads a shapefile containing the spatial data for habitats in Natura 2000 sites
# in a MS and joins it with tabular data 
# The function performs data transformations and exports 
# shapefiles for the sites with habitats and a GeoPackage containing the processed 
# data for further analysis.
#
# Arguments:
# n2k_MS_shp_path: Path to the shapefile containing habitats Natura 2000 site data for the MS
# hab_n2k_tab_path: Path to the tabular data file for Natura 2000 habitats
# output_path: Path to the output folder where shapefiles and GeoPackage will be saved

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
  
  # Filter data for sites with habitats
  #habitats <- left_join_data[!is.na(left_join_data$HABITATCODE),] #don't run this line for the changes analysis
  hab_n2k_ms <- left_join_data # run this line if you want to obtain all the natura 2000 sites
  
  # Changes name 
  #hab_n2k_ms <- habitats # don´n run this line if you want to obtain all the natura 2000 sites
  
  # Add a new column to indicate "D" for rows with "-" in RELSURFACE column
  hab_n2k_ms$RELSURFACE_D <- ifelse(hab_n2k_ms$RELSURFACE == "-", "D", 
                                    hab_n2k_ms$RELSURFACE)
  
  # Convert to sf object
  hab_n2k_ms_sf <- st_as_sf(hab_n2k_ms)
  
  # Extract the country code from the data frame
  country_code <- unique(hab_n2k_ms$MS)
  
  # Create the GeoPackage file path
  geopackage_path <- file.path(output_path, paste0("natura_2000_", country_code, "_habitats_all_sites_ETRS89.gpkg"))
  
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



#define the paths of the files to process

# File that contains Natura 2000 site data for Poland
n2k_MS_shp_path <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/Mapping/02_output/LT/Natura_2000_2017/natura_2000_LT_ETRS89.shp"
# File that contains the tabular data for Natura 2000 habitats
hab_n2k_tab_path <- "I:/biocon/ETC_Data_original/N2K_spatial_and_descriptive_end2017-25.05.2018/Tabular/HABITATS.txt"
# Set the output folder
output_path <- "02_output/LT/Natura_2000_2017"


# Call the function to process Natura 2000 data
process_hab_natura2000_data(n2k_MS_shp_path, hab_n2k_tab_path, output_path)



# Species ----------------------------------------------------------------
# Function to process species in Natura 2000 data and export shapefiles and GeoPackage  
# Description:
# This function processes species in Natura 2000 site data for MS.
# It reads a shapefile containing the spati al data for species in Natura 2000 sites
# in a MS and joins it with tabular data
# The function performs data transformations and exports
# shapefiles for the sites with species and a GeoPackage containing the processed
# data for further analysis.

# Arguments:
# n2k_MS_shp_path: Path to the shapefile containing species Natura 2000 site data for the MS
# spe_n2k_tab_path: Path to the tabular data file for Natura 2000 species
# output_path: Path to the output folder where shapefiles and GeoPackage will be saved

#create a function to process species in Natura 2000 data
process_spe_natura2000_data <- function(n2k_MS_shp_path, spe_n2k_tab_path, output_path) {
  
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
  
  # Filter data for sites with species
  #species <- left_join_data[!is.na(left_join_data$SPECIESCODE),] #don't run if you want to obtain all the natura 2000 sites 
  spe_n2k_ms <- left_join_data # if you not run the above line, run this.
  
  # Rename the joined data frame
  #spe_n2k_ms <- species # if you not filter the data, dont run this line.
  
  # Convert to sf object
  spe_n2k_ms_sf <- st_as_sf(spe_n2k_ms)
  
  # Extract the country code from the data frame
  country_code <- unique(spe_n2k_ms$MS)
  
  # Create the GeoPackage file path
  geopackage_path <- file.path(output_path, paste0("natura_2000_", country_code, "_species_all_sites_ETRS89.gpkg"))
  
  # Create the GeoPackage and write the sf object
  layer_name <- paste0("natura_2000_", country_code, "_species_ETRS89")
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


#define the paths of the files to process

# File that contains Natura 2000 site data for the member state
n2k_MS_shp_path <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/Mapping/02_output/LT/Natura_2000_2017/natura_2000_LT_ETRS89.shp"
# File that contains the tabular data for Natura 2000 species
spe_n2k_tab_path <- "I:/biocon/ETC_Data_original/N2K_spatial_and_descriptive_end2017-25.05.2018/Tabular/SPECIES.txt"
# Set the output folder
output_path <- "02_output/LT/Natura_2000_2017"


# Call the function to process Natura 2000 data
process_spe_natura2000_data(n2k_MS_shp_path, spe_n2k_tab_path, output_path)



