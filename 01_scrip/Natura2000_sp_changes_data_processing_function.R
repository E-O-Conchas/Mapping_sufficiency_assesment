#####################################################################
# Natura 2000 Species Changes Analysis and GeoPackage Export Script #
#####################################################################

# Introduction:
# This user-friendly script is designed to analyze the changes in Natura 
# 2000 species occurrences between two time periods and save 
# the results as a GeoPackage.
# 
# Functionality:
# The script loads spatial data for Natura 2000 species occurrences from 
# two different years (2018 and 2023) stored in GeoPackage files. It then 
# identifies the changes in species occurrences and sites between the two 
# periods, categorizing them as "Remaining in site," "Added to an existing site," 
# or "Added to a new site." The script also identifies deleted species occurrences/sites 
# and merges them with additional columns from the 2018 dataset. The final result 
# is a combined dataset of all changes and deletions.
# 
# Dependencies:
# To run this script, ensure that you have the following 
# libraries installed: "sf" for spatial data manipulation and "dplyr" for 
# data processing. If not installed, you can do so by running the following commands in R:
# 
# install.packages("sf")
# install.packages("dplyr")
# 
# Usage:
# 1. Prepare the GeoPackage files for Natura 2000 species occurrences in the 2018 and 2023 datasets.
# 2. Update the file paths for gpkg_file_2018, gpkg_file_2023, and output_file 
#    to point to the respective GeoPackage files and desired output directory.
# 3. Run the script, and it will perform the analysis and create a GeoPackage 
#    containing the combined changes in Natura 2000 species occurrences. Separate
#    layers for each species code will also be included in the GeoPackage.
# 
# Please note that this script assumes the input GeoPackage files have 
# the necessary data and that the columns for species occurrences and sites 
# are correctly formatted. Additionally, the script should be run in an R 
# environment with the required dependencies installed.

# Remove all objects from the environment
rm(list=ls())
gc()

# Load libraries
library(sf)
library(dplyr)

# Set the working directory
setwd()
getwd()


# Function

save_combined_habitat_changes <- function(gpkg_file_2018, gpkg_file_2023, output_file) {
  # Read species spatial data 2018
  spe_n2k_2018 <- st_read(gpkg_file_2018, layer = "natura_2000_LT_species_ETRS89") # every time when you run the function set the layers name
  
  #Read species spatial data 2023 
  spe_n2k_2023 <- st_read(gpkg_file_2023, layer = "natura_2000_LT_species_ETRS89") # every time when you run the function set the layers name
  
  # Converto to data frame
  spe_n2k_2023 <- as.data.frame(spe_n2k_2023)
  spe_n2k_2018 <- as.data.frame(spe_n2k_2018)
  
  # Convert SPECIESCODE to character data type
  spe_n2k_2023$SPECIESCODE <- as.integer(spe_n2k_2023$SPECIESCODE)
  spe_n2k_2018$SPECIESCODE <- as.integer(spe_n2k_2018$SPECIESCODE)
  
  
  # Correlation between Natura2000_2018 and Natura2000_2023
  species_changes <- spe_n2k_2023 
  species_changes$CATEGORY <- NA
  
  # Correlation between Natura2000_2018 and Natura2000_2023
  for (i in 1:nrow(species_changes)) {
    site_code <- species_changes$SITECODE[i]
    if (site_code %in% spe_n2k_2018$SITECODE) {
      if (any(spe_n2k_2018$SPECIESCODE[spe_n2k_2018$SITECODE == site_code] %in% species_changes$SPECIESCODE[i])) {
        # Remaining in site: Both site and habitat exist in 2018 and 2023 datasets
        species_changes$CATEGORY[i] <- "Remaining in site"
      } else {
        # Added to an existing site: Site exists in 2018 and additional habitat was identified and listed for an existing Natura2000
        species_changes$CATEGORY[i] <- "Added to an existing site"
      }
    } else {
      # Added to a new site: Site and habitat are new in 2023 dataset
      species_changes$CATEGORY[i] <- "Added to a new site"
    }
  }
  
  # Eliminates all the NA values from the data frame 2018
  spe_n2k_2018 <- spe_n2k_2018[!is.na(spe_n2k_2018$SPECIESCODE),]
  
  # Category "Deleted habitats/sites" as a data frame 
  deleted_species <- lapply(unique(spe_n2k_2018$SITECODE), function(site_code) {
    habitats_2018 <- spe_n2k_2018$SPECIESCODE[spe_n2k_2018$SITECODE == site_code]
    habitats_2023 <- species_changes$SPECIESCODE[species_changes$SITECODE == site_code]
    setdiff(habitats_2018, habitats_2023)
  })
  
  deleted_species <- data.frame(SITECODE = rep(unique(spe_n2k_2018$SITECODE), sapply(deleted_species, length)), SPECIESCODE = unlist(deleted_species))
  deleted_species$CATEGORY <- "Deleted from/with site" 
  
  # Merge with additional columns from the 2018 dataset
  deleted_species <- left_join(deleted_species, spe_n2k_2018, by = c("SITECODE", "SPECIESCODE"))
  
  # Combine deleted_species and habitats_changes
  changes_species_2018_2023 <- bind_rows(species_changes, deleted_species)
  
  # Convert to a sf
  changes_species_2018_2023_sf <- st_as_sf(changes_species_2018_2023)
  
  # Save as a geopackage
  country_code <- unique(changes_species_2018_2023_sf$MS)
  geopackage_path <- file.path(output_file, paste0("natura_2000_", country_code, "_species_changes_ETRS89.gpkg"))
  
  # Write the sf object to the GeoPackage
  st_write(changes_species_2018_2023_sf, geopackage_path, driver = "GPKG")
  
  # Separate layers for each habitat code
  species_codes <- unique(changes_species_2018_2023_sf$SPECIESCODE)
  for (code in species_codes) {
    species_data <- changes_species_2018_2023_sf[changes_species_2018_2023_sf$SPECIESCODE == code, ] #posiblemente aqui este la solucion
    layer_name <- paste0("specie_", code)
    st_write(species_data, geopackage_path, layer = layer_name, driver = "GPKG", append = TRUE)
  }
  
  print("The combined changes have been saved as a GeoPackage.")
  
}

# Common base path
base_path <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/Mapping/02_output/LT"

# Usage:
gpkg_file_2017 <- file.path(base_path, "Natura_2000_2017/natura_2000_LT_species_all_sites_ETRS89.gpkg")
gpkg_file_2023 <- file.path(base_path, "Natura_2000_2023/natura_2000_LT_species_all_sites_ETRS89.gpkg")
output_file <- file.path(base_path, "Natura_2000_changes_2017_2023")


# Run the function
save_combined_habitat_changes(gpkg_file_2017, gpkg_file_2023, output_file)




