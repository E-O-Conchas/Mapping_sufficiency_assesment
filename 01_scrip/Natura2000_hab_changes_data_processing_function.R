#####################################################################
# Natura 2000 Habitats Changes Analysis and GeoPackage Export Script #
#####################################################################

# Introduction:
# This user-friendly script is designed to analyze the changes in 
# Natura 2000 habitats between two time periods and save the results as a GeoPackage.
# 
# Functionality:
# The script loads spatial data for Natura 2000 habitats from two 
# different years stored in GeoPackage files. It then 
# identifies the changes in habitats and sites between the two periods, categorizing 
# them as "Remaining in site," "Added to an existing site," or "Added to a new site." 
# The script also identifies deleted habitats/sites and merges them with additional 
# columns from the 2018 dataset. The final result is a combined dataset of all changes and deletions.
# 
# Dependencies:
# To run this script, ensure that you have the following libraries 
# installed: "sf" for spatial data manipulation and "dplyr" for data processing.
# If not installed, you can do so by running the following commands in R:
# 
# install.packages("sf")
# install.packages("dplyr")
# 
# Usage:
# 1. Prepare the GeoPackage files for Natura 2000 habitats in the 2018 and 2023 datasets.
# 2. Update the file paths for gpkg_file_2018, gpkg_file_2023, and output_file
#    to point to the respective GeoPackage files and desired output directory.
# 3. Run the script, and it will perform the analysis and create a GeoPackage 
#    containing the combined changes in Natura 2000 habitats. Separate layers for 
#    each habitat code will also be included in the GeoPackage.
# 
# Please note that this script assumes the input GeoPackage files have the 
# necessary data and that the columns for habitats and sites are correctly 
# formatted.Additionally, the script should be run in an R environment with 
# the required dependencies installed.



# Remove all objects from the environment
rm(list=ls())
gc()


# Load libraries
library(sf)
library(dplyr)

# Function

save_combined_habitat_changes <- function(gpkg_file_path_2018, gpkg_file_path_2023, output_file_path) {
  # Read habitats spatial data 2018
  hab_n2k_2018 <- st_read(gpkg_file_path_2018, layer = "n2k_LT_hab_ETRS89")
  
  # Read habitats spatial data 2023
  hab_n2k_2023 <- st_read(gpkg_file_path_2023, layer = "n2k_LT_hab_ETRS89")
  
  # Convert to data frames
  hab_n2k_2018 <- as.data.frame(hab_n2k_2018)
  hab_n2k_2023 <- as.data.frame(hab_n2k_2023)
  
  # Correlation between Natura2000_2018 and Natura2000_2023
  habitat_changes <- hab_n2k_2023
  habitat_changes$CATEGORY <- NA
  
  for (i in 1:nrow(habitat_changes)) {
    site_code <- habitat_changes$SITECODE[i]
    if (site_code %in% hab_n2k_2018$SITECODE) {
      if (any(hab_n2k_2018$HABITATCODE[hab_n2k_2018$SITECODE == site_code] %in% habitat_changes$HABITATCODE[i])) {
        # Remaining in site: Both site and habitat exist in 2018 and 2023 datasets
        habitat_changes$CATEGORY[i] <- "Remaining in site"
      } else {
        # Added to an existing site: Site exists in 2018 and additional habitat was identified and listed for an existing Natura2000
        habitat_changes$CATEGORY[i] <- "Added to an existing site"
      }
    } else {
      # Added to a new site: Site and habitat are new in 2023 dataset
      habitat_changes$CATEGORY[i] <- "Added to a new site"
    }
  }
  
  # Eliminates all the NA values from the data frame 2018
  hab_n2k_2018 <- hab_n2k_2018[!is.na(hab_n2k_2018$HABITATCODE),]
  
  # Category "Deleted habitats/sites" as a data frame
  deleted_habitats <- lapply(unique(hab_n2k_2018$SITECODE), function(site_code) {
    habitats_2018 <- hab_n2k_2018$HABITATCODE[hab_n2k_2018$SITECODE == site_code]
    habitats_2023 <- habitat_changes$HABITATCODE[habitat_changes$SITECODE == site_code]
    setdiff(habitats_2018, habitats_2023)
  })
  
  deleted_habitats <- data.frame(SITECODE = rep(unique(hab_n2k_2018$SITECODE), sapply(deleted_habitats, length)), HABITATCODE = unlist(deleted_habitats))
  deleted_habitats$CATEGORY <- "Deleted from/with site"
  
  # Merge with additional columns from the 2018 dataset
  deleted_habitats <- left_join(deleted_habitats, hab_n2k_2018, by = c("SITECODE", "HABITATCODE"))
  
  # Combine deleted_habitats and habitats_changes
  changes_habitats_2018_2023 <- bind_rows(habitat_changes, deleted_habitats)
  
  # Convert to a sf
  changes_habitats_2018_2023_sf <- st_as_sf(changes_habitats_2018_2023)
  
  # Save as a geopackage
  country_code <- unique(changes_habitats_2018_2023_sf$MS)
  geopackage_path <- file.path(output_file_path, paste0(country_code, "_habitats_changes_ETRS89.gpkg"))
  
  # Write the sf object to the GeoPackage
  st_write(changes_habitats_2018_2023_sf, geopackage_path, driver = "GPKG")
  
  # Separate layers for each habitat code
  habitats_codes <- unique(changes_habitats_2018_2023_sf$HABITATCODE)
  for (code in habitats_codes) {
    habitat_data <- changes_habitats_2018_2023_sf[changes_habitats_2018_2023_sf$HABITATCODE == code, ]
    layer_name <- paste0("habitat_", code)
    st_write(habitat_data, geopackage_path, layer = layer_name, driver = "GPKG", append = TRUE)
  }
  
  print("The combined changes have been saved as a GeoPackage.")
}


# Usage:
gpkg_file_2018 <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/Maps/output/LT/Natura_2000_Network_2018/Habitats/n2k_LT_hab_ETRS89.gpkg"
gpkg_file_2023 <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/Maps/output/LT/Natura_2000_Network_2023/habitas/n2k_LT_hab_ETRS89.gpkg"
output_file <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/Maps/output/LT/Changes_Natura_2000_Network_2018_2023/habitats"


# Run the function
save_combined_habitat_changes(gpkg_file_2018, gpkg_file_2023, output_file)
