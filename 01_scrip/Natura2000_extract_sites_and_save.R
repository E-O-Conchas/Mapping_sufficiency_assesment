
################################################################################
# Function: extract_and_save_countries
# Description: Extracts and saves Natura 2000 sites for multiple countries
# Parameters:
#   - country_codes: A character vector of country codes for which sites need to be extracted
#   - spatial_data: Spatial data containing Natura 2000 sites information for all countries
#   - tabular_data: Tabular data containing Natura 2000 sites information for all countries
#   - output_path: The path to the output directory where the extracted sites will be saved
# Returns: None (outputs the extracted sites as shapefiles and prints any missing or extra site codes)
# Dependencies: tidyverse, sf, terra

# Usage example:
# country_codes <- c("PL", "LT", "DE")  # List of country codes
# output_folder <- "path/to/output/folder"
# extract_and_save_countries(country_codes, n2k, n2000_tabular, output_folder)

# Function to extract and save Natura 2000 sites for a given country
################################################################################


# Remove all objects from the environment
rm(list=ls())
gc()


# Set the working directory

setwd("I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/Maps/data")
getwd()


# Load libraries

library(sf)
library(tidyverse)
library(terra)
library(dplyr)

# Open data set

#open the spatial
n2k_path <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/Maps/data/n2k_spatial_and_descriptive_end2018/Natura2000_end2018_Shapefile/Natura2000_end2018_epsg3035.shp"
n2000 <- st_read(n2k_path)

# Check if there are duplicate in the sites 
length(unique(n2000$SITECODE))

#eliminate all the duplicate data (SITECODE)
n2k <- unique(n2000)

#open tabular CSV
n2k_tabular_path <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/Maps/data/n2k_spatial_and_descriptive_end2018/Tabular_2/NATURA2000SITES.csv"

n2k_tabular <- read.csv(n2k_tabular_path,
                        header = T,
                        sep = ",",
                        quote = "\"",
                        encoding = "UTF-8")

# Check names of columns
names(n2k_tabular)

path_txt <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/Maps/data/n2k_spatial_and_descriptive_end2018/Tabular_2/NATURA2000SITES.txt"

n2k_tabular_txt <- read.table(path_txt,
                              header = TRUE,
                              sep = ",",
                              quote = "\"")



#Function to extract and save Natura 2000 sites for a given country
extract_and_save_country <- function(country_code, spatial_data, tabular_data, output_path) {
  
  for (country_code in country_code){
    
    country_spatial <- spatial_data %>%
      filter(MS == country_code)
    
    country_tabular <- tabular_data %>%
      filter(COUNTRY_CODE == country_code)
    
    
    spatial_sites <- unique(country_spatial$SITECODE)
    tabular_sites <- unique(country_tabular$SITECODE)
    
    missing_sites <- setdiff(tabular_sites, spatial_sites)
    extra_sites <- setdiff(spatial_sites, tabular_sites)
    
    if (length(missing_sites) > 0) {
      print(paste("Missing site codes in spatial data for", country_code, ":", missing_sites))
    } else {
      print(paste("No missing site codes in spatial data for", country_code))
    }
    
    if (length(extra_sites) > 0) {
      print(paste("Extra site codes in spatial data for", country_code, ":", extra_sites))
    } else {
      print(paste("No extra site codes in spatial data for", country_code))
    }
    
    country_folder <- file.path(output_path, country_code)
    dir.create(country_folder, showWarnings = FALSE)
    
    # # Convert date column to the desired format
    # country_spatial$RELEASE_DA <- format(as.Date(country_spatial$RELEASE_DA), "%m/%d/%Y")
    
    # Convert date column to the desired format
    country_spatial$RELEASE_DA <- as.character(country_spatial$RELEASE_DA)
    
    #write the shapefile with the correct data fromat
    filename <- file.path(country_folder, paste("n2k_", country_code, "_ETRS89", ".shp", sep = ""))
    writeVector(vect(country_spatial), filename, overwrite = TRUE, options="ENCODING=UTF-8")
  }
}




# Test of the function
country_code <- c("LT")

#output path
output_path <- "I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/Maps/output/LT/Natura_2000_Network_2018"

extract_and_save_country(country_code, n2k, n2k_tabular, output_path)



