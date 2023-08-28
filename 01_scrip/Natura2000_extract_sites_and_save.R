
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
# extract_and_save_countries(country_codes, natura_2000, natura_2000_tab, output_folder)

# Function to extract and save Natura 2000 sites for a given country
################################################################################

# Remove all objects from the environment
rm(list=ls())
gc()


# Set the working directory if needed
setwd()


# Load libraries
library(sf)
library(tidyverse)
library(terra)
library(dplyr)

# Path spatial and tabular data
natura_2000_path <- "I:/biocon/ETC_Data_original/N2K_spatial_and_descriptive_end2017-25.05.2018/FME_60247F61_1692628888782_6276/SHAPE_1/OutputShape/Natura 2000 Spatial Data Official Release ETRS89 LAEA.shp"
natura_2000_tab_path <- "I:/biocon/ETC_Data_original/N2K_spatial_and_descriptive_end2017-25.05.2018/Tabular/NATURA2000SITES.txt"

# Open spatial data
natura_2000 <- st_read(natura_2000_path)

# Open tabular data
natura_2000_tab <- read.table(natura_2000_tab_path, 
                              header = TRUE, 
                              sep = ",",
                              encoding = "UTF-8")


# Eliminate duplicates if it is the case
if (anyDuplicated(natura_2000$SITECODE)) {
  natura_2000 <- natura_2000[!duplicated(natura_2000$SITECODE), ]
}



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
    
    # # Convert date column to the desired format
    # country_spatial$RELEASE_DA <- format(as.Date(country_spatial$RELEASE_DA), "%m/%d/%Y")
    
    # Convert date column to the desired format
    country_spatial$RELEASE_DA <- as.character(country_spatial$RELEASE_DA)
    
    #write the shapefile with the correct data fromat
    filename <- file.path(output_path, paste("natura_2000_", country_code, "_ETRS89", ".shp", sep = ""))
    writeVector(vect(country_spatial), filename, overwrite = TRUE, options="ENCODING=UTF-8")
  }
}


# Test of the function
country_code <- c("LT")

#output path
output_path <- "02_output/LT/Natura_2000_2017"

extract_and_save_country(country_code, natura_2000, natura_2000_tab, output_path)



