# Remove all from the environment
rm(list=ls())
gc()

# Set work directory if needed
setwd()


# Load libraries
library(stringr)
library(sf)
library(tidyverse)
library(dplyr)

# Path spatial and tabular data (set the path where the Natura 2000 file is located)
natura_2000_path <- "I:/biocon/ETC_Data_original/N2K_spatial_and_descriptive_end2017-25.05.2018/FME_60247F61_1692628888782_6276/SHAPE_1/OutputShape/Natura 2000 Spatial Data Official Release ETRS89 LAEA.shp"
natura_2000_tab_path <- "I:/biocon/ETC_Data_original/N2K_spatial_and_descriptive_end2017-25.05.2018/Tabular/NATURA2000SITES.txt"


# Open spatial data
natura_2000 <- st_read(natura_2000_path)

# Open tabular data
natura_2000_tab <- read.table(natura_2000_tab_path, 
                              header = T, 
                              sep = ",",
                              encoding = "UTF-8")

# View data
#View(natura_2000)
#View(natura_2000_tab)

# Compare sites codes
spatial_site_codes <- unique(natura_2000$SITECODE)
tabular_site_codes <- unique(natura_2000_tab$SITECODE)

# Find incongruity sitecodes 
missing_site_codes <- setdiff(spatial_site_codes, tabular_site_codes)

if(length(missing_site_codes) > 0){
  print(paste("Site codes present in spatial data but not in tabular data:", missing_site_codes))
} 

# Find any site codes present in tabular data but not in spatial data
extra_site_codes <- setdiff(tabular_site_codes, spatial_site_codes)

if(length(extra_site_codes) > 0){
  print(paste("Site codes present in tabular data but not in spatial data:", extra_site_codes))
} else { 
  print(paste("there are not missing site cod"))
}


# Function to check duplicate site codes
check_duplicate_site_codes <- function(data, col_name) {
  duplicated_site_codes <- duplicated(data[[col_name]])
  
  if (any(duplicated_site_codes)) {
    print(paste("Duplicate site codes found in", col_name, "column:"))
    duplicate_sites <- data[duplicated_site_codes, c("SITECODE","SITENAME","RELEASE_DA","MS")]
    print(duplicate_sites)
    release_date <- unique(duplicate_sites$RELEASE_DA) 
    write_sf(duplicate_sites, paste0("02_output/Natura_2000_duplicate_sites_release_",release_date, ".csv"), 
              quote = TRUE, sep = ",", fileEncoding = "UTF-8")
  } else {
    print(paste("No duplicate site codes found in", col_name, "column."))
  }
}

# check duplicate site codes
check_duplicate_site_codes(natura_2000, "SITECODE")
check_duplicate_site_codes(natura_2000_tab, "SITECODE")









