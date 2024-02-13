# Remove all objects from the environment
rm(list=ls())
gc()

# Load libraries
library(sf)
library(dplyr)

# Set you work directory
setwd("your work directory")
getwd()

# Define the paths to the data and the layer name
gpkg_file_2018_path <- "Path to the natura 2000 habitats that includes all the site, for the corresponding year"
gpkg_file_2023_path <- "Path to the natura 2000 habitats that includes all the site, for the corresponding year" 
output_file <- "Output folder"

# get the layer name
layer_name <- st_layers(gpkg_file_2018_path)$name[1]
layer_name_2 <- st_layers(gpkg_file_2023_path)$name[1]


# Read habitats spatial data 2018 and 2023 for Poland
natura2000_hab_2018 <- st_read(gpkg_file_2018_path, layer = layer_name) 
natura2000_hab_2023 <- st_read(gpkg_file_2023_path, layer = layer_name_2) 

# Reduce datasets to necessary columns only
hab_n2k_2018 <- natura2000_hab_2018 %>% select(SITECODE, HABITATCODE, DESCRIPTION, RELSURFACE_CATEGORIES)
hab_n2k_2023 <- natura2000_hab_2023 %>% select(SITECODE, HABITATCODE, DESCRIPTION, RELSURFACE_CATEGORIES)

# Convert to data frames
hab_n2k_2018_df <- as.data.frame(hab_n2k_2018)
hab_n2k_2023_df <- as.data.frame(hab_n2k_2023)

# Check how many Na are in the relsurface
sum(is.na(hab_n2k_2023_df$RELSURFACE_CATEGORIES))

# Free code for checking the data set before performing the categorization analysis


# Category 1 "remaining_in_site"
# Sites and habitat are listed for 2023 and 2018 (no changes)
# Habitats that are newly listed are not RELSURFACE_CATEGORIES D
remaining_in_site <- hab_n2k_2023_df %>%
  inner_join(hab_n2k_2018_df, by = c("SITECODE", "HABITATCODE"), suffix = c(".2023", ".2018"), multiple = 'all') %>%
  filter(RELSURFACE_CATEGORIES.2023 != "D" & RELSURFACE_CATEGORIES.2018 != "D") %>%
  mutate(CATEGORY = "Remaining in Site")

# Category 2: "Added to an existing sites"
# Habitats have been listed for an existing site in 2023, and these newly listed Habitats don’t belong to RELSURFACE_CATEGORIES “D”
# Habitats were categorized as RELSURFACE_CATEGORIES "D" in 2018 but have now been listed and categorized as RELSURFACE_CATEGORIES A, B, C, or "-”

# Identify sites that are new in 2023 by using `anti_join` to find SITECODES that do not exist in 2017 data.
new_sites_2023 <- hab_n2k_2023_df %>%
  anti_join(hab_n2k_2018_df, by = 'SITECODE')

# For the existing sites, make sure to exclude the newly identified Sites
existing_sites_2023 <- hab_n2k_2023_df %>%
  filter(!SITECODE %in% new_sites_2023$SITECODE)	

added_to_existing_site <- existing_sites_2023 %>%
  left_join(hab_n2k_2018_df, by = c("SITECODE", "HABITATCODE"), suffix = c(".2023", ".2018"), multiple = 'all') %>%
  filter((is.na(RELSURFACE_CATEGORIES.2018) & RELSURFACE_CATEGORIES.2023 != "D") |
           (RELSURFACE_CATEGORIES.2018 == "D" & !RELSURFACE_CATEGORIES.2023 %in% c("D"))) %>%
  mutate(CATEGORY = "Added to an existing site")

# category 3: "Added to a new site" 
# Sites and habitats were not listed in 2018 but in 2023 are newly listed 
# Habitats that are newly listed are categorized as A, B, C, or "-"

# Use Filter to further refine to new habitats that are not marked as "D"
added_to_a_new_site <-  hab_n2k_2023_df %>%
  filter(RELSURFACE_CATEGORIES %in% c("A", "B", "C", "")) %>%
  mutate(CATEGORY = "Added to a new site") %>%
  rename(RELSURFACE_CATEGORIES.2023 = RELSURFACE_CATEGORIES)

# Category 4: "deleted from/with site" ------------------------------------
# Sites and Habitats were listed in 2018 but are not longer in 2023
# Habitats that were in 2018 categorized as A, B, C or "-" but have in 2023 been categorized as "D"

# Criteria 1: Removal from Sites
deleted_from_site <- hab_n2k_2018_df %>%
  anti_join(hab_n2k_2023_df, by = c("SITECODE", "HABITATCODE")) %>%
  mutate(CATEGORY = "Deleted from site") %>%
  rename(RELSURFACE_CATEGORIES.2018 = RELSURFACE_CATEGORIES)

# Criteria 2: Population Decline
realsurface_decline <- hab_n2k_2018_df %>%
  inner_join(hab_n2k_2023_df, by = c("SITECODE", "HABITATCODE"), suffix = c(".2018", ".2023"), multiple = "all") %>%
  filter(RELSURFACE_CATEGORIES.2018 %in% c("A", "B", "C", "-") & RELSURFACE_CATEGORIES.2023 == "D") %>%
  mutate(CATEGORY = "Relsurface declined to D")


# Combine categories to create the final data set 
changes_habitats_1 <- bind_rows(
  remaining_in_site,
  added_to_existing_site,
  realsurface_decline)

changes_habitats_2 <- bind_rows(
  added_to_a_new_site,
  deleted_from_site)

# combine the geom from the changes habitats 1
changes_habitats_1 <- changes_habitats_1 %>%
  mutate(geom = coalesce(geom.2023, geom.2018)) %>%
  select(-geom.2023, -geom.2018)

# combine the two changes of habitats
changes_habitats <- bind_rows(
  changes_habitats_1,
  changes_habitats_2)


# consolidate the species names
changes_habitats$consolidated_habitats <- ifelse(!is.na(changes_habitats$DESCRIPTION.2023), changes_habitats$DESCRIPTION.2023,
                                                 ifelse(!is.na(changes_habitats$DESCRIPTION.2018), changes_habitats$DESCRIPTION.2018,
                                                        ifelse(!is.na(changes_habitats$DESCRIPTION), changes_habitats$DESCRIPTION, NA)))


# delete columns we don't need 
changes_habitats <- changes_habitats %>%
  select(-DESCRIPTION.2018, -DESCRIPTION.2023, -DESCRIPTION) %>%
  rename(HABITATNAME = consolidated_habitats)´

# Convert to sf
changes_habitats_sf <- st_as_sf(changes_habitats) 

# Set the CRS to be sure 
st_crs(changes_habitats_sf) <- 3035 

# Save as a geopackage
country_code <- 'Set the ISO2 country code'
geopackage_path <- file.path(output_file, paste0("natura_2000_",country_code, "_habitats_changes_ETRS89.gpkg"))

# Write the sf object to the GeoPackage
st_write(changes_habitats_sf, geopackage_path, driver = "GPKG")

# Separate layers for each habitat code
habitats_codes <- unique(changes_habitats_sf$HABITATCODE)
for (code in habitats_codes) {
  habitat_data <- changes_habitats_sf[changes_habitats_sf$HABITATCODE == code, ]
  layer_name <- paste0("habitat_", code)
  st_write(habitat_data, geopackage_path, layer = layer_name, driver = "GPKG", append = TRUE)
}

print("The combined changes have been saved as a GeoPackage.")



