# Remove all objects from the environment
rm(list=ls())
gc()

# Load libraries
library(sf)
library(dplyr)

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
natura2000_spe_2018 <- st_read(gpkg_file_2018_path, layer = layer_name) 
natura2000_spe_2023 <- st_read(gpkg_file_2023_path, layer = layer_name_2)

# Convert to data frames
spe_n2k_2018_df <- as.data.frame(spe_n2k_2018)
spe_n2k_2023_df <- as.data.frame(spe_n2k_2023)

# Reduce datasets to necessary columns only
spe_n2k_2018_df <- natura2000_spe_2018 %>% select(SITECODE, SPECIESCODE, SPECIESNAME, POPULATION_CATEGORIES)
spe_n2k_2023_df <- natura2000_spe_2023 %>% select(SITECODE, SPECIESCODE, SPECIESNAME, POPULATION_CATEGORIES)

# Convert the columns as character to be able to make the futher joins
spe_n2k_2018_df$SPECIESCODE <- as.character(spe_n2k_2018_df$SPECIESCODE)
spe_n2k_2023_df$SPECIESCODE <- as.character(spe_n2k_2023_df$SPECIESCODE)

# Free code for checking the data set before performing the categorization analysis


# Category 1: "remaining_in_site" 
# Sites and species area listed for 2023 and 2018 (no changes)
# Species that are newly listed are not POPULATION D
remaining_in_site <- spe_n2k_2023 %>%
  inner_join(spe_n2k_2018, by = c("SITECODE", "SPECIESCODE"), suffix = c(".2023", ".2018"),relationship = "many-to-many") %>%
  filter(POPULATION_CATEGORIES.2023 != "D" & POPULATION_CATEGORIES.2018 != "D") %>%
  mutate(CATEGORY = "Remaining in Site")


# Category 2: "Added to an existing sites" 
# Species have been listed for an existing site in 2023, and these newly listed species don’t belong to POPULATION “D”
# Species were categorized as POPULATION "D" in 2018 but have now been listed and categorized as POPULATION A, B, C, or "”
# Identify sites that are new in 2023 by using `anti_join` to find SITECODES that do not exist in 2018 data.
new_sites_2023 <- spe_n2k_2023_df %>%
  anti_join(spe_n2k_2018_df, by = "SITECODE")

# For the existing sites, make sure to exclude the newly identified sites.
existing_sites_2023 <- spe_n2k_2023_df %>%
  filter(!SITECODE %in% new_sites_2023$SITECODE)

added_to_existing_site <- existing_sites_2023 %>%
  left_join(spe_n2k_2018_df, by = c("SITECODE", "SPECIESCODE"), suffix = c(".2023", ".2018"), multiple = 'all') %>%
  filter((is.na(POPULATION_CATEGORIES.2018) & POPULATION_CATEGORIES.2023 != "D") |
           (POPULATION_CATEGORIES.2018 == "D" & !POPULATION_CATEGORIES.2023 %in% c("D"))) %>%
  mutate(CATEGORY = "Added to an existing site")


# category 3: "Added to a new site" 
# Sites and species were not listed in 2018 but in 2023 are newly listed 
# Species that are newly listed are categorized as A, B, C, or ""

# Use `filter` to further refine to new habitats that are not marked as "D".
added_to_a_new_site <- new_sites_2023 %>%
  filter(POPULATION_CATEGORIES %in% c("A", "B", "C", "no data")) %>%
  mutate(CATEGORY = "Added to a new site") %>% 
  rename(POPULATION_CATEGORIES.2023 = POPULATION_CATEGORIES)


# Category 4: "deleted from/with site" 
# Sites and species were listed in 2018 but are not longer in 2023
# Species that were in 2018 categorized as A, B, C or "" but have in 2023 been categorized as "D"

# Criteria 1: Removal from Sites
deleted_from_site <- spe_n2k_2018 %>%
  anti_join(spe_n2k_2023, by = c("SITECODE", "SPECIESCODE")) %>%
  mutate(CATEGORY = "Deleted from site") %>%
  rename(POPULATION_CATEGORIES.2017 = POPULATION_CATEGORIES)


# Criteria 2: Population Decline
population_decline <- spe_n2k_2018 %>%
  inner_join(spe_n2k_2023, by = c("SITECODE", "SPECIESCODE"), suffix = c(".2018", ".2023"), relationship = "many-to-many") %>%
  filter(POPULATION_CATEGORIES.2018 %in% c("A", "B", "C", "") & POPULATION_CATEGORIES.2023 == "D") %>%
  mutate(CATEGORY = "Population declined to D")


# Combine categories to form the final data set
changes_species_1 <- bind_rows(
  remaining_in_site,
  added_to_existing_site,
  population_decline)

changes_species_2 <- bind_rows(
  added_to_a_new_site,
  deleted_from_site)

# combine the geom from the changes species 1
changes_species_1 <- changes_species_1 %>%
  mutate(geom = coalesce(geom.2023, geom.2018)) %>%
  select(-geom.2023, -geom.2018)


# combine the two changes of species
changes_species <- bind_rows(
  changes_species_1,
  changes_species_2)


# consolidate the species names
changes_species$Consolidated_Species <- ifelse(!is.na(changes_species$SPECIESNAME.2023), changes_species$SPECIESNAME.2023,
                                               ifelse(!is.na(changes_species$SPECIESNAME.2018), changes_species$SPECIESNAME.2018,
                                                      ifelse(!is.na(changes_species$SPECIESNAME), changes_species$SPECIESNAME, NA)))


# delete columns we dont need 
changes_species <- changes_species %>%
  select(-SPECIESNAME.2018, -SPECIESNAME.2023, -SPECIESNAME) %>%
  rename(SPECIESNAME = Consolidated_Species)


# Convert to sf
changes_species_sf <- st_as_sf(changes_species) 

# Set the CRS to be sure 
st_crs(changes_species_sf) <- 3035 

# Save as a geopackage
country_code <- unique(changes_species_sf$MS)
geopackage_path <- file.path(output_file, paste0("natura2000_",country_code, "_species_changes_ETRS89.gpkg"))


# Write the sf object to the GeoPackage
st_write(changes_species_sf, geopackage_path, driver = "GPKG", overwrite = T)


# Separate layers for each habitat code
species_codes <- unique(changes_species_sf$SPECIESCODE)
for (code in species_codes) {
  species_data <- changes_species_sf[changes_species_sf$SPECIESCODE == code, ]
  layer_name <- paste0("species_", code)
  st_write(species_data, geopackage_path, layer = layer_name, driver = "GPKG", append = TRUE)
}

print("The combined changes have been saved as a GeoPackage.")


