# Remove all objects from the environment
rm(list=ls())
gc()

# Load libraries
library(sf)
library(dplyr)

setwd("I:/biocon/Emmanuel_Oceguera/projects/2023_03_ETC_BE/Task 1.1.7.2 Protected areas dataflows/Subtask 2.viii Sufficiency assesment/outputs/LT")


# Define the paths to the data and the layer name
gpkg_file_2017_path <- "Natura_2000_sites_v2017/species_new/natura_2000_LT_species_all_sites_ETRS89.gpkg"
gpkg_file_2023_path <- "Natura_2000_sites_v2023/species_new/natura_2000_LT_species_all_sites_ETRS89.gpkg"
output_file <- "Changes_Natura_2000_sites_v2017_v2023/species_new"

# get the layer name
layer_name <- st_layers(gpkg_file_2017_path)$name[1]

# Read species spatial data 2017 and 2023
nat2000_spe_2017 <- st_read(gpkg_file_2017_path, layer = layer_name)
nat2000_spe_2023 <- st_read(gpkg_file_2023_path, layer = layer_name) 


# Convert to data frames
spe_n2k_2017_df <- as.data.frame(nat2000_spe_2017)
spe_n2k_2023_df <- as.data.frame(nat2000_spe_2023)


names(spe_n2k_2017_df)

# Check how many Na are in the relsurface
sum(is.na(spe_n2k_2017_df$POPULATION))
sum(is.na(spe_n2k_2023_df$POPULATION))

# check the unique values for the population
unique(sort(spe_n2k_2017_df$POPULATION_CATEGORIES))
unique(sort(spe_n2k_2023_df$POPULATION_CATEGORIES))

# Reduce datasets to necessary columns only
spe_n2k_2017_df <- spe_n2k_2017_df %>% select(SITECODE, SPECIESCODE, SPECIESNAME, POPULATION_CATEGORIES, geom)
spe_n2k_2023_df <- spe_n2k_2023_df %>% select(SITECODE, SPECIESCODE, SPECIESNAME, POPULATION_CATEGORIES, geom)


# Convert the columns as character to be able to make the futher joins
spe_n2k_2017_df$SPECIESCODE <- as.character(spe_n2k_2017_df$SPECIESCODE)
spe_n2k_2023_df$SPECIESCODE <- as.character(spe_n2k_2023_df$SPECIESCODE)


# Category 1: "remaining_in_site" ------------------------------------------
# Sites and species area listed for 2023 and 2018 (no changes)
# Species that are newly listed are not POPULATION D
remaining_in_site <- spe_n2k_2023_df %>%
  inner_join(spe_n2k_2017_df, by = c("SITECODE", "SPECIESCODE"), suffix = c(".2023", ".2017"),relationship = "many-to-many") %>%
  filter(POPULATION_CATEGORIES.2023 != "D" & POPULATION_CATEGORIES.2017 != "D") %>%
  mutate(CATEGORY = "Remaining in Site")



# Category 2: "Added to an existing sites" --------------------------------
# Species have been listed for an existing site in 2023, and these newly listed species don’t belong to POPULATION “D”
# Species were categorized as POPULATION "D" in 2018 but have now been listed and categorized as POPULATION A, B, C, or "”

# Identify sites that are new in 2023 by using `anti_join` to find SITECODES that do not exist in 2017 data.
new_sites_2023 <- spe_n2k_2023_df %>%
  anti_join(spe_n2k_2017_df, by = "SITECODE")

# For the existing sites, make sure to exclude the newly identified sites.
existing_sites_2023 <- spe_n2k_2023_df %>%
  filter(!SITECODE %in% new_sites_2023$SITECODE)

added_to_existing_site <- existing_sites_2023 %>%
  left_join(spe_n2k_2017_df, by = c("SITECODE", "SPECIESCODE"), suffix = c(".2023", ".2017")) %>%
  filter((is.na(POPULATION_CATEGORIES.2017) & POPULATION_CATEGORIES.2023 != "D") |
           (POPULATION_CATEGORIES.2017 == "D" & !POPULATION_CATEGORIES.2023 %in% c("D"))) %>%
  mutate(CATEGORY = "Added to an existing site")



# category 3: "Added to a new site" ---------------------------------------
# Sites and species were not listed in 2018 but in 2023 are newly listed 
# Species that are newly listed are categorized as A, B, C, or "no data"

# Use `filter` to further refine to new habitats that are not marked as "D".
added_to_a_new_site <- new_sites_2023 %>%
  filter(POPULATION_CATEGORIES %in% c("A", "B", "C", "no data")) %>%
  mutate(CATEGORY = "Added to a new site") %>% 
  rename(POPULATION_CATEGORIES.2023 = POPULATION_CATEGORIES)


# Category 4: "deleted from/with site" ------------------------------------
# Sites and species were listed in 2018 but are not longer in 2023
# Species that were in 2018 categorized as A, B, C or "" but have in 2023 been categorized as "D"

# Criteria 1: Removal from Sites
deleted_from_site <- spe_n2k_2017_df %>%
  anti_join(spe_n2k_2023_df, by = c("SITECODE", "SPECIESCODE")) %>%
  mutate(CATEGORY = "Deleted from site") %>%
  rename(POPULATION_CATEGORIES.2017 = POPULATION_CATEGORIES)


# Criteria 2: Population Decline
population_decline <- spe_n2k_2017_df %>%
  inner_join(spe_n2k_2023_df, by = c("SITECODE", "SPECIESCODE"), suffix = c(".2017", ".2023"), multiple = "all") %>%
  filter(POPULATION_CATEGORIES.2017 %in% c("A", "B", "C", "no data") & POPULATION_CATEGORIES.2023 == "D") %>%
  mutate(CATEGORY = "Relsurface declined to D")



# Combine categories to form the final data set ---------------------------
changes_species_1 <- bind_rows(
  remaining_in_site,
  added_to_existing_site,
  population_decline)

changes_species_2 <- bind_rows(
  added_to_a_new_site,
  deleted_from_site)

# combine the geom from the changes species 1
changes_species_1 <- changes_species_1 %>%
  mutate(geom = coalesce(geom.2023, geom.2017)) %>%
  select(-geom.2023, -geom.2017)

# combine the two changes of species
changes_species <- bind_rows(
  changes_species_1,
  changes_species_2)


# consolidate the species names
changes_species$Consolidated_Species <- ifelse(!is.na(changes_species$SPECIESNAME.2023), changes_species$SPECIESNAME.2023,
                                               ifelse(!is.na(changes_species$SPECIESNAME.2017), changes_species$SPECIESNAME.2017,
                                                      ifelse(!is.na(changes_species$SPECIESNAME), changes_species$SPECIESNAME, NA)))



# delete columns we dont need 
changes_species <- changes_species %>%
  select(-SPECIESNAME.2017, -SPECIESNAME.2023, -SPECIESNAME) %>%
  rename(SPECIESNAME = Consolidated_Species)



# Convert to sf
changes_species_sf <- st_as_sf(changes_species) 

# Set the CRS to be sure 
st_crs(changes_species_sf) <- 3035 

# Save as a geopackage
country_code <- 'LT'
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

