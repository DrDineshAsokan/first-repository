# HandsOn Exercise 4

# Geocoding and Mapping
library(tidygeocoder)
library(tidyverse)
library(sf)
library(here)
library(ggmap)


# Load Data (non-spatial)
## The data has been taken from the National Medical Council (NMC) Website
## It contains information on all approved medical colleges for MBBS course

# Read in medical college data from a CSV file using the 'here' package to construct the file path
med_colleges_df <- here('data', 'NMC_MBBS_Medical_Colleges_List.csv') |> 
  read_csv()

# Use the glimpse function to get a quick overview of the data structure and first few rows
med_colleges_df |> 
  glimpse()

# Retrieve the names of the columns in the dataframe
med_colleges_df |> 
  names()

# Clean the column names using the 'clean_names' function from the 'janitor' package and display the new names
# This standardizes column names to lower_case_with_underscores
med_colleges_df |> 
  janitor::clean_names() |> 
  names()

# Reassign to save the changes
med_colleges_df <- med_colleges_df |> 
  janitor::clean_names()

# Clean Address
med_colleges_df <- med_colleges_df |> 
  mutate(address = str_remove(name_and_address_of_medical_college_medical_institution, "\xa0"))

# Geocode the Addresses
# Geocode medical colleges in Maharashtra using tidygeocoder package with the default 'osm' method
med_1 <- med_colleges_df |> 
  filter(state == 'Maharashtra') |> 
  tidygeocoder::geocode(address = address)

# Geocode medical colleges in Maharashtra using ggmap package with the default 'google' method
med_2 <- med_colleges_df |> 
  filter(state == 'Maharashtra') |> 
  ggmap::mutate_geocode(location = address)

# Geocode medical colleges in Maharashtra using tidygeocoder package, explicitly specifying the 'google' method
med_3 <- med_colleges_df |> 
  filter(state == 'Maharashtra') |> 
  tidygeocoder::geocode(address = address,
                        method = 'google')

# Perform reverse geocoding on the geocoded medical colleges from med_3
rev_med_3 <- med_3 |> 
  tidygeocoder::reverse_geocode(lat = lat, 
                                long = long, 
                                method = 'osm', 
                                address = 'reverse_address')

# Convert the geocoded medical colleges into sf format
med_1 |>
st_as_sf(coords = c('long', 'lat'))

# Convert the geocoded medical colleges in Maharashtra to sf format with CRS 4326
mh_geocoded_med_col_sf <- med_1 |> 
  filter(!is.na(lat)) |> 
  st_as_sf(coords = c('long', 'lat'),
           crs = 4326)

# Create a plot of the geocoded medical colleges in Maharashtra using ggplot with geom_sf()
mh_geocoded_med_col_sf |> 
  ggplot() +
  geom_sf()

# Read in the spatial file containing data for state-level medical colleges
state_med_colleges_sf <- read_rds(here('spatial_files', 'state_med_colleges_sf.rds'))

# Create a plot showing only Maharashtra state medical colleges overlaid on the previous plot
mh_geocoded_med_col_sf |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = state_med_colleges_sf |> filter(state == 'Maharashtra'),
          fill = 'transparent')

# Write the geocoded medical colleges in Maharashtra to an RDS file for future use
mh_geocoded_med_col_sf |> 
  write_rds(here('spatial_files', 
                 'mh_geocoded_med_col_sf.rds'))
# Geocode the Addresses
# Geocode medical colleges in Maharashtra using tidygeocoder package with the default 'osm' method
med_4 <- med_colleges_df |> 
  filter(state == 'Tamil Nadu') |> 
  tidygeocoder::geocode(address = address)

med_3 <- med_colleges_df |> 
  filter(state == 'Tamil Nadu') |> 
  tidygeocoder::geocode(address = address,
                        method = 'arcgis')
rev_med_3 <- med_3 |> 
  tidygeocoder::reverse_geocode(lat = lat, 
                                long = long, 
                                method = 'arcgis', 
                                address = 'reverse_address')
