# HandsOn Exercise 2

# Adding Attributes to Features

# Load Packages
library(sf)                   # Load the 'sf' package for handling spatial data
library(tidyverse)            # Load the 'tidyverse' package for data manipulation
library(here)                 # Load the 'here' package for constructing reliable file paths
library(ggspatial)

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

# Get State wise summaries in number of medical colleges
med_colleges_df |> 
  count(state, sort = TRUE) 

# Create a summary table with the number of medical colleges in each state
summary_tbl <- med_colleges_df |> 
  count(state, sort = TRUE) |> 
  rename(no_of_colleges = n)

# Check the total number of medical colleges 
summary_tbl |> 
  summarize(total = sum(no_of_colleges))

# Load Data (Spatial)
india_state_sf <- here("spatial_files", "India_State_Boundary.shp") |> 
  st_read()

# Check the CRS of the spatial data
india_state_sf |> st_crs()

# Convert the spatial data into Geodetic CRS
india_state_sf <- india_state_sf |> 
  st_transform(4326)

# Check the CRS of the spatial data
india_state_sf |> st_crs()

# Check the names of the columns in the spatial data
india_state_sf |> names()

# Rename the 'State_Name' column to 'state' for consistency with the medical college data
india_state_sf <- india_state_sf |> 
  rename(state = State_Name)

# Check the number of states in each of the datasets
india_state_sf$state |> unique()  # Spatial data
summary_tbl$state |> unique() # Non-Spatial data

# Check the state names that are present in the medical college data but not in the spatial data
(summary_tbl$state |> unique()) %in%  india_state_sf$state
(summary_tbl$state |> unique())[!(summary_tbl$state |> unique()) %in%  india_state_sf$state]

# Clean the State names so that they can be matched in both the datasets
summary_tbl <- summary_tbl |> 
  mutate(
    state = case_when(
      state == "Andaman Nicobar Islands" ~ "Andaman & Nicobar" ,
      state == "Chattisgarh"             ~ "Chhattishgarh",
      state == "Dadra and Nagar Haveli"  ~ "Daman and Diu and Dadra and Nagar Haveli",
      state == "Jammu & Kashmir"         ~ "Jammu and Kashmir",
      state == "Orissa"                  ~ "Odisha" ,
      state == "Pondicherry"             ~ "Puducherry" ,
      state == "Tamil Nadu"              ~ "Tamilnadu" ,
      state == "Telangana"               ~ "Telengana",
      TRUE ~ state
    )
  )


# Re-check the state names to see that if they are cleaned
(summary_tbl$state |> unique()) %in%  india_state_sf$state
(summary_tbl$state |> unique())[!(summary_tbl$state |> unique()) %in%  india_state_sf$state]

# Merge the non-spatial and spatial data based on the 'state' column
state_med_colleges_sf <- india_state_sf |> 
  st_make_valid() |>
  group_by(state) |>
  summarise() |>
  left_join(summary_tbl, by = "state")

# Check the structure of the merged data
state_med_colleges_sf

# Plot the map
state_med_colleges_sf |> 
  ggplot() +
  geom_sf()

# Create a Choropleth of Number of Medical Colleges in India
# Plot a Choropleth
state_med_colleges_sf |> 
  ggplot() +
  geom_sf(aes(fill = no_of_colleges))

# Change Color Palette
state_med_colleges_sf |> 
  ggplot() +
  geom_sf(aes(fill = no_of_colleges)) +
  scale_fill_viridis_c()


# Add Additional Map Elements
state_med_colleges_sf |> 
  ggplot() +
  geom_sf(aes(fill = no_of_colleges)) +
  geom_sf_label(aes(label = no_of_colleges)) +
  scale_fill_viridis_c() + 
  annotation_scale(location = "bl") + # Add Scale
  annotation_north_arrow(location = "tr", which_north = "true") + # add north arrow
  labs(title = "Spatial Distribution of Medical Colleges in India",
       subtitle = "Data from the National Medical Council (NMC)",
       caption = "Note: The states of Lakhwadeep and Ladakh do not have any medical colleges (Grey)",
       fill = "Number of Medical Colleges") +
  theme_minimal() 

# Save the plot
ggsave(file = here("plots", "med_college_choropleth_map.png"),
       width = 6, 
       height = 6,
       scale = 1.2)

# Save the Merged Data as an RDS file
state_med_colleges_sf |> 
  write_rds(here("spatial_files", "state_med_colleges_sf.rds"))

