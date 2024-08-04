# HandsOn Exercise 3

# Adding Attributes to Features (RASTER)

# Load Packages
library(sf)                   # Load the 'sf' package for handling spatial data
library(tidyverse)            # Load the 'tidyverse' package for data manipulation
library(here)                 # Load the 'here' package for constructing reliable file paths
library(raster)               # Load the 'raster' package for handling raster data
library(exactextractr)        # Load the 'exactextractr' package for extracting raster data
library(ggspatial)

# Load the Merged Data (spatial)
## The data has been created under the 'hands_on_exercise2' script

state_med_colleges_sf <- readr::read_rds(here("spatial_files", "state_med_colleges_sf.rds"))

# Create a new attribute for the dataset called state_population
# The population counts are coming from the SEDAC Raster <https://sedac.ciesin.columbia.edu/>
# The raster file is stored in the 'spatial_files' folder 
# (gpw_v4_population_count_rev11_2020_5_min.tif)
# Lets Load it

pop_raster <- raster(here("spatial_files", "gpw_v4_population_count_rev11_2020_5_min.tif"))

# Plot the raster
pop_raster |> 
  plot()

# Crop the raster to the extent of the state_med_colleges_sf dataset
ind_pop_raster <- pop_raster |> 
  raster::crop(state_med_colleges_sf) 

# Plot Cropped Raster
ind_pop_raster |> 
  plot()

# Extract Population Counts for each of the State
exactextractr::exact_extract(ind_pop_raster, 
                             state_med_colleges_sf, 
                             fun = "sum")

state_med_colleges_sf  <- state_med_colleges_sf |> 
  mutate(state_pop_2020 = exactextractr::exact_extract(ind_pop_raster, 
                                                       state_med_colleges_sf, 
                                                       fun = "sum"))
state_med_colleges_sf |> 
  write_rds(here("spatial_files", "state_med_colleges_sf.rds"))

state_med_colleges_sf |> 
  st_drop_geometry() |>
  summarise(tot_pop = sum(state_pop_2020))

# Create number of medical colleges per 1 million population
state_med_colleges_sf <- state_med_colleges_sf |> 
  mutate(med_coll_per_million = no_of_colleges / (state_pop_2020 / 1e6))

# Create a Choropleth of Medical Colleges per Million Population
# Plot a Choropleth
state_med_colleges_sf |> 
  ggplot() +
  geom_sf(aes(fill = med_coll_per_million))

# Change Color Palette
state_med_colleges_sf |> 
  ggplot() +
  geom_sf(aes(fill = med_coll_per_million)) +
  scale_fill_viridis_c(trans = "log") # Log Transformation


# Add Additional Map Elements
state_med_colleges_sf |> 
  ggplot() +
  geom_sf(aes(fill = med_coll_per_million)) +
  scale_fill_viridis_c(trans = "log") + 
  annotation_scale(location = "bl") + # Add Scale
  annotation_north_arrow(location = "tr", which_north = "true") + # add north arrow
  labs(title = "Medical Colleges per Million Population in India",
       subtitle = "Medical College Data from the National Medical Council (NMC) \nPopulation Data from SEDAC, NASA",
       caption = "Note: The states of Lakhwadeep and Ladakh do not have any medical colleges (Grey)",
       fill = "Medical Colleges per Million Population") +
  theme_minimal()

# Save the plot
ggsave(file = here("plots", "med_college_pm_choropleth_map.png"),
       width = 6, 
       height = 6,
       scale = 1.2)

# Save the Data as an RDS file
state_med_colleges_sf |> 
  write_rds(here("spatial_files", "state_med_colleges_sf.rds"))
