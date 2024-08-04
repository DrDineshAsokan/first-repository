# HandsOn Exercise 5

# Spatial Distribution of Malnutrition in India

# Load required libraries
library(tidyverse)   # For data manipulation and visualization
library(sf)          # For handling spatial data
library(here)        # For managing file paths
library(patchwork)   # For organizing multiple plots
library(ggspatial)   # For making pretty maps 

# Load the malnutrition data
malnutrition_df <- read_csv(here("data", "malnutrition_districts_nfhs.csv"))
# Load the India district shapefile
india_district_sf <- read_rds(here("spatial_files","india_district_sf.rds"))

# Explore the malnutrition data
malnutrition_df |> 
  glimpse()

# Explore the India district shapefile
india_district_sf |> 
  glimpse()

# Plot the India districts
india_district_sf |> 
  ggplot() +
  geom_sf()

# Check if the district names match in both datasets
india_district_sf$district_unique[!india_district_sf$district_unique %in% malnutrition_df$district_unique] 
malnutrition_df$district_unique[!malnutrition_df$district_unique %in% india_district_sf$district_unique]

# Explore malnutrition indicators with box plots
malnutrition_df |> 
  ggplot() +
  geom_boxplot(aes(x = indicator, y = nfhs5))

# Pivot data for better visualization
malnutrition_df |> 
  pivot_longer(cols = nfhs5:nfhs5, names_to = "nfhs", values_to = "value") |> 
  ggplot() +
  geom_boxplot(aes(x = indicator, y = value, fill = nfhs))

# Box plots for each state
malnutrition_df |> 
  pivot_longer(cols = nfhs5:nfhs5, names_to = "nfhs", values_to = "value") |> 
  ggplot() +
  geom_boxplot(aes(x = indicator, y = value, fill = nfhs)) + 
  facet_wrap(~state)

# Create choropleth maps for overweight indicator

# Filter data for overweight indicator
overweight_df <- malnutrition_df |> 
  filter(indicator == "overweight")

# Join spatial and overweight data
overweight_sf <- india_district_sf |> 
  left_join(overweight_df)

# Plot overweight data for NFHS-4
overweight_sf |> 
  ggplot() +
  geom_sf(aes(fill = nfhs5))

# Plot overweight data for NFHS-5
overweight_sf |> 
  ggplot() +
  geom_sf(aes(fill = nfhs5))

# Change color palette for better visualization

# NFHS-4 with color palette
overweight_sf |> 
  ggplot() +
  geom_sf(aes(fill = nfhs5)) + 
  scale_fill_distiller(palette = "Spectral")

# NFHS-5 with color palette
overweight_sf |> 
  ggplot() +
  geom_sf(aes(fill = nfhs5)) + 
  scale_fill_distiller(palette = "Spectral")

# Plot NFHS-4 and NFHS-5 side by side using patchwork

# Create individual plots
p_overweight_nfhs5 <- overweight_sf |> 
  ggplot() +
  geom_sf(aes(fill = nfhs5)) + 
  scale_fill_distiller(palette = "Spectral") +
  labs(title = "NFHS-4 (2015-16)")

p_overweight_nfhs5 <- overweight_sf |> 
  ggplot() +
  geom_sf(aes(fill = nfhs5)) + 
  scale_fill_distiller(palette = "Spectral") +
  labs(title = "NFHS-5 (2019-2021)")


# Lets add North Arrow and Scale from the ggspatial package
p_overweight_nfhs5 <- overweight_sf |> 
  ggplot() +
  geom_sf(aes(fill = nfhs5)) + 
  scale_fill_distiller(palette = "Spectral") +
  labs(title = "NFHS-4 (2015-16)") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_nautical) +
  annotation_scale()

p_overweight_nfhs5 <- overweight_sf |> 
  ggplot() +
  geom_sf(aes(fill = nfhs5)) + 
  scale_fill_distiller(palette = "Spectral") +
  labs(title = "NFHS-5 (2019-2021)") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_nautical) +
  annotation_scale()


# Combine the plots
p_overweight_nfhs5 + p_overweight_nfhs5

# Adjust scales for a common legend
(p_overweight_nfhs5 + p_overweight_nfhs5) + 
  plot_layout(guides = "collect") & 
  scale_fill_distiller(palette = "Spectral", limits = range(c(overweight_sf$nfhs5, overweight_sf$nfhs5)))

# Final plot with title and common legend
p_overweight <- ((p_overweight_nfhs5 + theme(legend.position = "none")) + p_overweight_nfhs5) + 
  plot_layout(ncol = 2,
              guides = "collect") +
  plot_annotation(title = "Distribution of overweight (%) among Under-5 chilren in Indian",
                  caption = "Data Source: National Family Health Survey (NFHS) \nhttps://rchiips.org/nfhs/",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 20))) & 
  scale_fill_distiller("overweight %", palette = "Spectral", limits = range(c(overweight_sf$nfhs5, overweight_sf$nfhs5)))

p_overweight

# Save the final plot
ggsave(here("plots", "overweight_plot.png"), width = 9, height = 6)

######################################################################

# Can you try it for the rest of the indicators?

