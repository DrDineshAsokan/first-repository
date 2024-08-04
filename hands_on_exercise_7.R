# HandsOn Exercise 6

# Spatial Clustering of Malnutrition in India


# The steps involved in the clustering analysis are as follows:
# - Filtering the malnutrition data for the overweight indicator.
# - Joining the overweight data with the spatial data for India districts.
# - Creating spatial weights to measure spatial relationships between districts.
# - Computing Local Moran's I statistics to identify spatial clustering patterns.
# - Assigning colors to different types of clustering patterns.
# - Creating cluster labels and categorizing the clusters.
# - Adding the clustering information to the spatial data.
# - Plotting the districts colored by the identified clustering patterns.

# Load required libraries
library(tidyverse)   # For data manipulation and visualization
library(sf)          # For handling spatial data
library(here)        # For managing file paths
library(patchwork)   # For organizing multiple plots
library(rgeoda)      # LISA Statistics
library(spdep)       # Global Moran's Statistics

sf_use_s2(FALSE)

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

# CLUSTERING OF DISTRICTS WITH HIGH PROPORTION OF overweight (NFHS-4)

######################################################
# GLOBAL CLUSTERING
######################################################

# Let us check for global clustering using `spdep` package

# Filter data for overweight indicator
overweight_df <- malnutrition_df |> 
  filter(indicator == "overweight")

# Join spatial data with overweight data
overweight_sf <- india_district_sf |> 
  left_join(overweight_df)

# STEP ONE: Define neighboring polygons
# Define the neighbors using the Queen contiguity criterion
nb <- poly2nb(overweight_sf, queen=TRUE)

# STEP TWO: Assign weights to the neighbors
# Create a weights list with a binary (0/1) weight scheme
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights

# STEP THREE: Compute the (weighted) neighbor mean values
# Estimate the spatial lag for each district
overweight_sf <- overweight_sf |> 
  mutate(spatial_lag = lag.listw(lw, overweight_sf$nfhs5))

# STEP FOUR: Plot Moran's Scatter Plot
# Moran's Scatter Plot shows the relationship between overweight and its spatial lag
overweight_sf |> 
  ggplot(aes(x = nfhs5, y = spatial_lag)) +
  geom_point() +
  geom_smooth(method = "lm")

spdep::moran.plot(overweight_sf$nfhs5, lw)

# OLS Regression to estimate the slope of the Moran's Scatter Plot
ols_model <- lm(spatial_lag ~ nfhs5, data = overweight_sf)
coef(ols_model)[2]

# STEP FIVE: Compute the Moran’s I statistic
# Moran's I statistic measures spatial autocorrelation
moran(overweight_sf$nfhs5, lw, length(nb), Szero(lw))

# STEP SIX: Perform a Hypothesis Test
# Test the hypothesis that overweight is randomly distributed across districts

# 6a: ANALYTICAL Method
# Perform Moran's I test using the analytical method
moran.test(overweight_sf$nfhs5, lw, alternative="greater")

# 6b: MONTE CARLO Method
# Run Monte Carlo simulation to test Moran's I statistic
set.seed(1234)
mc_moran <- moran.mc(overweight_sf$nfhs5, lw, nsim=9999, alternative="greater")

# 6c: Plot the results
# Plot the null distribution of Moran's I values
plot(mc_moran)
# Interpretation: The observed statistic (0.68111) is significantly higher than expected, indicating clustering.

# 6d: Display the resulting statistics
# Print the results of the Monte Carlo simulation
mc_moran



######################################################
# LOCAL CLUSTERING
######################################################

# Load necessary libraries for local clustering
library(rgeoda)  # For local spatial analysis

# STEP ONE: Create Spatial Weights
# Create a spatial weights object using Queen contiguity
queen_w <- queen_weights(overweight_sf)

# STEP TWO: Perform Local Moran's I Analysis
# Compute Local Moran's I statistics for overweight data
lisa <- local_moran(queen_w, overweight_sf["nfhs5"])

# Define color schemes for visualization based on Local Moran's I results
lisa_colors <- lisa_colors(lisa)

# Define labels and clusters from Local Moran's I results
lisa_labels <- lisa_labels(lisa)
lisa_clusters <- lisa_clusters(lisa)
lisa_pvals <- lisa_pvalues(lisa)
lisa_vals <- lisa_values(lisa)

# Create factor labels for clustering based on Local Moran's I results
lisa_labs <- factor(lisa_clusters, levels = 0:(length(lisa$labels)-1), labels = lisa$labels)

# Add Local Moran's I results to the spatial data
clustering_sf <- overweight_sf |>  bind_cols(lisa_clusters = lisa_clusters, 
                                           lisa_labs = lisa_labs, 
                                           lisa_pvals = lisa_pvals,
                                           lisa_vals = lisa_vals)

# STEP THREE: Categorize P-values
# Categorize p-values for plotting significance
clustering_sf <- clustering_sf |> 
  mutate(lisa_pvals_cat = case_when(
    lisa_pvals <= 0.001 ~ "p <= 0.001",
    lisa_pvals <= 0.01 ~ "p <= 0.01",
    lisa_pvals <= 0.05 ~ "p <= 0.05",
    TRUE ~ "Not Significant",
    is.na(lisa_pvals) ~ NA_character_
  ))

# Define colors for p-value categories
pval_colors <- c("#eeeeee", "#1b7837", "#a6dba0", "#d9f0d3")

# STEP FOUR: Create Plots
# Create plot showing local clustering results
p_hs <- clustering_sf  |>  
  ggplot() +
  geom_sf(aes(fill = lisa_labs), lwd=0.2) +
  scale_fill_manual(name = "Clustering", values = lisa_colors) +
  theme_bw()

# Create plot showing p-value categories
p_pval <- clustering_sf  |>  
  ggplot() +
  geom_sf(aes(fill = lisa_pvals_cat), lwd=0.2) +
  scale_fill_manual(name = "Local Moran's P Value", values = pval_colors) +
  theme_bw()

# STEP FIVE: Combine the Plots
# Combine the two plots into one figure for comparison
p_overweight <- (p_hs + p_pval) + 
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(title = "Spatial Clustering of overweight (%) among Under-5 Children in India",
                  caption = "Data Source: National Family Health Survey (NFHS) \nhttps://rchiips.org/nfhs/",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 20)))

# Display the combined plot
p_overweight

# Save the final plot to a file
ggsave(here("plots", "clustering_overweight_plot.png"), width = 9, height = 6)


######################################################################

# Can you try it for the rest of the indicators?
