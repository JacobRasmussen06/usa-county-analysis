#############################################################################
#
#  08_gmm_clustering.R
#
# Purpose:
# Explore and cluster the dataset using Gaussian Mixture Model (GMM) and evaluate,
#  aiding to find types of different counties and building a baseline for similarity.
#
# Outputs:
# - county_gmm_clusters.rds
# - cluster_method_comparison.csv
# - visualizations in figures/clustering
#
#############################################################################

# load required packages
library(tidyverse)
library(sf)
library(mclust)

#############################################################################
# Setup
#############################################################################
set.seed(123) # for reproducibility
county <- readRDS("data/finished/county_dataset.rds")

gmm_data <- county |>
  st_drop_geometry() |>
  mutate(voter_turnout = as.numeric(voter_turnout)) |> 
  select(
    GEOID,
    STATEFP,
    county_name,
    # Same variables as hierarchical
    population_density,
    pop_stability_index,
    median_age,
    diversity_index,
    foreign_born_pct,
    average_household_size,
    high_school_pct,
    public_school_pct,
    median_household_income,
    unemployment_rate,
    gini_index,
    labor_participation_rate,
    housing_cost_burden_pct,
    agriculture_pct,
    government_pct,
    retail_pct,
    education_healthcare_pct,
    mean_commute_time,
    drive_alone_pct,
    internet_access_pct,
    voter_turnout,
    dem_vote_share_2020,
    party_competitiveness_2020,
    terrain_ruggedness,
    forest_coverage_pct,
    water_coverage_pct,
    annual_precip,
    mean_temp,
    distance_to_coast_miles
  )

# Imputing missing data, just like with hierarchical
finished_data <- gmm_data |> 
  group_by(STATEFP) |>
  mutate(
    pop_stability_index = if_else(
      is.na(pop_stability_index),
      median(pop_stability_index, na.rm = TRUE),
      pop_stability_index
    ),
    voter_turnout = if_else(
      is.na(voter_turnout),
      median(voter_turnout, na.rm = TRUE),
      voter_turnout
    ),
    dem_vote_share_2020 = if_else(
      is.na(dem_vote_share_2020),
      median(dem_vote_share_2020, na.rm = TRUE),
      dem_vote_share_2020
    ),
    party_competitiveness_2020 = if_else(
      is.na(party_competitiveness_2020),
      median(party_competitiveness_2020, na.rm = TRUE),
      party_competitiveness_2020
    ),
    forest_coverage_pct = if_else(
      is.na(forest_coverage_pct),
      median(forest_coverage_pct, na.rm = TRUE),
      forest_coverage_pct),
    
    median_household_income = if_else(
      is.na(median_household_income),
      median(median_household_income, na.rm = TRUE),
      median_household_income
    ),
    mean_commute_time = if_else(
      is.na(mean_commute_time),
      median(mean_commute_time, na.rm = TRUE),
      mean_commute_time
    ),
    public_school_pct = if_else(
      is.na(public_school_pct),
      median(public_school_pct, na.rm = TRUE),
      public_school_pct
    )
  ) |>
  ungroup()

finished_data <- finished_data |>
  mutate( # the planning areas in Connecticut were missing data even after imputation, so instead of state median, national median was used to include these 9 county equivalents.
    across(
      c(
        pop_stability_index,
        voter_turnout,
        dem_vote_share_2020,
        party_competitiveness_2020,
        forest_coverage_pct
      ),
      ~ if_else(
        is.na(.x),
        median(.x, na.rm = TRUE),
        .x
      )
    )
  )

#############################################################################
# Clustering
#############################################################################

gmm_scaled <- finished_data |>
  select(-GEOID, -STATEFP, -county_name) |>
  scale()

# gmm_model <- Mclust(
  #gmm_scaled,
  # G = 5:20)
# This code was run to find the optimal amount of clusters. 14 was the classification the model made. 

gmm_model <- Mclust(gmm_scaled, G = 14)

table(gmm_model$classification) # The amount of clusters was relatively consistent with no major outliers except for one 78 county cluster

mean(gmm_model$uncertainty) # Uncertainty was quite low, around 0.04

gmm_probabilities <- as.data.frame(
  gmm_model$z
)
gmm_probabilities$GEOID <- gmm_data$GEOID
gmm_probabilities <- gmm_probabilities |> 
  select(GEOID, everything())

gmm_assignments <- tibble(
  GEOID = gmm_data$GEOID,
  gmm_cluster = factor(gmm_model$classification),
  gmm_uncertainty = gmm_model$uncertainty
)

county_gmm <- county |>
  left_join(
    gmm_assignments,
    by = "GEOID"
  )
# Save the dataset with the cluster assignments
saveRDS(county_gmm,"data/finished/county_gmm_clusters.rds")

#############################################################################
# Analysis and Visualizations
#############################################################################

# Probability distribution of the uncertainty
dist_prob <- ggplot(gmm_assignments,
       aes(x = gmm_uncertainty)) +
  geom_histogram(
    bins = 30,
    fill = "#3C8DAD",
    color = "black"
  ) +
  labs(
    title = "Distribution of GMM Assignment Uncertainty",
    subtitle = "Most Counties Have No Uncertainty, but Some Fit Into Multiple Clusters",
    x = "Uncertainty",
    y = "Counties"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold"))
ggsave(
  "figures/clustering/charts/gmm_cluster_uncertainty_distribution.png",
  dist_prob,
  width = 8,
  height = 5,
  dpi = 300
)

# Table of the 10 most uncertain counties
most_uncertain <- county_gmm |>
  st_drop_geometry() |>
  arrange(desc(gmm_uncertainty)) |>
  slice_head(n = 10) |>
  select(
    county_name, 
    STATEFP,
    gmm_cluster,
    gmm_uncertainty
  )

# Map of the county clusters
cluster_map <- ggplot(
  county_gmm |>
    filter(!is.na(gmm_cluster))) +
  geom_sf(
    aes(fill = gmm_cluster),
    color = NA) +
  scale_fill_viridis_d(
    option = "turbo") +
  labs(
    title = "GMM-Based County Clusters",
    subtitle = "Gaussian mixture model classification",
    fill = "Cluster") +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = .5
    ),
    plot.subtitle = element_text(
      size = 11,
      hjust = .5
    ),
    plot.caption = element_text(
      size = 8,
      hjust = .5
    ),
    legend.position = "right",
    legend.title = element_text(
      face = "bold"
    )
  )
ggsave(
  filename = "figures/clustering/maps/gmm_county_map.png",
  plot = cluster_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Profile of an average county in the cluster
gmm_profiles <- county_gmm |>
  filter(!is.na(gmm_cluster)) |>
  st_drop_geometry() |>
  group_by(gmm_cluster) |>
  summarise(
    across(
      where(is.numeric),
      ~mean(.x, na.rm = TRUE)
    )
  )
gmm_cluster_summary <- county_gmm |>
  st_drop_geometry() |>
  group_by(gmm_cluster) |>
  summarise(
    counties = n(),
    avg_income = mean(median_household_income, na.rm = TRUE),
    avg_age = mean(median_age, na.rm = TRUE),
    avg_density = mean(population_density, na.rm = TRUE),
    avg_diversity = mean(diversity_index, na.rm = TRUE),
    avg_temperature = mean(mean_temp, na.rm = TRUE),
    avg_ruggedness = mean(terrain_ruggedness, na.rm = TRUE)
  )

# Cluster size chart
size_chart <- ggplot(gmm_cluster_summary,
  aes(x = gmm_cluster, y = counties)) +
  geom_col(fill = "#3C8DAD", color = "black", width = .7) +
  geom_text(aes(label = counties), vjust = -0.4, size = 4.2) +
  labs(
    title = "Amount of Counties per Cluster Created by GMM Clustering",
    subtitle = "Some clusters vary, but most clusters are of a similar size.",
    x = "Cluster",
    y = "Counties"
  ) +
  theme_classic() + 
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold"))
ggsave(
  "figures/clustering/charts/gmm_cluster_sizes.png",
  size_chart,
  width = 8,
  height = 5,
  dpi = 300
)

# Heatmap of the cluster variables
gmm_heatmap <- gmm_profiles |>
  select(
    -homeownership_rate
  ) |>
  column_to_rownames("gmm_cluster") |>
  scale() |>
  as.data.frame()

gmm_heatmap_graph <- pheatmap(
  gmm_heatmap,
  cluster_rows = FALSE,
  cluster_cols = TRUE,
  main = "GMM Cluster Profiles"
)
ggsave(
  "figures/clustering/heatmaps/gmm_heatmap.png",
  gmm_heatmap_graph,
  width = 11,
  height = 5,
  dpi = 300
)

#############################################################################
# Compare to Previous Clustering
#############################################################################

county_hierarchical <- readRDS("data/finished/county_hierarchical_clusters.rds")
pca_clusters <- readRDS("data/finished/county_pca_clusters.rds")

# Comparison table of ARI
ari_comparison <- tibble(
  Method_1 = c(
    "PCA",
    "PCA",
    "Hierarchical"),
  Method_2 = c(
    "Hierarchical",
    "GMM",
    "GMM"),
  Adjusted_Rand_Index = c(
    adjustedRandIndex(
      pca_clusters$cluster13,
      county_hierarchical$cluster13),
    adjustedRandIndex(
      pca_clusters$cluster13,
      gmm_assignments$gmm_cluster),
    adjustedRandIndex(
      county_hierarchical$cluster13,
      gmm_assignments$gmm_cluster)))
# Notably, the GMM has low ARI (but not low enough to say it's randomly different) with both other clustering methods, indicating it's bringing something different to the table

# Save the comparison table
write_csv(ari_comparison,"data/finished/cluster_method_comparison.csv")

#############################################################################
# Analyzing Specific Counties in Clusters
#############################################################################

# Representative counties
gmm_clustered <- gmm_scaled |>
  as.data.frame() |>
  mutate(
    GEOID = finished_data$GEOID,
    cluster = gmm_assignments$gmm_cluster
  )

gmm_centers <- # GMM clusters don't come with a centroid, so one is calculated
  gmm_clustered |>
  group_by(cluster) |>
  summarise(across(where(is.numeric), mean))
feature_cols <- names(gmm_clustered)[
  !names(gmm_clustered) %in% c("GEOID", "cluster")
]
gmm_reps <- gmm_clustered |>
  rowwise() |>
  mutate(
    distance_to_center = sqrt(
      sum(
        (c_across(all_of(feature_cols)) -
            as.numeric(
              gmm_centers[
                gmm_centers$cluster == cluster,
                feature_cols
              ]))^2))) |>
  ungroup() |>
  group_by(cluster) |>
  slice_min(distance_to_center, n = 1) |>
  ungroup()
gmm_representatives_named <- gmm_reps |>
  left_join(
    county |> st_drop_geometry() |> select(GEOID, county_name),
    by = "GEOID"
  ) |>
  select(cluster, county_name, distance_to_center)

# Largest counties in the cluster
largest_counties <- county |>
  left_join(gmm_assignments, by="GEOID") |>
  group_by(gmm_cluster) |>
  arrange(desc(total_population)) |>
  select(GEOID, gmm_cluster, county_name, total_population) |> 
  slice_head(n=5)

#############################################################################
# End of Script
#############################################################################