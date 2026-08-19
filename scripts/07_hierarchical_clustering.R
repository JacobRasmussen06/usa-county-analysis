#############################################################################
#
# 07_hierarchical_clustering.R
#
# Purpose:
# Explore and cluster the dataset using hierarchical clustering, and evaluate,
# aiding to find the types of different counties and building a stronger backbone
#
# Outputs:
# - county_hierarchical_clusters.rds
# - visualizations in figures/clustering
#
#############################################################################

# load required packages
library(tidyverse)
library(sf)
library(cluster)
library(factoextra)
library(dendextend)
library(mclust)
library(pheatmap)

#############################################################################
# Setup
#############################################################################
set.seed(123) # for reproducibility
county <- readRDS("data/finished/county_dataset.rds")

# Selecting variables to be clustered
clust_data <- county |> 
  st_drop_geometry() |>
  mutate(voter_turnout = as.numeric(voter_turnout)) |> 
  select(
    GEOID,
    county_name,
    STATEFP,
    # Demographic variables
    population_density,
    pop_stability_index,
    median_age,
    diversity_index,
    foreign_born_pct,
    average_household_size,
    # Education Variables
    high_school_pct,
    public_school_pct,
    # Economic variables
    median_household_income,
    unemployment_rate,
    gini_index,
    labor_participation_rate,
    # Housing variables
    housing_cost_burden_pct,
    # Employment variables
    agriculture_pct,
    government_pct,
    retail_pct,
    education_healthcare_pct,
    # Transportation Variables
    mean_commute_time,
    drive_alone_pct,
    internet_access_pct,
    # Political variables
    voter_turnout,
    dem_vote_share_2020,
    party_competitiveness_2020,
    # Geographical variables
    terrain_ruggedness,
    forest_coverage_pct,
    water_coverage_pct,
    annual_precip,
    mean_temp,
    distance_to_coast_miles
  )

# Several variables had some missing data, but not much, so imputing was chosen as the way to move forward to include every county, unlike PCA.
clust_finished_data <- clust_data |> 
  group_by(STATEFP) |>
  mutate(
    pop_stability_index = if_else( # imputation for pop_stability_index
      is.na(pop_stability_index),
      median(pop_stability_index, na.rm = TRUE),
      pop_stability_index
    ),
    voter_turnout = if_else( # imputation for voter_turnout
      is.na(voter_turnout),
      median(voter_turnout, na.rm = TRUE),
      voter_turnout
    ),
    dem_vote_share_2020 = if_else( # imputation for dem_vote_share_2020
      is.na(dem_vote_share_2020),
      median(dem_vote_share_2020, na.rm = TRUE),
      dem_vote_share_2020
    ),
    party_competitiveness_2020 = if_else( # imputation for party_competitiveness_2020
      is.na(party_competitiveness_2020),
      median(party_competitiveness_2020, na.rm = TRUE),
      party_competitiveness_2020
    ),
    forest_coverage_pct = if_else( # imputation for forest_coverage_pct
      is.na(forest_coverage_pct),
      median(forest_coverage_pct, na.rm = TRUE),
      forest_coverage_pct),
    
    median_household_income = if_else( # imputation for median_household_income
      is.na(median_household_income),
      median(median_household_income, na.rm = TRUE),
      median_household_income
    ),
    mean_commute_time = if_else( # imputation for mean_commute_time
      is.na(mean_commute_time),
      median(mean_commute_time, na.rm = TRUE),
      mean_commute_time
    ),
    public_school_pct = if_else( # imputation for public_school_pct
      is.na(public_school_pct),
      median(public_school_pct, na.rm = TRUE),
      public_school_pct
    )
  ) |>
  ungroup()

clust_finished_data <- clust_finished_data |>
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

clust_scaled <- clust_finished_data |> 
  select(-GEOID, -county_name, -STATEFP) |> 
  scale()

dist_mat <- dist(clust_scaled, method = "euclidean")

hc <- hclust(dist_mat, method = "ward.D2")

plot(hc, labels = FALSE, hang = -1)

fviz_nbclust(clust_scaled, FUN = hcut, method = "silhouette")
fviz_nbclust(clust_scaled, FUN = hcut, method = "wss")
gap_stat <- clusGap(
  scale(clust_finished_data |> select(-GEOID, -county_name, -STATEFP)),
  FUN = kmeans,
  K.max = 20,
  B = 100
)

fviz_gap_stat(gap_stat)

ks <- c(7, 10, 13, 20) # based on the statistics, these 4 potential cluster k's were selected for future consideration

sil_results <- map_dfr(ks, function(k) {
  clust <- cutree(hc, k = k)
  sil <- silhouette(clust, dist_mat)
  tibble(
    k = k,
    avg_silhouette = mean(sil[, "sil_width"])
  )
}) # 7, 10, and 13 perform similarly, while 20 is much worse on silhouette.

hc_clusters <- clust_data |>
  select(GEOID) |>
  mutate(
    cluster7 = factor(cutree(hc, k = 7)),
    cluster10 = factor(cutree(hc, k = 10)),
    cluster13 = factor(cutree(hc, k = 13)),
    cluster20 = factor(cutree(hc, k = 20))
  )

county_hc_clusters <- county |>
  left_join(
    hc_clusters,
    by = "GEOID"
  )

table(hc_clusters$cluster7)
table(hc_clusters$cluster10)
table(hc_clusters$cluster13)
table(hc_clusters$cluster20) # too many clusters with small amounts of counties, eliminated from future consideration

#############################################################################
# Cluster Maps and Choosing an Amount of Clusters
#############################################################################

# Helper function for creating maps
make_cluster_map <- function(data, cluster_var, title){
  ggplot(
    data |>
      filter(!is.na({{cluster_var}}))
  ) +
    geom_sf(
      aes(fill = {{cluster_var}}),
      color = NA
    ) +
    scale_fill_viridis_d(
      option = "turbo",
      name = "Cluster"
    ) +
    labs(
      title = title,
      subtitle = "Clusters were created based on ~30 characteristics of the counties"
    ) +
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
}

cluster7_map <- make_cluster_map(
  county_hc_clusters,
  cluster7,
  "Hierarchical County Clusters (k = 7)"
) 
ggsave(
  filename = "figures/clustering/maps/hierarchical_k7_county_map.png",
  plot = cluster7_map,
  width = 10,
  height = 6,
  dpi = 300
)

cluster10_map <- make_cluster_map(
  county_hc_clusters,
  cluster10,
  "Hierarchical County Clusters (k = 10)"
)
ggsave(
  filename = "figures/clustering/maps/hierarchical_k10_county_map.png",
  plot = cluster10_map,
  width = 10,
  height = 6,
  dpi = 300
)

cluster13_map <- make_cluster_map(
  county_hc_clusters,
  cluster13,
  "Hierarchical County Clusters (k = 13)"
) 
ggsave(
  filename = "figures/clustering/maps/hierarchical_k13_county_map.png",
  plot = cluster13_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Based on the maps and the previous analysis, 13 clusters seems to be the correct sweet spot. 

#############################################################################
# Evaluation, Profiling, and Analysis of Clusters
#############################################################################

pca_clusters <- readRDS(
  "data/finished/county_pca_clusters.rds"
)

adjustedRandIndex(
  pca_clusters$cluster13,
  hc_clusters$cluster13
) # The two clustering methods are somewhat similar, better than random chance, but not overly crazy at ~.45.

county_hc_clusters <- county_hc_clusters |> 
  select(-cluster7, -cluster10, -cluster20)

# Save the county dataset with the clusters
saveRDS(county_hc_clusters,"data/finished/county_hierarchical_clusters.rds")

# County profiles containing the average for every variable from county dataset
hc_profiles <- county_hc_clusters |>
  filter(!is.na(cluster13)) |>
  st_drop_geometry() |>
  group_by(cluster13) |>
  summarise(
    across(
      where(is.numeric),
      ~mean(.x, na.rm = TRUE)
    )
  )

# Cluster summaries with just some variables included
hc_cluster_summary <- county_hc_clusters |>
  st_drop_geometry() |>
  group_by(cluster13) |>
  summarise(
    counties = n(),
    avg_income = mean(median_household_income, na.rm = TRUE),
    avg_age = mean(median_age, na.rm = TRUE),
    avg_density = mean(population_density, na.rm = TRUE),
    avg_diversity = mean(diversity_index, na.rm = TRUE),
    avg_temperature = mean(mean_temp, na.rm = TRUE),
    avg_ruggedness = mean(terrain_ruggedness, na.rm = TRUE)
  )
# Chart of size by cluster
clus_size_chart <- ggplot(
  hc_cluster_summary,
  aes(x = cluster13, y = counties)) +
  geom_col(fill = "#3C8DAD", color = "black", width = .7) +
  geom_text(aes(label = counties), vjust = -0.4, size = 4.2) +
  labs(
    title = "Amount of Counties per Cluster Created by Hierarchical Clustering",
    subtitle = "Some clusters, like 13, are small, while others, like 4 and 9, have loads of counties",
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
  "figures/clustering/charts/hierarchical_cluster_sizes.png",
  clus_size_chart,
  width = 8,
  height = 5,
  dpi = 300
)

# Heatmap!
hc_heatmap <- hc_profiles |>
  select(
    -homeownership_rate
  ) |>
  column_to_rownames("cluster13") |>
  scale() |>
  as.data.frame()

hc_heatmap_graph <- pheatmap(
  hc_heatmap,
  cluster_rows = FALSE,
  cluster_cols = TRUE,
  main = "Hierarchical Cluster Profiles"
)
ggsave(
  "figures/clustering/heatmaps/hierarchical_heatmap.png",
  hc_heatmap_graph,
  width = 11,
  height = 5,
  dpi = 300
)

hc_sizes <- hc_cluster_summary |> 
  select(cluster13, counties)
hc_profiles <- hc_profiles |> 
  left_join(hc_sizes, by = "cluster13")
hc_profiles <- hc_profiles |> 
  rename(cluster = cluster13, size = counties)

# Finding representative counties
hc_clustered <- clust_scaled |>
  as.data.frame() |>
  mutate(
    GEOID = clust_finished_data$GEOID,
    cluster = hc_clusters$cluster13
  )
hc_centers <- # Hierarchical clusters don't come with a centroid, so it is calculated here
  hc_clustered |>
  group_by(cluster) |>
  summarise(across(where(is.numeric), mean))
feature_cols <- names(hc_clustered)[!names(hc_clustered) %in% c("GEOID", "cluster")]

# Representative Counties
hc_representatives <- hc_clustered |>
  rowwise() |>
  mutate(
    distance_to_center = sqrt(
      sum(
        (c_across(all_of(feature_cols)) -
           as.numeric(hc_centers[hc_centers$cluster == cluster, feature_cols]))^2
      )
    )
  ) |>
  ungroup() |>
  group_by(cluster) |>
  slice_min(distance_to_center, n = 1) |>
  ungroup()
hc_representatives_named <- hc_representatives |>
  left_join(
    county |> st_drop_geometry() |> select(GEOID, county_name),
    by = "GEOID"
  ) |>
  select(cluster, county_name, distance_to_center) |>
  mutate(cluster = as.numeric(cluster)) |> 
  rename(
    rep_name = county_name
  ) |>
  select(
    cluster,
    rep_name,
    distance_to_center
  )


hc_profiles_final <- hc_profiles |> 
  mutate(
    cluster = as.numeric(cluster)
  ) |>
  left_join(
    hc_representatives_named,
    by = "cluster"
  )
# Largest counties in the cluster
largest_counties <- county |>
  left_join(
    hc_clusters,
    by="GEOID"
  ) |>
  group_by(cluster13) |>
  arrange(desc(total_population)) |>
  slice_head(n=3) |> 
  select(GEOID, county_name, cluster13, everything())

saveRDS(hc_profiles_final, "data/final/hc_clusters_profiles.rds")
saveRDS(largest_counties, "data/final/hc_largest_counties.rds")
#############################################################################
# End of Script
#############################################################################