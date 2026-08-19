#############################################################################
#
# 06_pca_clustering.R
#
# Purpose:
#  Using the PCA, perform k means clustering on the data
# finding out what types of different counties there are in the United States.
#
# Outputs:
# - 13 county clusters
# - visualizations in figures/clustering
#
#############################################################################

# Load required packages
library(factoextra)
library(cluster)
library(tidyverse)
library(mclust)
library(pheatmap)


#############################################################################
# Setup
#############################################################################
set.seed(123) # for reproducibility

county_pca <- readRDS("data/finished/county_pca.rds")

pca_scores <- county_pca |>
  st_drop_geometry() |>
  select(GEOID, starts_with("PC")) |>
  drop_na()

pca_scores_20 <- pca_scores |>
  select(PC1:PC20)
pca_scores_30 <- pca_scores |>
  select(PC1:PC30)

# Determine optimal k
fviz_nbclust(pca_scores_20, kmeans, method = "wss", k.max = 25)
fviz_nbclust(pca_scores_20, kmeans, method = "silhouette", k.max = 25)
gap_stat <- clusGap(pca_scores_20, FUN = kmeans, nstart = 25, K.max = 15,B = 100)
fviz_gap_stat(gap_stat)

# From these statistics, the following five candidates for best k were selected
km5 <- kmeans(pca_scores_20, centers = 5, nstart = 100, iter.max = 100)
km8 <- kmeans(pca_scores_20, centers = 8, nstart = 100, iter.max = 100)
km13 <- kmeans(pca_scores_20, centers = 13, nstart = 100, iter.max = 100)
km16 <- kmeans(pca_scores_20, centers = 16, nstart = 100, iter.max = 100)
km20 <- kmeans(pca_scores_20, centers = 20, nstart = 100, iter.max = 100)

cluster_sizes <- tibble(
  k = c(5,8,13,16,20),
  min_cluster = c(
    min(table(km5$cluster)),
    min(table(km8$cluster)),
    min(table(km13$cluster)),
    min(table(km16$cluster)),
    min(table(km20$cluster))
  ))

cluster_assignments <- pca_scores |>
  select(GEOID) |>
  mutate(
    cluster5 = factor(km5$cluster),
    cluster8 = factor(km8$cluster),
    cluster13 = factor(km13$cluster),
    cluster16 = factor(km16$cluster),
    cluster20 = factor(km20$cluster)
  )

county_clusters <- county_pca |>
  left_join(
    cluster_assignments,
    by = "GEOID"
  )

county_cluster_map <- county_clusters |>
  filter(!is.na(cluster13))

pcspace <- ggplot(county_clusters,
       aes(PC1, PC2, color = cluster13)) +
  geom_point(alpha = .6, size = 1.2) +
  scale_color_viridis_d(option = "turbo") +
  labs(
    title = "County Clusters in Principal Component Space",
    subtitle = "Clusters identified using k-means (k = 13)",
    x = "PC1",
    y = "PC2",
    color = "Cluster"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold"))
ggsave(
  "figures/clustering/charts/pc1_2_cluster_scatter.png",
  pcspace,
  width = 8,
  height = 5,
  dpi = 300
)


#############################################################################
# Cluster Maps and Selection
#############################################################################

k5_county_map <- ggplot(
  county_clusters |>
    filter(!is.na(cluster5))) +
  geom_sf(
    aes(fill = cluster5),
    color = NA) +
  scale_fill_viridis_d(
    option = "turbo") +
  labs(
    title = "PCA-Based County Clusters (k = 5)",
    subtitle = "Clusters generated from the first 20 principal components",
    fill = "Cluster") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = .5))
ggsave(
  filename = "figures/clustering/maps/pca_k5_county_map.png",
  plot = k5_county_map,
  width = 10,
  height = 6,
  dpi = 300
)
k8_county_map <- ggplot(
  county_clusters |>
    filter(!is.na(cluster8))) +
  geom_sf(
    aes(fill = cluster8),
    color = NA) +
  scale_fill_viridis_d(
    option = "turbo") +
  labs(
    title = "PCA-Based County Clusters (k = 8)",
    subtitle = "Clusters generated from the first 20 principal components",
    fill = "Cluster") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = .5))
ggsave(
  filename = "figures/clustering/maps/pca_k8_county_map.png",
  plot = k8_county_map,
  width = 10,
  height = 6,
  dpi = 300
)

k13_county_map <- ggplot(
  county_clusters |>
    filter(!is.na(cluster13))) +
  geom_sf(
    aes(fill = cluster13),
    color = NA) +
  scale_fill_viridis_d(
    option = "turbo") +
  labs(
    title = "PCA-Based County Clusters (k = 13)",
    subtitle = "Clusters generated from the first 20 principal components",
    fill = "Cluster") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = .5))
ggsave(
  filename = "figures/clustering/maps/pca_k13_county_map.png",
  plot = k13_county_map,
  width = 10,
  height = 6,
  dpi = 300
)

k16_county_map <- ggplot(
  county_clusters |>
    filter(!is.na(cluster16))) +
  geom_sf(
    aes(fill = cluster16),
    color = NA) +
  scale_fill_viridis_d(
    option = "turbo") +
  labs(
    title = "PCA-Based County Clusters (k = 16)",
    subtitle = "Clusters generated from the first 20 principal components",
    fill = "Cluster") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = .5))
ggsave(
  filename = "figures/clustering/maps/pca_k16_county_map.png",
  plot = k16_county_map,
  width = 10,
  height = 6,
  dpi = 300
)

k20_county_map <- ggplot(
  county_clusters |>
    filter(!is.na(cluster20))) +
  geom_sf(
    aes(fill = cluster20),
    color = NA) +
  scale_fill_viridis_d(
    option = "turbo") +
  labs(
    title = "PCA-Based County Clusters (k = 20)",
    subtitle = "Clusters generated from the first 20 principal components",
    fill = "Cluster") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = .5))
ggsave(
  filename = "figures/clustering/maps/pca_k20_county_map.png",
  plot = k20_county_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Based on both the maps and the table, it seems that k = 13 is the best option
# for the kmeans clustering. This is because while it has some small clusters,
# every cluster is meaningful and there is nuanced divide between each cluster

#############################################################################
# Robustness check Between PC30 and PC20
#############################################################################

# Testing for the PC30 robustness
km13_30 <- kmeans(
  pca_scores_30,
  centers = 13,
  nstart = 100,
  iter.max = 100
)
cluster_assignments_30 <- pca_scores |>
  select(GEOID) |>
  mutate(
    cluster13_30 = factor(km13_30$cluster)
  )
county_clusters_30 <- county_pca |>
  left_join(
    cluster_assignments_30,
    by = "GEOID"
  )
k13_pc30_county_map <- ggplot(
  county_clusters_30 |>
    filter(!is.na(cluster13_30))) +
  geom_sf(
    aes(fill = cluster13_30),
    color = NA) +
  scale_fill_viridis_d(
    option = "turbo") +
  labs(
    title = "PCA Clusters Using 30 Principal Components (k = 13)",
    subtitle = "90% of variance retained",
    fill = "Cluster") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = .5))
ggsave(
  filename = "figures/clustering/maps/pca_k13_pc30_county_map.png",
  plot = k13_pc30_county_map,
  width = 10,
  height = 6,
  dpi = 300
)

adjustedRandIndex(
  km13$cluster,
  km13_30$cluster
)

#############################################################################
# Cluster Profiles
#############################################################################
clustersize <- tibble(table(km13$cluster))
cluster_profiles_pc20 <- county_clusters |>
  filter(!is.na(cluster13)) |>
  st_drop_geometry() |>
  group_by(cluster13) |>
  summarise(
    across(
      where(is.numeric),
      ~mean(.x, na.rm = TRUE))) |> 


profile_scaled_20 <- cluster_profiles_pc20 |>
  column_to_rownames("cluster13") |>
  scale() |>
  as.data.frame()

cluster_profiles_heatmap <- cluster_profiles_pc20 |>
  select(
    -starts_with("PC"),
    -homeownership_rate)

profile_scaled_pc20 <- cluster_profiles_heatmap |>
  column_to_rownames("cluster13") |>
  scale() |>
  as.data.frame()

pc20_heatmap <- pheatmap(
  profile_scaled_pc20,
  cluster_rows = FALSE,
  cluster_cols = TRUE,
  main = "County Cluster Profiles (PC20 + k=13)"
)
ggsave(
  "figures/clustering/heatmaps/pc20_heatmap.png",
  pc20_heatmap,
  width = 11,
  height = 5,
  dpi = 300
)

cluster_profiles_pc30 <- county_clusters_30 |>
  filter(!is.na(cluster13_30)) |>
  st_drop_geometry() |>
  group_by(cluster13_30) |>
  summarise(
    across(
      where(is.numeric),
      ~mean(.x, na.rm = TRUE)
    )
  )
profile_scaled_pc30 <- cluster_profiles_pc30 |>
  select(
    -starts_with("PC"),
    -homeownership_rate
  ) |>
  column_to_rownames("cluster13_30") |>
  scale() |>
  as.data.frame()

pc30_heatmap <- pheatmap(
  profile_scaled_pc30,
  cluster_rows = FALSE,
  cluster_cols = TRUE,
  main = "County Cluster Profiles (30 PCA Components, k = 13)"
)
ggsave(
  "figures/clustering/heatmaps/pc30_heatmap.png",
  pc30_heatmap,
  width = 11,
  height = 5,
  dpi = 300
)

# Based on the heatmaps, PC20 seems to be more interpretable and easier to use than PC30, which does not give enough new nuance to be worth using.

#############################################################################
# Cluster Summary Statistics
#############################################################################

cluster_sizes <- county_clusters |>
  filter(!is.na(cluster13)) |>
  st_drop_geometry() |>
  group_by(cluster13) |>
  summarise(
    counties = n()
  ) |>
  ungroup()

cluster_profiles_final <- cluster_profiles_pc20 |> 
  left_join(cluster_sizes, by = "cluster13")

cluster_size_chart <- ggplot(
  cluster_sizes,
  aes(
    x = factor(cluster13),
    y = counties)) +
  geom_col(
    fill = "#3C8DAD",
    color = "black",
    width = .7) +
  geom_text(
    aes(label = counties),
    vjust = -0.4,
    size = 3.2) +
  labs(
    title = "Amount of Counties Per PCA Based Cluster",
    subtitle = "Some of the 13 clusters are small, while some are large.",
    x = "Cluster",
    y = "Number of Counties") +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold"))
ggsave(
  "figures/clustering/charts/pca_cluster_sizes.png",
  cluster_size_chart,
  width = 8,
  height = 5,
  dpi = 300
)

cluster_summary <- county_clusters |>
  filter(!is.na(cluster13)) |>
  st_drop_geometry() |>
  group_by(cluster13) |>
  summarise(
    counties = n(),
    median_age = mean(median_age, na.rm = TRUE),
    under_18 = mean(under_18_pct, na.rm = TRUE),
    diversity = mean(diversity_index, na.rm = TRUE),
    income = mean(median_household_income, na.rm = TRUE),
    poverty = mean(poverty_rate, na.rm = TRUE),
    unemployment = mean(unemployment_rate, na.rm = TRUE),
    college = mean(college_grad_pct, na.rm = TRUE),
    density = mean(population_density, na.rm = TRUE),
    commute = mean(mean_commute_time, na.rm = TRUE),
    temperature = mean(mean_temp, na.rm = TRUE),
    elevation = mean(mean_elevation, na.rm = TRUE),
    ruggedness = mean(terrain_ruggedness, na.rm = TRUE),
    agriculture = mean(agriculture_pct, na.rm = TRUE),
    manufacturing = mean(manufacturing_pct, na.rm = TRUE),
    government = mean(government_pct, na.rm = TRUE)
  )

#############################################################################
# Representative Counties
#############################################################################

cluster_centers <- as.data.frame(km13$centers)

pc20_clustered <- pca_scores_20 |>
  mutate(
    GEOID = pca_scores$GEOID,
    cluster = km13$cluster
  )

pc_cols <- names(pca_scores_20)

representatives <- pc20_clustered |>
  rowwise() |>
  mutate(
    distance_to_center = sqrt(
      sum(
        (c_across(all_of(pc_cols)) -
           cluster_centers[cluster, pc_cols])^2))) |>
  ungroup() |>
  group_by(cluster) |>
  slice_min(
    distance_to_center,
    n = 1
  ) |>
  ungroup()

county <- readRDS("data/finished/county_dataset.rds")

representatives_named <- representatives |>
  left_join(
    county |> select(GEOID, county_name),
    by = "GEOID"
  ) |>
  rename(
    rep_name = county_name
  ) |>
  select(
    cluster,
    rep_name,
    distance_to_center
  )


cluster_profiles_final <- cluster_profiles_final |> 
  mutate(
    cluster = as.numeric(cluster13)
  ) |>
  left_join(
    representatives_named,
    by = "cluster"
  )

cluster_profiles_final <- cluster_profiles_final |>
  left_join(
    pca_cluster_types |>
      select(cluster, cluster_name, cluster_color),
    by = "cluster"
  )

saveRDS(cluster_profiles_final, "data/final/pca_clusters_profiles.rds")

largest_counties <- county |>
  left_join(
    pc20_clustered |> select(GEOID, cluster),
    by = "GEOID"
  ) |>
  group_by(cluster) |>
  arrange(desc(total_population)) |>
  slice_head(n = 3) |>
  select(cluster, county_name, total_population)

saveRDS(largest_counties, "data/final/pca_largest_counties.rds")
#############################################################################
# Save a Cluster Dataset
#############################################################################

county_pca_clusters <- county_clusters |>
  select(
    GEOID,
    county_name,
    cluster5,
    cluster8,
    cluster13,
    cluster16,
    cluster20
  )

saveRDS(
  county_pca_clusters,
  "data/finished/county_pca_clusters.rds"
)

#############################################################################
# End of Script
#############################################################################