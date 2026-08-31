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
library(sf)

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
      ~mean(.x, na.rm = TRUE))) 

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

pca_profiles <- tibble(
  cluster = 1:13,
  cluster_name = c(
    "Frigid Retirement Communities",
    "Rural Appalachia / Lower Midwest",
    "Stable Urban Megacities",
    "Extraction Counties",
    "Northern Rural America",
    "Midwest Suburbia",
    "Western Highlands",
    "East Coast Aging Counties",
    "Sun Belt Metros",
    "Underserved Southern Communities",
    "Industrial Belt Cities",
    "Border Counties",
    "Growing Urban Centers"
  ),
  description = c(
    "By far, the counties in this cluster are the oldest, with over 28% of the population on average being over 65. These counties are among the coldest in the country, with a few outliers in California and Arizona. These counties have a lot of veterans, not a lot of immigrants, are typically much more rural, typically vote republican, and have a lot of forests.", 
    "These counties, almost entirely located in the Appalachia region into states like Missouri and Arkansas, are extremely rural in nature. This cluster has the largest number of counties (605), is not very diverse, and has low diversity and income, with a higher poverty rate and reliance on SNAP. It has the highest rate of construction workers, and overwhelmingly votes republican.", 
    "This cluster contains just 30 counties, mostly only urban centers. Its population is relatively stable, with minor population growth. Cities like San Francisco, New York, Chicago, and Boston all fit into this category. They’re extremely dense, and rely a lot on public transportation, and vote democratic in not very close elections.",
    "These counties, prevalent across Texas and Oklahoma as well as parts of the west and Plains, are unique because of their high level of workforce in agriculture, their lower population densities, high immigration, and overall high extraction based economies. These counties barely get any precipitation, vote overwhelmingly republican and have a low unemployment rate.",
    "These counties, almost all in the Great Plains region up towards the Canadian border, are extremely rural, with a population density average of just ~3.8. These counties are predominantly white, have lower than average poverty rates, and have low costs of living. They’re cold, don’t get a lot of rain, and are pretty flat.",
    "These counties, mostly in the upper midwest, stretch through most of the major cities in the region. Its biggest counties are direct suburbs of major cities in the area, while others are more rural. These counties are typically colder than usual, predominantly white with lower than average poverty rates. Like other more rural counties, they have a pretty low cost of living, and vote typically republican in uncompetitive elections.",
    "With little exceptions, these counties are predominantly counties with the Rocky Mountains in the backdrop. They’re more diverse than some of the other clusters, but still less diverse than the average, while they make more money than the average, and they have much more density than average. They have pretty competitive elections, typically favoring the democrats. These counties have high elevation, ruggedness, forest coverage, but not a lot of precipitation.",
    "These counties, almost all in Florida or across the east coast, have a high median age. These counties are pretty densely populated compared to the first cluster, and have a lower than average poverty rate. It rains a lot in these counties, and is pretty hot too. These counties are also usually covered in bodies of water.",
    "This group of metropolitan areas have found themselves in this cluster, predominantly being smaller metro areas or immediate suburbs in the southern half of the US. These counties are not as dense as the other metro clusters, but are pretty diverse and have a rising income. They’re hot, find themselves voting republican more often than not, and are flat.",
    "These counties, mostly in the south, obviously tend to be hotter and flatter, as well as more diverse, less dense, and poorer. An average county in this cluster has just under 24k people, and this population has been declining. These counties are typically much more rural, have very high income inequality and poverty. These counties also see a lot of rain compared to the average and have lots more forest. These counties are far and away the least internet-accessible counties.",
    "Another cluster of metropolitan areas finds us around the US in typically more industrial areas such as Summit County OH (with Akron), Wayne county, MI (with Detroit), and others. These cities across the midwest and other portions of the country are much less populated, poorer, and less diverse than the other metro counties that have been clustered. Their populations have stayed relatively stable thanks to a youthful population, and these counties have very competitive elections.",
    "These counties, spread near the Mexican border as well as near the Canadian border, are young counties whose diversity comes with a 10% immigrant population. These counties have the highest reliance on SNAP and poverty rate in the entire country, and the lowest income of any cluster. This comes with the highest unemployment rate as well. These counties get very little precipitation and their temperature varies depending on the geography of the county (which border it is close to).",
    "This cluster has a rapidly growing population, and contains cities like LA, Miami, and the immediate suburbs to many of the counties in Cluster Three. These counties sacrifice a bit of density, public transit usage, and immigrants in favor of a slightly less educated, slightly less impoverished population, and a lower cost of living. This cluster also has significantly more counties (193)."
  ),
  cluster_color = c(
    "#8E6BBE",  # Frigid Retirement Communities
    "#3A7D44",  # Rural Appalachia / Lower Midwest
    "#2F6CB3",  # Stable Urban Megacities
    "#B5651D",  # Extraction Counties
    "#6BA368",  # Northern Rural America
    "#F4C542",  # Midwest Suburbia
    "#8C5A2B",  # Western Highlands
    "#A07CC5",  # East Coast Aging Counties
    "#4F9DED",  # Sun Belt Metros
    "#E67E22",  # Underserved Southern Communities
    "#7F8C8D",  # Industrial Belt Cities
    "#C0392B",  # Border Counties
    "#5DADE2"   # Growing Urban Centers
  ))
saveRDS(pca_profiles, "data/final/pca_cluster_types.rds")

cluster_profiles_final <- cluster_profiles_final |>
  left_join(
    pca_profiles |>
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
