#############################################################################
#
# 05_pca.R
#
# Purpose:
#  Perform principal component analysis on the dataset
# and learn what types of things are important to separating counties, preparing for clustering in the next script
#
# Outputs:
# - visualizations in figures/pca
# - county_pca.RDS
#
#############################################################################

# Load required packages
library(tidyverse)
library(sf)
library(viridis)

#############################################################################
# Setup
#############################################################################

county <- readRDS("data/finished/county_dataset.rds")

# Fix missing forest coverage data to include it in PCA
county_for <- county |>
  group_by(STATEFP, RUCC_2023) |>
  mutate(
    forest_coverage_pct = if_else(
      is.na(forest_coverage_pct),
      median(forest_coverage_pct, na.rm = TRUE),
      forest_coverage_pct
    )
  ) |>
  ungroup()

#############################################################################
# Initialize PCA
#############################################################################

# Remove variables that are either identifiers or have too much missing data to use
pca_data <- county_for |>
  st_drop_geometry() |>
  select(
    -GEOID,
    -STATEFP,
    -COUNTYFP,
    -county_name,
    -RUCC_2023,
    -homicide_rate,
    -suicide_rate,
    -firearm_deaths_rate,
    -land_area_sq_miles,
    -total_population,
    -homeownership_rate
  )
# Log scale variables that are too varied
pca_data <- pca_data |>
  mutate(
    population_density = log1p(population_density),
    mean_elevation = log1p(mean_elevation)
  )

# Drop counties with missing data
pca_data_complete <- pca_data |>
  drop_na()

# Fix voter_turnout
pca_data_complete <- pca_data_complete |>
  mutate(
    voter_turnout = as.numeric(voter_turnout)
  )

# Save the missing counties for reference
pca_missing <- pca_data |>
  filter(if_any(everything(), is.na))

pca_model <- prcomp(
  pca_data_complete,
  center = TRUE,
  scale. = TRUE
)

#############################################################################
# PCA Variance
#############################################################################

pca_variance <- data.frame(
  PC = paste0("PC", 1:length(pca_model$sdev)),
  variance = pca_model$sdev^2 / sum(pca_model$sdev^2)
) |>
  mutate(
    cumulative_variance = cumsum(variance)
  )

pca_variance <- pca_variance |>
  mutate(
    PC = factor(
      PC,
      levels = PC[order(as.numeric(gsub("PC", "", PC)))]
    )
  )

saveRDS(
  pca_variance,
  "data/finished/pca_variance.rds"
)

# Scree Plot
scree <- ggplot(pca_variance,aes(x = PC, y = variance)) + 
  geom_col(
      fill = "#C49A00",
      width = 0.75) + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, .05))) +
  scale_x_discrete(
    breaks = paste0("PC",c(1, seq(5, length(levels(pca_variance$PC)), by = 5)))) +
  labs(
    title = "Scree Plot of Principal Components",
    subtitle = "Variance contribution of each principal component",
    x = NULL,
    y = "Variance Explained") +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(
      family = "sans"),
    plot.title = element_text(
      face = "bold",
      size = 18,
      margin = margin(b = 5)),
    plot.subtitle = element_text(
      size = 12,
      color = "black",
      margin = margin(b = 15)),
    axis.title.y = element_text(
      face = "bold",
      size = 13),
    axis.text = element_text(
      size = 11,
      color = "black"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1),
    axis.ticks.x = element_blank(),
    plot.margin = margin(
      10, 15, 10, 10))

ggsave(
  "figures/pca/scree_plot.png",
  scree,
  width = 8,
  height = 5,
  dpi = 300
)

# Cumulative Variance
cumvar <- ggplot(
  pca_variance,
  aes(
    x = PC,
    y = cumulative_variance)) +
  geom_line(
    color = "red",
    linewidth = 1.3,
    group = 1) +
  geom_hline(
    yintercept = c(0.80, 0.90, 0.95),
    linetype = "dashed",
    color = "grey50") +
  annotate(
    "text",
    x = 3,
    y = 0.80,
    label = "80%",
    vjust = -0.6,
    color = "black",
    size = 4) +
  annotate(
    "text",
    x = 3,
    y = 0.90,
    label = "90%",
    vjust = -0.3,
    color = "black",
    size = 4) +
  annotate(
    "text",
    x = 3,
    y = 0.95,
    label = "95%",
    vjust = -0.6,
    color = "black",
    size = 4) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, .03))) +
  scale_x_discrete(
    breaks = paste0(
      "PC",
      c(1, seq(5, length(levels(pca_variance$PC)), by = 5)))) +
  labs(
    title = "Cumulative Variance Explained by Principal Components",
    subtitle = "Total variance captured as additional components retained",
    x = NULL,
    y = "Cumulative Variance Explained") +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(
      family = "sans"),
    plot.title = element_text(
      face = "bold",
      size = 17,
      margin = margin(b = 5)),
    plot.subtitle = element_text(
      size = 12,
      color = "black",
      margin = margin(b = 15)),
    axis.title.y = element_text(
      face = "bold"),
    axis.text = element_text(
      color = "black"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1),
    axis.ticks.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(
      10, 15, 10, 10))
ggsave(
  "figures/pca/cumulative_variance.png",
  cumvar,
  width = 8,
  height = 5,
  dpi = 300
)

pca_variance |> 
  filter(
    cumulative_variance >= 0.8
  ) |> 
  slice(1)
pca_variance |> 
  filter(
    cumulative_variance >= 0.9
  ) |> 
  slice(1)

#############################################################################
# PCA Loadings
#############################################################################

pca_loadings <- as.data.frame(
  pca_model$rotation
) |>
  rownames_to_column("variable")

pca_loadings |>
  arrange(desc(abs(PC3))) |>
  select(variable, PC3) |>
  head(15)

saveRDS(
  pca_loadings,
  "data/finished/pca_loadings.rds"
)

pc1_loadings <- pca_loadings |>
  select(variable, PC1) |>
  rename(loading = PC1) |>
  arrange(desc(abs(loading))) |>
  slice_head(n = 15) |>
  arrange(loading)

# PC1 Loading Plot
pc1_loadings <- ggplot(
  pc1_loadings,
  aes(
    x = reorder(variable, loading),
    y = loading,
    fill = loading > 0)) +
  geom_col(
    color = "black",
    width = 0.7) +
  geom_hline(
    yintercept = 0,
    color = "grey40") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "TRUE" = "firebrick",
      "FALSE" = "steelblue")) +
  labs(
    title = "Top Variables Contributing to PC1 - Economic Development and Urbanization",
    subtitle = "Each Variable Contributes and shows the makeup of a county that would be high or low in this PC.",
    x = NULL,
    y = "Loading") +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      color = "black"
    ),
    axis.text = element_text(
      color = "black"
    )
  )
ggsave(
  "figures/pca/pc1_loadings.png",
  pc1_loadings,
  width = 11,
  height = 5,
  dpi = 300
)

# PC2 Loading Plot
pc2_loadings <- pca_loadings |>
  select(variable, PC2) |>
  rename(loading = PC2) |>
  arrange(desc(abs(loading))) |>
  slice_head(n = 15) |>
  arrange(loading)

pc2_loadings <- ggplot(
  pc2_loadings,
  aes(
    x = reorder(variable, loading),
    y = loading,
    fill = loading > 0)) +
  geom_col(
    color = "black",
    width = 0.7) +
  geom_hline(
    yintercept = 0,
    color = "grey40") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "TRUE" = "firebrick",
      "FALSE" = "steelblue")) +
  labs(
    title = "Top Variables Contributing to PC2 - Geographic Context and Rural Divide",
    subtitle = "Each Variable Contributes and shows the makeup of a county that would be high or low in this PC.",
    x = NULL,
    y = "Loading") +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      color = "black"
    ),
    axis.text = element_text(
      color = "black"
    )
  )
ggsave(
  "figures/pca/pc2_loadings.png",
  pc2_loadings,
  width = 11,
  height = 5,
  dpi = 300
)

# PC3 Loading Plot
pc3_loadings <- pca_loadings |>
  select(variable, PC3) |>
  rename(loading = PC3) |>
  arrange(desc(abs(loading))) |>
  slice_head(n = 15) |>
  arrange(loading)

pc3_loading <- ggplot(
  pc3_loadings,
  aes(
    x = reorder(variable, loading),
    y = loading,
    fill = loading > 0)) +
  geom_col(
    color = "black",
    width = 0.7) +
  geom_hline(
    yintercept = 0,
    color = "grey40") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "TRUE" = "firebrick",
      "FALSE" = "steelblue")) +
  labs(
    title = "Top Variables Contributing to PC3 - Demographic Structure and Lifestyle",
    subtitle = "Each Variable Contributes and shows the makeup of a county that would be high or low in this PC.",
    x = NULL,
    y = "Loading") +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      color = "black"
    ),
    axis.text = element_text(
      color = "black"
    )
  )
ggsave(
  "figures/pca/pc3_loadings.png",
  pc3_loading,
  width = 11,
  height = 5,
  dpi = 300
)

#############################################################################
# Adding PCA back to the Big Dataset
#############################################################################

pca_ids <- county_for |>
  st_drop_geometry() |>
  select(
    -STATEFP,
    -COUNTYFP,
    -county_name,
    -RUCC_2023,
    -homicide_rate,
    -suicide_rate,
    -firearm_deaths_rate,
    -land_area_sq_miles,
    -total_population,
    -homeownership_rate
  ) |>
  drop_na()

pca_scores <- as.data.frame(
  pca_model$x
)

pca_scores$GEOID <- pca_ids$GEOID

county_pca <- county_for |>
  left_join(
    pca_scores,
    by = "GEOID"
  )

saveRDS(county_pca, "data/finished/county_pca.rds")

#############################################################################
# PCA Maps
#############################################################################

## Each map has two versions, one in the same color scheme used for all other maps, making it consistent,
## and one that improves readability but is in a different color scheme. These maps are the exact same except for this.


## PC1
# RWB Version
county_pc1_rwb <- ggplot(county_pca) +
  geom_sf(
    aes(fill = PC1),
    color = NA) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    name = "Score") +
  labs(
    title = "Economic Development and Urbanization Scores (PC1) Across U.S. Counties",
    subtitle = "Derived from PCA, this map shows the score counties got for PC1 - Economic Development and Urbanization,",
    caption = "High scores indicate counties with higher income, education, housing, and population density") +
    theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = .5)
  )
ggsave(
  filename = "figures/pca/pc1_rwb_map.png",
  plot = county_pc1_rwb,
  width = 10,
  height = 6,
  dpi = 300
)

# Plasma version
pc1_plas_map <- ggplot(county_pca) +
  geom_sf(
    aes(fill = PC1),
    color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Score",
    na.value = "grey90") +
  labs(
    title = "Economic Development and Urbanization Scores (PC1) Across U.S. Counties",
    subtitle = "Derived from PCA, this map shows the score counties got for PC1 - Economic Development and Urbanization,",
    caption = "High scores indicate counties with higher income, education, housing, and population density.") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = .5)
  )
ggsave(
  filename = "figures/pca/pc1_plasma_map.png",
  plot = pc1_plas_map,
  width = 10,
  height = 6,
  dpi = 300
)

## PC2
# RWB Version
pc2_rwb_map <- ggplot(county_pca) +
  geom_sf(
    aes(fill = PC2),
    color = NA) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    name = "Score") +
  labs(
    title = "Geographic Context and Rural Divide (PC2) Across U.S. Counties",
    subtitle = "Derived from PCA, this map shows the score counties got for PC2 - Geographic Context and Rural Divide,",
    caption = "High scores indicate counties that are far from coastline and have higher elevation, less diversity, and more healthy economies.") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = .5)
  )
ggsave(
  filename = "figures/pca/pc2_rwb_map.png",
  plot = pc2_rwb_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Plasma Version
pc2_plas_map <- ggplot(county_pca) +
  geom_sf(
    aes(fill = PC2),
    color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Score",
    na.value = "grey90") +
  labs(
    title = "Geographic Context and Rural Divide (PC2) Across U.S. Counties",
    subtitle = "Derived from PCA, this map shows the score counties got for PC2 - Geographic Context and Rural Divide,",
    caption = "High scores indicate counties that are far from coastline and have higher elevation, less diversity, and more healthy economies.") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = .5)
  )
ggsave(
  filename = "figures/pca/pc2_plasma_map.png",
  plot = pc2_plas_map,
  width = 10,
  height = 6,
  dpi = 300
)

## PC3
# RWB Version
pc3_rwb_map <- ggplot(county_pca) +
  geom_sf(
    aes(fill = PC3),
    color = NA) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    name = "Score") +
  labs(
    title = "Demographic Structure and Lifestyle (PC3) Across U.S. Counties",
    subtitle = "Derived from PCA, this map shows the score counties got for PC3 - Demographic Structure and Lifestyle,",
    caption = "High scores indicate counties with younger populations with more immigrants and stronger labor participation.") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = .5)
  )
ggsave(
  filename = "figures/pca/pc3_rwb_map.png",
  plot = pc3_rwb_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Plasma Version
pc3_plas_map <- ggplot(county_pca) +
  geom_sf(
    aes(fill = PC3),
    color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Score",
    na.value = "grey90") +
  labs(
    title = "Demographic Structure and Lifestyle (PC3) Across U.S. Counties",
    subtitle = "Derived from PCA, this map shows the score counties got for PC3 - Demographic Structure and Lifestyle,",
    caption = "High scores indicate counties with younger populations with more immigrants and stronger labor participation.") +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = .5))
ggsave(
  filename = "figures/pca/pc3_plasma_map.png",
  plot = pc3_plas_map,
  width = 10,
  height = 6,
  dpi = 300
)

#############################################################################
# End of Script
#############################################################################