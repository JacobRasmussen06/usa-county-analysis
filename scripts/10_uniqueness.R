#############################################################################
#
#  10_uniqueness.R
#
# Purpose: Build a baseline that can be used to find unique counties, both unique
# from their neighboring counties as well as unique to the whole country, unique to their cluster,
# and a combined uniqueness score obtained from the other three.
#
# Outputs:
# - national_isolation.csv
# - county_regional_neighbors.rds
# - county_regional_uniqueness.rds
# - county_cluster_uniqueness.rds
# - county_uniqueness.rds
#
#
#############################################################################

# load required packages
library(tidyverse)
library(sf)
library(arrow)

#############################################################################
# Uniqueness Based on National Isolation 
#############################################################################
similarity <- read_parquet("data/finished/county_similarity.parquet")
county <- readRDS("data/finished/county_dataset.rds")
k <- 50 # will be the k closest counties based on similarity

national_isolation_scores <- similarity |> 
  group_by(source_GEOID) |> 
  summarise(
    isolation_score = mean(distance[similarity_rank <= k]), # finding the isolation from the k closest counties
    .groups = 'drop'
  ) |> 
  arrange(desc(isolation_score)) |> 
  mutate(
    isolation_rank = row_number()
  )

county_names <- county |>
  st_drop_geometry() |>
  select(GEOID, county_name)

national_isolation_scores <- national_isolation_scores |>
  left_join(county_names, by = c("source_GEOID" = "GEOID"))

write_csv(national_isolation_scores,"data/finished/national_isolation.csv")

#############################################################################
# Uniqueness to Neighboring Counties
#############################################################################

# Finding the first neighbor (bordering counties)
neighbor1 <- st_touches(county)

# Function that will find all the neighbors of the neighboring counties
get_second_order <- function(i, nb) {
  first <- nb[[i]]
  if (length(first) == 0) {
    return(integer(0))
  }
  second <- unique(unlist(nb[first]))
  second <- setdiff(second, c(i, first)) 
  second
}

neighbor2 <- map(seq_along(neighbor1), get_second_order, nb = neighbor1)
neighbors_2 <- tibble(GEOID = county$GEOID, second_neighbors = neighbor2)

regional_neighbors <- tibble(
  GEOID = county$GEOID, 
  regional_neighbors = map2(neighbor1, neighbor2, ~ sort(unique(c(.x, .y))) ) )

# A few counties are only islands, so manual neighbors are added so that they are included
manual_neighbors <- tribble(
  ~GEOID,   ~neighbor,
  "25007", "25001",
  "25007", "25019",
  "25019", "25001",
  "25019", "25007",
  "36085", "36047",
  "36085", "36081",
  "36085", "34017",
  "36085", "34023",
  "53055", "53057",
  "53055", "53061"
)

manual_neighbors_list <- manual_neighbors |>
  group_by(GEOID) |>
  summarise(
    regional_neighbors = list(match(neighbor, county$GEOID)),
    .groups = "drop"
  )

regional_neighbors <- regional_neighbors |>
  left_join(
    manual_neighbors_list,
    by = "GEOID",
    suffix = c("", "_manual")) |>
  mutate(
    regional_neighbors = map2(
      regional_neighbors,
      regional_neighbors_manual,
      ~ if(length(.x) == 0) .y else .x)) |>
  select(GEOID, regional_neighbors)

regional_neighbors <- regional_neighbors |> 
  mutate(n_regional = map_int(regional_neighbors, length)) 

# Saving the neighbor counties
saveRDS(regional_neighbors, "data/finished/county_regional_neighbors.rds" )

regional_long <- regional_neighbors |>
  unnest_longer(regional_neighbors) |>
  rename(
    source_GEOID = GEOID,
    comparison_GEOID = regional_neighbors
  ) |>
  mutate(
    comparison_GEOID = county$GEOID[comparison_GEOID]
  )

regional_scores <- regional_long |>
  left_join(
    similarity,
    by = c("source_GEOID","comparison_GEOID")
  )

# Calculating the Uniqueness
regional_uniqueness <- regional_scores |>
  group_by(source_GEOID) |>
  summarise(
    regional_uniqueness = mean(distance),
    regional_sd = sd(distance),
    regional_max = max(distance),
    n_neighbors = n(),
    .groups = "drop"
  )
regional_uniqueness <- regional_uniqueness |>
  arrange(desc(regional_uniqueness)) |>
  mutate(
    regional_rank = row_number()
  )
# Save dataset
saveRDS(regional_uniqueness, "data/finished/county_regional_uniqueness.rds")

#############################################################################
# Uniqueness Based on Distance From Cluster Center
#############################################################################

# Load in GMM Clusters
gmm_clusters <- readRDS("data/finished/county_gmm_clusters.rds")
gmm_cluster_assignments <- gmm_clusters |> 
  select(
    GEOID,
    county_name,
    gmm_cluster,
    gmm_uncertainty,
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

# Scale the variables
scaled_cluster <- gmm_cluster_assignments |> 
  mutate(voter_turnout = as.numeric(voter_turnout)) |> 
  st_drop_geometry() |> 
  select(-GEOID, -county_name, -gmm_cluster, -gmm_uncertainty) |>
  scale()

# Re-add relevant columns
scaled_cluster <- bind_cols(
  gmm_cluster_assignments |> select(GEOID, county_name, gmm_cluster, gmm_uncertainty),
  scaled_cluster
)

cluster_vars <- c(
  "population_density",
  "pop_stability_index",
  "median_age",
  "diversity_index",
  "foreign_born_pct",
  "average_household_size",
  "high_school_pct",
  "public_school_pct",
  "median_household_income",
  "unemployment_rate",
  "gini_index",
  "labor_participation_rate",
  "housing_cost_burden_pct",
  "agriculture_pct",
  "government_pct",
  "retail_pct",
  "education_healthcare_pct",
  "mean_commute_time",
  "drive_alone_pct",
  "internet_access_pct",
  "voter_turnout",
  "dem_vote_share_2020",
  "party_competitiveness_2020",
  "terrain_ruggedness",
  "forest_coverage_pct",
  "water_coverage_pct",
  "annual_precip",
  "mean_temp",
  "distance_to_coast_miles"
)

# Find the centers of each clusters in a similar way to when clustering was performed
cluster_centers <- scaled_cluster |>
  group_by(gmm_cluster) |>
  summarise(
    across(
      all_of(cluster_vars),
      mean
    ),
    .groups = "drop"
  )

cluster_unique <- scaled_cluster |>
  st_drop_geometry() |> 
  left_join(
    cluster_centers,
    by = "gmm_cluster",
    suffix = c("", "_center")
  )

# Find uniqueness score within cluster
cluster_unique <- cluster_unique |>
  rowwise() |>
  mutate(
    cluster_uniqueness = sqrt(
      sum(
        (c_across(all_of(cluster_vars)) -
           c_across(all_of(paste0(cluster_vars, "_center"))))^2,
        na.rm = TRUE
      )
    )
  ) |>
  ungroup()

cluster_uniqueness <- cluster_unique |>
  select(
    GEOID,
    gmm_cluster,
    gmm_uncertainty,
    cluster_uniqueness
  ) |>
  arrange(desc(cluster_uniqueness)) |>
  mutate(
    cluster_unique_rank = row_number()
  )

# Save dataset
saveRDS(cluster_uniqueness,"data/finished/county_cluster_uniqueness.rds")

#############################################################################
# Combined Uniqueness Score
#############################################################################

# Select relevant columns from each existing uniqueness dataset
national_isolation_scores <- national_isolation_scores |> 
  mutate(GEOID = source_GEOID) |> 
  select(GEOID, isolation_score, isolation_rank)

regional_uniqueness <- regional_uniqueness |> 
  mutate(GEOID = source_GEOID) |> 
  select(GEOID, regional_uniqueness, regional_sd, regional_max, n_neighbors, regional_rank)

cluster_uniqueness <- cluster_uniqueness |> 
  select(GEOID, gmm_uncertainty, cluster_uniqueness, cluster_unique_rank)

# Combine all into one dataset
uniqueness_combined <- national_isolation_scores |>
  left_join(
    regional_uniqueness,
    by = "GEOID") |>
  left_join(
    cluster_uniqueness,
    by = "GEOID")

# Use percentiles to craft a uniqueness combined score
uniqueness_combined <- uniqueness_combined |>
  mutate(
    isolation_pct = percent_rank(isolation_score),
    regional_pct = percent_rank(regional_uniqueness),
    cluster_pct = percent_rank(cluster_uniqueness)
  )

uniqueness_combined <- uniqueness_combined |>
  mutate(
    combined_uniqueness =
      .4 * isolation_pct + # Weighted after testing to put more weight onto national and regional isolation
      .35 * regional_pct + 
      .25 * cluster_pct)

# Scale to be easier to interpret
uniqueness_combined <- uniqueness_combined |>
  mutate(
    uniqueness_score = combined_uniqueness * 100
  )

uniqueness_combined <- uniqueness_combined |>
  arrange(desc(combined_uniqueness)) |>
  mutate(
    uniqueness_combined_rank = row_number()
  )

# Select relevant columns
uniqueness_combined <- uniqueness_combined |> 
  select(GEOID, uniqueness_score, uniqueness_combined_rank, 
         isolation_score, isolation_rank, regional_uniqueness, n_neighbors, regional_rank,
         cluster_uniqueness, gmm_uncertainty, cluster_unique_rank)

saveRDS(uniqueness_combined,"data/finished/county_uniqueness.rds")

#############################################################################
# End of Script
#############################################################################
