#############################################################################
#
#  09_similarity.R
#
# Purpose: Determine how similar counties are to each other and produce a dataset
# that can be easily referenced and searched in the dashboard and interactive map.
#
# Outputs:
# - county_similarity_parquet
#
#############################################################################

# load required packages
library(tidyverse)
library(sf)
library(arrow)

# Load county dataset
county <- readRDS("data/finished/county_dataset.rds")


#############################################################################
# Build variable set for similarity engine
#############################################################################

# Select used variables
similarity_data <- county |> 
  st_drop_geometry() |>
  mutate(voter_turnout = as.numeric(voter_turnout)) |> 
  select(
    GEOID,
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
    college_grad_pct,
    public_school_pct,
    # Economic variables
    median_household_income,
    unemployment_rate,
    gini_index,
    poverty_rate,
    # Housing variables
    housing_cost_burden_pct,
    crowding_rate,
    median_gross_rent,
    # Employment variables
    labor_participation_rate,
    agriculture_pct,
    government_pct,
    retail_pct,
    education_healthcare_pct,
    # Transportation Variables
    mean_commute_time,
    drive_alone_pct,
    internet_access_pct,
    public_transit_pct,
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
    distance_to_coast_miles,
    mean_elevation
  )

# Imputing missing data, just like with hierarchical and gmm clustering
similarity_finished <- similarity_data |> 
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
    ),
    median_gross_rent = if_else(
      is.na(median_gross_rent),
      median(median_gross_rent, na.rm = TRUE),
      median_gross_rent
    )
  ) |>
  ungroup()

similarity_finished <- similarity_finished |>
  mutate(
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

similarity_finished <- similarity_finished |> 
  select(-STATEFP)

#############################################################################
# Weighted scaling
#############################################################################

# Certain weights were added after testing to ensure that similarity scores were accurate
weights <- c(
  population_density = 1,
  pop_stability_index = 1,
  median_age = 1,
  diversity_index = 1,
  foreign_born_pct = 1,
  average_household_size = 1,
  high_school_pct = 1,
  college_grad_pct = 1,
  public_school_pct = 1,
  median_household_income = 1,
  unemployment_rate = 1,
  gini_index = 1,
  poverty_rate = 1,
  housing_cost_burden_pct = 1,
  crowding_rate = 1,
  median_gross_rent = 1,
  labor_participation_rate = 1,
  agriculture_pct = 1,
  government_pct = 1,
  retail_pct = 1,
  education_healthcare_pct = 1,
  mean_commute_time = 1,
  drive_alone_pct = 1,
  internet_access_pct = 1,
  public_transit_pct = 1,
  voter_turnout = 1,
  dem_vote_share_2020 = 1,
  party_competitiveness_2020 = 1,
  terrain_ruggedness = .7,
  forest_coverage_pct = .5,
  water_coverage_pct = .5,
  annual_precip = .5,
  mean_temp = .5,
  distance_to_coast_miles = .8,
  mean_elevation = .7
)

scaled_data <- similarity_finished |> 
  column_to_rownames("GEOID") |>
  scale()

similarity_center <- attr(scaled_data, "scaled:center")
similarity_scale <- attr(scaled_data, "scaled:scale")

saveRDS(list(center = similarity_center, scale = similarity_scale, weights = weights), 
        "data/finished/similarity_parameters.rds")
scaled_data_weighted <- sweep(
  scaled_data,
  2,
  weights,
  "*"
)

#############################################################################
# Calculate distances
#############################################################################

distance_matrix <- dist(scaled_data_weighted,method = "euclidean")

distance_df <- as.data.frame(as.matrix(distance_matrix))

#############################################################################
# Create Similarity Dataset
#############################################################################

similarity_results <- distance_df |> 
  mutate(GEOID = similarity_data$GEOID) |> 
  pivot_longer(
    cols = -GEOID,
    names_to="comparison_GEOID",
    values_to="distance"
  ) |> 
  rename(source_GEOID = GEOID)

similarity_results <- similarity_results |> 
  filter(source_GEOID != comparison_GEOID)

similarity_results <- similarity_results |> 
  group_by(source_GEOID) |> 
  arrange(distance) |> 
  mutate(
    similarity_rank = row_number()
  ) |> 
  ungroup()

write_parquet(
  similarity_results,
  "data/finished/county_similarity.parquet"
)

#############################################################################
# End of Script
#############################################################################
