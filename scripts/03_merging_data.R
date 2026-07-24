#############################################################################
#
# 03_merging_data.R
#
# Purpose:
# Merge all engineered feature datasets into a single dataset
# and perform final cleaning to it.
#
# Outputs:
# - county_dataset.rds
#
#############################################################################

# Required packages
library(tidyverse)
library(sf)

#############################################################################
# Load Data
#############################################################################

acs <- read_csv("data/raw/acs_cleaned_ready.csv")
county_geo <- readRDS("data/raw/county_geography.rds")
climate_features <- readRDS("data/raw/climate_features.rds")
elevation_features <- readRDS("data/raw/elevation_features.rds")
tri_features <- readRDS("data/raw/tri_features.rds")
forest_features <- readRDS("data/raw/forest_features.rds")
coast_features <- readRDS("data/raw/coast_features.rds")
politics_features <- readRDS("data/raw/politics_features.rds")
chrr <- readRDS("data/raw/chrrdata.rds")
rucc <- read_csv("data/raw/Ruralurbancontinuumcodes2023.csv")

#############################################################################
# Standardize GEOIDs 
#############################################################################

acs <- acs |> 
  mutate(GEOID = as.character(GEOID))
county_geo <- county_geo |> 
  mutate(GEOID = as.character(GEOID))
climate_features <- climate_features |> 
  mutate(GEOID = as.character(GEOID))
coast_features <- coast_features |> 
  mutate(GEOID = as.character(GEOID))
elevation_features <- elevation_features |> 
  mutate(GEOID = as.character(GEOID))
forest_features <- forest_features |> 
  mutate(GEOID = as.character(GEOID))
tri_features <- tri_features |> 
  mutate(GEOID = as.character(GEOID))
politics_features <- politics_features |> 
  mutate(GEOID = as.character(GEOID))
chrr <- chrr |> 
  mutate(GEOID = as.character(GEOID))
rucc <- rucc |> 
  mutate(GEOID = as.character(GEOID))

#############################################################################
# Merge Feature Tables 
#############################################################################

county_dataset <- county_geo |> 
  left_join(acs, by = "GEOID") |> 
  left_join(climate_features, by = "GEOID") |> 
  left_join(coast_features, by = "GEOID") |> 
  left_join(elevation_features, by = "GEOID") |> 
  left_join(forest_features, by = "GEOID") |> 
  left_join(tri_features, by = "GEOID") |> 
  left_join(politics_features, by = "GEOID") |> 
  left_join(chrr, by = "GEOID") |> 
  left_join(rucc, by = "GEOID")
county_dataset <- county_dataset |>
  mutate(
    population_density =
      total_population / land_area_sq_miles
  )

#############################################################################
# Clean the dataset
#############################################################################

# The dataset's analysis is restricted to the lower 48 states + DC
# This is because several geographic variables like distance to 
# coastline, climate, etc. are not directly comparable. 

clean_county_dataset <- county_dataset |>
  filter(
    !STATEFP %in% c(
      "02", # Alaska
      "15", # Hawaii
      "60", # American Samoa
      "66", # Guam
      "69", # Northern Mariana Islands
      "72", # Puerto Rico
      "78"  # US Virgin Islands
    )
  )

clean_county_dataset <- clean_county_dataset |> 
  rename(county_name = NAME.y, firearm_deaths_rate = firearm_deaths, comp_swing_2000_2020 = comp_swing,
         water_coverage_pct = water_coverage)

clean_county_dataset <- clean_county_dataset |> 
  select(
    GEOID, STATEFP, COUNTYFP, county_name, total_population, population_density, population_growth_5yr,
    pop_stability_index, median_age, under_18_pct, over_65_pct, average_household_size, married_pct,
    diversity_index, foreign_born_pct, veteran_pct, disability_rate, RUCC_2023, high_school_pct, some_college_pct, 
    college_grad_pct, masters_or_higher_pct, public_school_pct, snap_pct, gini_index, poverty_rate, 
    median_household_income, income_growth, median_earnings, median_gross_rent, median_home_value,
    median_year_built, housing_cost_burden_pct, homeownership_rate, vacancy_rate, crowding_rate, 
    unemployment_rate, labor_participation_rate, agriculture_pct, construction_pct, manufacturing_pct,
    arts_tourism_pct, finance_pct, information_pct, retail_pct, education_healthcare_pct, government_pct, 
    technical_pct, mean_commute_time, public_transit_pct, walk_bike_to_work_pct, drive_alone_pct, 
    work_from_home_pct, internet_access_pct, homicide_rate, suicide_rate, firearm_deaths_rate, 
    voter_turnout, dem_vote_share_2020, party_competitiveness_2020, dem_swing_2000_2020, comp_swing_2000_2020,
    land_area_sq_miles, mean_elevation, terrain_ruggedness, water_coverage_pct, distance_to_coast_miles, 
    forest_coverage_pct, mean_temp, annual_precip, geometry
    )

#############################################################################
# Save Final Dataset
#############################################################################

write_csv(
  st_drop_geometry(clean_county_dataset),
  "data/finished/county_dataset.csv"
)
saveRDS(
  clean_county_dataset,
  "data/finished/county_dataset.rds"
)

cat("Finished merging the data and created a final dataset. \n")

#############################################################################
# End of Script
#############################################################################