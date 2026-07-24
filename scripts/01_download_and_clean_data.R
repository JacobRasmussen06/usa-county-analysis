#############################################################################
#
# 01_download_and_clean_data.R
#
# Purpose: 
#     Download all raw datasets used to construct the final dataset
#   and perform cleaning so they can be safely and effectively used
#   in feature engineering.
#
# Outputs:
# - ACS data
# - County geography
# - RUCC classifications
# - Climate features
# - Elevation
# - Terrain Ruggedness
# - Coast distance
# - Forest coverage
# - Politics
# - CHR&R health variables
#
#
#############################################################################

# All libraries required for this script: (install.packages("(library)") if you do not have them)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(prism)
library(terra)
library(exactextractr)
library(elevatr)
library(rnaturalearth)
library(units)
library(FedData)


#############################################################################
# ACS Data
#############################################################################

# Run once to install your Census API key:
# census_api_key("YOUR_KEY", install = TRUE)
# Notes for how to obtain a Census API key are in the README

demographic_variables <- c(
  total_population = "B01003_001", # total / land area from later for pop density
  median_age = "B01002_001",
  average_household_size = "B25010_001",
  foreign_born = "B05002_013", # foreign / total population for foreign born%
  male_under5 = "B01001_003",
  male_5_9    = "B01001_004",
  male_10_14  = "B01001_005",
  male_15_17  = "B01001_006",
  female_under5 = "B01001_027",
  female_5_9  = "B01001_028",
  female_10_14 = "B01001_029",
  female_15_17 = "B01001_030", # needed to calculate under 18%
  male_65_66 = "B01001_020",
  male_67_69 = "B01001_021",
  male_70_74 = "B01001_022",
  male_75_79 = "B01001_023",
  male_80_84 = "B01001_024",
  male_85_plus = "B01001_025",
  female_65_66 = "B01001_044",
  female_67_69 = "B01001_045",
  female_70_74 = "B01001_046",
  female_75_79 = "B01001_047",
  female_80_84 = "B01001_048",
  female_85_plus = "B01001_049", # needed to calculate over 65%,
  adult_population = "B21001_001",
  veterans = "B21001_002", # veterans / adult population since kids cannot be in the military
  total_population_race = "B02001_001",
  white = "B02001_002",
  black = "B02001_003",
  american_indian = "B02001_004",
  asian = "B02001_005",
  pacific_islander = "B02001_006",
  other_race = "B02001_007",
  two_or_more = "B02001_008", # will use to calculate diversity index
  marital_population = "B12001_001",
  married_male = "B12001_006",
  married_female = "B12001_015", # for % married
  disability_total = "B18102_001",
  disability_with = "B18102_004"
)

education_variables <- c(
  education_total = "B15003_001",
  high_school = "B15003_017",
  ged = "B15003_018", # ged will be combined with high school to produce the high school %
  some_college_lt1 = "B15003_019",
  some_college_gt1 = "B15003_020",
  associates = "B15003_021", # combined with some college
  bachelors = "B15003_022",
  masters = "B15003_023",
  professional = "B15003_024",
  doctorate = "B15003_025", # engineer all variables as pct
  total_enrolled = "B14001_002",
  male_public_school = "B14003_003",
  female_public_school = "B14003_031", # used for public school %
  population_25_plus = "B15003_001"
)

economy_variables <- c(
  median_household_income = "B19013_001",
  median_earnings = "B20002_001",
  poverty_total = "B17001_001",
  below_poverty = "B17001_002", # used to calculate poverty rate
  labor_force = "B23025_003",
  unemployed = "B23025_005", # unemployment rate
  population_16_plus = "B23025_001", # used for labor force participation
  gini_index = "B19083_001", 
  households_total = "B22003_001",
  snap_households = "B22003_002" # used for % of hh using snap
)

housing_variables <- c(
  median_home_value = "B25077_001",
  median_gross_rent = "B25064_001",
  housing_units_total = "B25003_001",
  owner_occupied = "B25003_002", # used to calculate homeownership rate
  housing_units_total_2 = "B25002_001",
  vacant_units = "B25002_003", # used for vacancy rate
  median_year_built = "B25035_001", 
  renter_households = "B25070_001",
  rent_30_34 = "B25070_007",
  rent_35_39 = "B25070_008",
  rent_40_49 = "B25070_009",
  rent_50_plus = "B25070_010", # housing cost burden
  occupied_units = "B25014_002",
  crowded_1_01_1_50 = "B25014_005",
  crowded_1_50_plus = "B25014_006", # crowding rate variable
  int_households_total = "B28002_001",
  internet_subscription = "B28002_002" # for broadband internet access % 
)

employment_variables <- c(
  employed_population = "C24030_001",
  agriculture = "C24030_003",
  construction = "C24030_007",
  manufacturing = "C24030_011",
  wholesale_trade = "C24030_015",
  retail = "C24030_017",
  transportation = "C24030_021",
  information = "C24030_023",
  finance = "C24030_025",
  technical = "C24030_027",
  education_health = "C24030_029",
  arts_recreation = "C24030_033",
  other_services = "C24030_035",
  government = "C24030_037" # each sector over total employed population for the pct
)

transportation_variables <- c(
  workers_commuting = "B08301_001",
  drive_alone = "B08301_003",
  public_transit = "B08301_010",
  bicycle = "B08301_018", # combined with walking
  walk = "B08301_019",
  work_from_home = "B08301_021" # each variable over the workers_commuting
)
all_acs_variables <- c(
  demographic_variables,
  education_variables,
  economy_variables, 
  housing_variables,
  employment_variables,
  transportation_variables
)
subject_vars <- c(
  mean_commute_time = "S0801_C01_046"
)
acs_raw <- get_acs(
  geography = "county",
  variables = all_acs_variables,
  year = 2023,
  survey = "acs5"
)
acs_subject <- get_acs(
  geography = "county",
  variables = subject_vars,
  year = 2023,
  survey = "acs5"
)
# Subject table variable (downloaded separately because they
# cannot be requested with detailed table variables)

acs_subject <- acs_subject |>
  select(
    GEOID,
    mean_commute_time = estimate
  )

acs_raw <- acs_raw |>
  left_join(
    acs_subject,
    by = "GEOID"
  )
write_csv(acs_raw, "data/raw/acs_initialpull.csv")

###############################################################
# TIGER County Geography
###############################################################

counties_tiger <- counties(
  cb = TRUE,
  year = 2023
)
county_geo <- counties_tiger |> 
  select(
    GEOID,
    NAME,
    STATEFP,
    COUNTYFP,
    ALAND,
    AWATER,
    geometry
  )
county_geo <- county_geo |> 
  mutate(
    land_area_sq_miles = ALAND / 2589988.10, # convert to sq. miles,
    water_coverage = AWATER / (ALAND + AWATER)
  )
write_csv(
  st_drop_geometry(county_geo),
  "data/raw/county_geography.csv"
)
saveRDS(
  county_geo,
  "data/raw/county_geography.rds"
)

###############################################################
# Rural Urban Continuum Codes
###############################################################

rucc <- read_csv(
  "data/raw/Ruralurbancontinuumcodes2023.csv"
)
rucc <- rucc |> 
  rename(
    "GEOID" = FIPS
  )
rucc_wide <- rucc |>
  select(GEOID, County_Name, Attribute, Value) |>
  pivot_wider(
    names_from = Attribute,
    values_from = Value
  )
write_csv(
  rucc_wide,
  "data/raw/Ruralurbancontinuumcodes2023.csv"
)

###############################################################
# PRISM Climate Data
###############################################################

prism_set_dl_dir("data/prism")

# Downloads PRISM 30-year climate normals.
# Files are cached in data/prism.

get_prism_normals(
  type = "tmean",
  resolution = "4km",
  mon = 1:12
) 

get_prism_normals(
  type = "ppt",
  resolution = "4km",
  mon = 1:12
)


zip_files <- list.files(
  "data/prism",
  pattern = "\\.zip$",
  recursive = TRUE,
  full.names = TRUE
)



for (z in zip_files) {
  unzip(
    z,
    exdir = "data/prism/extracted"
  )
}

temp_files <- list.files(
  "data/prism/extracted",
  pattern = "tmean.*\\.tif$",
  full.names = TRUE
)
temp_stack <- rast(temp_files)

annual_temp <- mean(temp_stack)

ppt_files <- list.files(
  "data/prism/extracted",
  pattern = "ppt.*\\.tif$",
  full.names = TRUE
)
ppt_stack <- rast(ppt_files)

annual_precip <- sum(ppt_stack)

county_temp <- exact_extract(
  annual_temp,
  county_geo,
  "mean"
)

county_precip <- exact_extract(
  annual_precip,
  county_geo,
  "mean"
)

climate_features <- county_geo |>
  st_drop_geometry() |>
  select(GEOID, NAME) |>
  mutate(
    mean_temp = county_temp,
    annual_precip = county_precip
  )

saveRDS(
  climate_features,
  "data/raw/climate_features.rds"
)

###############################################################
# Obtaining elevation and terrain ruggedness
###############################################################

counties_ll <- st_transform(county_geo, 4326)

elev <- get_elev_raster(
  locations = counties_ll,
  z = 6,
  clip = "locations"
)

elev <- rast(elev)
counties_proj <- st_transform(
  county_geo,
  crs(elev)
)

county_elevation <- exact_extract(
  elev,
  counties_proj,
  "mean"
)
elevation_features <- county_geo |>
  st_drop_geometry() |>
  select(GEOID, NAME) |>
  mutate(
    mean_elevation = county_elevation
  )

saveRDS(
  elevation_features,
  "data/raw/elevation_features.rds"
)

tri_raster <- focal(
  elev,
  w = matrix(1, 5, 5),
  fun = sd,
  na.rm = TRUE
)

counties_proj3 <- st_transform(
  county_geo,
  crs(tri_raster)
)

county_tri <- exact_extract(
  tri_raster,
  counties_proj,
  "mean"
)

tri_features <- county_geo |>
  st_drop_geometry() |>
  select(GEOID, NAME) |>
  mutate(
    terrain_ruggedness = county_tri
  )

saveRDS(
  tri_features,
  "data/raw/tri_features.rds"
)

###############################################################
# Obtaining distance from coastline
###############################################################

coast <- ne_download(
  scale = "medium",
  type = "coastline",
  category = "physical",
  returnclass = "sf"
)

counties_proj <- st_transform(county_geo, 5070)
coast_proj <- st_transform(coast, 5070)

centroids <- st_centroid(counties_proj)
dist_matrix <- st_distance(centroids, coast_proj)
min_dist_m <- apply(dist_matrix, 1, min)

coast_features <- county_geo |>
  st_drop_geometry() |>
  select(GEOID, NAME) |>
  mutate(
    distance_to_coast_miles = as.numeric(set_units(min_dist_m, "miles"))
  )
saveRDS(coast_features, "data/raw/coast_features.rds")

###############################################################
# Forest Coverage
###############################################################

fia <- st_read(
  "data/raw/S_USA.Lndcv_FIA_CntyEst_2015_PL/S_USA.Lndcv_FIA_CntyEst_2015_PL.shp"
)
fia <- fia |> 
  rename("GEOID" = STATE_CNTY, "NAME" = CNTY_NAME)

forest_features <- fia |>
  st_drop_geometry() |>
  mutate(
    forest_coverage_pct =
      (FORESLAND_ / SAMPLEDLAN) * 100
  ) |>
  select(
    GEOID,
    NAME,
    forest_coverage_pct
  )
saveRDS(
  forest_features,
  "data/raw/forest_features.rds"
)

###############################################################
# Political Features
###############################################################

elections <- read_csv("data/raw/countypres_2000-2024.csv")
elections <- elections |>
  mutate(
    GEOID = sprintf(
      "%05d",
      county_fips
    )
  )
elections_clean <- elections |>
  semi_join(
    county_geo,
    by = "GEOID"
  )
elections_politics <- elections_clean |>
  filter(
    year %in% c(2000, 2020),
    party %in% c("DEMOCRAT", "REPUBLICAN")
  )
election_votes <- elections_politics |>
  group_by(
    GEOID,
    year,
    party,
    totalvotes
  ) |>
  summarise(
    votes = sum(candidatevotes, na.rm = TRUE),
    .groups = "drop"
  )
election_wide <- election_votes |>
  pivot_wider(
    names_from = party,
    values_from = votes,
    values_fill = 0
  )
politics_metrics <- election_wide |>
  mutate(
    dem_vote_share =
      DEMOCRAT / totalvotes,
    party_competitiveness = 1 -
      abs(
        (DEMOCRAT / (DEMOCRAT + REPUBLICAN)) -
          (REPUBLICAN / (DEMOCRAT + REPUBLICAN))
      )
  )
politics_features <- politics_metrics |>
  select(
    GEOID,
    year,
    dem_vote_share,
    party_competitiveness
  ) |>
  pivot_wider(
    names_from = year,
    values_from = c(
      dem_vote_share,
      party_competitiveness
    )
  )
politics_features <- politics_features |>
  mutate(
    dem_swing_2000_2020 =
      dem_vote_share_2020 -
      dem_vote_share_2000,
    comp_swing = 
      party_competitiveness_2020 - 
      party_competitiveness_2000
  )

politics_features <- politics_features |>
  select(
    GEOID,
    dem_vote_share_2020,
    party_competitiveness_2020,
    dem_swing_2000_2020,
    comp_swing
  )
saveRDS(
  politics_features,
  "data/raw/politics_features.rds"
)


###############################################################
# County Health Rankings & Roadmaps Data
###############################################################

chrr <- read_csv(
  "data/raw/analytic_data2025_v3.csv", col_names = TRUE
)
chrr <- chrr |> 
  rename("GEOID" = `5-digit FIPS Code`)
chrr <- chrr |> 
  rename(voter_turnout = `Voter Turnout raw value`, suicide_rate = `Suicides raw value`,
         homicide_rate = `Homicides raw value`, 
         firearm_deaths = `Firearm Fatalities raw value`) |> 
  select(GEOID, Name, voter_turnout, homicide_rate, firearm_deaths, suicide_rate)
chrr_clean <- chrr |>
  semi_join(
    county_geo,
    by = "GEOID"
  )
saveRDS(
  chrr_clean,
  "data/raw/chrrdata.rds"
)

cat("Finished downloading and cleaning raw datasets.\n")

###############################################################
# End of Script
###############################################################