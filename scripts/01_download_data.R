###################################################################
#
# 01_download_data.R
#
#
#
#
#
#
###################################################################

library(tidycensus)
library(tidyverse)


census_api_key("0284737ce531e26d237dfd8af2a15149598f27ce", install = TRUE)

acs_variables <- c(
  population = "B01003_001",
  median_age = "B01002_001",
  median_income = "B19013_001",
  poverty = "B17001_002",
  unemployment = "B23025_005",
  bachelors_degree = "B15003_022",
  median_home_value = "B25077_001",
  median_rent = "B25064_001"
)


county_data <- get_acs(
  geography = "county",
  variables = acs_variables,
  year = 2023,
  survey = "acs5"
)

write_csv(county_data, "data/raw/acs_initialpull.csv")

county_data <- county_data |> 
  group_by(NAME,) |> 
  pivot_wider(names_from = NAME, values_from = estimate)
