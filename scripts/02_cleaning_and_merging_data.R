library(tidyverse)


acs_raw <- read_csv(
  "data/raw/acs_initialpull.csv"
)


acs_clean <- acs_raw |> 
  select(GEOID, NAME, variable, estimate) |> 
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) |> 
  rename(county = NAME)
