library(tidyverse)
library(sf)
library(lubridate)


acs <- read_csv("data/raw/acs_initialpull.csv")

acs_wide <- acs |>
  select(GEOID, NAME, variable, estimate) |>
  pivot_wider(
    names_from = variable,
    values_from = estimate
  )
acs_features <- acs_wide |> 
  mutate(
    under_18_pct = 
      (male_under5 + male_5_9 + male_10_14 + male_15_17 + 
         female_under5 + female_5_9 + female_10_14 + female_15_17)
    / total_population,
    over_65_pct = 
      (male_65_66 + male_67_69 + male_70_74 + male_75_79 + male_80_84 + male_85_plus +
      female_65_66 + female_67_69 + female_70_74 + female_75_79 + female_80_84 + female_85_plus)
    / total_population,
    foreign_born_pct = foreign_born / total_population,
    veteran_pct = veterans / adult_population, # Since children cannot be in the military
    high_school_pct = (high_school + ged) / adult_population,
    some_college_pct = (some_college_lt1 + some_college_gt1 + associates) / adult_population,
    college_grad_pct = bachelors / adult_population,
    masters_or_higher_pct = (masters + doctorate + professional) / adult_population,
    public_school_pct = (female_public_school + male_public_school) / total_enrolled,
    unemployment_rate = unemployed / labor_force,
    poverty_rate = below_poverty / poverty_total,
    labor_participation_rate = labor_force / population_16_plus,
    snap_pct = snap_households / households_total,
    homeownership_rate = owner_occupied / occupied_units,
    vacancy_rate = vacant_units / housing_units_total_2,
    crowding_rate = (crowded_1_01_1_50 + crowded_1_50_plus) / occupied_units,
    housing_cost_burden_pct = (rent_30_34 + rent_35_39 + rent_40_49 + rent_50_plus) 
    / renter_households,
    agriculture_pct = agriculture / employed_population,
    construction_pct = construction / employed_population,
    manufacturing_pct = manufacturing / employed_population,
    arts_tourism_pct = arts_recreation / employed_population,
    finance_pct = finance / employed_population,
    information_pct = information / employed_population,
    retail_pct = retail / employed_population,
    education_healthcare_pct = education_health / employed_population,
    government_pct = government / employed_population, 
    technical_pct = technical / employed_population,
    public_transit_pct = public_transit / workers_commuting,
    work_from_home_pct = work_from_home / labor_force,
    walk_bike_to_work_pct = (walk + bicycle) / workers_commuting,
    drive_alone_pct = drive_alone / workers_commuting,
    internet_access_pct = internet_subscription / int_households_total,
    married_pct = (married_male + married_female) / marital_population,
    mean_commute_time = mean_commute_time / 60, # Convert to minutes
    disability_rate = disability_with / disability_total,
    # Creating a diversity index modeled off of the Simpson Diversity Index for biodiversity in a region
    white_pct = white / total_population_race,
    black_pct = black / total_population_race,
    american_indian_pct = american_indian / total_population_race,
    asian_pct = asian / total_population_race,
    pacific_islander_pct = pacific_islander / total_population_race,
    other_pct = other_race / total_population_race,
    two_or_more_pct = two_or_more / total_population_race,
    diversity_index = 1 - (
      (white_pct)^2 +
      (black_pct)^2 +
      (american_indian_pct)^2 +
      (asian_pct)^2 +
      (pacific_islander_pct)^2 +
      (other_pct)^2 +
      (two_or_more_pct)^2 )
  ) |> 
  select(
    GEOID, NAME, total_population, median_age, under_18_pct, over_65_pct, average_household_size, 
    foreign_born_pct, veteran_pct, high_school_pct, some_college_pct, college_grad_pct, public_school_pct,
    masters_or_higher_pct, public_school_pct, median_household_income, unemployment_rate, poverty_rate, 
    labor_participation_rate, gini_index, median_earnings, snap_pct, mean_commute_time, median_home_value,
    median_gross_rent, homeownership_rate, vacancy_rate, median_year_built, crowding_rate,
    housing_cost_burden_pct, agriculture_pct, construction_pct, manufacturing_pct, arts_tourism_pct, 
    finance_pct, information_pct, education_healthcare_pct, government_pct, technical_pct, retail_pct,
    public_transit_pct, work_from_home_pct, internet_access_pct, disability_rate, diversity_index, married_pct,
    walk_bike_to_work_pct, drive_alone_pct
  )


acs_2018_features <- get_acs(
  geography = "county",
  variables = c(
    population_2018 = "B01003_001",
    median_hh_income_2018 = "B19013_001"
  ),
  year = 2018,
  survey = "acs5",
  output = "wide"
)

acs_2018_features <- acs_2018_features |>
  select(
    GEOID,
    population_2018 = population_2018E,
    median_hh_income_2018 = median_hh_income_2018E
  )
acs_features <- acs_features |>
  left_join(
    acs_2018_features,
    by = "GEOID"
  )
acs_features <- acs_features |>
  mutate(
    population_growth_5yr =
      (total_population - population_2018) /
      population_2018,
    population_weight =
      total_population / (total_population + 1000),
    pop_stability_index =
      case_when(
        population_growth_5yr < 0 ~
          exp(-2 * abs(population_growth_5yr) * population_weight),
        
        TRUE ~
          exp(-abs(population_growth_5yr) * population_weight)
      )
  )
inflation_factor <- 1.20
acs_features <- acs_features |>
  mutate(
    income_2018_real =
      median_hh_income_2018 * inflation_factor,
    
    income_growth =
      (median_household_income - income_2018_real) /
      income_2018_real
  )

acs_clean_features <- acs_features |> 
  select(
  GEOID, NAME, total_population, median_age, under_18_pct, over_65_pct, average_household_size, 
  foreign_born_pct, veteran_pct, high_school_pct, some_college_pct, college_grad_pct, public_school_pct,
  masters_or_higher_pct, public_school_pct, median_household_income, unemployment_rate, poverty_rate, 
  labor_participation_rate, gini_index, median_earnings, snap_pct, mean_commute_time, median_home_value,
  median_gross_rent, homeownership_rate, vacancy_rate, median_year_built, crowding_rate,
  housing_cost_burden_pct, agriculture_pct, construction_pct, manufacturing_pct, arts_tourism_pct, 
  finance_pct, information_pct, retail_pct, education_healthcare_pct, government_pct, technical_pct,
  public_transit_pct, work_from_home_pct, internet_access_pct, disability_rate, diversity_index, income_growth,
  population_growth_5yr, pop_stability_index, married_pct, walk_bike_to_work_pct, drive_alone_pct
)


write_csv(acs_clean_features, "data/raw/acs_cleaned_ready.csv")
