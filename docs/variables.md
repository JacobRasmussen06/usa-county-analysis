| Variable | Why? | Source | Variable Name in Dataset |
|-----------------|-----------------|--------------------|-----------------|
| **Population** | Demographics | ACS | *total_population* |
| **Population density** | Tells more than raw population due to land area differences between counties | Population from ACS + land area from Census TIGER/Line | *population_density* |
| **Median Age** | Shows age | ACS | *median_age* |
| **Under 18%** | Shows age | ACS | *under_18_pct* |
| **Over 65%** | Shows age | ACS | *over_65_pct* |
| **Avg. Household Size** | Family structure | ACS | *average_household_size* |
| **Foreign-born %** | Shows immigration patterns | ACS | *foreign_born_pct* |
| **Veteran %** | More community makeup | ACS | *veteran_pct* |
| **Diversity index** | Diversity in population | Engineered | *diversity_index* |
| **High school graduate %** | Shows education | ACS | *high_school_pct* |
| **Bachelor's graduate %** | Shows higher education | ACS | *college_grad_pct* |
| **Masters+ graduate %** | Shows high education | ACS | *masters_or_higher_pct* |
| **Some college %** | Education metric | ACS | *some_college_pct* |
| **Public School %** | Education metric | ACS | *public_school_pct* |
| **Median household income** | Wealth metric | ACS | *median_household_income* |
| **Unemployment rate** | Employment | ACS | *unemployment_rate* |
| **Poverty rate** | Economy | ACS | *poverty_rate* |
| **Labor force participation** | Employment status | ACS | *labor_participation_rate* |
| **Gini index** | Income inequality | ACS | *gini_index* |
| **Median earnings** | Measures singular income | ACS | *median_earnings* |
| **% of Households on SNAP** | Economic need | ACS | *snap_pct* |
| **Average commute time** | Transportation | ACS | *mean_commute_time* |
| **Median home value** | Housing market | ACS | *median_home_value* |
| **Median gross rent** | Housing market | ACS | *median_gross_rent* |
| **Homeownership rate** | Housing | ACS | *homeownership_rate* |
| **Vacancy rate** | Housing | ACS | *vacancy_rate* |
| **Median year built** | Housing | ACS | *median_year_built* |
| **Crowding rate** | Potential overcrowding | ACS | *crowding_rate* |
| **Housing cost burden** | Affordability | ACS | *housing_cost_burden_pct* |
| **% Agriculture** | Employment | ACS | *agriculture_pct* |
| **% Construction** | Employment | ACS | *construction_pct* |
| **% Manufacturing** | Employment | ACS | *manufacturing_pct* |
| **% Arts/Tourism** | Employment | ACS | *arts_tourism_pct* |
| **% Finance** | Employment | ACS | *finance_pct* |
| **% Information** | Employment | ACS | *information_pct* |
| **% Retail** | Employment | ACS | *retail_pct* |
| **% Healthcare/Education** | Employment | ACS | *education_healthcare_pct* |
| **% Government** | Employment | ACS | *government_pct* |
| **% Professional/Technical** | Employment | ACS | *technical_pct* |
| **% Commute with Public Transit** | Public Transportation | ACS | *public_transit_pct* |
| **% Work from Home** | Commute Time | ACS | *work_from_home_pct* |
| **% Walk/Bike to Work** | Commute Time | ACS | *walk_bike_to_work_pct* |
| **% Drive Alone** | Commute | ACS | *drive_alone_pct* |
| **Elevation** | Geography | USGS | *mean_elevation* |
| **Mean annual temperature** | Geography | PRISM | *mean_temp* |
| **Annual precipitation** | Geography | PRISM | *annual_precip* |
| **Land area** | Geography | Census TIGER/Line | *land_area_sq_miles* |
| **Distance to coastline** | Geography | Engineered | *distance_to_coast_miles* |
| **5 year population change** | Demographics | ACS 2023 vs ACS 2018 | *population_growth_5yr* |
| **Homicide rate** | Crime | County Health Rankings & Roadmaps | *homicide_rate* |
| **Suicide Rate** | Mental Health | CHR&R | *suicide_rate* |
| **Firearm Fatalities** | Safety | CHR&R | *firearm_deaths_rate* |
| **Income growth (inflation adjusted)** | Economy | ACS 2023 vs ACS 2018 (Inflation adjusted using Consumer Price Index from BLS) | *income_growth* |
| **Population Stability Index** | Demographics | Engineered | *pop_stability_index* |
| **Broadband internet access %** | Access to internet | ACS | *internet_access_pct* |
| **% Married** | Demographics | ACS | *married_pct* |
| **Disability Rate** | Demographics | ACS | *disability_rate* |
| **Urban/Rural Classification** | Demographics | USDA Rural-Urban Continuum Codes | *RUCC_2023* |
| **Water Coverage** | Geography | TIGER/Line | *water_coverage_pct* |
| **Terrain Ruggedness (Std Dev of Elevation)** | Geography | USGS | *terrain_ruggedness* |
| **Forest Coverage** | Geography | FIA County Estimates | *forest_coverage_pct* |
| **Voter turnout** | Civic engagement | CHR&R | *voter_turnout* |
| **Democratic vote share %** | Political makeup | MIT Election Data and Science Lab (MEDSL) County Presidential Election Returns | *dem_vote_share_2020* |
| **Party competitiveness (\|Republican%-Democrat%\|)** | Political makeup | Engineered from MIT Election Data and Science Lab (MEDSL) County Presidential Election Returns | *party_competitiveness_2020* |
| **20 Year Democrat Trend** | Political makeup | Engineered from MIT Election Data and Science Lab (MEDSL) County Presidential Election Returns | *dem_swing_2000_2020* |
| **20 Year Competitiveness trend** | Political Makeup | Engineered from MIT Election Data and Science Lab (MEDSL) County Presidential Election Returns | *comp_swing_2000_2020* |
