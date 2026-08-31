# US County Geography Data Dictionary

## Overview

This dataset contains the features used for the county-level dataset for the contiguous United States counties. This dataset contains features of several different categories: demographic, socioeconomic, environmental, geographic, health, and political.

Data sources include:

-   American Community Survey (ACS) 2023 5-year estimates
-   TIGER/Line county geography
-   PRISM climate normals
-   USGS elevation data
-   NLCD/FIA land coverage data
-   County Health Rankings
-   County Presidential Election Results
-   Natural Earth Coastline
-   Rural-Urban Continuum Codes

| Variable | Formula (if applicable) | Source | Variable Name in Dataset | Methodology | Category |
|------------|------------|------------|------------|------------|------------|
| **GEOID** | N/A | TIGER | *GEOID* | Unique five digit identifier for each county | Identification |
| **State FIPS Code** | N/A | TIGER | *STATEFP* | Identifies the state the county is in | Identification |
| **County FIPS Code** | N/A | TIGER | *COUNTYFP* | Identifies the county | Identification |
| **County Name** | N/A | TIGER | *county_name* | Identifies the county by name | Identification |
| **Population** | N/A | ACS | *total_population* | Baseline population level | Demographics |
| **Population density** | Population / Land Area | ACS + Census TIGER/Line | *population_density* | Accounts for county size by measuring population concentration rather than raw population | Demographics |
| **5 year population change** | (2023 Population - 2018 Population) / 2018 Population | ACS 2023 vs ACS 2018 | *population_growth_5yr* | Measures recent population growth or decline | Demographics |
| **Population Stability Index** | Custom exponential decay function | Engineered | *pop_stability_index* | Measures population stability, with higher values indicating more stable populations | Demographics |
| **Median Age** | N/A | ACS | *median_age* | Shows how old the county population is | Demographics |
| **Under 18%** | Sum of all residents under 18 / Population | ACS | *under_18_pct* | Shows the percentage of children in a county | Demographics |
| **Over 65%** | Sum of all residents over 65 / Population | ACS | *over_65_pct* | Shows the percentage of seniors in a county | Demographics |
| **Average Household Size** | N/A | ACS | *average_household_size* | Shows the average number of residents per household | Demographics |
| **% Married** | Married residents / marital population | ACS | *married_pct* | Shows the percentage of married residents | Demographics |
| **Diversity Index** | 1 - Sum of squared racial proportions | Engineered from ACS | *diversity_index* | Based on Simpson Diversity Index where values closer to 1 indicate greater diversity | Demographics |
| **Foreign-born %** | Foreign-born population / Total population | ACS | *foreign_born_pct* | Measures the percentage of residents born outside the United States | Demographics |
| **Veteran %** | Veterans / Adult population | ACS | *veteran_pct* | Measures the percentage of adult residents who are veterans | Demographics |
| **Disability Rate** | Residents with disability / Total disability population | ACS | *disability_rate* | Measures prevalence of disability in county residents | Demographics |
| **Urban/Rural Classification** | N/A | USDA Rural-Urban Continuum Codes | *RUCC_2023* | Classifies counties based on metropolitan status and rural characteristics | Demographics |
| **High school graduate %** | High school graduates / Adult population | ACS | *high_school_pct* | Shows percentage of residents whose highest education is high school | Education |
| **Some college %** | Some college + associates degree / Adult population | ACS | *some_college_pct* | Shows residents with post-secondary education below bachelor's degree | Education |
| **Bachelor's graduate %** | Bachelor's degree holders / Adult population | ACS | *college_grad_pct* | Shows college education attainment | Education |
| **Masters+ graduate %** | Graduate/professional degrees / Adult population | ACS | *masters_or_higher_pct* | Shows advanced education attainment | Education |
| **Public School %** | Public school enrollment / Total enrollment | ACS | *public_school_pct* | Shows percentage of students attending public schools | Education |
| **% of Households on SNAP** | SNAP Households / Total Households | ACS | *snap_pct* | Measures the percentage of households receiving Supplemental Nutrition Assistance Program benefits | Socioeconomic |
| **Gini Index** | N/A | ACS | *gini_index* | Measures income inequality within a county, where higher values indicate greater inequality | Socioeconomic |
| **Poverty Rate** | Residents below poverty line / Total population | ACS | *poverty_rate* | Measures the percentage of residents living below the federal poverty line | Socioeconomic |
| **Median Household Income** | N/A | ACS | *median_household_income* | Measures the median income level of households within a county | Socioeconomic |
| **Income Growth (inflation adjusted)** | (2023 Income - 2018 Income adjusted for inflation) / 2018 Income adjusted for inflation | ACS 2023 vs ACS 2018 + CPI adjustment | *income_growth* | Measures real income growth while accounting for inflation between time periods | Socioeconomic |
| **Median Earnings** | N/A | ACS | *median_earnings* | Measures the median earnings of individual workers within a county | Socioeconomic |
| **Median Gross Rent** | N/A | ACS | *median_gross_rent* | Measures typical rental costs within a county | Housing |
| **Median Home Value** | N/A | ACS | *median_home_value* | Measures the typical value of owner-occupied homes within a county | Housing |
| **Median Year Built** | N/A | ACS | *median_year_built* | Measures the median age of housing stock within a county | Housing |
| **Housing Cost Burden** | Households spending 30%+ of income on rent / Total renter households | ACS | *housing_cost_burden_pct* | Measures housing affordability pressure among renters | Housing |
| **Homeownership Rate** | Owner occupied homes / Occupied housing units | ACS | *homeownership_rate* | Measures the percentage of occupied housing units owned by residents | Housing |
| **Vacancy Rate** | Vacant housing units / Total housing units | ACS | *vacancy_rate* | Measures the percentage of housing units currently vacant | Housing |
| **Crowding Rate** | Crowded households / Occupied households | ACS | *crowding_rate* | Measures housing overcrowding based on residents per room | Housing |
| **Unemployment Rate** | Unemployed workers / Labor force | ACS | *unemployment_rate* | Measures the percentage of workers actively seeking employment | Employment |
| **Labor Force Participation** | Labor force / Population over 16 | ACS | *labor_participation_rate* | Measures the percentage of working-age residents participating in the labor force | Employment |
| **% Agriculture** | Agricultural workers / Total employed population | ACS | *agriculture_pct* | Shows the share of employment tied to agriculture | Employment |
| **% Construction** | Construction workers / Total employed population | ACS | *construction_pct* | Shows the share of employment tied to construction | Employment |
| **% Manufacturing** | Manufacturing workers / Total employed population | ACS | *manufacturing_pct* | Shows the share of employment tied to manufacturing | Employment |
| **% Arts/Tourism** | Arts and recreation workers / Total employed population | ACS | *arts_tourism_pct* | Shows the share of employment tied to arts, entertainment, and recreation industries | Employment |
| **% Finance** | Finance workers / Total employed population | ACS | *finance_pct* | Shows the share of employment tied to finance and insurance industries | Employment |
| **% Information** | Information workers / Total employed population | ACS | *information_pct* | Shows the share of employment tied to information industries | Employment |
| **% Retail** | Retail workers / Total employed population | ACS | *retail_pct* | Shows the share of employment tied to retail industries | Employment |
| **% Healthcare/Education** | Healthcare and education workers / Total employed population | ACS | *education_healthcare_pct* | Shows the share of employment tied to education and healthcare industries | Employment |
| **% Government** | Government workers / Total employed population | ACS | *government_pct* | Shows the share of employment tied to government industries | Employment |
| **% Professional/Technical** | Professional and technical workers / Total employed population | ACS | *technical_pct* | Shows the share of employment tied to professional, scientific, and technical industries | Employment |
| **Average Commute Time** | N/A | ACS | *mean_commute_time* | Measures the average commute time for workers in minutes | Transportation |
| **% Commute with Public Transit** | Public transit commuters / Total commuters | ACS | *public_transit_pct* | Measures reliance on public transportation for commuting to and from work | Transportation |
| **% Walk/Bike to Work** | Walking + bicycle commuters / Total commuters | ACS | *walk_bike_to_work_pct* | Measures the percentage of commuters using active transportation methods to and from work | Transportation |
| **% Drive Alone** | Drive alone commuters / Total commuters | ACS | *drive_alone_pct* | Measures dependence on individual vehicle commuting to and from work | Transportation |
| **% Work from Home** | Work from home workers / Labor force | ACS | *work_from_home_pct* | Measures the percentage of workers who work remotely | Transportation |
| **Broadband Internet Access %** | Households with internet subscription / Total households | ACS | *internet_access_pct* | Measures household access to internet services | Infrastructure |
| **Homicide Rate** | N/A | County Health Rankings & Roadmaps | *homicide_rate* | Measures homicide prevalence within a county | Health/Safety |
| **Suicide Rate** | N/A | County Health Rankings & Roadmaps | *suicide_rate* | Measures suicide prevalence within a county | Health/Safety |
| **Firearm Fatalities** | N/A | County Health Rankings & Roadmaps | *firearm_deaths_rate* | Measures firearm-related mortality within a county | Health/Safety |
| **Voter Turnout** | N/A | County Health Rankings & Roadmaps | *voter_turnout* | Measures civic participation through voter turnout | Civic Engagement |
| **Democratic Vote Share %** | Democratic votes / Total votes | MIT Election Data and Science Lab County Presidential Election Returns | *dem_vote_share_2020* | Measures Democratic presidential vote share in the 2020 election | Politics |
| **Party Competitiveness** | 1 - abs(Republican Vote Share - Democratic Vote Share) | Engineered from MIT Election Data and Science Lab County Presidential Election Returns | *party_competitiveness_2020* | Measures how competitive the two major parties were in the 2020 presidential election | Politics |
| **20 Year Democrat Trend** | Democratic vote share 2020 - Democratic vote share 2000 | Engineered from MIT Election Data and Science Lab County Presidential Election Returns | *dem_swing_2000_2020* | Measures long-term change in Democratic presidential support | Politics |
| **20 Year Competitiveness Trend** | Competitiveness 2020 - Competitiveness 2000 | Engineered from MIT Election Data and Science Lab County Presidential Election Returns | *comp_swing_2000_2020* | Measures whether counties have become more or less politically competitive over time | Politics |
| **Land Area** | N/A | Census TIGER/Line | *land_area_sq_miles* | Measures county land area in square miles | Geography |
| **Elevation** | N/A | USGS elevation data via elevatr | *mean_elevation* | Measures average elevation of county terrain | Geography |
| **Terrain Ruggedness (Std Dev of Elevation)** | Standard deviation of elevation values within county | Engineered from elevation raster | *terrain_ruggedness* | Measures variation in terrain elevation, with higher values representing more rugged terrain | Geography |
| **Water Coverage** | Water Area / Total Area | Census TIGER/Line | *water_coverage_pct* | Measures the percentage of county area covered by water | Geography |
| **Distance to Coastline** | Distance between county centroid and nearest coastline | Natural Earth coastline + engineered | *distance_to_coast_miles* | Measures proximity of county center to coastline | Geography |
| **Forest Coverage** | Forest Land Area / Sampled Land Area | FIA County Estimates | *forest_coverage_pct* | Measures the percentage of county land classified as forest | Geography |
| **Mean Annual Temperature** | N/A | PRISM Climate Data | *mean_temp* | Measures average annual temperature | Climate |
| **Annual Precipitation** | N/A | PRISM Climate Data | *annual_precip* | Measures average annual precipitation | Climate |
| **Geometry** | N/A | Census TIGER/Line | *geometry* | County polygon geometry | Geography |
