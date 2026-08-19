# Ordinal Function
ordinal <- function(x) {
  ifelse(
    x %% 100 %in% c(11, 12, 13),
    paste0(x, "th"),
    paste0(
      x,
      c("th", "st", "nd", "rd", rep("th", 6))[(x %% 10) + 1]
    )
  )
}

# County Explorer
top_percent_label <- function(value, data, higher_is_better = TRUE){
  percentile <- mean(data <= value, na.rm = TRUE) * 100
  if(higher_is_better){
    top_percent <- round(100 - percentile)
    if(top_percent <= 50){
      paste0("Top ", top_percent, "% Nationally")
    } else {
      paste0("Bottom ", round(percentile), "% Nationally")
    }
  } else {
    bottom_percent <- round(100 - percentile)
    if(bottom_percent <= 50){
      paste0("Bottom ", bottom_percent, "% Nationally")
    } else {
      paste0("Top ", round(percentile), "% Nationally")
    }
  }
}

higher_better_percentile <- function(value, data){
  mean(data <= value, na.rm = TRUE) * 100
}

# Variable Explorer

variable_metadata <- tribble(
  ~variable, ~label, ~unit, ~formatter, ~digits,
  
  "total_population", "Population", NA_character_, "comma", 0,
  "population_density", "Population Density", "(people / sq. mile)", "comma", 1,
  "population_growth_5yr", "5-year Population Growth", "(%)", "percent", 1,
  "pop_stability_index", "Population Stability Index", NA_character_, "decimal", 2,
  "median_age", "Median Age", "(years)", "decimal", 1,
  "under_18_pct", "Under 18%", NA_character_, "percent", 1,
  "over_65_pct", "Over 65%", NA_character_, "percent", 1,
  "average_household_size", "Average Household Size", NA_character_, "decimal", 2,
  "married_pct", "% Married", NA_character_, "percent", 1,
  "diversity_index", "Diversity Index", NA_character_, "decimal", 2,
  "foreign_born_pct", "Foreign-born %", NA_character_, "percent", 1,
  "veteran_pct", "Veteran %", NA_character_, "percent", 1,
  "disability_rate", "Disability Rate", "(%)", "percent", 1,
  "RUCC_2023", "Urban/Rural Classification Codes", NA_character_, "integer", 0,
  "high_school_pct", "High School Graduate %", NA_character_, "percent", 1,
  "some_college_pct", "Some College %", NA_character_, "percent", 1,
  "college_grad_pct", "Bachelor's Degree %", NA_character_, "percent", 1,
  "masters_or_higher_pct", "Masters+ Degree %", NA_character_, "percent", 1,
  "public_school_pct", "Public School %", NA_character_, "percent", 1,
  "snap_pct", "% of Households on SNAP", NA_character_, "percent", 1,
  "gini_index", "Gini Index", NA_character_, "decimal", 2,
  "poverty_rate", "Poverty Rate", "(%)", "percent", 1,
  "median_household_income", "Median Household Income", "(USD)", "dollar", 0,
  "income_growth", "Inflation Adjusted Income Growth", "(%)", "percent", 1,
  "median_earnings", "Median Earnings", "(USD)", "dollar", 0,
  "median_gross_rent", "Median Gross Rent", "(USD/month)", "dollar", 0,
  "median_home_value", "Median Home Value", "(USD)", "dollar", 0,
  "median_year_built", "Median Year Built", NA_character_, "integer", 0,
  "housing_cost_burden_pct", "Housing Cost Burden", "(%)", "percent", 1,
  "homeownership_rate", "Homeownership Rate", "(%)", "percent", 1,
  "vacancy_rate", "Vacancy Rate", "(%)", "percent", 1,
  "crowding_rate", "Crowding Rate", "(%)", "percent", 1,
  "unemployment_rate", "Unemployment Rate", "(%)", "percent", 1,
  "labor_participation_rate", "Labor Force Participation", "(%)", "percent", 1,
  "agriculture_pct", "% Workers in Agriculture", NA_character_, "percent", 1,
  "construction_pct", "% Workers in Construction", NA_character_, "percent", 1,
  "manufacturing_pct", "% Workers in Manufacturing", NA_character_, "percent", 1,
  "arts_tourism_pct", "% Workers in Arts/Tourism", NA_character_, "percent", 1,
  "finance_pct", "% Workers in Finance", NA_character_, "percent", 1,
  "information_pct", "% Workers in Information", NA_character_, "percent", 1,
  "retail_pct", "% Workers in Retail", NA_character_, "percent", 1,
  "education_healthcare_pct", "% Workers in Education/Healthcare", NA_character_, "percent", 1,
  "government_pct", "% Workers in Government", NA_character_, "percent", 1,
  "technical_pct", "% Workers in Technical/Professional", NA_character_, "percent", 1,
  "mean_commute_time", "Mean Commute Time", "(minutes)", "decimal", 1,
  "public_transit_pct", "% Commute with Public Transit", NA_character_, "percent", 1,
  "walk_bike_to_work_pct", "% Walk/Bike to Work", NA_character_, "percent", 1,
  "drive_alone_pct", "% Drive Alone", NA_character_, "percent", 1,
  "work_from_home_pct", "% Work from Home", NA_character_, "percent", 1,
  "internet_access_pct", "Broadband Internet Access", "(%)", "percent", 1,
  "voter_turnout", "Voter Turnout", "(%)", "percent", 1,
  "dem_vote_share_2020", "Democratic Vote Share (2020)", "(%)", "percent", 1,
  "party_competitiveness", "Party Competitiveness (2020)", NA_character_, "decimal", 2,
  "dem_swing_2000_2020", "20 Year Democratic Trend", NA_character_, "decimal", 2,
  "comp_swing_2000_2020", "20 Year Competitiveness Trend", NA_character_, "decimal", 2,
  "land_area_sq_miles", "Land Area", "(sq. miles)", "comma", 1,
  "terrain_ruggedness", "Terrain Ruggedness", NA_character_, "decimal", 2,
  "water_coverage_pct", "Water Coverage", "(%)", "percent", 1,
  "forest_coverage_pct", "Forest Coverage", "(%)", "percent", 1,
  "mean_temp", "Mean Annual Temperature", "(°C)", "decimal", 1,
  "annual_precip", "Annual Precipitation", "(in)", "comma", 0
)

variables <- setNames(
  variable_metadata$variable,
  variable_metadata$label
)

get_label <- function(var){
  variable_metadata |>
    filter(variable == var) |>
    pull(label)
}

get_unit <- function(var){
  variable_metadata |>
    filter(variable == var) |>
    pull(unit)
}

get_formatter <- function(var){
  variable_metadata |>
    filter(variable == var) |>
    pull(formatter)
}

get_digits <- function(var){
  variable_metadata |>
    filter(variable == var) |>
    pull(digits)
}

get_description <- function(var){
  variable_metadata |>
    filter(variable == var) |>
    pull(description)
}

variable_summary_card <- function(variable, label, unit, county_data){
  values <- county_data[[variable]]
  div(
    class = "summary-card",
    h3(label),
    if(!is.na(unit)){
      p(
        strong("Unit: "),
        unit)
    },
    tags$hr(),
    h4("Summary Statistics"),
    tags$ul(
      tags$li(
        strong("Mean: "),
        round(mean(values, na.rm = TRUE), 2)),
      tags$li(
        strong("Median: "),
        round(median(values, na.rm = TRUE), 2)),
      tags$li(
        strong("Minimum: "),
        round(min(values, na.rm = TRUE), 2)),
      tags$li(
        strong("Maximum: "),
        round(max(values, na.rm = TRUE), 2)),
      tags$li(
        strong("Standard Deviation: "),
        round(sd(values, na.rm = TRUE), 2)),
      tags$li(
        strong("Missing Counties: "),
        sum(is.na(values)))
    )
  )
}

variable_high_low_card <- function(county_data, variable, label, unit, direction){
  
  data <- county_data |>
    st_drop_geometry() |>
    select(county_name, all_of(variable)) |>
    arrange(.data[[variable]])
  if(direction == "high"){
    data <- data |>
      slice_tail(n = 5) |> 
      arrange(desc(.data[[variable]]))
    title <- "Highest 5 Counties"
  } else {
    data <- data |>
      slice_head(n = 5)
    title <- "Lowest 5 Counties"
  }
  div(
    class = "summary-card",
    h4(title),
    tags$ul(
      lapply(seq_len(nrow(data)), function(i){
        tags$li(
          paste0(
            data$county_name[i],
            ": ",
            round(data[[variable]][i],2)))
      })
    )
  )
}

variable_percentile_card <- function(county_data, variable, label, unit){
  
  data <- county_data |>
    st_drop_geometry() |>
    select(county_name, all_of(variable)) |>
    filter(!is.na(.data[[variable]]))
  
  percentiles <- quantile(
    data[[variable]],
    probs = c(.25, .5, .75)
  )
  
  find_closest <- function(value){
    data |>
      mutate(
        difference = abs(.data[[variable]] - value)
      ) |>
      arrange(difference) |>
      slice_head(n = 3)
  }
  
  p25 <- find_closest(percentiles[1])
  p50 <- find_closest(percentiles[2])
  p75 <- find_closest(percentiles[3])
  
  div(
    class = "summary-card",
    
    h4("County Percentiles"),
    
    strong("25th Percentile"),
    tags$ul(
      lapply(seq_len(nrow(p25)), function(i){
        tags$li(
          paste0(
            p25$county_name[i],
            ": ",
            round(p25[[variable]][i],2)
          )
        )
      })
    ),
    
    strong("Median (50th Percentile)"),
    tags$ul(
      lapply(seq_len(nrow(p50)), function(i){
        tags$li(
          paste0(
            p50$county_name[i],
            ": ",
            round(p50[[variable]][i],2)
          )
        )
      })
    ),
    
    strong("75th Percentile"),
    tags$ul(
      lapply(seq_len(nrow(p75)), function(i){
        tags$li(
          paste0(
            p75$county_name[i],
            ": ",
            round(p75[[variable]][i],2)
          )
        )
      })
    )
  )
}


# County Comparison
calculate_similarity <- function(county_data, county1, county2, params){
  
  variables <- names(params$weights)
  selected <- county_data |>
    st_drop_geometry() |>
    mutate(GEOID = as.character(GEOID), voter_turnout = as.numeric(voter_turnout)) |>
    filter(GEOID %in% c(county1, county2)) |>
    select(
      GEOID,
      all_of(variables))
  scaled <- scale(
    selected |> select(-GEOID),
    center = params$center,
    scale = params$scale)
  
  weighted <- sweep(scaled, 2, params$weights, "*")
  distance <- dist(weighted)[1]
  similarity <- 100 - distance
  similarity
}

county_raw_comparison_table <- function(county_data, county1, county2){
  comparison_variables <- c(
    "population_density",
    "median_age",
    "diversity_index",
    "median_household_income",
    "college_grad_pct",
    "unemployment_rate",
    "poverty_rate",
    "internet_access_pct",
    "mean_commute_time",
    "mean_temp",
    "voter_turnout")
  variable_labels <- c(
    population_density = "Population Density",
    median_age = "Median Age",
    diversity_index = "Diversity Index",
    median_household_income = "Median Household Income",
    college_grad_pct = "College Graduate %",
    unemployment_rate = "Unemployment Rate",
    poverty_rate = "Poverty Rate",
    internet_access_pct = "Internet Access %",
    mean_commute_time = "Mean Commute Time",
    mean_temp = "Mean Temperature",
    voter_turnout = "Voter Turnout")
  data <- county_data |>
    st_drop_geometry() |>
    mutate(
      GEOID = as.character(GEOID),
      voter_turnout = as.numeric(voter_turnout)) |>
    filter(
      GEOID %in% c(county1, county2)) |>
    select(
      county_name,
      all_of(comparison_variables))
  county_names <- data$county_name
  table_data <- data |>
    pivot_longer(
      cols = all_of(comparison_variables),
      names_to = "variable",
      values_to = "value") |>
    mutate(
      variable = variable_labels[variable]) |>
    select(
      variable,
      county_name,
      value) |>
    pivot_wider(
      names_from = county_name,
      values_from = value)
  table_data <- table_data |>
    mutate(
      Difference = .data[[county_names[1]]] - .data[[county_names[2]]])
  table_data <- table_data |>
    mutate(
      across(
        -variable,
        as.numeric)) |>
    mutate(
      across(
        -variable,
        ~ case_when(
          variable %in% c(
            "College Graduate %",
            "Unemployment Rate",
            "Poverty Rate",
            "Internet Access %",
            "Voter Turnout"
          ) ~ paste0(round(.x * 100, 1), "%"),
          
          variable == "Median Household Income" ~ scales::dollar(.x),
          
          TRUE ~ as.character(round(.x, 2)))))
  table_data
}

most_similar_display <- most_similar |>
  mutate(
    similarity = 100 - distance) |>
  left_join(
    county |>
      st_drop_geometry() |>
      select(
        GEOID,
        county_name),
    by = c("source_GEOID" = "GEOID")) |>
  rename(
    county1 = county_name) |>
  left_join(
    county |>
      st_drop_geometry() |>
      select(
        GEOID,
        county_name),
    by = c("comparison_GEOID" = "GEOID")) |>
  rename(
    county2 = county_name)


least_similar_display <- least_similar |>
  mutate(
    similarity = 100 - distance) |>
  left_join(
    county |>
      st_drop_geometry() |>
      select(
        GEOID,
        county_name),
    by = c("source_GEOID" = "GEOID")) |>
  rename(county1 = county_name) |>
  left_join(
    county |>
      st_drop_geometry() |>
      select(
        GEOID,
        county_name),
    by = c("comparison_GEOID" = "GEOID")) |>
  rename(
    county2 = county_name)


# Clusters

cluster_profile_card <- function(profile, size, largest_counties, county_data){
  density_pct <- round(mean(county_data$population_density < profile$population_density, na.rm = TRUE) * 100)
  diversity_pct <- round(mean(county_data$diversity_index < profile$diversity_index, na.rm = TRUE) * 100)
  temp_pct <- round(mean(county_data$mean_temp < profile$mean_temp, na.rm = TRUE) * 100)
  poverty_pct <- round(mean(county_data$poverty_rate < profile$poverty_rate, na.rm = TRUE) * 100)
  income_pct <- round(mean(county_data$median_household_income < profile$median_household_income, na.rm = TRUE) * 100)
  median_age_pct <- round(mean(county_data$median_age < profile$median_age, na.rm = TRUE) * 100)
  grad_pct <- round(mean(county_data$college_grad_pct < profile$college_grad_pct, na.rm = TRUE) * 100)
  div(
    class = "cluster-card",
    div(
      style = paste0(
        "height: 8px;
         background:",
        profile$cluster_color,
        ";
         border-radius: 8px 8px 0 0;
         margin: -20px -20px 15px -20px;")),
    h3(profile$cluster_name),
    tags$hr(),
    fluidRow(
      column(
        6,
        strong("Representative County"),
        br(),
        profile$rep_name,
        br(), br(),
        strong("Number of Counties"),
        br(),
        scales::comma(size)
      ),
      column(
        6,
        strong("Largest Counties"),
        tags$ul(
          lapply(
            largest_counties$county_name,
            tags$li)))),
    tags$hr(),
    h4("Characteristics"),
    tags$ul(
      tags$li(
        strong("Population Density: "),
        scales::comma(round(profile$population_density, 1)),
        ",", em(ordinal(density_pct)," percentile among US Counties.")),
      tags$li(
        strong("Diversity Index: "),
        round(profile$diversity_index, 2),
        ",", em(ordinal(diversity_pct), " percentile among US Counties.")),
      tags$li(
        strong("Average Temperature: "),
        round(profile$mean_temp, 1),
        "°C,", em(ordinal(temp_pct), " percentile among US Counties.")),
      tags$li(
        strong("Poverty Rate: "),
        scales::percent(profile$poverty_rate),
        ",", em(ordinal(poverty_pct), " percentile among US Counties.")),
      tags$li(
        strong("Median Household Income: "),
        scales::dollar(profile$median_household_income),
        ",", em(ordinal(income_pct), " percentile among US Counties.")),
      tags$li(
        strong("Median Age: "),
        round(profile$median_age, 1),
        ",", em(ordinal(median_age_pct), " percentile among US Counties.")),
      tags$li(
        strong("Bachelor's Degree %: "),
        scales::percent(profile$college_grad_pct),
        ",", em(ordinal(grad_pct), " percentile among US Counties."))
    )
  )
}