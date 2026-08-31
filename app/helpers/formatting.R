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
  "party_competitiveness_2020", "Party Competitiveness (2020)", NA_character_, "decimal", 2,
  "dem_swing_2000_2020", "20 Year Democratic Trend", NA_character_, "percent", 1,
  "comp_swing_2000_2020", "20 Year Competitiveness Trend", NA_character_, "percent", 1,
  "land_area_sq_miles", "Land Area", "(sq. miles)", "comma", 1,
  "terrain_ruggedness", "Terrain Ruggedness", NA_character_, "decimal", 2,
  "water_coverage_pct", "Water Coverage", "(%)", "percent", 1,
  "forest_coverage_pct", "Forest Coverage", "(%)", "percent", 1,
  "mean_temp", "Mean Annual Temperature", "(°C)", "decimal", 1,
  "annual_precip", "Annual Precipitation", "(mm)", "comma", 0,
  "uniqueness_score", "Uniqueness Score", "(%)", "percent", 1,
  "pca_cluster_name", "PCA Cluster", NA_character_, "category", NA,
  "hc_cluster_name", "Hierarchical Cluster", NA_character_, "category", NA,
  "gmm_cluster_name", "GMM Cluster", NA_character_, "category", NA
)

variables <- setNames(
  variable_metadata$variable,
  variable_metadata$label
)

numeric_variables <- variable_metadata |>
  filter(formatter != "category") |>
  select(variable, label)

numeric_variables <- setNames(
  numeric_variables$variable,
  numeric_variables$label
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

format_variable_value <- function(value, variable){
  if (is.na(value)) return(NA_character_)
  formatter <- get_formatter(variable)
  digits <- get_digits(variable)
  unit <- get_unit(variable)
  formatted <- switch(
    formatter,
    percent = paste0(round(value * 100, digits), "%"),
    dollar = scales::dollar(round(value, digits)),
    comma = scales::comma(round(value, digits)),
    integer = scales::comma(round(value, 0)),
    decimal = as.character(round(value, digits)),
    category = as.character(value),
    as.character(round(value, 2))
  )
  if (formatter %in% c("decimal", "comma", "integer") && !is.na(unit)) {
    formatted <- paste0(formatted, " ", unit)
  }
  formatted
}

variable_summary_card <- function(variable, label, unit, county_data){
  values <- county_data[[variable]]
  stats <- list(
    "Mean" = mean(values, na.rm = TRUE),
    "Median" = median(values, na.rm = TRUE),
    "Minimum" = min(values, na.rm = TRUE),
    "Maximum" = max(values, na.rm = TRUE),
    "Std. Deviation" = sd(values, na.rm = TRUE)
  )
  div(
    class = "summary-card",
    h3(label),
    if (!is.na(unit)) p(class = "variable-explorer-unit", strong("Unit: "), unit),
    tags$hr(),
    h4("Summary Statistics"),
    div(
      class = "stat-grid",
      lapply(names(stats), function(stat_name){
        div(
          class = "stat-card",
          div(class = "stat-label", stat_name),
          div(class = "stat-value", format_variable_value(stats[[stat_name]], variable))
        )
      }),
      div(
        class = "stat-card",
        div(class = "stat-label", "Missing Counties"),
        div(class = "stat-value", sum(is.na(values)))
      )
    )
  )
}

variable_high_low_card <- function(county_data, variable, label, unit, direction){
  
  data <- county_data |>
    st_drop_geometry() |>
    select(county_name, all_of(variable)) |>
    filter(!is.na(.data[[variable]])) |>
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
    div(
      class = "rank-list",
      lapply(seq_len(nrow(data)), function(i){
        div(
          class = "rank-row",
          div(class = "rank-label", paste0(i, ". ", data$county_name[i])),
          div(class = "rank-value", format_variable_value(data[[variable]][i], variable))
        )
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
  
  rank_block <- function(label_text, data_subset){
    tagList(
      div(class = "eyebrow", label_text),
      div(
        class = "rank-list",
        lapply(seq_len(nrow(data_subset)), function(i){
          div(
            class = "rank-row",
            div(class = "rank-label", data_subset$county_name[i]),
            div(class = "rank-value", format_variable_value(data_subset[[variable]][i], variable))
          )
        })
      )
    )
  }
  
  div(
    class = "summary-card",
    h4("County Percentiles"),
    rank_block("25th Percentile", p25),
    rank_block("Median (50th Percentile)", p50),
    rank_block("75th Percentile", p75)
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
    "total_population",
    "population_density",
    "pop_stability_index",
    "median_age",
    "diversity_index",
    "gini_index",
    "median_household_income",
    "college_grad_pct",
    "unemployment_rate",
    "poverty_rate",
    "internet_access_pct",
    "mean_commute_time",
    "mean_temp",
    "terrain_ruggedness",
    "annual_precip",
    "forest_coverage_pct",
    "voter_turnout",
    "party_competitiveness_2020",
    "uniqueness_score")
  variable_labels <- c(
    total_population = "Population",
    population_density = "Population Density",
    pop_stability_index = "Population Stability Index",
    median_age = "Median Age",
    diversity_index = "Diversity Index",
    gini_index = "Gini Index",
    median_household_income = "Median Household Income",
    college_grad_pct = "College Graduate %",
    unemployment_rate = "Unemployment Rate",
    poverty_rate = "Poverty Rate",
    internet_access_pct = "Internet Access %",
    mean_commute_time = "Mean Commute Time",
    mean_temp = "Mean Temperature",
    terrain_ruggedness = "Terrain Ruggedness",
    annual_precip = "Annual Precipitation (mm)",
    forest_coverage_pct = "Forest Coverage %",
    voter_turnout = "Voter Turnout",
    party_competitiveness_2020 = "Political Party Competitiveness",
    uniqueness_score = "Uniqueness Score")
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
    county1 = pmin(source_GEOID, comparison_GEOID),
    county2 = pmax(source_GEOID, comparison_GEOID)
  ) |>
  distinct(county1, county2, .keep_all = TRUE) |>
  mutate(
    similarity = 100 - distance
  ) |>
  arrange(desc(similarity)) |>
  left_join(
    county |>
      st_drop_geometry() |>
      select(
        GEOID,
        county_name
      ),
    by = c("county1" = "GEOID")
  ) |>
  rename(county1_name = county_name) |>
  left_join(
    county |>
      st_drop_geometry() |>
      select(
        GEOID,
        county_name
      ),
    by = c("county2" = "GEOID")
  ) |>
  rename(county2_name = county_name)


least_similar_display <- least_similar |>
  mutate(
    county1 = pmin(source_GEOID, comparison_GEOID),
    county2 = pmax(source_GEOID, comparison_GEOID)
  ) |>
  distinct(county1, county2, .keep_all = TRUE) |>
  mutate(
    similarity = 100 - distance
  ) |>
  arrange(similarity) |>
  left_join(
    county |>
      st_drop_geometry() |>
      select(
        GEOID,
        county_name
      ),
    by = c("county1" = "GEOID")
  ) |>
  rename(county1_name = county_name) |>
  left_join(
    county |>
      st_drop_geometry() |>
      select(
        GEOID,
        county_name
      ),
    by = c("county2" = "GEOID")
  ) |>
  rename(county2_name = county_name)


# Clusters

cluster_characteristic_vars <- c(
  "population_density",
  "diversity_index",
  "mean_temp",
  "poverty_rate",
  "median_household_income",
  "median_age",
  "college_grad_pct"
)

cluster_profile_card <- function(
    profile,
    size,
    largest_counties,
    county_data,
    click_id
){
  density_pct <- round(mean(county_data$population_density < profile$population_density, na.rm = TRUE) * 100)
  diversity_pct <- round(mean(county_data$diversity_index < profile$diversity_index, na.rm = TRUE) * 100)
  temp_pct <- round(mean(county_data$mean_temp < profile$mean_temp, na.rm = TRUE) * 100)
  poverty_pct <- round(mean(county_data$poverty_rate < profile$poverty_rate, na.rm = TRUE) * 100)
  income_pct <- round(mean(county_data$median_household_income < profile$median_household_income, na.rm = TRUE) * 100)
  median_age_pct <- round(mean(county_data$median_age < profile$median_age, na.rm = TRUE) * 100)
  grad_pct <- round(mean(county_data$college_grad_pct < profile$college_grad_pct, na.rm = TRUE) * 100)
  div(
    class = "cluster-card",
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
      click_id,
      profile$cluster
    ),
    div(
      style = paste0(
        "height: 8px;
       background:", profile$cluster_color, ";
       border-radius: 8px 8px 0 0;
       margin: -20px -20px 15px -20px;"
      )
    ),
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
          lapply(largest_counties$county_name, tags$li)))),
    tags$hr(),
    h4("Characteristics"),
    tags$ul(
      tags$li(strong("Population Density: "), scales::comma(round(profile$population_density, 1)), ",", em(ordinal(density_pct), " percentile among US Counties.")),
      tags$li(strong("Diversity Index: "), round(profile$diversity_index, 2), ",", em(ordinal(diversity_pct), " percentile among US Counties.")),
      tags$li(strong("Average Temperature: "), round(profile$mean_temp, 1), "°C,", em(ordinal(temp_pct), " percentile among US Counties.")),
      tags$li(strong("Poverty Rate: "), scales::percent(profile$poverty_rate), ",", em(ordinal(poverty_pct), " percentile among US Counties.")),
      tags$li(strong("Median Household Income: "), scales::dollar(profile$median_household_income), ",", em(ordinal(income_pct), " percentile among US Counties.")),
      tags$li(strong("Median Age: "), round(profile$median_age, 1), ",", em(ordinal(median_age_pct), " percentile among US Counties.")),
      tags$li(strong("Bachelor's Degree %: "), scales::percent(profile$college_grad_pct), ",", em(ordinal(grad_pct), " percentile among US Counties."))
    ),
    div(class = "cluster-card-hint", "Click for details →")
  )
}

cluster_key_variable_table <- function(profile, county_data, variables = cluster_characteristic_vars){
  rows <- lapply(variables, function(var){
    pct <- round(mean(county_data[[var]] < profile[[var]], na.rm = TRUE) * 100)
    value <- profile[[var]]
    formatted <- switch(
      var,
      population_density = scales::comma(round(value, 1)),
      diversity_index = as.character(round(value, 2)),
      mean_temp = paste0(round(value, 1), "°C"),
      poverty_rate = scales::percent(value),
      median_household_income = scales::dollar(value),
      median_age = as.character(round(value, 1)),
      college_grad_pct = scales::percent(value),
      as.character(round(value, 2))
    )
    tags$tr(
      tags$td(get_label(var)),
      tags$td(class = "data-figure", formatted),
      tags$td(paste0(ordinal(pct), " percentile"))
    )
  })
  tags$table(
    class = "table table-striped cluster-detail-table",
    tags$thead(
      tags$tr(tags$th("Variable"), tags$th("Value"), tags$th("National Percentile"))
    ),
    tags$tbody(rows)
  )
}

cluster_descriptions <- list(
  pca = list(
    "1" = "By far, the counties in this cluster are the oldest, with over 28% of the population on average being over 65. These counties are among the coldest in the country, with a few outliers in California and Arizona. These counties have a lot of veterans, not a lot of immigrants, are typically much more rural, typically vote Republican, and have a lot of forests.",
    "2" = "These counties, almost entirely located in the Appalachia region into states like Missouri and Arkansas, are extremely rural in nature. This cluster has the largest number of counties (605), is not very diverse, and has low diversity and income, with a higher poverty rate and reliance on SNAP. It has the highest rate of construction workers, and overwhelmingly votes Republican.",
    "3" = "This cluster contains just 30 counties, mostly only urban centers. Its population is relatively stable, with minor population growth. Cities like San Francisco, New York, Chicago, and Boston all fit into this category. They’re extremely dense, and rely a lot on public transportation, and vote Democratic in not very close elections.",
    "4" = "These counties, prevalent across Texas and Oklahoma as well as parts of the West and Plains, are unique because of their high level of workforce in agriculture, their lower population densities, high immigration, and overall high extraction based economies. These counties barely get any precipitation, vote overwhelmingly republican and have a low unemployment rate.",
    "5" = "These counties, almost all in the Great Plains region up towards the Canadian border, are extremely rural, with a population density average of just ~3.8. These counties are predominantly white, have lower than average poverty rates, and have low costs of living. They’re cold, don’t get a lot of rain, and are pretty flat.",
    "6" = "These counties, mostly in the upper midwest, stretch through most of the major cities in the region. Its biggest counties are direct suburbs of major cities in the area, while others are more rural. These counties are typically colder than usual, predominantly white with lower than average poverty rates. Like other more rural counties, they have a pretty low cost of living, and vote typically Republican in uncompetitive elections.",
    "7" = "With little exceptions, these counties are predominantly counties with the Rocky Mountains in the backdrop. They’re more diverse than some of the other clusters, but still less diverse than the average, while they make more money than the average, and they have much more density than average. They have pretty competitive elections, typically favoring the democrats. These counties have high elevation, ruggedness, forest coverage, but not a lot of precipitation.",
    "8" = "These counties, almost all in Florida or across the east coast, have a high median age. These counties are pretty densely populated compared to the first cluster, and have a lower than average poverty rate. It rains a lot in these counties, and is pretty hot too. These counties are also usually covered in bodies of water.",
    "9" = "This group of metropolitan areas have found themselves in this cluster, predominantly being smaller metro areas or immediate suburbs in the southern half of the US. These counties are not as dense as the other metro clusters, but are pretty diverse and have a rising income. They’re hot, find themselves voting republican more often than not, and are flat.",
    "10" = "These counties, mostly in the south, obviously tend to be hotter and flatter, as well as more diverse, less dense, and poorer. An average county in this cluster has just under 24k people, and this population has been declining. These counties are typically much more rural, have very high income inequality and poverty. These counties also see a lot of rain compared to the average and have lots more forest. These counties are far and away the least internet-accessible counties.",
    "11" = "Another cluster of metropolitan areas finds us around the US in typically more industrial areas such as Summit County OH (with Akron), Wayne county, MI (with Detroit), and others. These cities across the midwest and other portions of the country are much less populated, poorer, and less diverse than the other metro counties that have been clustered. Their populations have stayed relatively stable thanks to a youthful population, and these counties have very competitive elections.",
    "12" = "These counties, spread near the Mexican border as well as near the Canadian border, are young counties whose diversity comes with a 10% immigrant population. These counties have the highest reliance on SNAP and poverty rate in the entire country, and the lowest income of any cluster. This comes with the highest unemployment rate as well. These counties get very little precipitation and their temperature varies depending on the geography of the county (which border it is close to).",
    "13" = "This cluster has a rapidly growing population, and contains cities like LA, Miami, and the immediate suburbs to many of the counties in Cluster Three. These counties sacrifice a bit of density, public transit usage, and immigrants in favor of a slightly less educated, slightly less impoverished population, and a lower cost of living. This cluster also has significantly more counties than the other urban cluster (193)."
  ),
  hc = list(
    "1" = "These counties are large population suburban counties of major metropolitan areas such as the suburbs of Dallas-Fort Worth, Minneapolis, Chicago, and Columbus. These counties are all over the central and eastern US, and are typically wealthy suburban counties. Their residents are more likely to be college graduates than the average American, and these counties have some of the lowest income inequality in the country. They have somewhat competitive elections but typically vote Republican, and have around average diversity.",
    "2" = "These counties, with counties like Knox County, TN, Wayne County, MI, and Maricopa County, AZ, are mid to large sized metropolitan areas. They differ from the two other metro clusters in that they are not as dense or diverse. This diversity comes from their lack of immigrants. With these areas, these counties are quite similar to the ones in cluster one, except with more density and diversity. Alongside that, they are on average a bit less wealthy and a bit more impoverished, vote for the Democratic party a bit more in more competitive elections.",
    "3" = "These counties, mostly in the south, obviously tend to be hotter and flatter, as well as more diverse, less dense, and poorer. An average county in this cluster has just under 25k people, and this population has been declining. These counties are typically much more rural, have very high income inequality and poverty. These counties also see a lot of rain compared to the average and have lots more forest. These counties are far and away the least internet-accessible counties.",
    "4" = "With lots of counties across Appalachia and the south, this stretch of counties tends to be poorer, less diverse, elevated, rugged, and forested. Like cluster three, the residents in these counties are more impoverished than usual, and there is high income inequality. Residents of this county typically vote Republican.",
    "5" = "This cluster features nearly every major city not in cluster 2 except for New York, which remarkably got its own cluster. Counties in this cluster, such as LA County, Cook County, and Harris County, are extremely densely populated with young diverse populations and typically make a lot of money. Cost of living is way higher in these counties, and they have a large proportion of college graduates. These counties typically vote Democratic.",
    "6" = "These counties, such as Salt Lake County, UT and Buncombe County, NC, are rugged counties that contain cities or communities that are hubs in their region, such as Salt Lake City or Asheville. These counties are less densely populated than even suburban counties, and are low on diversity. These counties are typically cooler, potentially due to most of them being in the northwest or in either of the mountain ranges. These counties have pretty close elections, have older populations, smaller households, and a decently low poverty rate.",
    "7" = "These counties are typically suburbs, mere miles away from metropolitan centers. Some counties, like Cuyahoga, contain cities not already in other clusters. These counties are similar to those from Cluster Two, except their residents are older, a bit more richer, and less impoverished, with slightly less competitive elections favoring the Republicans.",
    "8" = "These agricultural centers in Texas, Oklahoma, and a bit further west, are interesting counties with unique characteristics. They’re younger than average, are incredibly sparsely populated, and have a lot of immigrants. They’re not as highly educated, and have around average poverty. They tend to get the least rain of any counties, and are typically highly elevated with little forests.",
    "9" = "These counties across the midwest and Plains regions of the US feature low diversity, colder, flatter land. These counties have high rates of workers in construction compared to the country. They have high labor participation and low unemployment, with a decently low cost of living.",
    "10" = "These counties, such as Montezuma County, CO, or St. Louis County, MN, are freezing counties across the US, ranging from the PNW to Rockies to Upper Midwest to Northeast, these counties are much older than average, feature low diversity, and are relatively wealthy. Unlike other suburban counties, these counties usually have rising populations. These counties feature the most veterans of any cluster as well. These counties don’t get a lot of precipitation, and when they do it’s usually snowfall. Elections in these counties typically favor republicans.",
    "11" = "These counties, mostly in the Great plains region, are extremely rural, with a population density of only 2.8, meaning there are 2.8 people per square mile. These counties are cold and not diverse at all, and are old, but not as old as the counties in cluster ten. These counties have low reliance on SNAP, and extremely low costs of living. They get very little precipitation, have almost no forests, and overwhelmingly vote Republican.",
    "12" = "These counties, across both the Canadian and Mexican borders, have youthful populations, large families, lots of immigrants and diversity, and varying temperatures depending on which side of the country you’re on. These counties vary wildly in their rural-urban split, with some counties like Clark County, NV holding Las Vegas, a metro center, and some being in the sparsely populated regions of Montana. These counties struggle with poverty and rely heavily on SNAP, and have quite competitive elections.",
    "13" = "With just three counties, this cluster is one of the most intriguing of any that any of the methods have spat out. It only contains Kings County, New York County, and Bronx County. Interestingly, Richmond County and Queens County (which contain the city’s other two burroughs), are not included. These counties are unique in that they’re the most densely populated, public transit oriented, diverse populations in the entire country. The hierarchical method decided that Downtown New York was so different from everything else in the country that it warranted its own mini cluster."
  ),
  gmm = list(
    "1" = "A group of pretty diverse, hot, flat counties predominantly in the south, both rural and suburban. Typically votes Republican by a large margin and gets quite a bit of rainfall. These counties have some of the lowest access to the internet.",
    "2" = "A group of counties with cool climate and close to either the coast or big bodies of water inland with a rugged landscape. Higher education is more prevalent, leading to greater average income and lower percentage of residents relying on SNAP. However, these counties have some of the highest cost of living, with the 2nd highest housing cost burden of any of the 14 clusters. These counties tend to be near urban centers as suburbs or exurbs.",
    "3" = "Older than usual counties across the inland mountainous areas of the eastern US. These counties typically have lower diversity than usual, more high school only educated residents, a Republican dominated political sphere, and tend to be covered in forest and rain.",
    "4" = "These counties tend to be much younger and more diverse compared to their geographical neighbors as well as slightly hotter and flatter. They surround metropolitan areas such as Milwaukee, Detroit, and Springfield IL and have a lower than average poverty rate.",
    "5" = "A set of predominantly white counties mainly in the Appalachian regions of West Virginia down towards Tennessee, these counties tend to be older, have less adults in the workforce, be covered in forests, and overwhelmingly vote republican. This group of counties has the highest average of people whose highest form of education was high school.",
    "6" = "These counties have the lowest population stability, losing residents more than any other cluster. These counties tend to have diversity rates that match cities, owing to their high population of POC as only 2% of the population is foreign born. These counties have the highest poverty rate and SNAP percentage in the country, and some of the lowest average income. Homes are worth very little here, and only on average 73% of people have access to the internet. They tend to be pretty competitive politically, and are the flattest, hottest, and rainiest counties in the country.",
    "7" = "These counties are almost all located in Illinois, Iowa, Wisconsin, and Minnesota. They are cold and not very densely populated, owing to their massive size, not very diverse, and are almost as lacking of forested areas as cities, owing to their agricultural nature. They have the smallest household sizes of anyone in the country, and vote Republican.",
    "8" = "As the name implies, these counties are mainly in the suburbs of metropolitan areas, and smaller metropolitan areas themselves, especially in the midwest and east coast. Lots of eastern PA and New Jersey fall into this cluster, and they have extremely high population density for a non urban group of counties. Its largest counties include cities like Fort Worth and Columbus and its surrounding areas. These counties are young and diverse and have competitive elections on average.",
    "9" = "These counties in the west have the lowest population density of any county group in the country with just over 9 people per sq. mile on average here. These counties tend to be older, the most rural by RUCC codes of any county cluster, and a larger than average percentage of the workforce in agriculture. They also feature rugged terrain, high elevation, and extremely Republican favored elections.",
    "10" = "This cluster features a denser population with counties such as Miami-Dade, Kings County (NYC), and others. The cities in this cluster tend to be not growing in population that fast, if at all, and tend to be younger, with higher household sizes, and a less educated population. The poverty rate and SNAP rate in this cluster is double that of the other big metro cluster, with the residents of 10 making much less money. They have more competitive elections, while still predominantly voting democrat. It also includes some outlier counties across the US.",
    "11" = "These counties don’t get a lot of rain, and have interestingly the lowest mean commute time of any cluster. They have the highest agriculture percentage, making sense given their location in the plains. They have a very low unemployment rate and some of the most affordable communities in the county and vote overwhelmingly Republican.",
    "12" = "These counties have low, but not super low, population sizes, super low diversity, and tend to be less educated. Despite this, they have pretty low income inequality compared to others, and rank pretty middle of the road in every other metric. They’re flat and vote Republican, much like other small population county clusters.",
    "13" = "This cluster is much richer than the other urban counties, containing counties like LA County, Maricopa County, and Harris County. Their populations tend to be less impoverished, growing, and they are more populated on average (potentially owing to the fact the other cluster, which contains some outliers). They’re the most diverse group of counties in the country, are more educated, less impoverished, and have higher costs of living. Overall, these counties are very similar, with a few key differences.",
    "14" = "These counties are the oldest counties in the country, and mainly are either in the northern stretches of the Upper Midwest, or the most southern stretches on the Gulf Coast. These counties are flat, rainy/snowy, and either extremely cold or hot, and covered in water, whether the Gulf Coast or Great Lakes."
  )
)

get_cluster_description <- function(method, cluster_id){
  key <- as.character(cluster_id)
  text <- cluster_descriptions[[method]][[key]]
  if (is.null(text)) "Description coming soon." else text
}

cluster_detailed_profile <- function(
    profile,
    size,
    largest_counties,
    description,
    back_id,
    county_data,
    plot_output_id,
    variables = cluster_characteristic_vars
){
  div(
    class = "cluster-detail-panel",
    
    h2(profile$cluster_name),
    
    p(
      class = "cluster-detail-description",
      description
    ),
    
    div(
      class = "cluster-detail-summary",
      div(
        class = "cluster-detail-stat",
        span("Counties"),
        strong(scales::comma(size))
      ),
      div(
        class = "cluster-detail-stat",
        span("Representative County"),
        strong(profile$rep_name)
      )
    ),
    
    h3("Largest Counties"),
    tags$ul(
      lapply(largest_counties$county_name, tags$li)
    ),
    
    h3("Cluster Profile"),
    div(class = "card", plotOutput(plot_output_id, height = "350px")),
    
    h3("Key Variables"),
    cluster_key_variable_table(profile, county_data, variables),
    
    actionButton(
      inputId = back_id,
      label = "← Back to All Clusters",
      class = "county-action-button"
    )
  )
}