# Variable Explorer

plot_variable_map <- function(county_data, variable, title, subtitle, label = NULL){
  ggplot(county_data) +
    geom_sf(
      aes(fill = .data[[variable]]),
      color = NA) +
    scale_fill_viridis_c(
      option = "plasma",
      na.value = "grey90",
      name = label) +
    labs(
      title = title,
      subtitle = subtitle) +
    theme_void() +
    theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = .5),
      plot.subtitle = element_text(
        size = 12,
        hjust = .5),
      legend.position = "right")
}

plot_variable_distribution <- function(county_data, variable, title){
  ggplot(
    county_data,
    aes(x = .data[[variable]])) +
    geom_histogram(
      bins = 40,
      fill = "#3C8DAD",
      color = "black") +
    labs(
      title = title,
      x = NULL,
      y = "Number of Counties") +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(
        size = 15,
        face = "bold",
        hjust = .5),
      plot.subtitle = element_text(
        size = 12,
        hjust = .5),
      axis.text.y = element_text(size = 10),
      legend.position = "none",
      panel.grid = element_blank())
}

plot_variable_boxplot <- function(county_data, variable, title){
  
  county_data |>
    st_drop_geometry() |>
    ggplot(
      aes(
        y = .data[[variable]]
      )) +
    geom_boxplot(
      fill = "steelblue",
      alpha = .7) +
    coord_flip() + 
    labs(
      title = title,
      y = NULL,
      x = NULL) +
    theme_classic(base_size = 14)
}

# County Comparison
county_comparison_plot <- function(county_data, county1, county2, params, comparison_variables){
  variables <- comparison_variables
  comparison_data <- county_data |>
    st_drop_geometry() |>
    filter(GEOID %in% c(county1, county2)) |>
    mutate(voter_turnout = as.numeric(voter_turnout)) |> 
    select(
      GEOID,
      county_name,
      all_of(variables))
  scaled <- comparison_data |>
    select(all_of(variables)) |>
    scale(
      center = params$center[variables],
      scale = params$scale[variables]
    )
  
  weighted <- sweep(
    scaled,
    2,
    params$weights[variables],
    "*"
  )
  weighted <- as.data.frame(weighted)
  weighted$county_name <- comparison_data$county_name
  weighted$county_name <- comparison_data$county_name
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
    voter_turnout = "Voter Turnout"
  )
  plot_data <- weighted |>
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable",
      values_to = "scaled_value"
    ) |>
    mutate(
      variable = variable_labels[variable]
    )
  
  ggplot(
    plot_data,
    aes(
      x = scaled_value,
      y = reorder(variable, scaled_value),
      fill = county_name)) +
    geom_col(
      position = "dodge", color = "black") +
    labs(
      title = "County Comparison on Key Features",
      subtitle = "Variables standardized using similarity model parameters",
      x = "Standardized Value",
      y = NULL,
      fill = "County"
    ) +
    scale_fill_manual(
      values = c(
        "red",
        "blue")) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = .5
      ),
      plot.subtitle = element_text(
        hjust = .5
      ),
      axis.text.y = element_text(
        size = 10, face = "italic"
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Clustering

plot_cluster_map <- function(county_data, cluster_variable, cluster_types, title, subtitle){
  cluster_colors <- cluster_types |>
    select(cluster_name, cluster_color) |>
    tibble::deframe()
  ggplot(county_data) +
    geom_sf(
      aes(fill = .data[[cluster_variable]]),
      color = NA) +
    scale_fill_manual(
      values = cluster_colors,
      na.value = "black") +
    labs(
      title = title,
      subtitle = subtitle,
      fill = NULL) +
    theme_void() +
    theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = .5),
      plot.subtitle = element_text(
        size = 12,
        hjust = .5),
      legend.position = "right",
      legend.text = element_text(
        size = 12, 
        face = "italic",
        color = "black"
      ),
      legend.title = element_blank())
}

plot_cluster_sizes <- function(cluster_profiles, size_variable, cluster_variable, title, subtitle){
  cluster_colors <- setNames(
    cluster_profiles$cluster_color,
    cluster_profiles[[cluster_variable]])
  ggplot(
    cluster_profiles,
    aes(
      x = reorder(.data[[cluster_variable]], .data[[size_variable]]),
      y = .data[[size_variable]],
      fill = .data[[cluster_variable]])) +
    geom_col(
      color = "black",
      width = .7)  +
    scale_fill_manual(
      values = cluster_colors) +
    geom_text(
      aes(label = .data[[size_variable]]),
      hjust = -0.2,
      size = 4.2) +
    coord_flip() +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Number of Counties") +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(
        size = 15,
        face = "bold",
        hjust = .5),
      plot.subtitle = element_text(
        size = 12,
        hjust = .5),
      axis.text.y = element_text(
        size = 10),
      legend.position = "none")
}

