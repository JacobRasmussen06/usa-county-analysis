# Variable Explorer

plot_variable_map <- function(county_data, variable, subtitle = "County level distribution"){
  variable_label <- get_label(variable)
  variable_unit <- get_unit(variable)
  display_title <- if (is.na(variable_unit)) variable_label else paste(variable_label, variable_unit)
  ggplot(county_data) +
    geom_sf(
      aes(fill = .data[[variable]]),
      color = NA) +
    scale_fill_viridis_c(
      option = "plasma",
      na.value = "grey90",
      name = display_title) +
    labs(
      title = display_title,
      subtitle = subtitle) +
    theme_void() +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = .5, color = "#16324F"),
      plot.subtitle = element_text(size = 12, hjust = .5, color = "#6B7680"),
      legend.position = "right")
}

plot_variable_distribution <- function(county_data, variable){
  variable_label <- get_label(variable)
  variable_unit <- get_unit(variable)
  display_title <- if (is.na(variable_unit)) variable_label else paste(variable_label, variable_unit)
  ggplot(
    county_data,
    aes(x = .data[[variable]])) +
    geom_histogram(
      bins = 40,
      fill = "#2C6E9E",
      color = "#16324F",
      alpha = 0.85) +
    labs(
      title = paste("Distribution of", display_title),
      x = NULL,
      y = "Number of Counties") +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = .5, color = "#16324F"),
      axis.text = element_text(size = 10, color = "#16324F"),
      legend.position = "none",
      panel.grid = element_blank())
}

plot_variable_boxplot <- function(county_data, variable){
  variable_label <- get_label(variable)
  variable_unit <- get_unit(variable)
  display_title <- if (is.na(variable_unit)) variable_label else paste(variable_label, variable_unit)
  county_data |>
    st_drop_geometry() |>
    ggplot(
      aes(
        y = .data[[variable]]
      )) +
    geom_boxplot(
      fill = "#2C6E9E",
      color = "#16324F",
      alpha = .75) +
    coord_flip() + 
    labs(
      title = paste(display_title, "Distribution"),
      y = NULL,
      x = NULL) +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = .5, color = "#16324F"),
      axis.text = element_text(color = "#16324F"))
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

plot_cluster_heatmap <- function(cluster_profiles, cluster_variable = "cluster13", title, subtitle) {
  
  heatmap_data <- cluster_profiles |>
    select(
      all_of(cluster_variable),
      population_density,
      diversity_index,
      mean_temp,
      poverty_rate,
      median_household_income,
      median_age,
      college_grad_pct
    ) |>
    tidyr::pivot_longer(
      cols = -all_of(cluster_variable),
      names_to = "variable",
      values_to = "value"
    ) |>
    group_by(variable) |>
    mutate(
      value = as.numeric(scale(value))
    ) |>
    ungroup()
  
  ggplot(
    heatmap_data,
    aes(
      x = variable,
      y = .data[[cluster_variable]],
      fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "#2C6E9E",
      mid = "white",
      high = "#C98A3E",
      midpoint = 0) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      fill = "Relative\nvalue") +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(
        size = 15,
        face = "bold",
        hjust = .5),
      plot.subtitle = element_text(
        size = 11,
        hjust = .5),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1),
      panel.grid = element_blank())
}

plot_gmm_uncertainty <- function(
    cluster_profiles,
    cluster_variable = "cluster",
    uncertainty_variable = "gmm_uncertainty",
    title = "Average GMM Uncertainty by Cluster",
    subtitle = "Lower uncertainty indicates more confident cluster assignments"
) {
  cluster_colors <- setNames(
    cluster_profiles$cluster_color,
    cluster_profiles$cluster_name)
  ggplot(
    cluster_profiles,
    aes(
      x = reorder(
        cluster_name,
        .data[[uncertainty_variable]]),
      y = .data[[uncertainty_variable]],
      fill = cluster_name)) +
    geom_col(
      color = "black",
      width = 0.7) +
    geom_text(
      aes(
        label = round(.data[[uncertainty_variable]], 2)),
      hjust = -0.12,
      size = 4.2) +
    scale_fill_manual(
      values = cluster_colors) +
    coord_flip() +
    labs(
      title = title,
      subtitle = subtitle,
      x = "GMM Cluster",
      y = "Average Uncertainty") +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(
        size = 15,
        face = "bold",
        hjust = 0.5),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5),
      axis.text.y = element_text(
        size = 10),
      legend.position = "none")
}

plot_cluster_profile <- function(profile, county_data, variables = cluster_characteristic_vars, cluster_color){
  z_data <- purrr::map_dfr(variables, function(var){
    z <- (profile[[var]] - mean(county_data[[var]], na.rm = TRUE)) / sd(county_data[[var]], na.rm = TRUE)
    tibble::tibble(variable = get_label(var), z_score = as.numeric(z))
  })
  
  ggplot(z_data, aes(x = reorder(variable, z_score), y = z_score)) +
    geom_col(fill = cluster_color, color = "#16324F", width = 0.65) +
    geom_hline(yintercept = 0, color = "#6B7680", linewidth = 0.6) +
    coord_flip() +
    labs(
      title = "Cluster Profile vs. National Average",
      subtitle = "Standard deviations above/below the average U.S. county",
      x = NULL,
      y = "Standard deviations from average"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = .5, color = "#16324F"),
      plot.subtitle = element_text(hjust = .5, color = "#6B7680", size = 11),
      axis.text = element_text(color = "#16324F"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}