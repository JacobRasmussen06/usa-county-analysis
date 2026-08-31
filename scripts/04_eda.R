#############################################################################
#
# 04_eda.R
#
# Purpose:
#  Perform exploratory data analysis on the county dataset, learning
# about the features and how the dataset operates.
#
# Outputs:
# - All visualizations in figures/eda folder
#
#############################################################################

# Load required packages
library(tidyverse)
library(sf)
library(corrplot)
library(viridis)

#############################################################################
# Setup
#############################################################################

county <- readRDS("data/finished/county_dataset.rds")

numeric_vars <- county |>
  st_drop_geometry() |>
  select(where(is.numeric))

#############################################################################
# Explore Variables With Histograms
#############################################################################

# Population Density
popdens_graph <- ggplot(county,
       aes(x = log10(population_density))) +
  geom_histogram(bins = 35, color = "white", fill = "steelblue") +
  labs(
    title = "Distribution of Population Density per County",
    subtitle = "Population density displayed on a log10 scale",
    x = "log(Population Density)",
    caption = "Source: U.S. Census Bureau ACS",
    y = "Number of Counties"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    plot.caption = element_text(size = 8)
  )
ggsave(
  "figures/eda/histograms/populationdensity_histogram.png",
  popdens_graph,
  width = 8,
  height = 5,
  dpi = 300
)

# Median Household Income
hh_income_graph <- ggplot(county, aes(x = median_household_income)) +
  geom_histogram(bins = 35, fill = "gold", color = "black") +
  labs(
    title = "Distribution of Median Household Income by County",
    subtitle = "The data is roughly normal centered around $60000 as a median household income per county",
    x = "Median Household Income ($)",
    y = "Number of Counties",
    caption = "Source: U.S Census Bureau ACS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    plot.caption = element_text(size = 8)
  )

ggsave(
  "figures/eda/histograms/median_income_histogram.png",
  hh_income_graph,
  width = 8,
  height = 5,
  dpi = 300
)

# Housing Cost Burden
hcb_graph <- ggplot(county, aes(x = housing_cost_burden_pct)) +
  geom_histogram(bins = 35, fill = "#E67E22", color = "white") +
  labs(
    title = "Distribution of Housing Cost Burden per County",
    subtitle = "In lots of counties, renters spend a large part of their budget on housing.",
    x = "Share of Renters Spending 30%+ of Income on Housing",
    y = "Number of Counties",
    caption = "Source: U.S Census Bureau ACS"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    plot.caption = element_text(size = 8)
  )

ggsave(
  "figures/eda/histograms/housing_cost_burden_histogram.png",
  hcb_graph,
  width = 8,
  height = 5,
  dpi = 300
)

# Mean Commute Time
mct_data <- ggplot(county, aes(x = mean_commute_time)) +
  geom_histogram(bins = 35, fill = "#8E44AD", color = "white") +
  labs(
    title = "Distribution of Mean Commute Time by County",
    subtitle = "The data is approximately normally distributed around mean 25.",
    x = "Mean Commute Time (Minutes)",
    y = "Number of Counties",
    caption = "Source: U.S. Census Bureau ACs"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    plot.caption = element_text(size = 8)
  )

ggsave(
  "figures/eda/histograms/commute_time_histogram.png",
  mct_data,
  width = 8,
  height = 5,
  dpi = 300
)

# Party competitiveness
party_comp_graph <- ggplot(county, aes(x = party_competitiveness_2020)) +
  geom_histogram(bins = 35, fill = "#D1495B", color = "white") +
  labs(
    title = "Distribution of Political Competitiveness by County",
    subtitle = "Most counties are not very competitive.",
    x = "Political Competitiveness",
    y = "Number of Counties",
    caption = "Source: MIT Election Lab\nHigher values indicate closer Democratic-Republican vote shares"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    plot.caption = element_text(size = 8)
  )

ggsave(
  "figures/eda/histograms/party_competitiveness_histogram.png",
  party_comp_graph,
  width = 8,
  height = 5,
  dpi = 300
)

# Gini index
gini_graph <- ggplot(county, aes(x = gini_index)) +
  geom_histogram(
    bins = 35,
    fill = "#C77CFF",
    color = "white"
  ) +
  labs(
    title = "Distribution of County Income Inequality",
    subtitle = "Measured using the Gini Index (closer to 0 = more equal)",
    x = "Gini Index",
    y = "Number of Counties",
    caption = "Source: U.S. Census Bureau ACS"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    plot.caption = element_text(size = 8)
  )

ggsave(
  "figures/eda/histograms/gini_index_histogram.png",
  gini_graph,
  width = 8,
  height = 5,
  dpi = 300
)

# Forest coverage
forest_graph <- ggplot(county, aes(x = forest_coverage_pct)) +
  geom_histogram(
    bins = 35,
    fill = "#4CAF50",
    color = "white"
  ) +
  labs(
    title = "Distribution of Forested Land Per County",
    subtitle = "The distribution of forested land is quite unique, with no real difference other than the extremes.",
    x = "% of Land is Forest",
    y = "Number of Counties",
    caption = "Source: FIA County Estimates"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 11, hjust = .5),
    plot.caption = element_text(size = 8)
  )

ggsave(
  "figures/eda/histograms/forested_land_histogram.png",
  forest_graph,
  width = 8,
  height = 5,
  dpi = 300
)

# RUCC
rucc_graph <- ggplot(county,
       aes(x = factor(RUCC_2023, levels = 1:9))) +
  geom_bar(fill = "#3C8DAD", color = "black") +
  labs(
    title = "Distribution of Rural-Urban Continuum Codes",
    subtitle = "1 = Most Urban, 9 = Most Rural",
    x = "RUCC Code",
    y = "Number of Counties",
    caption = "Source: USDA Rural-Urban Continuum Codes"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    plot.caption = element_text(size = 8),
    panel.grid = element_blank()
  )

ggsave(
  "figures/eda/histograms/rucc_distribution.png",
  rucc_graph,
  width = 8,
  height = 5,
  dpi = 300
)

#############################################################################
# Explore Correlation with Heatmaps
#############################################################################

cor_matrix <- cor(
  numeric_vars,
  use = "pairwise.complete.obs"
)

cor_table <- as.data.frame(as.table(cor_matrix)) |>
  rename(
    Variable1 = Var1,
    Variable2 = Var2,
    Correlation = Freq
  ) |>
  filter(
    Variable1 != Variable2
  ) |>
  mutate(
    pair = map2_chr(
      Variable1,
      Variable2,
      ~paste(sort(c(.x, .y)), collapse = "_")
    )
  ) |>
  distinct(pair, .keep_all = TRUE) |>
  arrange(desc(abs(Correlation)))

# Helper function that creates every correlation plot as each correlation plot is nearly identical in setup
make_corrplot <- function(data, title, filename){
  cor_mat <- cor(
    data,
    use = "pairwise.complete.obs"
  )
  png(
    paste0("figures/eda/heatmaps/", filename, ".png"),
    width = 1800,
    height = 1800,
    res = 250
  )
  corrplot(
    cor_mat,
    method = "color",
    type = "upper",
    diag = FALSE,
    order = "hclust",
    tl.col = "black",
    tl.cex = .8,
    addCoef.col = "black",
    number.cex = .75,
    col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200),
    mar = c(0,0,2,0),
    title = title
  )
  dev.off()
}

# Demographic correlation
demo_vars <- numeric_vars |>
  select(
    population_density,
    median_age,
    under_18_pct,
    over_65_pct,
    average_household_size,
    married_pct,
    diversity_index,
    foreign_born_pct,
    veteran_pct,
    disability_rate,
    population_growth_5yr,
    pop_stability_index
  )
make_corrplot(demo_vars, "Demographic Variables Correlation", "corr_demographics")

# Economic correlation
econ <- numeric_vars |>
  select(
    high_school_pct,
    some_college_pct,
    college_grad_pct,
    masters_or_higher_pct,
    median_household_income,
    income_growth,
    median_earnings,
    poverty_rate,
    snap_pct,
    gini_index,
    unemployment_rate,
    labor_participation_rate
  )

make_corrplot(econ, "Socioeconomic Variables Correlation", "corr_economics")

# Employment correlation
employment <- numeric_vars |>
  select(
    agriculture_pct,
    construction_pct,
    manufacturing_pct,
    arts_tourism_pct,
    finance_pct,
    information_pct,
    retail_pct,
    education_healthcare_pct,
    government_pct,
    technical_pct
  )

make_corrplot(employment, "Employment Sector Correlations", "corr_employment")

# Transportation correlation
transport <- numeric_vars |>
  select(
    mean_commute_time,
    public_transit_pct,
    walk_bike_to_work_pct,
    drive_alone_pct,
    work_from_home_pct,
    internet_access_pct
  )

make_corrplot(transport, "Transportation Variable Correlations", "corr_transport")

# Geographical correlation
geo <- numeric_vars |>
  select(
    land_area_sq_miles,
    population_density,
    mean_elevation,
    terrain_ruggedness,
    water_coverage_pct,
    distance_to_coast_miles,
    forest_coverage_pct,
    mean_temp,
    annual_precip
  )

make_corrplot(geo, "Geographic Variable Correlations", "corr_geography")

# Political Correlation
politics <- county |>
  st_drop_geometry() |> 
  select(
    homicide_rate,
    suicide_rate,
    firearm_deaths_rate,
    voter_turnout,
    dem_vote_share_2020,
    party_competitiveness_2020,
    dem_swing_2000_2020,
    comp_swing_2000_2020
  )
politics <- politics |>
  mutate(
    across(
      c(
        homicide_rate,
        suicide_rate,
        firearm_deaths_rate,
        voter_turnout
      ),
      readr::parse_number
    )
  )
make_corrplot(politics, "Politics & Health Variable Correlations", "corr_politics")

# Top 20 Correlation
top_correlations <- cor_table |>
  filter(Variable1 != Variable2) |>
  mutate(
    Variable1 = as.character(Variable1),
    Variable2 = as.character(Variable2),
    pair = paste(
      pmin(Variable1, Variable2),
      pmax(Variable1, Variable2),
      sep = " - "
    ),
    abs_correlation = abs(Correlation)
  ) |>
  distinct(pair, .keep_all = TRUE) |>
  arrange(desc(abs_correlation)) |>
  slice_head(n = 20)

top_corr_plot <- ggplot(
  top_correlations,
  aes(
    x = reorder(pair, correlation),
    y = correlation,
    fill = correlation
  )
) +
  geom_col(
    color = "black",
    linewidth = 0.2
  ) +
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 0.5
  ) +
  coord_flip() +
  geom_text(
    aes(
      label = round(correlation, 2)
    ),
    hjust = ifelse(top_correlations$correlation > 0, -0.1, 1.1),
    size = 3
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  scale_y_continuous(
    breaks = seq(-1, 1, 0.25),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(
    title = "Some Strong Between County-Level Features",
    subtitle = "Ranked by absolute correlation strength",
    x = NULL,
    y = "Correlation",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = "grey85"
    ),
    plot.title = element_text(
      hjust = 0,
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0,
      size = 11
    ),
    axis.text.y = element_text(
      size = 10
    ),
    legend.position = "right"
  ) 

ggsave(
  "figures/eda/heatmaps/top_20_correlation.png",
  top_corr_plot,
  width = 11,
  height = 5,
  dpi = 300
)

#############################################################################
# Explore the Data Spatially with Maps
#############################################################################

# Median Income Map
med_earn_map <- ggplot(county) +
  geom_sf(
    aes(fill = median_earnings),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::label_dollar(),
    name = "Median\nIncome"
  ) +
  labs(
    title = "U.S. Counties' Median Earnings",
    subtitle = "Median Earnings per person measured",
    caption = "Source: U.S. Census Bureau ACS"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8)
  )
ggsave(
  filename = "figures/eda/maps/median_earnings_map.png",
  plot = med_earn_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Terrain Ruggedness Map
terrain_ruggedness_map <- ggplot(county) +
  geom_sf(
    aes(fill = terrain_ruggedness),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Terrain\nRuggedness"
  ) +
  labs(
    title = "U.S Counties Terrain Ruggedness",
    subtitle = "Ruggedness indicates standard deviation of elevation",
    caption = "Source: USGS Elevation Data"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8)
  )
ggsave(
  filename = "figures/eda/maps/terrain_ruggedness_map.png",
  plot = terrain_ruggedness_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Forest Coverage
forest_cov_map <- ggplot(county) +
  geom_sf(
    aes(fill = forest_coverage_pct),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Forest\nCoverage (%)"
  ) +
  labs(
    title = "Much of the US is Covered in Forest",
    subtitle = "Aside from the Great Plains Region, Most US Counties Have High Forest Coverage",
    caption = "Source: FIA County Estimates"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8)
  )
ggsave(
  filename = "figures/eda/maps/forest_coverage_pct_map.png",
  plot = forest_cov_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Precipitation
precip_map <- ggplot(county) +
  geom_sf(
    aes(fill = annual_precip),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Annual\nPrecipitation (mm)"
  ) +
  labs(
    title = "Precipitation by US County",
    subtitle = "The Pacific Northwest and Tornado Alley areas get the most precipitation",
    caption = "Source: PRISM Climate Data"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8)
  )
ggsave(
  filename = "figures/eda/maps/annual_precipitation_map.png",
  plot = precip_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Poverty Rate
poverty_map <- ggplot(county) +
  geom_sf(
    aes(fill = poverty_rate),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Poverty Rate"
  ) +
  labs(
    title = "Poverty Rate by US County",
    subtitle = "There are many underserved counties across the US, especially in the south.",
    caption = "Source: U.S Census Bureau ACS"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8)
  )
ggsave(
  filename = "figures/eda/maps/poverty_rate_map.png",
  plot = poverty_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Diversity Index
diversity_map <- ggplot(county) +
  geom_sf(
    aes(fill = diversity_index),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Diversity Index"
  ) +
  labs(
    title = "Diversity by US County",
    subtitle = "Southern counties are on average far more diverse than northern counties, except northern cities.",
    caption = "Source: U.S Census Bureau ACS"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8)
  )
ggsave(
  filename = "figures/eda/maps/diversity_index_map.png",
  plot = diversity_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Population Stability Index
psi_map <- ggplot(county) +
  geom_sf(
    aes(fill = pop_stability_index),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Population\nStablity Index"
  ) +
  labs(
    title = "Population Stability Index by US County",
    subtitle = "Regions like the South and Midwest, with high migration patterns, have lower stability.",
    caption = "Source: U.S Census Bureau ACS"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8)
  )

ggsave(
  filename = "figures/eda/maps/pop_stability_index_map.png",
  plot = psi_map,
  width = 10,
  height = 6,
  dpi = 300
)

# RUCC
rucc_map <- ggplot(county) +
  geom_sf(
    aes(fill = RUCC_2023),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "RUCC\nCodes"
  ) +
  labs(
    title = "Rural Urban Continuum Codes by US County",
    subtitle = "There is a clear rural and urban divide in the US.",
    caption = "Source: USDA Rural-Urban Continuum Codes"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8)
  )
ggsave(
  filename = "figures/eda/maps/rucc_map.png",
  plot = rucc_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Internet access
internet_map <- ggplot(county) +
  geom_sf(
    aes(fill = internet_access_pct),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Internet\nAccess"
  ) +
  labs(
    title = "Internet Access by US County",
    subtitle = "Some places in the south and plains areas of the US lack consistent internet.",
    caption = "Source: U.S. Census Bureau ACS"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8)
  )

ggsave(
  filename = "figures/eda/maps/internet_access_map.png",
  plot = internet_map,
  width = 10,
  height = 6,
  dpi = 300
)

# Median Age
median_age_map <- ggplot(county) +
  geom_sf(
    aes(fill = median_age),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Median\nAge"
  ) +
  labs(
    title = "Median Age by US County",
    subtitle = "Rural Counties and Southern Counties tend to be older on average.",
    caption = "Source: U.S. Census Bureau ACS"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = .5),
    plot.subtitle = element_text(size = 12, hjust = .5),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8)
  )

ggsave(
  filename = "figures/eda/maps/median_age_map.png",
  plot = median_age_map,
  width = 10,
  height = 6,
  dpi = 300
)

#############################################################################
# End of Script
#############################################################################
