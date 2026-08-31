library(shiny)
library(splines)

stat_card <- function(label, value, rank = NULL){
  div(
    class = "stat-card",
    div(class = "stat-label", label),
    div(class = "stat-value", value),
    if (!is.null(rank)) div(class = "stat-rank", rank)
  )
}

county_explorer_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Welcome to the County Explorer!"),
    fluidRow(
      column(
        3,
        selectizeInput(
          ns("county"),
          "Select a County",
          choices = NULL,
          selected = character(0),
          options = list(
            placeholder = "Search for a county..."))),
      column(
        5,
        h4("COTD"),
        actionButton(
          ns("county_of_day"),
          "⭐ Explore Today's County",
          class = "county-action-button"
        )
      ),
      column(
        4,
        h4("Surprise Me!"),
        actionButton(
          ns("random_county"),
          "🎲 Choose a Random County",
          class = "random-county-button",
          width = "100%"))), 
    hr(),
    h2(textOutput(ns("county_title"))),
    uiOutput(ns("summary_cards")),
    div(
      style = "margin-bottom: 30px;",
      plotOutput(
        ns("county_comparison"),
        height = "500px",
        width = "100%")),
    div(
      style = "margin-bottom: 30px;",
      plotOutput(
        ns("uniqueness_distribution"),
        height = "500px",
        width = "100%")),
    hr(),
    conditionalPanel(
      condition = sprintf("input['%s'] != ''", ns("county")),
      h3(class = "county-section-title", "Demographics"),
      uiOutput(ns("demographics")),
      h3(class = "county-section-title", "Economy"),
      uiOutput(ns("economy")),
      h3(class = "county-section-title", "Politics"),
      uiOutput(ns("politics")),
      h3(class = "county-section-title", "Geography and Climate"),
      uiOutput(ns("geography")),
      hr(),
      h3(class = "county-section-title", "Similar and Different Counties"),
      uiOutput(ns("similar_counties"))),
  )
}


county_explorer_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      selected_county <- reactive({
        req(input$county)
        county |>
          mutate(GEOID = as.character(GEOID)) |>
          filter(GEOID == input$county)
      })
      county_of_day <- reactive({
        today <- as.Date(Sys.time(), tz = "America/Chicago")
        day_number <- as.integer(today)
        county$GEOID[
          (day_number %% nrow(county)) + 1
        ]
      })
      observeEvent(TRUE, {
        updateSelectizeInput(
          session,
          "county",
          choices = setNames(
            county$GEOID,
            county$county_name
          ),
          server = FALSE,
          selected = ""
        )
      }, once = TRUE)
      observeEvent(input$county_of_day, {
        day_number <- as.integer(Sys.Date())
        county_index <- (day_number %% nrow(county)) + 1
        county_of_day_id <- county$GEOID[county_index]
        updateSelectizeInput(
          session,
          "county",
          selected = county_of_day_id
        )
      })
      observeEvent(input$random_county, {
        random_county <- sample(county$GEOID, 1)
        
        updateSelectizeInput(
          session,
          "county",
          selected = random_county
        )
      })
      output$county_title <- renderText({
        req(selected_county())
        paste0(selected_county()$county_name)
      })
      output$summary_cards <- renderUI({
        req(selected_county())
        selected <- selected_county()
        fluidRow(
          column(
            4,
            div(
              class = "county-summary-card county-summary-population",
              div(
                class = "county-summary-label",
                "POPULATION"),
              div(
                class = "county-summary-value",
                scales::comma(selected$total_population)),
              div(
                class = "county-summary-rank",
                top_percent_label(
                  selected$total_population,
                  county$total_population)))),
          column(
            4,
            div(
              class = "county-summary-card county-summary-income",
              div(
                class = "county-summary-label",
                "MEDIAN HOUSEHOLD INCOME"),
              div(
                class = "county-summary-value",
                scales::dollar(selected$median_household_income)),
              div(
                class = "county-summary-rank",
                top_percent_label(
                  selected$median_household_income,
                  county$median_household_income)))),
          column(
            4,
            div(
              class = "county-summary-card county-summary-uniqueness",
              div(
                class = "county-summary-label",
                "UNIQUENESS"),
              div(
                class = "county-summary-value",
                round(selected$uniqueness_score, 1)),
              div(
                class = "county-summary-rank",
                top_percent_label(
                  selected$uniqueness_score,
                  county$uniqueness_score)))),
          column(
            4,
            div(
              class = "county-summary-card county-summary-pca",
              div(
                class = "county-summary-label",
                "PCA CLUSTER"),
              div(
                class = "county-summary-value county-summary-cluster-value",
                selected$pca_cluster_name))),
          column(
            4,
            div(
              class = "county-summary-card county-summary-hc",
              div(
                class = "county-summary-label",
                "HIERARCHICAL CLUSTER"),
              div(
                class = "county-summary-value county-summary-cluster-value",
                selected$hc_cluster_name))),
          column(
            4,
            div(
              class = "county-summary-card county-summary-gmm",
              div(
                class = "county-summary-label",
                "GMM CLUSTER"),
              div(
                class = "county-summary-value county-summary-cluster-value",
                selected$gmm_cluster_name))))
      })
      output$county_comparison <- renderPlot({
        req(selected_county())
        selected <- selected_county()
        comparison <- tibble(
          variable = c(
            "Income",
            "Education",
            "Population Density",
            "Diversity",
            "Internet Access",
            "Housing Value",
            "Employment",
            "Poverty"
          ),
          percentile = c(
            higher_better_percentile(
              selected$median_household_income,
              county$median_household_income),
            higher_better_percentile(
              selected$college_grad_pct,
              county$college_grad_pct),
            higher_better_percentile(
              selected$population_density,
              county$population_density),
            higher_better_percentile(
              selected$diversity_index,
              county$diversity_index),
            higher_better_percentile(
              selected$internet_access_pct,
              county$internet_access_pct),
            higher_better_percentile(
              selected$median_home_value,
              county$median_home_value),
            higher_better_percentile(
              selected$labor_participation_rate,
              county$labor_participation_rate),
            100 - higher_better_percentile(
              selected$poverty_rate,
              county$poverty_rate)))
        ggplot(
          comparison,
          aes(
            x = reorder(variable, percentile),
            y = percentile)) +
          geom_col(fill = "#2C6E9E", color = "#16324F") +
          coord_flip() +
          scale_y_continuous(
            limits = c(0,100)) +
          labs(
            title = paste0(
              selected$county_name,
              " Compared to US Counties"),
            subtitle = "Percentile rank among all US counties",
            x = NULL,
            y = "Percentile") +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 18, face = "bold", hjust = .5, color = "#16324F"),
            plot.subtitle = element_text(size = 12, hjust = .5, color = "#6B7680"),
            axis.title = element_text(size = 10, face = "bold", color = "#16324F"),
            axis.title.y = element_text(size = 10, face = "bold", color = "#16324F"),
            axis.text = element_text(size = 12, face = "italic", color = "#16324F"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          )
      })
      output$uniqueness_distribution <- renderPlot({
        req(selected_county())
        selected <- selected_county()
        score_range <- range(county$uniqueness_score, na.rm = TRUE)
        relative_position <- (selected$uniqueness_score - score_range[1]) / diff(score_range)
        label_hjust <- if (relative_position > 0.8) 1.05 else -0.05
        ggplot(
          county,
          aes(x = uniqueness_score)) +
          geom_histogram(
            bins = 40,
            fill = "#3F8577",
            color = "#16324F",
            alpha = 0.75) +
          geom_vline(
            xintercept = selected$uniqueness_score,
            linetype = "dashed",
            linewidth = 1,
            color = "#C98A3E") +
          annotate(
            "text",
            x = selected$uniqueness_score,
            y = Inf,
            label = paste0(
              selected$county_name,
              "\n\t",
              round(selected$uniqueness_score, 2)),
            vjust = 2,
            hjust = label_hjust,
            size = 5,
            fontface = "bold",
            color = "#16324F") +
          labs(
            title = "How Unique Is This County?",
            subtitle = "Compared to all US counties",
            x = "Uniqueness Score",
            y = "Number of Counties") +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 18, face = "bold", hjust = .5, color = "#16324F"),
            plot.subtitle = element_text(size = 12, hjust = .5, color = "#6B7680"),
            axis.title = element_text(size = 10, face = "bold", color = "#16324F"),
            axis.title.y = element_text(size = 10, face = "bold", color = "#16324F"),
            axis.text = element_text(size = 12, face = "italic", color = "#16324F"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          )
      })
      output$demographics <- renderUI({
        req(selected_county())
        selected <- selected_county()
        tagList(
          fluidRow(
            column(
              3,
              stat_card(
                "Population Density",
                paste0(round(selected$population_density, 1), "/ sq. mi"),
                top_percent_label(
                  selected$population_density,
                  county$population_density))),
            column(
              3,
              stat_card(
                "Population Stability Index",
                round(selected$pop_stability_index, 2),
                top_percent_label(
                  selected$pop_stability_index,
                  county$pop_stability_index))),
            column(
              3,
              stat_card(
                "Median Age",
                round(selected$median_age, 1),
                top_percent_label(
                  selected$median_age,
                  county$median_age))),
            column(
              3,
              stat_card(
                "Average Household Size",
                round(selected$average_household_size, 2),
                top_percent_label(
                  selected$average_household_size,
                  county$average_household_size)))
          ),
          fluidRow(
            column(
              3,
              stat_card(
                "Diversity Index",
                round(selected$diversity_index, 2),
                top_percent_label(
                  selected$diversity_index,
                  county$diversity_index))),
            column(
              3,
              stat_card(
                "Foreign Born %",
                paste0(round(selected$foreign_born_pct * 100, 2), "%"),
                top_percent_label(
                  selected$foreign_born_pct,
                  county$foreign_born_pct))),
            column(
              3, 
              stat_card(
                "Public School %",
                paste0(round(selected$public_school_pct * 100, 2), "%"),
                top_percent_label(
                  selected$public_school_pct,
                  county$public_school_pct))),
            column(
              3,
              stat_card(
                "High School Highest Education %",
                paste0(round(selected$high_school_pct * 100, 2), "%"),
                top_percent_label(
                  selected$high_school_pct,
                  county$high_school_pct)))
          ))
      })
      output$economy <- renderUI({
        req(selected_county())
        selected <- selected_county()
        tagList(
          fluidRow(
            column(
              3,
              stat_card(
                "Median Earnings per Person",
                scales::dollar(selected$median_earnings),
                top_percent_label(
                  selected$median_earnings,
                  county$median_earnings))),
            column(
              3,
              stat_card(
                "5 Year Income Growth %",
                paste0(round(selected$income_growth * 100, 2), "%"),
                top_percent_label(
                  selected$income_growth,
                  county$income_growth))),
            column(
              3,
              stat_card(
                "Median Home Value",
                scales::dollar(selected$median_home_value),
                top_percent_label(
                  selected$median_home_value,
                  county$median_home_value))),
            column(
              3,
              stat_card(
                "% of Households with Internet",
                paste0(round(selected$internet_access_pct * 100, 2), "%"),
                top_percent_label(
                  selected$internet_access_pct,
                  county$internet_access_pct)))
          ),
          fluidRow(
            column(
              3,
              stat_card(
                "Poverty Rate",
                paste0(round(selected$poverty_rate * 100, 2), "%"),
                top_percent_label(
                  selected$poverty_rate,
                  county$poverty_rate,
                  higher_is_better = FALSE))),
            column(
              3,
              stat_card(
                "Gini Index",
                round(selected$gini_index, 2),
                top_percent_label(
                  selected$gini_index,
                  county$gini_index,
                  higher_is_better = FALSE))),
            column(
              3,
              stat_card(
                "Residents on SNAP %",
                paste0(round(selected$snap_pct * 100, 2), "%"),
                top_percent_label(
                  selected$snap_pct,
                  county$snap_pct,
                  higher_is_better = FALSE))),
            column(
              3,
              stat_card(
                "Housing Cost Burden %",
                paste0(round(selected$housing_cost_burden_pct * 100, 2), "%"),
                top_percent_label(
                  selected$housing_cost_burden_pct,
                  county$housing_cost_burden_pct,
                  higher_is_better = FALSE)))
          ),
          fluidRow(
            column(
              3,
              stat_card(
                "Unemployment Rate",
                paste0(round(selected$unemployment_rate * 100, 2), "%"),
                top_percent_label(
                  selected$unemployment_rate,
                  county$unemployment_rate,
                  higher_is_better = FALSE))),
            column(
              3, 
              stat_card(
                "Labor Participation Rate",
                paste0(round(selected$labor_participation_rate * 100, 2), "%"),
                top_percent_label(
                  selected$labor_participation_rate,
                  county$labor_participation_rate))),
            column(
              3,
              stat_card(
                "Mean Commute Time",
                paste0(round(selected$mean_commute_time, 1), " Min."),
                top_percent_label(
                  selected$mean_commute_time,
                  county$mean_commute_time))),
            column(
              3, 
              stat_card(
                "% of Workers Using Public Transit",
                paste0(round(selected$public_transit_pct * 100, 2), "%"),
                top_percent_label(
                  selected$public_transit_pct,
                  county$public_transit_pct)))
          ),
          fluidRow(
            column(
              3,
              stat_card(
                "% of Workers in Agriculture",
                paste0(round(selected$agriculture_pct * 100, 2), "%"),
                top_percent_label(
                  selected$agriculture_pct,
                  county$agriculture_pct))),
            column(
              3,
              stat_card(
                "% of Workers in Government",
                paste0(round(selected$government_pct * 100, 2), "%"),
                top_percent_label(
                  selected$government_pct,
                  county$government_pct))),
            column(
              3,
              stat_card(
                "% of Workers in Education/Health",
                paste0(round(selected$education_healthcare_pct * 100, 2), "%"),
                top_percent_label(
                  selected$education_healthcare_pct,
                  county$education_healthcare_pct))),
            column(
              3,
              stat_card(
                "% of Workers in Construction",
                paste0(round(selected$construction_pct * 100, 2), "%"),
                top_percent_label(
                  selected$construction_pct,
                  county$construction_pct)))
          ))
      })
      output$politics <- renderUI({
        req(selected_county())
        selected <- selected_county()
        fluidRow(
          column(
            3,
            stat_card(
              "Voter Turnout (2020 Election)",
              paste0(round(as.numeric(selected$voter_turnout) * 100, 2), "%"),
              top_percent_label(
                as.numeric(selected$voter_turnout),
                as.numeric(county$voter_turnout)))),
          column(
            3,
            stat_card(
              "Democratic Party Vote Share (2020)",
              paste0(round(selected$dem_vote_share_2020 * 100, 2), "%"))),
          column(
            3,
            stat_card(
              "Party Competitiveness (2020)",
              round(selected$party_competitiveness_2020, 2))),
          column(
            3,
            stat_card(
              "20 Year Democratic Vote Swing",
              paste0(round(selected$dem_swing_2000_2020 * 100, 2), "%")))
        )
      })
      output$geography <- renderUI({
        req(selected_county())
        selected <- selected_county()
        tagList(
          fluidRow(
            column(
              4,
              stat_card(
                "Land Area",
                paste0(scales::comma(round(selected$land_area_sq_miles,0)), " sq. mi"),
                top_percent_label(
                  selected$land_area_sq_miles,
                  county$land_area_sq_miles))),
            column(
              4,
              stat_card(
                "Mean Elevation",
                paste0(scales::comma(round(selected$mean_elevation,0)), " ft"),
                top_percent_label(
                  selected$mean_elevation,
                  county$mean_elevation))),
            column(
              4,
              stat_card(
                "Terrain Ruggedness",
                paste0(round(selected$terrain_ruggedness, 2), " ft"),
                top_percent_label(
                  selected$terrain_ruggedness,
                  county$terrain_ruggedness)))
          ),
          fluidRow(
            column(
              3,
              stat_card(
                "% of Forest Land",
                paste0(round(selected$forest_coverage_pct, 2), "%"),
                top_percent_label(
                  selected$forest_coverage_pct,
                  county$forest_coverage_pct))),
            column(
              3,
              stat_card(
                "% of County is Water",
                paste0(round(selected$water_coverage_pct, 2), "%"),
                top_percent_label(
                  selected$water_coverage_pct,
                  county$water_coverage_pct))),
            column(
              3,
              stat_card(
                "Mean Temperature",
                paste0(round(selected$mean_temp, 1), "°C"),
                top_percent_label(
                  selected$mean_temp,
                  county$mean_temp))),
            column(
              3,
              stat_card(
                "Avg. Annual Precipitation",
                paste0(scales::comma(round(selected$annual_precip, 1)), " mm."),
                top_percent_label(
                  selected$annual_precip,
                  county$annual_precip)))
          )
        )
      })
      output$similar_counties <- renderUI({
        req(selected_county())
        current <- input$county
        similar <- most_similar |>
          filter(source_GEOID == current) |>
          left_join(
            county |>
              select(GEOID, county_name),
            by = c("comparison_GEOID" = "GEOID")
          ) |>
          mutate(
            similarity = paste0(round(100 - distance, 1), "% Similar")
          )
        different <- least_similar |>
          filter(source_GEOID == current) |>
          left_join(
            county |>
              select(GEOID, county_name),
            by = c("comparison_GEOID" = "GEOID")
          ) |>
          mutate(
            similarity = paste0(round(100 - distance, 1), "% Similar")
          )
        county_row <- function(data){
          tagList(
            lapply(seq_len(nrow(data)), function(i){
              div(
                class = "similar-county-row",
                div(
                  class = "similar-county-name",
                  tags$a(
                    href = "#",
                    onclick = sprintf(
                      "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                      ns("explore_clicked"),
                      data$comparison_GEOID[i]
                    ),
                    paste0(data$similarity_rank[i], ". ", data$county_name[i])
                  )
                ),
                div(
                  class = "similar-county-score",
                  data$similarity[i]
                )
              )
            })
          )
        }
        tagList(
          h4("Most Similar Counties"),
          div(class = "similar-counties-list", county_row(similar)),
          hr(),
          h4("Most Different Counties"),
          div(class = "similar-counties-list", county_row(different))
        )
      })
      observeEvent(input$explore_clicked, {
        updateSelectizeInput(
          session,
          "county",
          selected = input$explore_clicked
        )
      })
    })
}