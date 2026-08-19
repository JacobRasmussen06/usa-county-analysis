library(shiny)
library(splines)

county_explorer_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Welcome to the County Explorer!"),
    selectizeInput(
      ns("county"),
      "Select a County",
      choices = NULL,
      selected = character(0),
      options = list(
        placeholder = "Search for a county..."
      )
    ), 
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
      h3("Demographics"),
      uiOutput(ns("demographics")),
      h3("Economy"),
      uiOutput(ns("economy")),
      h3("Politics"),
      uiOutput(ns("politics")),
      h3("Geography and Climate"),
      uiOutput(ns("geography")),
      hr(),
      h3("Similar and Different Counties"),
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
          wellPanel(
            h4("Population"),
            h3(scales::comma(selected$total_population)),
            span(
              top_percent_label(
                selected$total_population,
                county$total_population),
              style = "font-size: 14px; color: gray;"
            )
          )
        ),
        column(
          4,
          wellPanel(
            h4("Income"),
            h3(scales::dollar(selected$median_household_income)),
            span(
              top_percent_label(
                selected$median_household_income,
                county$median_household_income),
              style = "font-size: 14px; color: gray;"
            )
          )),
        column(
          4,
          wellPanel(
            h4("Uniqueness"),
            h3(round(selected$uniqueness_score,1)),
            span(
              top_percent_label(
                selected$uniqueness_score,
                county$uniqueness_score),
              style = "font-size: 14px; color: gray;"
            )
          )
        ),
        column(
          4,
          wellPanel(
            h4("PCA Cluster"),
            h3(selected$pca_cluster)
          )),
        column(
          4,
          wellPanel(
            h4("Hierarchical Cluster"),
            h3(selected$hc_cluster)
          )),
        column(
          4,
          wellPanel(
            h4("GMM Cluster"),
            h3(selected$gmm_cluster)
          )))
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
        geom_col(fill = "steelblue", color = "black") +
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
          plot.title = element_text(size = 18, face = "bold", hjust = .5),
          plot.subtitle = element_text(size = 12, hjust = .5),
          axis.title = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 12, face = "italic"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
    })
    output$uniqueness_distribution <- renderPlot({
      req(selected_county())
      
      selected <- selected_county()
      
      ggplot(
        county,
        aes(x = uniqueness_score)
      ) +
        geom_histogram(
          bins = 40,
          fill = "#C77CFF",
          color = "black",
          alpha = 0.7
        ) +
        geom_vline(
          xintercept = selected$uniqueness_score,
          linetype = "dashed",
          linewidth = 1
        ) +
        annotate(
          "text",
          x = selected$uniqueness_score,
          y = Inf,
          label = paste0(
            selected$county_name,
            "\n\t",
            round(selected$uniqueness_score, 2)
          ),
          vjust = 2,
          hjust = -0.03,
          size = 5,
          fontface = "bold"
        ) +
        labs(
          title = "How Unique Is This County?",
          subtitle = "Compared to all US counties",
          x = "Uniqueness Score",
          y = "Number of Counties"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", hjust = .5),
          plot.subtitle = element_text(size = 12, hjust = .5),
          axis.title = element_text(size = 10, face = "bold"),
          axis.title.y = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 12, face = "italic"),
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
          wellPanel(
            h5("Population Density"),
            h3(paste0(round(selected$population_density, 1), "/ sq. mi")),
            span(
              top_percent_label(
                selected$population_density,
                county$population_density),
              style = "font-size: 14px; color: gray;"
            )
          )),
        column(
          3,
          wellPanel(
            h5("Population Stability Index"),
            h3(round(selected$pop_stability_index, 2)),
            span(
              top_percent_label(
                selected$pop_stability_index,
                county$pop_stability_index),
              style = "font-size: 14px; color: gray;"
            )
          )),
        column(
          3,
          wellPanel(
            h5("Median Age"),
            h3(round(selected$median_age, 1)),
            span(
              top_percent_label(
                selected$median_age,
                county$median_age),
              style = "font-size: 14px; color: gray;"
            )
          )),
        column(
          3,
          wellPanel(
            h5("Average Household Size"),
            h3(selected$average_household_size),
            span(
              top_percent_label(
                selected$average_household_size,
                county$average_household_size),
              style = "font-size: 14px; color: gray;"
            )
          ))
      ),
      fluidRow(
        column(
          3,
          wellPanel(
            h5("Diversity Index"),
            h3(round(selected$diversity_index, 2)),
            span(
              top_percent_label(
                selected$diversity_index,
                county$diversity_index),
              style = "font-size: 14px; color: gray;"
            )
          )),
        column(
          3,
          wellPanel(
            h5("Foreign Born %"),
            h3(paste0(round(selected$foreign_born_pct * 100, 2), "%")),
            span(
              top_percent_label(
                selected$foreign_born_pct,
                county$foreign_born_pct),
              style = "font-size: 14px; color: gray;"
            )
          )),
        column(
          3, 
          wellPanel(
            h5("Public School %"),
            h3(paste0(round(selected$public_school_pct * 100, 2), "%")),
            span(
              top_percent_label(
                selected$public_school_pct,
                county$public_school_pct),
              style = "font-size: 14px; color: gray;"
            )
          )),
        column(
          3,
          wellPanel(
            h5("High School Highest Education %"),
            h3(paste0(round(selected$high_school_pct * 100, 2), "%")),
            span(
              top_percent_label(
                selected$high_school_pct,
                county$high_school_pct),
              style = "font-size: 14px; color: gray;"
            )
          ))
      ))
    })
    output$economy <- renderUI({
      req(selected_county())
      selected <- selected_county()
      tagList(
        fluidRow(
          column(
            3,
            wellPanel(
              h5("Median Earnings per Person"),
              h3(scales::dollar(selected$median_earnings)),
              span(
                top_percent_label(
                  selected$median_earnings,
                  county$median_earnings),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("5 Year Income Growth %"),
              h3(paste0(round(selected$income_growth * 100, 2), "%")),
              span(
                top_percent_label(
                  selected$income_growth,
                  county$income_growth),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("Median Home Value"),
              h3(scales::dollar(selected$median_home_value)),
              span(
                top_percent_label(
                  selected$median_home_value,
                  county$median_home_value),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("% of Households with Internet"),
              h3(paste0(round(selected$internet_access_pct * 100, 2), "%")),
              span(
                top_percent_label(
                  selected$internet_access_pct,
                  county$internet_access_pct),
                style = "font-size: 14px; color: gray;"
              )
            ))
        ),
        fluidRow(
          column(
            3,
            wellPanel(
              h5("Poverty Rate"),
              h3(paste0(round(selected$poverty_rate * 100, 2), "%")),
              span(
                top_percent_label(
                  selected$poverty_rate,
                  county$poverty_rate,
                  higher_is_better = FALSE),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("Gini Index"),
              h3(round(selected$gini_index, 2)),
              span(
                top_percent_label(
                  selected$gini_index,
                  county$gini_index,
                  higher_is_better = FALSE),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("Residents on SNAP %"),
              h3(paste0(round(selected$snap_pct * 100, 2), "%")),
              span(
                top_percent_label(
                  selected$snap_pct,
                  county$snap_pct,
                  higher_is_better = FALSE),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("Housing Cost Burden %"),
              h3(paste0(round(selected$housing_cost_burden_pct * 100, 2), "%")),
              span(
                top_percent_label(
                  selected$housing_cost_burden_pct,
                  county$housing_cost_burden_pct,
                  higher_is_better = FALSE),
                style = "font-size: 14px; color: gray;"
              )
            ))
        ),
        fluidRow(
          column(
            3,
            wellPanel(
              h5("Unemployment Rate"),
              h3(paste0(round(selected$unemployment_rate * 100, 2), "%")),
              span(
                top_percent_label(
                  selected$unemployment_rate,
                  county$unemployment_rate,
                  higher_is_better = FALSE),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3, 
            wellPanel(
              h5("Labor Participation Rate"),
              h3(paste0(round(selected$labor_participation_rate * 100, 2), "%")),
              span(
                top_percent_label(
                  selected$labor_participation_rate,
                  county$labor_participation_rate),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("Mean Commute Time"),
              h3(paste0(selected$mean_commute_time, " Min.")),
              span(
                top_percent_label(
                  selected$mean_commute_time,
                  county$mean_commute_time),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3, 
            wellPanel(
              h5("% of Workers Using Public Transit"),
              h3(paste0(round(selected$public_transit_pct * 100, 2), "%")),
              span(
                top_percent_label(
                  selected$public_transit_pct,
                  county$public_transit_pct),
                style = "font-size: 14px; color: gray;"
              )
            ))
        ),
        fluidRow(
          column(
            3,
            wellPanel(
              h5("% of Workers in Agriculture"),
              h3(paste0(round(selected$agriculture_pct * 100, 2), "%")),
              span(
                top_percent_label(
                  selected$agriculture_pct,
                  county$agriculture_pct),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("% of Workers in Government"),
              h3(paste0(round(selected$government_pct * 100, 2), "%")),
              span(
                top_percent_label(
                  selected$government_pct,
                  county$government_pct),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("% of Workers in Education/Health"),
              h3(paste0(round(selected$education_healthcare_pct * 100, 2), "%")),
              span(
                top_percent_label(
                  selected$education_healthcare_pct,
                  county$education_healthcare_pct),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("% of Workers in Construction"),
              h3(paste0(round(selected$construction_pct * 100, 2), "%")),
              span(
                top_percent_label(
                  selected$construction_pct,
                  county$construction_pct),
                style = "font-size: 14px; color: gray;"
              )
            ))
        ))
    })
    output$politics <- renderUI({
      req(selected_county())
      selected <- selected_county()
      fluidRow(
        column(
          3,
          wellPanel(
            h5("Voter Turnout (2020 Election)"),
            h3(paste0(round(as.numeric(selected$voter_turnout) * 100, 2), "%")),
            span(
              top_percent_label(
                as.numeric(selected$voter_turnout),
                as.numeric(county$voter_turnout)),
              style = "font-size: 14px; color: gray;"
            )
          )),
        column(
          3,
          wellPanel(
            h5("Democratic Party Vote Share (2020)"),
            h3(paste0(round(selected$dem_vote_share_2020 * 100, 2), "%"))
          )),
        column(
          3,
          wellPanel(
            h5("Party Competitiveness (2020)"),
            h3(round(selected$party_competitiveness_2020, 2))
          )),
        column(
          3,
          wellPanel(
            h5("20 Year Democratic Vote Swing"),
            h3(paste0(round(selected$dem_swing_2000_2020 * 100, 2), "%"))
          ))
      )
    })
    output$geography <- renderUI({
      req(selected_county())
      selected <- selected_county()
      tagList(
        fluidRow(
          column(
            4,
            wellPanel(
              h5("Land Area"),
              h3(paste0(scales::comma(round(selected$land_area_sq_miles,0)), " sq. mi")),
              span(
                top_percent_label(
                  selected$land_area_sq_miles,
                  county$land_area_sq_miles),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            4,
            wellPanel(
              h5("Mean Elevation"),
              h3(paste0(scales::comma(round(selected$mean_elevation,0)), " ft")),
              span(
                top_percent_label(
                  selected$mean_elevation,
                  county$mean_elevation),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            4,
            wellPanel(
              h5("Terrain Ruggedness"),
              h3(paste0(round(selected$terrain_ruggedness, 2), " ft")),
              span(
                top_percent_label(
                  selected$terrain_ruggedness,
                  county$terrain_ruggedness),
                style = "font-size: 14px; color: gray;"
              )
            ))
        ),
        fluidRow(
          column(
            3,
            wellPanel(
              h5("% of Forest Land"),
              h3(paste0(round(selected$forest_coverage_pct, 2), "%")),
              span(
                top_percent_label(
                  selected$forest_coverage_pct,
                  county$forest_coverage_pct),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("% of County is Water"),
              h3(paste0(round(selected$water_coverage_pct, 2), "%")),
              span(
                top_percent_label(
                  selected$water_coverage_pct,
                  county$water_coverage_pct),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("Mean Temperature"),
              h3(paste0(round(selected$mean_temp, 1),"°C")),
              span(
                top_percent_label(
                  selected$mean_temp,
                  county$mean_temp),
                style = "font-size: 14px; color: gray;"
              )
            )),
          column(
            3,
            wellPanel(
              h5("Avg. Annual Precipitation"),
              h3(paste0(scales::comma(round(selected$annual_precip, 1)), " in.")),
              span(
                top_percent_label(
                  selected$annual_precip,
                  county$annual_precip),
                style = "font-size: 14px; color: gray;"
              )
            ))
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
            fluidRow(
              column(
                1,
                data$similarity_rank[i]
              ),
              column(
                8,
                tags$a(
                  href = "#",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                    ns("explore_clicked"),
                    data$comparison_GEOID[i]
                  ),
                  data$county_name[i]
                )
              ),
              column(
                3,
                data$similarity[i]))
          })
        )
      }
      tagList(
        h4("Most Similar Counties"),
        fluidRow(
          column(1, strong("Rank")),
          column(8, strong("County")),
          column(3, strong("Similarity"))
        ),
        county_row(similar),
        hr(),
        h4("Most Different Counties"),
        fluidRow(
          column(1, strong("Rank")),
          column(8, strong("County")),
          column(3, strong("Similarity"))
        ),
        county_row(different)
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