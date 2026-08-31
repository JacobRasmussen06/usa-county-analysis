library(shiny)

comparison_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("County Comparison"),
    uiOutput(ns("similar_intro")),
    div(
      class = "comparison-selector",
      h2("Compare Two Counties"),
      p(
        "Select two counties to compare their demographics, economy, ",
        "politics, geography, and overall similarity."
      ),
      fluidRow(
        column(
          6,
          selectizeInput(
            ns("county1"),
            "County A",
            choices = NULL,
            selected = character(0),
            options = list(
              placeholder = "Search for a county..."
            )
          )
        ),
        column(
          6,
          selectizeInput(
            ns("county2"),
            "County B",
            choices = NULL,
            selected = character(0),
            options = list(
              placeholder = "Search for a county..."
            )
          )
        )
      )
    ),
    h3(textOutput(ns("county_titles"))),
    uiOutput(ns("similarity_card")),
    div(
      style = "margin-bottom: 50px;",
      plotOutput(ns("comparison_plot"), height="700px")
    ),
    uiOutput(ns("compare_counties")),
    fluidRow(
      column(
        6,
        uiOutput(ns("raw_comparison"))),
      column(
        6,
        uiOutput(ns("similar_counties")))
    ),
    hr(),
    uiOutput(ns("similarity"))
  )
}


comparison_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      selected_counties <- reactive({
        req(input$county1)
        req(input$county2)
        list(
          county1 = input$county1,
          county2 = input$county2
        )
      })
      similarity_score <- reactive({
        counties <- selected_counties()
        calculate_similarity(county, counties$county1, counties$county2, similarity_params)
      })
      observeEvent(TRUE, {
        choices <- setNames(
          county$GEOID,
          county$county_name)
        updateSelectizeInput(
          session,
          "county1",
          choices = choices,
          server = FALSE,
          selected = "")
        updateSelectizeInput(
          session,
          "county2",
          server = FALSE,
          choices = choices,
          selected = "")
      }, once = TRUE)
      output$similar_intro <- renderUI({
        tagList(
          h2("How Similarity Works"),
          p("In this project, counties are directly compared based on a calculated score, ",
            "called their similarity score. To calculate similarity, important variables like ",
            "population density, median age, mean temperature, and around 20 more variables ",
            "were standardized and the Euclidean distance between them was calculated. Similarity ",
            "is equal to 100 - Euclidean distance, so for example, if two counties have a distance of ",
            "5.6, their similarity score is 94.4 and thus they are 94.4% similar.")
        )
      })
      output$compare_counties <- renderUI({
        counties <- selected_counties()
        validate(
          need(
            counties$county1 != counties$county2,
            "Please select two different counties to compare."
          )
        )
      })
      output$county_titles <- renderText({
        req(selected_counties())
        counties <- selected_counties()
        county1_name <- county$county_name[
          county$GEOID == counties$county1]
        county2_name <- county$county_name[
          county$GEOID == counties$county2]
        validate(
          need(
            counties$county1 != counties$county2,
            ""
          )
        )
        paste0(county1_name, " vs. ", county2_name)
      })
        output$similarity_card <- renderUI({
          req(selected_counties()) 
          counties <- selected_counties() 
          validate( 
            need( counties$county1 != counties$county2, "")) 
          county1_name <- county$county_name[ county$GEOID == counties$county1] 
          county2_name <- county$county_name[ county$GEOID == counties$county2] 
          div( class = "summary-card", 
               h4("County Similarity"), 
               p(strong(county1_name), " and ", strong(county2_name), " are ", 
                 strong(paste0(round(similarity_score(), 1), "%")), " similar.")) 
          })
      output$comparison_plot <- renderPlot({
        counties <- selected_counties()
        validate(
          need(
            counties$county1 != counties$county2,
            ""
          )
        )
        comparison_vars <- c("population_density", "median_age", "diversity_index", "median_household_income", "college_grad_pct", 
                                  "unemployment_rate", "poverty_rate", "internet_access_pct", "mean_commute_time", "mean_temp", "voter_turnout")
        county_comparison_plot(county_data = county, county1 = counties$county1, 
                               county2 = counties$county2, params = similarity_params, comparison_variables = comparison_vars)
      }, height = 700)
      output$raw_comparison <- renderUI({
        counties <- selected_counties()
        validate(
          need(
            counties$county1 != counties$county2,
            ""
          )
        )
        tagList(
          h3("Raw Variable Comparison"),
          tableOutput(ns("comparison_table"))
        )
      })
      output$comparison_table <- renderTable({
        counties <- selected_counties()
        county_raw_comparison_table(
          county,
          counties$county1,
          counties$county2
        )
      }, digits = 2, striped = TRUE)
      output$similar_counties <- renderUI({
        counties <- selected_counties()
        validate(
          need(
            counties$county1 != counties$county2,
            ""))
        county_lists <- list(
          county1 = counties$county1,
          county2 = counties$county2)
        similar_section <- function(geoid){
          county_name <- county$county_name[
            county$GEOID == geoid]
          similar <- most_similar |>
            filter(
              source_GEOID == geoid) |>
            slice_head(n = 5) |>
            left_join(
              county |> 
                select(GEOID, county_name),
              by = c("comparison_GEOID" = "GEOID")) |>
            mutate(
              similarity = paste0(
                round(100 - distance,1), "%"))
          tagList(
            h4(county_name),
            tags$table(
              class = "table table-striped",
              tags$thead(
                tags$tr(
                  tags$th("County"),
                  tags$th("Similarity"))),
              tags$tbody(
                lapply(
                  seq_len(nrow(similar)),
                  function(i){
                    tags$tr(
                      tags$td(similar$county_name[i]),
                      tags$td(similar$similarity[i]))
                  }
                ))))}
        tagList(
          h3("Most Similar Counties"),
          similar_section(county_lists$county1),
          hr(),
          similar_section(county_lists$county2))
      })
      output$similarity <- renderUI({
        tagList(
          h2("Understanding Similarity Scores"),
          p("The similarity engine compares counties using their Euclidean distance on several ",
          "variables. While this does do a good job at finding scores, you may notice that no score ",
          "drops below ~65%. This is because, while counties in the US are different from each other, ",
          "because they all exist in the United States, a country with relatively standard baselines for several ",
          "of the tracked variables, even in the most different of counties, structural characteristics like a relatively ",
          "low poverty rate compared to the world, but not to each other, standard median ages due to a stable population, etc., are ",
          "shared between every county, making them inherently similar, and their differences are not large enough in scale ",
          "to make them considerably more different than 65% similar."),
          h3("Most Similar County Pairs"),
          uiOutput(ns("most_similar_table_ui")),
          h3("Most Distinct County Pairs"),
          uiOutput(ns("least_similar_table_ui")))
      })
      output$most_similar_table_ui <- renderUI({
        tableOutput(ns("most_similar_table"))
      })
      output$least_similar_table_ui <- renderUI({
        tableOutput(ns("least_similar_table"))
      })
      output$most_similar_table <- renderTable({
        most_similar_display |>
          slice_head(n = 15) |>
          transmute(
            `County A` = county1_name,
            `County B` = county2_name,
            Similarity = paste0(
              round(similarity, 1),
              "%"
            )
          )
      })
      output$least_similar_table <- renderTable({
        least_similar_display |>
          slice_head(n = 15) |>
          transmute(
            `County A` = county1_name,
            `County B` = county2_name,
            Similarity = paste0(
              round(similarity, 1),
              "%"
            )
          )
        
      })
    })
}