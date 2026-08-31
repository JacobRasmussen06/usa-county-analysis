library(shiny)

uniqueness_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Uniqueness of Counties"),
    uiOutput(ns("uniqueness_intro")),
    hr(),
    h2("The Most Unique Counties"),
    checkboxInput(
      ns("exclude_ny"),
      "Exclude the five New York City boroughs",
      value = FALSE),
    fluidRow(
      column(
        4,
        div(
          class = "card",
          h4("By National Distance"),
          p(class = "uniqueness-table-blurb", "Farthest, on average, from their 100 closest counties nationally."),
          tableOutput(ns("national_unique")))),
      column(
        4,
        div(
          class = "card",
          h4("By Neighboring Counties"),
          p(class = "uniqueness-table-blurb", "Most different from the counties that directly border them."),
          tableOutput(ns("neighbor_unique")))),
      column(
        4,
        div(
          class = "card",
          h4("By Cluster"),
          p(class = "uniqueness-table-blurb", "Most different from other counties in their own statistical cluster."),
          tableOutput(ns("cluster_unique"))))),
    hr(),
    h2("How Uniqueness Is Distributed"),
    fluidRow(
      column(
        6,
        div(class = "card", plotOutput(ns("uniqueness_distribution"), height = "380px"))),
      column(
        6,
        div(class = "card", plotOutput(ns("uniqueness_subscore_comparison"), height = "380px")))),
    hr(),
    h2("Explore a County's Uniqueness"),
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
    uiOutput(ns("county_uniqueness")),
    plotOutput(
      ns("uniqueness_plot"),
      height = "450px"
    )
  )
}


uniqueness_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      county_of_day <- reactive({
        today <- as.Date(Sys.time(), tz = "America/Chicago")
        day_number <- as.integer(today)
        county$GEOID[
          (day_number %% nrow(county)) + 1
        ]
      })
      filtered_uniqueness <- reactive({
        
        data <- county |>
          st_drop_geometry() |> 
          mutate(
            isolation_score = as.numeric(unlist(isolation_score)),
            regional_uniqueness = as.numeric(unlist(regional_uniqueness)),
            cluster_uniqueness = as.numeric(unlist(cluster_uniqueness)),
            uniqueness_score = as.numeric(unlist(uniqueness_score)),
            
            isolation_rank = as.numeric(unlist(isolation_rank)),
            regional_rank = as.numeric(unlist(regional_rank)),
            cluster_unique_rank = as.numeric(unlist(cluster_unique_rank)),
            uniqueness_combined_rank = as.numeric(unlist(uniqueness_combined_rank))
          )
        
        if (input$exclude_ny) {
          data <- data |>
            filter(!GEOID %in% c(
              "36061",
              "36047",
              "36081",
              "36005",
              "36085"
            ))
        }
        
        data
      })
      observe({
        choices <- setNames(
          filtered_uniqueness()$GEOID,
          filtered_uniqueness()$county_name) 
        updateSelectizeInput(
          session,
          "county",
          choices = choices,
          server = FALSE)
      })
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
      output$uniqueness_intro <- renderUI(
        tagList(
          h3("How Does Uniqueness Work"),
          p("A county's uniqueness is derived in a similar way to its similarity to other counties: ",
            "by its Euclidean distance. That is, each variable used to calculate the distance is counted ",
            "as a dimension, leading to k-dimensional space with k variables, and distance is calculated using the ",
            "k-dimensional distance formula. Uniqueness takes the mean of a county's Euclidean distance based on three subcategories: ",
            "its mean distance from its 100 closest counties by distance, its mean distance from the counties it borders and the ",
            "counties that border the counties the original county borders, giving 2-20 border counties per county, and finally its mean distance from the counties in its cluster. Each score tells a story about the county, ",
            "and the combined uniqueness score was calculated by combining the 3 sub-uniqueness scores.")))
      output$national_unique <- renderTable({
        filtered_uniqueness() |>
          arrange(desc(isolation_score)) |>
          slice_head(n = 10) |>
          select(
            County = county_name,
            `Uniqueness Score` = isolation_score,
            Rank = isolation_rank) |>
          mutate(
            `Uniqueness Score` =
              paste0(round(`Uniqueness Score`, 1)))
      })
      output$neighbor_unique <- renderTable({
        filtered_uniqueness() |>
          arrange(desc(regional_uniqueness)) |>
          slice_head(n = 10) |>
          select(
            County = county_name,
            `Uniqueness Score` = regional_uniqueness,
            Rank = regional_rank) |>
          mutate(
            `Uniqueness Score` =
              paste0(round(`Uniqueness Score`, 1)))
      })
      output$cluster_unique <- renderTable({
        filtered_uniqueness() |>
          arrange(desc(cluster_uniqueness)) |>
          slice_head(n = 10) |>
          select(
            County = county_name,
            `Uniqueness Score` = cluster_uniqueness,
            Rank = cluster_unique_rank) |>
          mutate(
            `Uniqueness Score` =
              paste0(round(`Uniqueness Score`, 1)))
      })
      output$uniqueness_distribution <- renderPlot({
        ggplot(filtered_uniqueness(), aes(x = uniqueness_score)) +
          geom_histogram(bins = 40, fill = "#2C6E9E", color = "#16324F", alpha = 0.8) +
          labs(
            title = "Distribution of Uniqueness Scores",
            subtitle = "Combined score, all counties",
            x = "Uniqueness Score",
            y = "Number of Counties"
          ) +
          theme_minimal(base_size = 13) +
          theme(
            plot.title = element_text(face = "bold", hjust = .5, color = "#16324F"),
            plot.subtitle = element_text(hjust = .5, color = "#6B7680"),
            panel.grid.minor = element_blank()
          )
      })
      output$uniqueness_subscore_comparison <- renderPlot({
        plot_data <- filtered_uniqueness() |>
          select(isolation_score, regional_uniqueness, cluster_uniqueness) |>
          tidyr::pivot_longer(everything(), names_to = "type", values_to = "score") |>
          mutate(
            type = dplyr::recode(
              type,
              isolation_score = "National",
              regional_uniqueness = "Neighbors",
              cluster_uniqueness = "Cluster"
            )
          )
        ggplot(plot_data, aes(x = type, y = score, fill = type)) +
          geom_boxplot(color = "#16324F", alpha = .85) +
          scale_fill_manual(values = c(
            "National" = "#2C6E9E",
            "Neighbors" = "#3F8577",
            "Cluster" = "#C98A3E"
          )) +
          labs(
            title = "How the Three Measures Compare",
            subtitle = "Score distributions across all counties",
            x = NULL,
            y = "Uniqueness Score"
          ) +
          theme_minimal(base_size = 13) +
          theme(
            legend.position = "none",
            plot.title = element_text(face = "bold", hjust = .5, color = "#16324F"),
            plot.subtitle = element_text(hjust = .5, color = "#6B7680"),
            panel.grid.minor = element_blank()
          )
      })
      selected_county <- reactive({
        req(input$county)
        filtered_uniqueness() |>
          filter(GEOID == input$county)
      })
      output$county_uniqueness <- renderUI({
        data <- selected_county()
        req(nrow(data) == 1)
        reference <- filtered_uniqueness()
        div(
          class = "summary-card",
          h4(data$county_name),
          div(
            class = "uniqueness-stat-grid",
            div(
              class = "stat-card",
              div(class = "stat-label", "Combined Uniqueness"),
              div(class = "stat-value", round(data$uniqueness_score, 1)),
              div(class = "stat-rank", top_percent_label(data$uniqueness_score, reference$uniqueness_score))),
            div(
              class = "stat-card",
              div(class = "stat-label", "National Distance"),
              div(class = "stat-value", round(data$isolation_score, 1)),
              div(class = "stat-rank", top_percent_label(data$isolation_score, reference$isolation_score))),
            div(
              class = "stat-card",
              div(class = "stat-label", "Neighbor Distance"),
              div(class = "stat-value", round(data$regional_uniqueness, 1)),
              div(class = "stat-rank", top_percent_label(data$regional_uniqueness, reference$regional_uniqueness))),
            div(
              class = "stat-card",
              div(class = "stat-label", "Cluster Distance"),
              div(class = "stat-value", round(data$cluster_uniqueness, 1)),
              div(class = "stat-rank", top_percent_label(data$cluster_uniqueness, reference$cluster_uniqueness)))
          )
        )
      })
      output$uniqueness_plot <- renderPlot({
        data <- selected_county()
        req(nrow(data) == 1)
        plot_data <- tibble(
          type = c(
            "National",
            "Neighbors",
            "Cluster"),
          score = c(
            data$isolation_score,
            data$regional_uniqueness,
            data$cluster_uniqueness))
        ggplot(
          plot_data,
          aes(
            x = score,
            y = reorder(type, score),
            fill = type)) +
          geom_col(color = "#16324F") +
          scale_fill_manual(values = c(
            "National" = "#2C6E9E",
            "Neighbors" = "#3F8577",
            "Cluster" = "#C98A3E"
          )) +
          scale_x_continuous(
            labels = function(x) paste0(x, "%")) +
          labs(
            title = "Uniqueness Profile",
            subtitle = paste0("How ", data$county_name, " compares across the three measures"),
            x = "Uniqueness Score",
            y = NULL) +
          theme_minimal(base_size = 13) +
          theme(
            legend.position = "none",
            plot.title = element_text(
              size = 18,
              face = "bold",
              hjust = .5,
              color = "#16324F"),
            plot.subtitle = element_text(hjust = .5, color = "#6B7680"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank())
      }, height = 450)
    })
}