library(shiny)

uniqueness_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Uniqueness of Counties"),
    uiOutput(ns("uniqueness_intro")),
    hr(),
    h2("Most Unique U.S Counties"),
    h2("The Most Unique Counties"),
    checkboxInput(
      ns("exclude_ny"),
      "Exclude the five New York City burroughs",
      value = FALSE),
    fluidRow(
      column(
        4,
        tableOutput(ns("national_unique"))),
      column(
        4,
        tableOutput(ns("neighbor_unique"))),
      column(
        4,
        tableOutput(ns("cluster_unique")))),
    hr(),
    h2("Explore a County's Uniqueness"),
    selectizeInput(
      ns("county"),
      "Select a county",
      choices = NULL,
      options = list(
        placeholder = "Search for a county...")),
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
      selected_county <- reactive({
        req(input$county)
        filtered_uniqueness() |>
          filter(GEOID == input$county)
      })
      output$county_uniqueness <- renderUI({
        county <- selected_county()
        req(nrow(county) == 1)
        div(
          class = "summary-card",
          h3(county$county_name),
          p("Combined uniqueness score: ",
            strong(
              paste0(
                round(county$uniqueness_score, 1)))),
          p("National isolation rank: ",
            strong(county$isolation_rank)),
          p("Neighbor uniqueness rank: ",
            strong(county$regional_rank)),
          p("Cluster uniqueness rank: ",
            strong(county$cluster_unique_rank)),
          p("Combined uniqueness rank: ",
            strong(county$uniqueness_combined_rank)
          ))
      })
      output$uniqueness_plot <- renderPlot({
        county <- selected_county()
        req(nrow(county) == 1)
        plot_data <- tibble(
          type = c(
            "National",
            "Neighbors",
            "Cluster"),
          score = c(
            county$isolation_score,
            county$regional_uniqueness,
            county$cluster_uniqueness))
        ggplot(
          plot_data,
          aes(
            x = score,
            y = reorder(type, score))) +
          geom_col(
            fill = "steelblue",
            color = "black") +
          scale_x_continuous(
            labels = function(x) paste0(x, "%")) +
          labs(
            title = "Uniqueness Profile",
            x = "Uniqueness Score",
            y = NULL) +
          theme_minimal() +
          theme(
            plot.title = element_text(
              size = 18,
              face = "bold",
              hjust = .5),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank())
      }, height = 450)
    })
}