library(shiny)

variable_explorer_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Variable Explorer"),
    uiOutput(ns("var_explorer_intro")),
    selectizeInput(
      ns("variable"),
      "Explore a variable!",
      choices = NULL,
      selected = character(0),
      options = list(
        placeholder = "Choose a variable...")),
    hr(),
    uiOutput(ns("variable_explorer")),
  )
}


variable_explorer_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      selected_variable <- reactive({
        req(input$variable) 
        input$variable
      })
      selected_metadata <- reactive({
        variable_metadata |>
          filter(variable == selected_variable()) |>
          slice(1)
      })
      observeEvent(TRUE, {
        updateSelectizeInput(
          session,
          "variable",
          choices = variables,
          server = FALSE,
          selected = ""
        )
      }, once = TRUE)
      output$var_explorer_intro <- renderUI({
        tagList(
          p(
            "The data pipeline for this project was merged from several data sources, ",
            "feature engineering, and creation of a widespread dataset of U.S. counties. ",
            "Explore the over 60 variables ranging from demographics, economics, politics, ",
            "and more. For further information on the sources and function of variables, please ",
            "refer to the (Link to) Data Dictionary."
          )
        )
      })
      output$variable_explorer <- renderUI({
        req(selected_variable())
        tagList(
          plotOutput(
            ns("variable_map"),
            height = "900px",
            width = "100%"
          ),
          fluidRow(
            column(
              6,
              plotOutput(
                ns("variable_dist"),
                height = "400px")),
            column(
              6,
              uiOutput(
                ns("summary_card")))
          ),
          hr(),
          fluidRow(
            column(
              6,
              uiOutput(
                ns("high_counties"))
            ),
            column(
              6,
              uiOutput(ns("low_counties"))
            )
          ),
          fluidRow(
            column(
              6,
              uiOutput(
                ns("percentiles"))
            ),
            column(
              6,
              plotOutput(
                ns("boxplot"),
                height = "400px")
            )
          )
        )
      })
      output$variable_map <- renderPlot({
        meta <- selected_metadata()
        req(nrow(meta) == 1)
        display_title <- ifelse(
          is.na(meta$unit),
          meta$label,
          paste(meta$label, meta$unit))
        plot_variable_map(county_data = county, variable = meta$variable, title = display_title, subtitle = "County level distribution", label = display_title)
      }, height = 900)
      output$variable_dist <- renderPlot({
          meta <- selected_metadata()
          display_title <- ifelse(
            is.na(meta$unit),
            meta$label,
            paste(meta$label, meta$unit))
          plot_variable_distribution(county_data = county, variable = meta$variable, title = paste("Distribution of", display_title))
        })
      output$summary_card <- renderUI({
        meta <- selected_metadata()
        req(nrow(meta) == 1)
        variable_summary_card(
          variable = meta$variable,
          label = meta$label,
          unit = meta$unit,
          county_data = county
        )
      })
      output$high_counties <- renderUI({
        meta <- selected_metadata()
        variable_high_low_card(county_data = county, variable = meta$variable, label = meta$label, unit = meta$unit, direction = "high")
      })
      output$low_counties <- renderUI({
        meta <- selected_metadata()
        variable_high_low_card(county_data = county, variable = meta$variable, label = meta$label, unit = meta$unit, direction = "low")
      })
      output$percentiles <- renderUI({
        meta <- selected_metadata()
        variable_percentile_card(county_data = county, variable = meta$variable, label = meta$label, unit = meta$unit)
      })
      output$boxplot <- renderPlot({
        meta <- selected_metadata()
        display_title <- ifelse(
          is.na(meta$unit),
          meta$label,
          paste(meta$label, meta$unit))
        plot_variable_boxplot(county_data = county, variable = meta$variable, title = paste(display_title, "Distribution"))
      })
    })
}