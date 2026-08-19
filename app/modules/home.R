library(shiny)

home_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Welcome to the U.S. County Explorer Dashboard!"),
    uiOutput(ns("home_intro"))
  )
}


home_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      output$home_intro <- renderUI({
        tagList(
          p(
            "In the continental United States, there are 3,109 county or county-equivalents ",
            "broadly ranging across many different ways of life, whether rural or urban, ",
            "diverse or not, more well off or not, etc. Every county has something unique to ",
            "offer, but there are patterns you see across multiple counties. This project aims ",
            "to answer three distinct questions:"
          ),
          tags$ol(
            tags$li(
              "What kinds of counties exist?"
            ),
            tags$li(
              "What counties are unusual compared to the counties around them?"
            ),
            tags$li(
              "What counties are similar to each other statistically?"
            )),
          p(
            "Through the course of this project, those three questions have been answered, ",
            "and this is the result. This dashboard contains all information, insights, and findings ",
            "found during the course of developing this project. It comes with an accompanying GitHub ",
            "repository (link here), a detailed report spanning the entire workflow of the project, ",
            "and an interactive map."
          ),
          p(
            "Distinct pages on exploring counties, variables, similarity, clusters, uniqueness, ",
            "methodology, a data dictionary, and about the project can all be found in the top row."
          ),
          p(
            "Thank you for exploring U.S. counties and enjoy!"
          )
        )
      })
    })
}