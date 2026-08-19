library(shiny)
library(here)

data_dictionary_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Data Dictionary"),
    includeMarkdown(here("docs", "data_dictionary.md"))
  )
}


data_dictionary_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){})
}