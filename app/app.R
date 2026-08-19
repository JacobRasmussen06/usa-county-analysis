library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(bslib)
library(here)

source("R/load_data.R")
source("helpers/plots.R")
source("helpers/formatting.R")

source("modules/home.R")
source("modules/county_explorer.R")
source("modules/variable_explorer.R")
source("modules/comparison.R")
source("modules/cluster.R")
source("modules/uniqueness.R")
source("modules/methodology.R")
source("modules/data_dictionary.R")
source("modules/about.R")

ui <- page_navbar(
  title = "U.S. County Explorer Dashboard",
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css",href = "styles.css")),
  nav_panel("Home", home_ui("home")),
  nav_panel("County Explorer", county_explorer_ui("county_explorer")),
  nav_panel("Variable Explorer", variable_explorer_ui("variable_explorer")),
  nav_panel("County Comparison", comparison_ui("comparison")),
  nav_panel("Clusters", cluster_ui("cluster")),
  nav_panel("Uniqueness", uniqueness_ui("uniqueness")),
  nav_panel("Methodology", methodology_ui("methodology")),
  nav_panel("Data Dictionary", data_dictionary_ui("data_dictionary")),
  nav_panel("About", about_ui("about"))
)

server <- function(input, output, session){
  home_server("home")
  county_explorer_server("county_explorer")
  variable_explorer_server("variable_explorer")
  comparison_server("comparison")
  cluster_server("cluster")
  uniqueness_server("uniqueness")
  methodology_server("methodology")
  about_server("about")
}

shinyApp(ui, server)