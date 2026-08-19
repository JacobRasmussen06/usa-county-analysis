library(shiny)

about_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("About the Project, Data, and Author"),
    uiOutput(ns("project_about")),
    hr(),
    uiOutput(ns("data_about")),
    hr(),
    uiOutput(ns("author_about"))
  )
}


about_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      output$project_about <- renderUI({
        tagList(
          h2("About the Project"),
          p("This project was a cumulation of weeks of work processing and cleaning data, ",
            "engineering features, doing clustering analysis, and building deliverables. ",
            "Every script used during the course of this project is publicly available ",
            "in the accompanying GitHub repository.")
        )
      })
      output$data_about <- renderUI({
        tagList(
          h2("About the Data"),
          p("Several different data sources were processed and combined together to ",
            "craft a final dataset."),
          tags$ul(
            tags$li(
              "The American Community Survey (ACS) 2023 5 year estimates served as the primary ", 
              "source of the demographic, housing, economic, and educational characteristics of ",
              "counties. Variables like median household income, median age, poverty rate, educational ",
              "attainment, and much more were obtained from the ACS. Furthermore, the 2018 ACS was also ",
              "used to construct 5 year changes in income and population, the latter of which being used ",
              "to construct a population stability index. A diversity index was also engineered using race ",
              "data from the ACS."),
            tags$li(
              "TIGER/Line county boundaries were used as the backbone for all geographic visualizations throughout ",
              "the project. In addition to county boundaries, TIGER/Line was the data source for a county's land area ",
              "and its water coverage percentage."
            ),
            tags$li(
              "PRISM Climate normals were processed and used to characterize long-term conditions in a county's climate. ",
              "Mean annual temperature and annual precipitation were obtained from this data source."
            ),
            tags$li(
              "Via the R package elevatr, USGS elevation rasters were obtained and summarize to produce a county's mean ",
              "elevation and the ruggedness (standard deviation) of its terrain."
            ),
            tags$li(
              "National Land Cover Database and Forest Inventory and Analysis products were used to estimate forest ",
              "coverage, the amount of land in a county that is forested."
            ),
            tags$li(
              "The County Health Rankings & Roadmaps was the data source for the homicide rate, suicide rate, and firearm fatalities of a county, ",
              "as well as its voter turnout in the 2020 election."
            ),
            tags$li(
              "The MIT Election Data served as the data source for the political variables of the dataset. ",
              "These variables include the percentage of votes the Democratic party earned in the 2020 election, ",
              "as well as three engineered variables: party competitiveness, and 20 year changes in both democratic ",
              "vote share and party competitiveness."
            ),
            tags$li(
              "The USDA Rural Urban Continuum Codes were used to effectively distinguish metropolitan counties from rural counties, ",
              "validate the results of a particular decision made, and provide standardized metrics of urbanization among counties."
            )
          ),
        p("Overall, the data pipeline for this project was thorough, containing several data sources and steps. First data was obtained ",
        "in raw form. Then, it was cleaned, and variables were engineered. Then, every source was merged into one dataset using a county's ",
        "5 digit identifier, referred to as its GEOID in the project. Following the merger, the dataset was used as the backbone to clustering ",
        "analysis. Finally, the dataset was used as an engine to be used in this Shiny Dashboard and Interactive Map. The full pipeline of data ",
        "can be found in the first three scripts of the scripts folder, found in the accompanying GitHub repository."),
        p("The author gratefully thanks each and every data source for their contribution ",
          "to this project and county-level data as a whole. Links to websites for each source ",
          "can be found here: (add links).")
        )
      })
      output$author_about <- renderUI({
        tagList(
          h2("About the Author"),
          p("I am Jacob Rasmussen, a third year Data Science and Statistics Double Major ",
            "currently studying at the University of Wisconsin - Madison. Here is a link to my GitHub, ",
            "where you can find this project and another project on predicting breakout NHL forwards. ",
            "Outside of data analytics and statistics, I am heavily interested in sports, chess, books, ",
            "and much more. Thank you for interacting with my project!")
        )
      })
    })
}