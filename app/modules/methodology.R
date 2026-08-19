library(shiny)

methodology_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    h1("Methodology"),
    p("This page of the dashboard serves as a more techincal explanation of the ",
      "progress of this project. For even further technical depth, read the technical ",
      "report, which can be found here."),
    hr(),
    h2("Pipeline"),
    imageOutput(ns("pipeline")),
    hr(),
    h2("Data Collection"),
    p("In order to maintain a complete data pipeline, several data sources were compiled ",
      "to create the final dataset. The full list of sources can be found in both the ",
      "data dictionary and about pages. Collection of this data involved several different methods ",
      "including APIs, using R packages to obtain data, and more."),
    hr(),
    h2("Feature Engineering"),
    p("Before moving on to any sort of analysis, several features had to be engineered or cleaned to be usable. ",
      "Most features were easily calculable, like under_18_pct, which was simply calculated by summing the amount of residents ",
      "from the age groups under 18 and dividing the total population. Most variables with the suffix 'pct' or 'rate' were like this, ",
      "taking a group of the population and summing them, then dividing by the total population. The variables engineered in this way were: ",
      "under_18_pct, over_65_pct, foreign_born_pct, veteran_pct, married_pct, high_school_pct, some_college_pct, ",
      "college_grad_pct, masters_or_higher_pct, public_school_pct, unemployment_rate, poverty_rate, labor_participation_rate, ",
      "snap_pct, internet_access_pct, homeownership_rate, vacancy_rate, crowding_rate, housing_cost_burden_pct, agriculture_pct, ",
      "construction_pct, manufacturing_pct, arts_tourism_pct, finance_pct, information_pct, retail_pct, ",
      "education_healthcare_pct, government_pct, technical_pct, public_transit_pct, walk_bike_to_work_pct, drive_alone_pct, ",
      "work_from_home_pct, internet_access_pct. Similarly, water_coverage_pct and forest_coverage_pct used this same framework. ",
      "The variable distance_to_coast_miles was engineered using the county centroid (a geographical point) and the coastline data, calculating ",
      "the distance between them. The political variables, dem_vote_share_2020 and party_competitiveness_2020, were calculated ",
      "using available political data, with dem_vote_share_2020 being Democratic votes / total votes, while ",
      "competitiveness was 1 - abs(Republican vote share - Democratic vote share). The variables dem_swing_2000_2020 and comp_swing_2000_2020 were calculated ",
      "by finding those values for 2000 and taking the difference between them and the ones from 2020. Similarly, income_growth_5yr and ",
      "population_growth_5yr were calculated in the same way, taking 2018 data - 2023 data. Population density is obviously Population / Land Area ",
      "and that leaves the two indexes as the only two engineered variables not discussed. The pop_stability_index used a custom decay function ",
      "which punished counties who lost population more than counties who gained population, but still reasonably punished counties who gained too much. ",
      "The index was intended to discover what counties had stable populations, where closer to 1 means more stable, and it did an adequate job of that. ",
      "Diversity index was based on the Simpson Diversity Index for biodiversity, where it was calculated as ",
      "1 - the Sum of Squared proportions of races. Each race listed on the ACS had its proportion taken, and the sum of squares of that ",
      "subtracted from 1 was the Diversity Index. This index does an excellent job showing what counties are predominantly white and which counties ",
      "are more diverse. Overall, feature engineering proved to be an interesting challenge with the amount of variables that had to be engineered."),
    hr(),
    h2("Similarity Calculation"),
    h3("Standardization"),
    withMathJax(helpText("$$z = \\frac{x-\\mu}{\\sigma}$$")),
    p("In order to calculate similarity, variables were standardized onto a scale that ",
      "kept the variables in order and allowed Euclidean distance to be calculated."),
    hr(),
    h3("Variable Weighting"),
    p("After some initial runs, the variables used for similarity scores, specifically ",
      "the geographic variables like mean temperature, were weighted less than all the ",
      "other variables. This was done to ensure similar counties were not just the counties ",
      "immediate neighbors, but that the geography still mattered. No other variable type was weighted."),
    h3("Calculating Similarity"),
    p("After weights were applied and the variables were scaled, the similarity was a simple calculation ",
      "of Euclidean distance on the k dimensional space, where k was the amount of variables. Then, ",
      "similarity is simply equal to 100 - Euclidean distance. This calculation was done to intentionally be simple ",
      "and easily interpretable. "),
    h2("Uniqueness"),
    p("Uniqueness was also an easy calculation derived from Euclidean distance. The uniqueness score ",
      "of every county was contrived of three sub-uniqueness scores: a county's national isolation, ",
      "a county's uniqueness from their direct neighbors, and a county's uniqueness within their cluster. ",
      "Each of these sub-uniqueness scores were calculated by taking the mean of the distance of a particular subset ",
      "of counties. In the national isolation rank, it was chosen to use the mean distance from a county's 50 closest counties ",
      "by Euclidean distance, because 25 was too little, and anything more than 100 caused issues of outliers. For cluster uniqueness, ",
      "it used a county's distance from the other counties in its GMM cluster, and for the neighbors, it used the ",
      "distance from the county's direct neighbors (counties it borders), and the counties that border the counties it borders. ",
      "This gave a nice range of 2 - ~20 counties for every county, which was deemed acceptable. Then, the total uniqueness ",
      "was calculated from weighting each of the sub-uniqueness scores and combining them."),
    h2("Clustering"),
    h3("Choosing Methods"),
    p("Three different methods were chosen to cluster U.S. counties. Three different methods were chosen to have more ",
      "cluster data to work with, to ensure that clustering indeed was suitable, and to maintain a decent variety of methods."),
    fluidRow(
      column(
        4, 
        wellPanel(
          h5("PCA Clustering"),
          p(
            "PCA Clustering first uses principal component analysis, which finds ",
            "important dimensions by combining narrower variables into principal ",
            "components. Then, this approach identified how counties compare ",
            "based on these components, grouping them into 13 clusters."),
      )),
      column(
        4,
        wellPanel(
          h5("Hierarchical Clustering"),
          p(
            "Hierarchical Clustering builds a similarity tree between components, ",
            "which allows groups to naturally form after recursively splitting a large ",
            "cluster into much smaller, more defined clusters of similar counties. This ",
            "method also found 13 clusters as a sweet spot."),
      )),
      column(
        4,
        wellPanel(
          h5("GMM Clustering"),
          p(
            "Gaussian Mixture Model Clustering, unlike the other two methods, is ",
            "probabilistic, meaning for each county, it gives a probability that it ",
            "belongs to any specific one of the 14 clusters deemed to contain all U.S ",
            "counties, with the county belonging to the cluster it has a highest probability for."
          )
      ))),
    h3("Cluster Archetypes"),
    p("Several different archetypes of U.S. counties were uncovered by clustering methods. ",
      "From the 40 clusters that were created (13 PCA + 13 Hierarchical + 14 GMM), I personally ",
      "went at condensed them into 13 reappearing clusters that appeared in at least one of the clustering methods.", 
      "These archetypes are: Midwest Suburbia, Great Plains Rural Areas, Frigid Retirement Communities, ",
      "Underserved Southern America, Rural Appalachia, Extraction / Border Counties, The Rural Recreational West, ",
      "Mid Size Metro Areas, Rugged Coastal Suburbs, Established Industrial Urban Cores, Global Super Cities, ",
      "Affluent Suburbia, and Sunny Retirement Communities. Further information on each archetype can be found in the report."),
    h2("Dashboard"),
    p("The dashboard you're reading this on was an idea conceived at the beginning of the project. It seemed like ",
      "a natural end point for the project to allow users to interact with the project in a similar way I did. The results of ",
      "this project are fascinating, and exploring counties where you live, work, or want to live or work is an interesting way to get ",
      "to better know these counties. This dashboard was created using Shiny, where each module is its own individual R script. The ",
      "scripts for the dashboard can be found in the GitHub repository.")
    )
}


methodology_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      output$Pipeline <- renderImage({
        
      })
      
    })
}