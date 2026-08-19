library(shiny)

cluster_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Types of Counties"),
    uiOutput(ns("cluster_intro")),
    hr(),
    uiOutput(ns("cluster_explorer"))
  )
}


cluster_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      selected_method <- reactiveVal(NULL)
      observeEvent(input$explore_pca, {
        selected_method("pca")})
      observeEvent(input$explore_hc, {
        selected_method("hc")})
      observeEvent(input$explore_gmm, {
        selected_method("gmm")})
      output$pca_map <- renderPlot({
        plot_cluster_map(
          county_data = county,
          cluster_variable = "pca_cluster_name",
          cluster_types = pca_cluster_types, 
          title = "PCA-Based County Archetypes",
          subtitle = "Clusters generated from 20 principal components"
        )
      },
      height = 600)
      output$pca_sizes <- renderPlot({
        plot_cluster_sizes(
          cluster_profiles = pca_cluster_profiles,
          size_variable = "counties",
          cluster_variable = "cluster_name",
          title = "Number of Counties in Each PCA Cluster",
          subtitle = "Distribution of counties across PCA-generated groups"
        )
      })
      output$hc_map <- renderPlot({
        plot_cluster_map(
          county_data = county,
          cluster_variable = "hc_cluster_name",
          cluster_types = hc_cluster_types, 
          title = "Hierarchical Clustering County Archetypes",
          subtitle = "Clusters generated from hierarchical clusters"
        )
      },
      height = 600)
      output$hc_sizes <- renderPlot({
        plot_cluster_sizes(
          cluster_profiles = hc_cluster_profiles,
          size_variable = "size",
          cluster_variable = "cluster_name",
          title = "Number of Counties in Each Hierarchical Cluster",
          subtitle = "Distribution of counties in hierarchical clusters"
        )
      },
      height = 400)
      output$gmm_map <- renderPlot({
        plot_cluster_map(
          county_data = county,
          cluster_variable = "gmm_cluster_name",
          cluster_types = gmm_cluster_types, 
          title = "GMM Cluster County Archetypes",
          subtitle = "Clusters generated from GMM probabilities assigned to each county"
        )
      },
      height = 600)
      output$gmm_sizes <- renderPlot({
        plot_cluster_sizes(
          cluster_profiles = gmm_cluster_profiles,
          size_variable = "size",
          cluster_variable = "cluster_name",
          title = "Number of Counties in Each GMM Archetype",
          subtitle = "Distribution of counties across the GMM based clusters"
        )
      },
      height = 400)
      output$cluster_intro <- renderUI({
        tagList(
          p(
            "Counties in the U.S. differ dramatically based on their demographics, ",
            "economics, geography, climate, politics, and other characteristics. ",
            "To separate the counties into groups of similar counties, three different ",
            "unsupervised learning approaches were applied to cluster the counties."
          ),
          h4("Three Different Ways to Group Counties"),
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
                actionButton(
                  ns("explore_pca"),
                  "Explore PCA Clusters"))
            ),
            column(
              4,
              wellPanel(
                h5("Hierarchical Clustering"),
                p(
                  "Hierarchical Clustering builds a similarity tree between components, ",
                  "which allows groups to naturally form after recursively splitting a large ",
                  "cluster into much smaller, more defined clusters of similar counties. This ",
                  "method also found 13 clusters as a sweet spot."),
                actionButton(
                  ns("explore_hc"),
                  "Explore Hierarchical Clusters"))
            ),
            column(
              4,
              wellPanel(
                h5("GMM Clustering"),
                p(
                  "Gaussian Mixture Model Clustering, unlike the other two methods, is ",
                  "probabilistic, meaning for each county, it gives a probability that it ",
                  "belongs to any specific one of the 14 clusters deemed to contain all U.S ",
                  "counties, with the county belonging to the cluster it has a highest probability for."
                ),
                actionButton(
                  ns("explore_gmm"),
                  "Explore GMM Clusters"))
            )),
          p(
            strong("Note: "),
            "Each clustering method finds a different perspective on American counties. ",
            "There is no definitive grouping of U.S. counties."
          ))
      })
      output$pca_profiles <- renderUI({
        tagList(
          lapply(seq_len(nrow(pca_cluster_profiles)), function(i) {
            profile <- pca_cluster_profiles[i, ]
            cluster_profile_card(
              profile = profile,
              size = profile$counties,
              largest_counties =
                pca_largest_counties |>
                dplyr::filter(cluster == profile$cluster),
              county_data = county
            )
          })
        )
      })
      output$hc_profiles <- renderUI({
        tagList(
          lapply(seq_len(nrow(hc_cluster_profiles)), function(i) {
            profile <- hc_cluster_profiles[i, ]
            cluster_profile_card(
              profile = profile,
              size = profile$size,
              largest_counties =
                hc_largest_counties |>
                dplyr::filter(cluster13 == profile$cluster),
              county_data = county
            )
          })
        )
      })
      output$gmm_profiles <- renderUI({
        tagList(
          lapply(seq_len(nrow(gmm_cluster_profiles)), function(i) {
            profile <- gmm_cluster_profiles[i, ]
            cluster_profile_card(
              profile = profile,
              size = profile$size,
              largest_counties =
                gmm_largest_counties |>
                dplyr::filter(gmm_cluster == profile$cluster),
              county_data = county
            )
          })
        )
      })
      output$cluster_explorer <- renderUI({
        req(selected_method())
        method <- selected_method()
        if(method == "pca"){
          tagList(
            h2("PCA County Archetypes"),
            plotOutput(
              ns("pca_map"),
              height = "600px",
              width = "100%"
            ),
            fluidRow(
              column(
                6,
                plotOutput(
                  ns("pca_sizes"), 
                  height = "400px"
                )
              ),
              column(
                6,
                plotOutput(
                  ns("pca_heatmap"),
                  height = "400px"
                )
              )
            ),
            h2("Cluster Profiles"),
            uiOutput(
              ns("pca_profiles")
            )
          )
        }
        else if(method == "hc"){
          tagList(
            h2("Hierarchical County Archetypes"),
            plotOutput(
              ns("hc_map"),
              height = "600px",
              width = "100%"
            ),
            fluidRow(
              column(
                6,
                plotOutput(
                  ns("hc_sizes"), 
                  height = "400px"
                )),
              column(
                6,
                plotOutput(
                  ns("hc_heatmap"),
                  height = "400px"
                )
              )
            ),
            uiOutput(
              ns("hc_profiles")
            )
          )
        }
        
        else if(method == "gmm"){
          tagList(
            h2("GMM County Archetypes"),
            plotOutput(
              ns("gmm_map"),
              height = "600px",
              width = "100%"
            ),
            fluidRow(
              column(
                6,
                plotOutput(
                  ns("gmm_sizes"), 
                  height = "400px"
                )
              ),
              column(
                6,
                plotOutput(
                  ns("gmm_heatmap"),
                  height = "400px"
                )
              )
            ),
            uiOutput(
              ns("gmm_profiles")
            )
          )
        }
      })
    })
}