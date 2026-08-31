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
      selected_cluster <- reactiveVal(NULL)
      observeEvent(input$explore_pca, {
        selected_method("pca")})
      observeEvent(input$explore_hc, {
        selected_method("hc")})
      observeEvent(input$explore_gmm, {
        selected_method("gmm")})
      observeEvent(input$pca_cluster_clicked, {
        selected_cluster(input$pca_cluster_clicked)})
      observeEvent(input$hc_cluster_clicked, {
        selected_cluster(input$hc_cluster_clicked)})
      observeEvent(input$gmm_cluster_clicked, {
        selected_cluster(input$gmm_cluster_clicked)})
      observeEvent(input$back_to_clusters, {
        selected_cluster(NULL)
      })
      current_cluster_detail <- reactive({
        req(selected_cluster())
        method <- selected_method()
        cluster_id <- selected_cluster()
        
        if (method == "pca") {
          profiles <- pca_cluster_profiles
          largest_source <- pca_largest_counties
          largest_col <- "cluster"
          size_col <- "counties"
        } else if (method == "hc") {
          profiles <- hc_cluster_profiles
          largest_source <- hc_largest_counties
          largest_col <- "cluster13"
          size_col <- "size"
        } else {
          profiles <- gmm_cluster_profiles
          largest_source <- gmm_largest_counties
          largest_col <- "gmm_cluster"
          size_col <- "size"
        }
        
        profile <- profiles |> dplyr::filter(cluster == cluster_id)
        req(nrow(profile) == 1)
        
        largest <- largest_source |> dplyr::filter(.data[[largest_col]] == cluster_id)
        
        list(
          profile = profile,
          size = profile[[size_col]],
          largest_counties = largest,
          description = get_cluster_description(method, cluster_id)
        )
      })
      
      output$cluster_detail_view <- renderUI({
        detail <- current_cluster_detail()
        cluster_detailed_profile(
          profile = detail$profile,
          size = detail$size,
          largest_counties = detail$largest_counties,
          description = detail$description,
          back_id = ns("back_to_clusters"),
          county_data = county,
          plot_output_id = ns("cluster_profile_plot")
        )
      })
      
      output$cluster_profile_plot <- renderPlot({
        detail <- current_cluster_detail()
        plot_cluster_profile(
          profile = detail$profile,
          county_data = county,
          cluster_color = detail$profile$cluster_color
        )
      }, height = 350)
      
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
      output$pca_heatmap <- renderPlot({
        plot_cluster_heatmap(
          cluster_profiles = pca_cluster_profiles,
          title = "PCA Cluster Characteristics",
          subtitle = "Relative characteristics of each county archetype"
        )
      }, height = 400)
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
      output$hc_dendrogram <- renderPlot({
        plot(
          hc,
          labels = FALSE,
          hang = -1,
          main = "Hierarchical Clustering Dendrogram",
          xlab = "Counties",
          ylab = "Height"
        )
      })
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
      output$gmm_uncertainty <- renderPlot({
        plot_gmm_uncertainty(
          cluster_profiles = gmm_cluster_profiles,
          uncertainty_variable = "gmm_uncertainty"
        )
      }, height = 400)
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
              div(
                class = "method-card",
                h5("PCA Clustering"),
                p(
                  "PCA Clustering first uses principal component analysis, which finds ",
                  "important dimensions by combining narrower variables into principal ",
                  "components. Then, this approach identified how counties compare ",
                  "based on these components, grouping them into 13 clusters."),
                actionButton(
                  ns("explore_pca"),
                  "Explore PCA Clusters",
                  class = "county-action-button"))
            ),
            column(
              4,
              div(
                class = "method-card",
                h5("Hierarchical Clustering"),
                p(
                  "Hierarchical Clustering builds a similarity tree between components, ",
                  "which allows groups to naturally form after recursively splitting a large ",
                  "cluster into much smaller, more defined clusters of similar counties. This ",
                  "method also found 13 clusters as a sweet spot."),
                actionButton(
                  ns("explore_hc"),
                  "Explore Hierarchical Clusters",
                  class = "county-action-button"))
            ),
            column(
              4,
              div(
                class = "method-card",
                h5("GMM Clustering"),
                p(
                  "Gaussian Mixture Model Clustering, unlike the other two methods, is ",
                  "probabilistic, meaning for each county, it gives a probability that it ",
                  "belongs to any specific one of the 14 clusters deemed to contain all U.S ",
                  "counties, with the county belonging to the cluster it has a highest probability for."
                ),
                actionButton(
                  ns("explore_gmm"),
                  "Explore GMM Clusters",
                  class = "county-action-button"))
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
              county_data = county,
              click_id = ns("pca_cluster_clicked")
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
              county_data = county,
              click_id = ns("hc_cluster_clicked")
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
              county_data = county,
              click_id = ns("gmm_cluster_clicked")
            )
          })
        )
      })
      output$cluster_explorer <- renderUI({
        req(selected_method())
        method <- selected_method()
        if (!is.null(selected_cluster())) {
          return(uiOutput(ns("cluster_detail_view")))
        }
        
        if(method == "pca"){
          tagList(
            h2("PCA County Archetypes"),
            div(class = "card", plotOutput(
              ns("pca_map"),
              height = "600px",
              width = "100%"
            )),
            div(
              class = "card",
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
            div(class = "card", plotOutput(
              ns("hc_map"),
              height = "600px",
              width = "100%"
            )),
            div(
              class = "card",
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
                    ns("hc_dendrogram"),
                    height = "400px"
                  )
                )
              )
            ),
            h2("Cluster Profiles"),
            uiOutput(
              ns("hc_profiles")
            )
          )
        }
        
        else if(method == "gmm"){
          tagList(
            h2("GMM County Archetypes"),
            div(class = "card", plotOutput(
              ns("gmm_map"),
              height = "600px",
              width = "100%"
            )),
            div(
              class = "card",
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
                    ns("gmm_uncertainty"),
                    height = "400px"
                  )
                )
              )
            ),
            h2("Cluster Profiles"),
            uiOutput(
              ns("gmm_profiles")
            )
          )
        }
      })
    })
}