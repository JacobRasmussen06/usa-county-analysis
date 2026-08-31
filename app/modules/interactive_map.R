library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(htmltools)

county <- county |> 
  mutate(voter_turnout = as.numeric(voter_turnout)) |>
  st_transform(4326)

detail_stat <- function(selected, variable){
  div(
    class = "county-detail-stat",
    span(get_label(variable)),
    strong(format_variable_value(selected[[variable]], variable))
  )
}

map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Interactive County Map"),
    p(
      "Explore U.S. counties! Hover a county to see its statistics, ",
      "or click it to see in depth information."),
    selectInput(
      ns("map_variable"),
      "Color map by: ",
      choices = variables,
      selected = "total_population"
    ),
    div(
      class = "leaflet-map-container",
      div(
        class = "leaflet-map-wrapper",
        leafletOutput(ns("county_map"), height = "800px")
      ),
      absolutePanel(
        top = 70,
        right = 20,
        fixed = FALSE,
        draggable = TRUE,
        uiOutput(ns("hover_card")))
    ),
    uiOutput(ns("county_detail"))
  )
}

map_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      selected_county <- reactiveVal(NULL)
      hover_id <- reactiveVal(NULL)
      observeEvent(input$county_map_shape_mouseover, {
        hover_id(input$county_map_shape_mouseover$id)
      })
      observeEvent(input$county_map_shape_mouseout, {
        hover_id(NULL)
      })
      output$county_map <- renderLeaflet({
        leaflet(county) |>
          addProviderTiles(
            providers$Esri.WorldGrayCanvas
          ) |>
          setView(
            lng = -96,
            lat = 38,
            zoom = 4
          ) |>
          addPolygons(
            layerId = ~GEOID,
            color = "#555555",
            weight = 0.5,
            fillColor = "#6baed6",
            fillOpacity = 0.6,
            highlightOptions = highlightOptions(
              weight = 2,
              color = "#000000",
              fillOpacity = 0.8,
              bringToFront = TRUE
            )
          )
      })
      hovered_county <- reactive({
        req(hover_id())
        selected <- county |>
          filter(GEOID == hover_id())
        req(nrow(selected) == 1)
        selected
      })
      observeEvent(input$county_map_shape_click, {
        click <- input$county_map_shape_click
        req(click$id)
        selected_county(click$id)
        selected <- county |>
          filter(GEOID == click$id)
        req(nrow(selected) == 1)
        bbox <- st_bbox(selected)
        leafletProxy("county_map", session = session) |>
          leaflet::invokeMethod(data = NULL, method = "invalidateSize") |>
          fitBounds(
            lng1 = as.numeric(bbox["xmin"]),
            lat1 = as.numeric(bbox["ymin"]),
            lng2 = as.numeric(bbox["xmax"]),
            lat2 = as.numeric(bbox["ymax"]),
            options = list(animate = TRUE, duration = 0.75)) |> 
          addPolygons(
            data = selected,
            layerId = "selected_highlight",
            color = "#2C6E9E",
            weight = 4,
            opacity = 1,
            fillOpacity = 0,
            label = ~county_name,
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "top",
              textOnly = TRUE,
              style = list(
                "font-weight" = "700",
                "font-size" = "16px",
                "color" = "#16324F",
                "text-shadow" = "0 0 3px white, 0 0 3px white"
              )
            ),
            options = pathOptions(interactive = FALSE)
          )
      })
      observeEvent(input$back_to_map, {
        selected_county(NULL)
        leafletProxy("county_map", session = session) |>
          removeShape(layerId = "selected_highlight") |>
          setView(lng = -96, lat = 38, zoom = 4)
      })
      output$hover_card <- renderUI({
        req(is.null(selected_county()))
        req(hover_id())
        selected <- hovered_county()
        div(
          class = "county-hover-card",
          div(
            class = "county-hover-title",
            selected$county_name),
          div(
            class = "county-hover-subtitle",
            "County profile"),
          div(
            class = "county-hover-stats",
            div(
              class = "county-hover-stat",
              span("Population"),
              strong(format(selected$total_population, big.mark = ","))),
            div(
              class = "county-hover-stat",
              span("Median Age"),
              strong(round(selected$median_age, 1))),
            div(
              class = "county-hover-stat",
              span("Median Income"),
              strong(
                paste0(
                  "$",
                  format(selected$median_household_income, big.mark = ",")))),
            div(
              class = "county-hover-stat",
              span("Diversity Index"),
              strong(round(selected$diversity_index, 2))),
            div(
              class = "county-hover-stat",
              span("Poverty Rate"),
              strong(
                paste0(
                  round(selected$poverty_rate * 100, 1),
                  "%"))),
            div(
              class = "county-hover-stat",
              span("Gini Index"),
              strong(round(selected$gini_index, 2))),
            div(
              class = "county-hover-stat",
              span("Political Competitiveness"),
              strong(round(selected$party_competitiveness_2020, 2)))),
          div(
            class = "county-hover-section",
            "GEOGRAPHY"),
          div(
            class = "county-hover-row",
            span("Terrain Ruggedness"),
            strong(round(selected$terrain_ruggedness, 2))),
          div(
            class = "county-hover-row",
            span("Forest Coverage"),
            strong(
              paste0(
                round(selected$forest_coverage_pct * 100, 1),
                "%"))),
          div(
            class = "county-hover-section",
            "CLUSTERING"),
          div(
            class = "county-hover-cluster",
            span("PCA"),
            strong(selected$pca_cluster_name)),
          div(
            class = "county-hover-cluster",
            span("HC"),
            strong(selected$hc_cluster_name)),
          div(
            class = "county-hover-cluster",
            span("GMM"),
            strong(selected$gmm_cluster_name)),
          div(
            class = "county-hover-uniqueness",
            span("Uniqueness"),
            strong(
              paste0(
                round(selected$uniqueness_score, 1),
                "%"))))
      })
      observeEvent(input$map_variable , {
        selected_variable <- input$map_variable
        values <- county[[selected_variable]]
        metadata <- variable_metadata |>
          filter(variable == selected_variable)
        variable_label <- metadata$label
        if (is.numeric(values)) {
          pal <- colorNumeric(
            palette = "plasma",
            domain = values,
            na.color = "#cccccc"
          )
        } else {
          pal <- colorFactor(
            palette = "Set2",
            domain = values,
            na.color = "#cccccc"
          )
        }
        leafletProxy("county_map") |>
          clearControls() |>
          clearShapes() |>
          addPolygons(
            data = county,
            layerId = ~GEOID,
            color = "#555555",
            weight = 0.5,
            fillColor = pal(values),
            fillOpacity = 0.7,
            highlightOptions = highlightOptions(
              weight = 2,
              color = "#000000",
              fillOpacity = 0.85,
              bringToFront = TRUE)) |>
          addLegend(
            position = "bottomright",
            pal = pal,
            values = values,
            title = variable_label
          )
      })
      output$county_detail <- renderUI({
        req(selected_county())
        selected <- county |> 
          filter(GEOID == selected_county())
        req(nrow(selected) == 1)
        div(
          class = "county-detail-panel",
          actionButton(
            session$ns("back_to_map"),
            "← Back to full map",
            class = "back-to-map"),
          div(
            class = "county-detail-card",
            div(
              class = "county-detail-title",
              selected$county_name),
            div(
              class = "county-detail-subtitle",
              "County profile"),
            h4("Demographics"),
            div(
              class = "county-detail-grid",
              detail_stat(selected, "total_population"),
              detail_stat(selected, "population_density"),
              detail_stat(selected, "median_age"),
              detail_stat(selected, "diversity_index"),
              detail_stat(selected, "under_18_pct"),
              detail_stat(selected, "over_65_pct"),
              detail_stat(selected, "average_household_size"),
              detail_stat(selected, "foreign_born_pct")),
            h4("Economics"),
            div(
              class = "county-detail-grid",
              detail_stat(selected, "median_household_income"),
              detail_stat(selected, "median_earnings"),
              detail_stat(selected, "poverty_rate"),
              detail_stat(selected, "unemployment_rate"),
              detail_stat(selected, "college_grad_pct"),
              detail_stat(selected, "median_home_value")),
            h4("Geography"),
            div(
              class = "county-detail-grid",
              detail_stat(selected, "land_area_sq_miles"),
              detail_stat(selected, "terrain_ruggedness"),
              detail_stat(selected, "forest_coverage_pct"),
              detail_stat(selected, "water_coverage_pct"),
              detail_stat(selected, "mean_temp"),
              detail_stat(selected, "annual_precip"),
              div(
                class = "county-detail-stat",
                span("Mean Elevation"),
                strong(paste0(scales::comma(round(selected$mean_elevation, 0)), " ft")))),
            h4("Politics"),
            div(
              class = "county-detail-grid",
              detail_stat(selected, "dem_vote_share_2020"),
              detail_stat(selected, "voter_turnout"),
              detail_stat(selected, "party_competitiveness_2020"),
              detail_stat(selected, "dem_swing_2000_2020")),
            h4("Clusters and Uniqueness"),
            div(
              class = "county-detail-clusters",
              div(
                span("PCA Cluster"),
                strong(selected$pca_cluster_name)),
              div(
                span("Hierarchical Cluster"),
                strong(selected$hc_cluster_name)),
              div(
                span("GMM Cluster"),
                strong(selected$gmm_cluster_name))),
            div(
              class = "county-detail-uniqueness",
              span("Uniqueness Score"),
              strong(paste0(
                round(selected$uniqueness_score, 1),
                "%"))),
            h4("Similar Counties"),
            div(
              class = "similar-counties-list",
              {
                similar <- most_similar |>
                  filter(source_GEOID == selected$GEOID) |>
                  left_join(
                    county |>
                      st_drop_geometry() |>
                      select(GEOID, county_name),
                    by = c("comparison_GEOID" = "GEOID")) |>
                  mutate(
                    similarity = paste0(
                      round(100 - distance, 1),
                      "% Similar"))
                req(nrow(similar) > 0)
                lapply(seq_len(nrow(similar)), function(i) {
                  div(
                    class = "similar-county-row",
                    div(
                      class = "similar-county-name",
                      similar$county_name[i]),
                    div(
                      class = "similar-county-score",
                      similar$similarity[i]))
                })
              }
            )
          )
        )
      })
    }
  )
}