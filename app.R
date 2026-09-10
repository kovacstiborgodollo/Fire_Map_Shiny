
library(shiny)
library(sf)
library(leaflet)
library(leafem)
library(RColorBrewer)
library(leaflet.extras)
library(dplyr)

library(h3jsr)         # H3 geospatial indexing

fire_prob_6 <- readRDS("data/fire_prob_6.rds")
fire_prob_8 <- readRDS("data/fire_prob_8.rds")

h3_6 <- readRDS("data/h3_6.rds")
h3_8 <- readRDS("data/h3_8.rds")

h3_8 <- h3_8 %>% bind_cols(fire_prob_8 %>% select(h3_8_address))

bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
pal_6 <- colorBin("YlOrRd", domain = fire_prob_6$fireprob_avg, bins = bins)
pal_8 <- colorBin("YlOrRd", domain = fire_prob_8$fireprob, bins = bins)

STADIA_API_KEY <- "10723f7e-133c-48a1-9228-9ad8c5ca7c83"


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      h2 {
        font-size: 28px;       /* Change this value as needed */
        font-weight: bold;     /* Optional: make it bold */
        color: #2c3e50;        /* Optional: change text color */
        margin-top: 10px;      /* Optional: adjust spacing */
      }
    "))
  ),
  

    # Application title
    titlePanel(title = div(img(src = "corvinus-university-budapest.png", width = 80), "Fire susceptibility assessment in the Carpathians using an interpretable framework")),

    p(style = "color:black; font-size:12px;",
    "Scientific Reports volume 15, Article number: 30207 (2025) "),
  
    HTML("<p><a href='https://www.nature.com/articles/s41598-025-10296-4'>https://www.nature.com/articles/s41598-025-10296-4</a></p>"),
  
    p(style = "color:blue; font-size:12px;",
      "Abstract: "),
    
    p(style = "color:black; font-size:10px;",
      "Climate change endangers the Carpathian region by increasing the risk of fires. In response, our study provides a harmonised dataset with twenty-seven variables and develops an interpretable machine learning-based framework for assessing fire susceptibility across all seven countries of the region. We applied a two-stage process: first, using various feature selection techniques to refine predictors before the modeling phase, and second, utilising the SHAP framework to interpret model predictions. Between these steps, advanced machine learning models were optimised and trained in the H2O environment, demonstrating high predictive accuracy. Our findings revealed eight fire susceptibility clusters. The resulting dataset, susceptibility maps, and detailed interpretative insights serve as a valuable resource for local communities and policy-makers in the region."),
  
  # this part sets the tabs that take through the user the process of ....
  
    mainPanel(
    
    tabsetPanel(type = "tabs",
                
                # Inspect the file that was loaded by selecting columns
                
                  tabPanel("Overview",
                        h2('Overview'),
                        actionButton("reset_button", "Reset view"),
                        leafletOutput("map", height = "600px")
                  ), 
                
                  tabPanel("Detail", 
                         h2("Detail"),
                         leafletOutput("map_detail", height = "600px")
                  ) 
                )

    )
)


server <- function(input, output, session) {
  
  initial_lat = 47
  initial_lng = 23
  initial_zoom = 6      
  
  resolution <- 8         # H3 resolution (higher = smaller hexagons)
  no_of_rings <- 20     
  
  # Initial map
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$Stadia.StamenTonerLite,  # You can change to other Stadia styles
                       options = providerTileOptions(
                         apiKey = STADIA_API_KEY
                       ))

  })
  
  observe({
    input$reset_button
    leafletProxy("map") %>% 
      addMouseCoordinates() %>% 
      addPolygons(data = h3_6, weight = 0, 
                  fillColor = ~pal_6(fire_prob_6$fireprob_avg), 
                  opacity = 0.2, 
                  label = ~paste0(
                    "max: ", format(fire_prob_6$fireprob_max,digits = 2, nsmall = 2),
                    "\n", 
                    "avg: ", format(fire_prob_6$fireprob_avg,digits = 2, nsmall = 2)),
                  highlightOptions = highlightOptions(color = "red",      
                                                      weight = 0,         
                                                      bringToFront = TRUE)
                  ) %>% 
      addLegend(pal = pal_6,
                values = fire_prob_6$fireprob_avg, 
                position = "bottomright", title = "Avg Fire Probability"
                ) |> 
      setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom)
  })
  



  # Observe click events on the map
  observeEvent(input$map_click, {
    click <- input$map_click
    
    if (is.null(click))
      return()
    
    click_coords <- c(click$lng, click$lat)
    
    if (!is.null(click$lat) && !is.null(click$lng)) {
      # Zoom in around clicked coordinates
        
        leafletProxy("map") %>%
        setView(lng = click$lng, lat = click$lat, zoom = 11) %>%
        clearMarkers() %>%
        addMarkers(lng = click$lng, lat = click$lat,
                   popup = paste0("Lat: ", round(click$lat, 5),
                                  "<br>Lng: ", round(click$lng, 5)))
      
        
        
    }
    

    center_h3 <- eventReactive(input$map_click, {
      h3jsr::point_to_cell(c(click$lng, click$lat), res = resolution)
    })
    
    output$value <- renderText({center_h3()})
    
    h3_8_filtered <- eventReactive(input$map_click, {
      cells_within_radius <- unlist(get_disk(center_h3(), ring_size = no_of_rings))
      h3_8_filt <- h3_8 %>% dplyr::filter(h3_8_address %in% cells_within_radius)
      h3_8_filt
      })
    
    fire_prob_8_filtered <- eventReactive(input$map_click, {
      cells_within_radius <- unlist(get_disk(center_h3(), ring_size = no_of_rings))
      fire_prob_8_filt <- fire_prob_8 %>% dplyr::filter(h3_8_address %in% cells_within_radius)
      fire_prob_8_filt
    })

    output$map_detail <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$Stadia.StamenTonerLite,  # You can change to other Stadia styles
                           options = providerTileOptions(
                             apiKey = STADIA_API_KEY
                           )) %>%
          setView(lng = click$lng, lat = click$lat, zoom = 11) %>%
          clearMarkers() %>%
          addPolygons(data = h3_8_filtered(), weight = 0, 
                    fillColor = ~pal_8(fire_prob_8_filtered()$fireprob), 
                    opacity = 0.2, 
                    label = ~paste0("fire_prob: ", format(fire_prob_8_filtered()$fireprob,digits = 2, nsmall = 2)),
                    highlightOptions = highlightOptions(color = "red",      
                                                        weight = 0,         
                                                        bringToFront = TRUE)
          ) %>% 
          addLegend(pal = pal_6,
                  values = fire_prob_8_filtered()$fireprob, 
                  position = "bottomright", title = "Fire Probability"
          ) %>%
          addMarkers(lng = click$lng, lat = click$lat,
                   popup = paste0("Lat: ", round(click$lat, 5),
                                  "<br>Lng: ", round(click$lng, 5)))
    
    })
    
  })
  

}





# Run the application 
shinyApp(ui = ui, server = server)
