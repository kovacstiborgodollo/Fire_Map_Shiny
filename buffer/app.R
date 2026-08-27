#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Observe click events on the map
  observeEvent(input$map_click, {
    click <- input$map_click
    
    if (!is.null(click$lat) && !is.null(click$lng)) {
      # Zoom in around clicked coordinates
      leafletProxy("map") %>%
        removeShape("InitialHexagons6") |> 
        addPolygons(data = h3_7, weight = 0, 
                    fillColor = ~pal(fire_prob_7$fireprob_avg), 
                    opacity = 0.2, 
                    label = ~paste0(
                      "max: ", format(fire_prob_7$fireprob_max,digits = 2, nsmall = 2),
                      "\n", 
                      "avg: ", format(fire_prob_7$fireprob_avg,digits = 2, nsmall = 2)),
                    highlightOptions = highlightOptions(color = "red",      
                                                        weight = 0,         
                                                        bringToFront = TRUE),
                    layerId = "ZoomHexagons7") %>%
        setView(lng = click$lng, lat = click$lat, zoom = 10) %>%
        clearMarkers() %>%
        addMarkers(lng = click$lng, lat = click$lat,
                   popup = paste0("Lat: ", round(click$lat, 5),
                                  "<br>Lng: ", round(click$lng, 5)))
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
