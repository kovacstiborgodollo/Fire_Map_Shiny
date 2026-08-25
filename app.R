#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(sf)
library(leaflet)
library(leafem)
library(RColorBrewer)
library(leaflet.extras)  

fire_prob_6 <- readRDS("data/fire_prob_6.rds")
fire_prob_7 <- readRDS("data/fire_prob_7.rds")
fire_prob_8 <- readRDS("data/fire_prob_8.rds")

h3_6 <- readRDS("data/h3_6.rds")
h3_7 <- readRDS("data/h3_7.rds")
h3_8 <- readRDS("data/h3_8.rds")

bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
pal <- colorBin("YlOrRd", domain = fire_prob_6$fireprob_avg, bins = bins)



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

    sliderInput("h3_resolution", 
                label = "Resolution",
                min = 6, max = 8, step = 1,
                value = 6),
    
    # Sidebar with a slider input for number of bins 
    leafletOutput("map", height = "600px")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$Stadia.StamenTonerLite) %>% 
      addMouseCoordinates() %>% 
      addPolygons(data = h3_6, weight = 0, 
                  fillColor = ~pal(fire_prob_6$fireprob_avg), 
                  opacity = 0.2, 
                  label = ~paste0(
                    "max: ", format(fire_prob_6$fireprob_max,digits = 2, nsmall = 2),
                    "\n", 
                    "avg: ", format(fire_prob_6$fireprob_avg,digits = 2, nsmall = 2)),
                  highlightOptions = highlightOptions(color = "red",      
                                                      weight = 0,         
                                                      bringToFront = TRUE 
                  )) %>% 
      addLegend(pal = pal,
                values = fire_prob_6$fireprob_avg, 
                position = "bottomright", title = "Avg Fire Probability") %>% 
      setView(lng = 23, lat = 47, zoom = 6)
  })
}

observeEvent(input$map_click, {
  click <- input$map_click
  print(paste("Latitude:", click$lat, "Longitude:", click$lng, "Zoom level", click$zoom))
})

# Run the application 
shinyApp(ui = ui, server = server)
