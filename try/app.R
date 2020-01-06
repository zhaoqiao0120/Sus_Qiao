#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(sf)          # classes and functions for vector data
library(spData)        # load geographic data
library(dplyr)
library(sp)
#library(stringr) # for working with strings (pattern matching)
library(spDataLarge)   # load larger geographic data
library(stplanr)      # geographic transport data package
#library(tmap) 
library(mapview)
library(shiny)

car <- st_read("car_route.shp")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Potential of Emissions Saving"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(2,
                 sliderInput("elev", h3("Elevation Gain"),
                             min = 0, max = 60, value = 30)),
          column(4,
          )
        ),
        fluidRow(
          column(2,
                 sliderInput("ratio", h3("Cycling time/Driving time"),
                             min = 0.5, max = 2, value = 1.2))),
        fluidRow(
          column(2,
                 sliderInput("dist", h3("Cycling Distance (km)"),
                             min = 1.0, max = 10.0, value = 4.6))),
      )
      ),
    mainPanel(plotOutput("map"))
    )
    

server <- function(input, output) {
  output$map <- renderPlot({
    best <- car[car$time_ratio <=input$ratio & car$cyc_dist <= input$dist & car$gain_elev <= input$elev,]
    best_ln = SpatialLinesNetwork(best)
    edge_best = igraph::edge_betweenness(best_ln@g)
    mapview(best_ln@sl$geometry,lwd=edge_best / 3000,color = "chocolate", alpha = 0.8)
  })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
