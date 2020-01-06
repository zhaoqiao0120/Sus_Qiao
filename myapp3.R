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
library(shinydashboard)
library(leaflet)
library(shinyjs)
car <- st_read("car_route.shp")
load(file="Data/CT.Rdata")
load(file="Data/scenario1.Rdata")
load(file="Data/scenario2.Rdata")
load(file="Data/Car1.Rdata")
load(file="Data/Car2.Rdata")


  ui <- navbarPage("Emission Saving", id="nav",
             
             tabPanel("Interactive map",
                      
                      tags$head(
                        # Include our custom CSS
                        includeCSS("styles.css")),
                        
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Emissions"),
                                        
                                        radioButtons("radio1",label = "",
                                                     choices = list("Scenario 1" = 1,"Scenario 2" = 2, "Play" = 3),selected = 3),
                                        sliderInput("elev", h3("Elevation Gain"),min = 0, max = 60, value = 30),
                                        sliderInput("ratio", h3("Cycling/Driving Time"),min = 0.5, max = 2, value = 1.2),
                                        sliderInput("dist", h3("Cycling Distance (km)"),min = 1.0, max = 10.0, value = 4.6)),
                                        leafletOutput("map1", height="10%")
                          ),

             
             tabPanel("Data explorer",
                      
             ),
             
             conditionalPanel("false", icon("crosshair"))
  )
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    output$map <- renderLeaflet({
      m <- mapview(Island,alpha = 0.8)
      m@map
    })
    
  }


# Run the application 
shinyApp(ui = ui, server = server)
