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

if (interactive()){
  navbarPage("Emission Saving", id="nav",
             
             tabPanel("Interactive map",
                      
                          
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map", width="100%", height="100%"),
                          
                          # Shiny versions prior to 0.11 should use class = "modal" instead.
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("ZIP explorer"),
                                        
                                        selectInput("color", "Color", vars),
                                        selectInput("size", "Size", vars, selected = "adultpop"),
                                        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                         # Only prompt for threshold when coloring or sizing by superzip
                                                         numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                        ),
                                        
                                        plotOutput("histCentile", height = 200),
                                        plotOutput("scatterCollegeIncome", height = 250)
                          ),
                          
                          tags$div(id="cite",
                                   'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960-2010'), ' by Charles Murray (Crown Forum, 2012).'
                          )
                      ),

             
             tabPanel("Data explorer",
                      
             ),
             
             conditionalPanel("false", icon("crosshair"))
  )
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    
  }
}





# Run the application 
shinyApp(ui = ui, server = server)
