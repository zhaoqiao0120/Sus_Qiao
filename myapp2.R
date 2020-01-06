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
load(file="Data/split.Rdata")

if (interactive()){
  # Define UI for application that draws a histogram
  ui <- dashboardPage( skin = 'black',
                       dashboardHeader(title = 'Carbon Emission'),
                       dashboardSidebar(
                         sidebarMenu(disable=TRUE,
                                     menuItem('Emission', tabName = 'leaflet', icon = icon('book'))
                         )),
                       dashboardBody(mainPanel(h2("Potentially Cyclable Trip")),
                                     useShinyjs(),
                                     tabItems(
                                       tabItem(tabName = 'leaflet',
                                               fluidRow(
                                                 column(3,
                                                        radioButtons("radio1",label = "",
                                                                     choices = list("Scenario 1" = 1,"Scenario 2" = 2, "Play" = 3),selected = 3))
                                               ),
                                               
                                               fluidRow(
                                                 column(6,box(leafletOutput("map1", width="100%", height=400),
                                                              width = 12)),
                                                 column(6,box(leafletOutput("map2", width="100%", height=400),
                                                              width = 12))
                                               ),
                                               
                                               fluidRow(
                                                 column(2,
                                                        sliderInput("elev", h3("Elevation Gain"),
                                                                    min = 0, max = 60, value = 30)),
                                                 column(2,
                                                        sliderInput("ratio", h3("Cycling/Driving Time"),
                                                                    min = 0.5, max = 2, value = 1.2)),
                                                 column(2,
                                                        sliderInput("dist", h3("Cycling Distance (km)"),
                                                                    min = 1.0, max = 10.0, value = 4.6)),
                                                 column(2,
                                                        sliderInput("elev2", h3("Elevation Gain"),
                                                                    min = 0, max = 60, value = 30)),
                                                 column(2,
                                                        sliderInput("ratio2", h3("Cycling/Driving Time"),
                                                                    min = 0.5, max = 2, value = 1.2)),
                                                 column(2,
                                                        sliderInput("dist2", h3("Cycling Distance (km)"),
                                                                    min = 1.0, max = 10.0, value = 4.6))
                                               ),
                                               
                                               h2("Emissions Saving by Origin"),
                                               fluidRow(
                                                 column(6,box(leafletOutput("map3", width="100%", height=400),
                                                              width = 12)),
                                                 column(6,box(leafletOutput("map4", width="100%", height=400),
                                                              width = 12))
                                               ),
                                       )
                                     )
                       )
  )
  
  
  
  
  server <- function(input, output, session) {
    output$map1 <- renderLeaflet({
      m <- mapview(CT_split,zcol=("Car"),alpha = 0.8)
      m@map
    })
    output$map2 <- renderLeaflet({
      m <- mapview(CT_split,zcol=("Active"),alpha = 0.8)
      m@map
    })
    output$map3 <- renderLeaflet({
      m <- mapview(Island,alpha = 0.8)
      m@map
    })
    output$map4 <- renderLeaflet({
      m <- mapview(Island,alpha = 0.8)
      m@map
    })
    
    #best <- car[car$time_ratio <=input$ratio & car$cyc_dist <= input$dist & car$gain_elev <= input$elev,]
    #best_ln = SpatialLinesNetwork(best)
    #edge_best = igraph::edge_betweenness(best_ln@g)
    observe({
      sco <- input$radio1
      if (sco == 1){
        updateSliderInput(session,"elev",value =30)
        updateSliderInput(session,"dist",value =4.6)
        updateSliderInput(session,"ratio",value =2)
        disable("elev")
        disable("dist")
        disable("ratio")
        
        
        #updateSliderInput(session,"elev2",value =30)
        #updateSliderInput(session,"dist2",value =4.6)
        #updateSliderInput(session,"ratio2",value =2)
        #disable("elev2")
        #disable("dist2")
        #disable("ratio2")
        output$map1 <- renderLeaflet({
          edge_best1 = igraph::edge_betweenness(car_1_ln@g)
          m <- mapview(car_1_ln@sl$geometry,lwd=edge_best1 / 3000,color = "chocolate", alpha = 0.8, layer.name="Scenario 1 - Cycable Trip")
          m@map
        })
        output$map3 <- renderLeaflet({
          orange = colorRampPalette(c("#ffffd4", "#fed98e","#fe9929","#e31a1c"))
          m <- mapview(save_1_joined,zcol = ("Saving"), aplha.regions = 1, at = quantile(save_1_joined[which(!is.na(save_1_joined$Saving)),]$Saving),
                       legend = TRUE, layer.name="Scenario 1 - Saving", col.regions = orange)
          m@map
        })
        

      }
      else if (sco ==2) 
      {#updateSliderInput(session,"elev",value =30)
        #updateSliderInput(session,"dist",value =4.6)
        #updateSliderInput(session,"ratio",value =1.2)
        #disable("elev")
        #disable("dist")
        #disable("ratio")
        
        updateSliderInput(session,"elev2",value =30)
        updateSliderInput(session,"dist2",value =4.6)
        updateSliderInput(session,"ratio2",value =1.2)
        disable("elev2")
        disable("dist2")
        disable("ratio2")
        
        output$map2 <- renderLeaflet({
          edge_best2 = igraph::edge_betweenness(car_2_ln@g)
          m <- mapview(car_2_ln@sl$geometry,lwd=edge_best2 / 3000,color = "purple", alpha = 0.8, layer.name="Scenario 2 - Cycable Trip")
          m@map
        })
        output$map4 <- renderLeaflet({
          purple = colorRampPalette(c("#edf8fb", "#b3cde3","#8c96c6","#88419d"))
          m <- mapview(save_2_joined,zcol = ("Saving"),alpha.regions = 1,at = quantile(save_2_joined[which(!is.na(save_2_joined$Saving)),]$Saving),
                       legend = TRUE, layer.name="Scenario 2 - Saving", col.regions = purple)
          m@map
        })}
      
      
      else{
        enable("elev")
        enable("dist")
        enable("ratio")
        
        enable("elev2")
        enable("dist2")
        enable("ratio2")
      }
      
    })
    
  
  }
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
