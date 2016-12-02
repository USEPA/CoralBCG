library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinythemes)

# data
data(crl_abu)

# variables
vrs <- names(crl_abu)
vrs <- sort(vrs[!vrs %in% c('Station', 'lat', 'lon')])

# Define UI for application
shinyUI(fluidPage(
  
  theme = shinytheme('journal'),

  tabsetPanel(
    
    tabPanel('By species', 
      
      leafletOutput("map", width = "100%", height = 500),
      
      # main panel
      column(width = 12,
          
        # spacing
        fluidRow(HTML('<p><h3>Puerto Rico, 2011 stony coral surveys </h3></p>')),
        
        #spacing
        fluidRow(HTML('<p></p>')),
          
        # first row of plots
        fluidRow(
          
          column(width = 4, 
            
            selectInput(inputId = 'spp', 
              label = h4('Select species'),
              choices = vrs,
              selected = 'Richness'
              ),
            
            sliderInput("pt_scl", 
              label = "Point scales", 
              min = 1, 
              max = 10,
              value = 2, 
              step = 0.5
              )
            
            ),
          
          column(width = 8, 
           
            plotOutput('abuplot', width='100%', height='300px')
            
            )
        
          )
    
        )
      
      ), 
    
    tabPanel('By site',
    
      leafletOutput("mapstat", width = "100%", height = 500),
      
      # main panel
      column(width = 12,
          
        # spacing
        fluidRow(HTML('<p><h3>Click a station</h3></p>')),
        
        plotOutput('staplot', width='100%', height='300px')
       
      )
    
    )
  
  )  

))