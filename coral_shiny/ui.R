library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinythemes)

# data
data(rel_abu)
data(crl_met)

# variables
vrs <- names(crl_met)
vrs <- sort(vrs[!vrs %in% c('station_code', 'habitat', 'latitude', 'longitude')])

# Define UI for application
shinyUI(fluidPage(
  
  theme = shinytheme('journal'),

  tabsetPanel(
    
    tabPanel('By metric', 
      
      # metric selection, point sizes
      fluidRow(
        
        column(width = 12,HTML('<p><h3>Puerto Rico, NOAA 2014 stony coral surveys </h3></p>')),
        
        column(width = 3, 
          
          selectInput(inputId = 'metric', 
            label = h4('Select metric'),
            choices = vrs,
            selected = 'tot_rich'
            )
          
          ),
        
        column(width = 3, 
          
          sliderInput("pt_scl", 
            label = h4("Point scales"), 
            min = 1, 
            max = 10,
            value = 2, 
            step = 0.5
            )
          
          ), 
        
        column(width = 3, 
          
          sliderInput("pt_dif", 
            label = h4("Size difference"), 
            min = 1, 
            max = 10,
            value = 7, 
            step = 1
            )
          
          ), 
        
        column(width = 3, 
          h4('By habitat type'),
          checkboxInput('byhab', 
            label = NULL,
            value = FALSE,
            width = NULL
            )
          
          )
        
        ),
    
      leafletOutput("map", width = "100%", height = 500),
      
      # main panel
      column(width = 12,
        
        #spacing
        fluidRow(HTML('<p></p>')),
          
        plotOutput('abuplot', width='100%', height='300px')
          
      )

    )#,
    
    # tabPanel('By site',
    # 
    #   leafletOutput("mapstat", width = "100%", height = 500),
    # 
    #   # main panel
    #   column(width = 12,
    # 
    #     # spacing
    #     fluidRow(HTML('<p><h3>Click a station</h3></p>')),
    # 
    #     plotOutput('staplot', width='100%', height='300px')
    # 
    #   )
    # 
    # )
  
  )  

))