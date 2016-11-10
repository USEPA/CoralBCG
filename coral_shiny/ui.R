library(shiny)
library(shinythemes)

# data
data(crl_abu)

# variables
vrs <- names(crl_abu)
vrs <- sort(vrs[!vrs %in% c('Station', 'lat', 'lon')])

# Define UI for application
shinyUI(fluidPage(
  
  theme = shinytheme('journal'), 
  
  # main panel
  mainPanel(width = 12,
      
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
          )
        ),
      
      column(width = 2, 
        numericInput(inputId = 'left', 
          label = h4('left'),
          value = -67.43174
          )
        ),
      
      column(width = 2, 
        numericInput(inputId = 'bottom', 
          label = h4('bottom'),
          value = 17.57098
          )
        ),
      
      column(width = 2, 
        numericInput(inputId = 'right', 
          label = h4('right'),
          value = -65.53003
          )
        ),
      
      column(width = 2, 
        numericInput(inputId = 'top', 
          label = h4('top'),
          value = 18.58847
          )
        ),
      
      column(width = 12, 
        
        plotOutput("map", width = '1200px', height = '750px')
        
        )
      
      ) 
    
    )
        
))