library(tidyverse)
library(leaflet)
library(RColorBrewer)

data(crl_abu)

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  ## dynamic UI

  # data to plot
  dat <- reactive({
    
    spp <- input$spp
    
    # select data
    names(crl_abu)[names(crl_abu) %in% spp] <- 'spp'
    toplo <- select(crl_abu, Station, lon, lat, spp)
    toplo
    
  })

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    pal <- brewer.pal(11, 'Spectral')
    colorNumeric(pal, dat()$spp)
  })
  
  # map for abundance
  output$map <- renderLeaflet({

    # base map for spp abu
    leaflet(crl_abu) %>% addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
    
  })
  
  # map for stations
  output$mapstat <- renderLeaflet({

    # base map for stations
    leaflet(crl_abu) %>% addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>% 
      addCircleMarkers(radius = 10, weight = 0, fillOpacity = 0, label = ~paste0('Station ', Station),
        layerId = ~Station
        ) %>% 
      addLabelOnlyMarkers(~lon, ~lat, 
        label = ~paste0(Station), 
        labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T, textsize = '12px', 
          offset = c(0, -9))
      )
      
  })

  # points on abu map
  observe({
    
    pal <- colorpal()
    pt_scl <- input$pt_scl

    leafletProxy("map", data = dat()) %>%
      clearMarkers() %>%
      clearControls() %>% 
      addCircleMarkers(radius = ~ pt_scl * spp, weight = 1, color = "#777777",
        fillColor = ~pal(spp), fillOpacity = 0.7, label = ~paste0('Station ', Station, ': ', spp)
      ) %>% 
      addLegend(position = "bottomleft",
        pal = pal, values = ~spp, title = 'Count'
      )
    
  })
  
  output$abuplot <- renderPlot({
    
    toplo <- dat() %>% 
      arrange(spp) %>% 
      mutate(Station = factor(Station, levels = Station[order(spp)]))
    
    p <- ggplot(toplo, aes(x = Station, y = spp, fill = spp)) + 
      geom_bar(stat = 'identity') + 
      theme_minimal() +
      scale_fill_gradientn(colours = brewer.pal(11, 'Spectral')) +
      ylab('Coral counts at each station') + 
      theme(legend.position = "none", 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(.1, "cm")
        )
    
    print(p)
    
  })
  
  ## get station on map click
  
  # bindings for station click 
  makeReactiveBinding('selectedstation')
  
  # Set selected station when marker is clicked
  observeEvent(input$mapstat_marker_click, {
    selectedstation <<- input$mapstat_marker_click$id
  })
  
  # Clear the selected station when map background is clicked
  observeEvent(input$mapstat_click, {
    selectedstation <<- NULL
  })
  
  # change selected station to red
  observe({
    
    # check if its been clicked
    validate(need(selectedstation, FALSE)) 

    toplo2 <- filter(dat(), Station %in% selectedstation)
    
    leafletProxy("mapstat", data = toplo2) %>%
      clearGroup(group = 'selstat') %>% 
      addCircleMarkers(radius = 10, weight = 0, fillOpacity = 0.7, color = 'red', group = 'selstat'
        ) 

  })   
  
  # species plots on site click
  output$staplot <- renderPlot({
    
    # check if its been clicked
    validate(need(selectedstation, FALSE)) 

    toplo3 <- filter(crl_abu, Station %in% selectedstation) %>% 
      gather('Species', 'Count', -Station, -lat, -lon, -Richness) 
    p <- ggplot(toplo3, aes(x = Species, y = Count)) + 
      geom_bar(stat = 'identity') + 
      scale_y_continuous('Coral counts') + 
      theme_minimal() +
      theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(.1, "cm")
        ) + 
      ggtitle(paste('Station', selectedstation))
    
    print(p)
    
  })
  
  
})