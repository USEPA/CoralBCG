library(tidyverse)
library(leaflet)
library(RColorBrewer)

data(rel_abu)
data(crl_met)

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  ## dynamic UI

  # data to plot
  dat <- reactive({
    
    metric <- input$metric
    
    # select data
    names(crl_met)[names(crl_met) %in% metric] <- 'metric'
    toplo <- select(crl_met, station_code, habitat, longitude, latitude, metric)
    toplo
    
  })

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    pal <- brewer.pal(9, 'Reds')[3:9]
    colorNumeric(pal, dat()$metric)
  })
  
  colorpal2 <- reactive({
    pal <- brewer.pal(9, 'Set1')[1:5]
    colorFactor(pal, domain = unique(dat()$habitat))
  })
  
  # map for abundance
  output$map <- renderLeaflet({

    # base map for spp abu
    leaflet(crl_met) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    
  })
  
  # map for stations
  output$mapstat <- renderLeaflet({

    # base map for stations
    leaflet(crl_abu) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>% 
      addCircleMarkers(radius = 10, weight = 0, fillOpacity = 0, label = ~paste0('Station ', station_code),
        layerId = ~station_code
        ) %>% 
      addLabelOnlyMarkers(~longitude, ~latitude, 
        label = ~paste0(station_code), 
        labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T, textsize = '12px', 
          offset = c(0, -9))
      )
      
  })

  # points on abu map
  observe({
    
    byhab <- input$byhab
    pt_scl <- input$pt_scl
    pt_dif <- input$pt_dif
    
    prox <- leafletProxy("map", data = dat()) %>%
      clearMarkers() %>%
      clearControls()
  
    # fix colors
    if(byhab){
  
      pal <- colorpal2()
      
      prox %>% 
        addCircleMarkers(radius = ~ pt_scl * scales::rescale(metric, c(pt_scl, pt_scl * pt_dif)), 
          weight = 1, fillColor = ~pal(habitat), fillOpacity = 0.7, 
          label = ~paste0('Station ', station_code, '(', habitat, '): ', metric)
        ) %>% 
        addLegend(position = "bottomleft",
          pal = pal, values = ~habitat, title = 'Habitat type'
        )
        
    } else {
      
      pal <- colorpal()
      
      prox %>% 
        addCircleMarkers(radius = ~ pt_scl * scales::rescale(metric, c(pt_scl, pt_scl * pt_dif)), 
          weight = 1, fillColor = ~ pal(metric), fillOpacity = 0.7, 
          label = ~paste0('Station ', station_code, ': ', metric)
        ) %>% 
        addLegend(position = "bottomleft",
          pal = pal, values = ~metric, title = 'Count'
        )
      
    }
      
  })
  
# metric plot  
output$abuplot <- renderPlot({
    
    byhab <- input$byhab
    
    toplo <- dat() %>% 
      arrange(metric) %>% 
      mutate(station_code = factor(station_code, levels = station_code[order(metric)]))
    
    thm <- theme_minimal() +
      theme(legend.position = "none", 
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5), 
        axis.text.y = element_text(size = 12), 
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.line.x = element_line(), 
        axis.line.y = element_line(), 
        axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(.1, "cm")
        )
      
    if(byhab){
      
      p <- ggplot(toplo, aes(x = station_code, y = metric, fill = habitat)) + 
        geom_bar(stat = 'identity') + 
        scale_fill_manual(values = brewer.pal(9, 'Set1')[1:5]) +
        scale_x_discrete('Station number') + 
        ylab('Metric estimate') +
        facet_wrap(~ habitat, ncol = 5, scales = 'free_x') + 
        thm
      
    } else {   
      
      p <- ggplot(toplo, aes(x = station_code, y = metric, fill = metric)) + 
        geom_bar(stat = 'identity') + 
        scale_fill_gradientn(colours = brewer.pal(9, 'Reds')[3:9]) +
        scale_x_discrete('Station number') + 
        ylab('Metric estimate') +
        thm
      
    }
    
 
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

    toplo2 <- filter(dat(), station_code %in% selectedstation)

    leafletProxy("mapstat", data = toplo2) %>%
      clearGroup(group = 'selstat') %>%
      addCircleMarkers(radius = 10, weight = 0, fillOpacity = 0.7, color = 'red', group = 'selstat'
      )

  })

  # # species plots on site click
  # output$staplot <- renderPlot({
  # 
  #   # check if its been clicked
  #   validate(need(selectedstation, FALSE))
  # 
  #   toplo3 <- filter(crl_met, Station %in% selectedstation) %>%
  #     gather('Species', 'Count', -Station, -latitude, -longitude, -Richness)
  #   p <- ggplot(toplo3, aes(x = Species, y = Count)) +
  #     geom_bar(stat = 'identity') +
  #     scale_y_continuous('Coral counts') +
  #     theme_minimal() +
  #     theme(legend.position = "none",
  #       axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
  #       axis.title.x = element_blank(),
  #       axis.text.y = element_text(size = 12),
  #       axis.title.y = element_text(size = 14),
  #       axis.line.x = element_line(),
  #       axis.line.y = element_line(),
  #       axis.ticks.x = element_line(),
  #       axis.ticks.y = element_line(),
  #       axis.ticks.length = unit(.1, "cm")
  #       ) +
  #     ggtitle(paste('Station', selectedstation))
  # 
  #   print(p)
  # 
  # })
  
  
})