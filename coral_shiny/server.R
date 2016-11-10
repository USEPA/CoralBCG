library(tidyverse)
library(readxl)
library(ggmap)

data(crl_abu)

# Define server logic required to generate and plot data
shinyServer(function(input, output, session) {
  
  ## dynamic UI
  
  # run model
  runmod <- eventReactive(input$runmod, {
    
    # progress
    progress <- shiny::Progress$new(session, min=1, max=1)
    progress$set(message = 'FishTank is running...')
    on.exit(progress$close())

    # format parameter inputs 
    parsin <- form_parinps(input)

    # format initial condition inputs
    iniin <- form_iniinps(input)

    # p1z1 switch
    p1z1 <- input$p1z1

    # run model
    run_mod(pars = parsin, inps = iniin, out_var = NULL,  p1z1 = p1z1)
      
  })

  # first variable plot
  output$map <- renderPlot({
     
    # input
    left <- input$left
    bottom <- input$bottom
    right <- input$right
    top <- input$top
    log <- input$log
    spp <- input$spp
    
    # extent
    ext <- c(left, bottom, right, top)
    map <- get_stamenmap(ext, zoom = 10, maptype = "toner-lite")
    
    # select data
    names(crl_abu)[names(crl_abu) %in% spp] <- 'spp'
    toplo <- select(crl_abu, lon, lat, spp)
    
    ylab <- 'Richness'
    if(spp != 'Richness')
      ylab <- 'Abundance'
    
    p <- ggmap(map) + 
      geom_point(data = toplo, aes(x = lon, y = lat, size = spp, fill = spp), pch = 21, alpha = 0.8) + 
      scale_fill_distiller(palette = 'Spectral', direction = 1, guide = guide_legend(title = ylab)) +
      scale_size(range = c(1, 12), guide = guide_legend(title = ylab)) + 
      theme_bw() + 
      theme(
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()
      )
    
    print(p)
    
    })

})