# estimate 3d surface area of a coral colony
# uses eqns from Courtney et al. 2007, from table 3-3 Santavy et al. EPA field manual
# or size conversion equations from appendix J
# or simple sphere if not in the above
#
# spp chr string of coral species to estimate
# h numeric of colony height (cm)
# d1 numeric of colony diameter (cm), diameter of major axis if ellipse
# d2 numeric of colony diameter (dm), diameter of minor axis if ellipse
#
# if both d1 and d2 are provided, they are converted to a single value for the diameter of a circle with the same area as the ellipse
est_3d <- function(spp, h, d1, d2 = d1){
  
  data(conv)

  # sanity check
  stopifnot(class(spp) %in% 'character')
  
  # convert diameter to single value
  d <- sqrt(d1 * d2)
  
  # get size estimates
  est <- NA

  # use eqns if in estfun
  if(spp %in% names(estfun)){
    
    fun <- estfun[spp][[1]]
    est <- fun(h, d)
  
  }

  # use size conversions if not in estfun
  chkconv <- conv$spec %in% spp
  if(any(chkconv) & is.na(est)){
      
    M <- conv[chkconv, 'conv']
    est <- pi * ((h + d / 2) / 2) ^ 2 * M
      
  }
  
  # simple calc for surface area if none of the above
  # assumes a sphere
  other <- c("Agaricia grahamae", "Agaricia spp", "Helioceris cucullata", "Madracis auretenra", "Madracis senaria", "Madracis spp", "Meandrina jacksoni", "Orbicella annularis", "Orbicella faveolata", "Orbicella franksi", "Orbicella spp", "Pseudodiploria clivosa", "Pseudodiploria strigosa", "Scolymia cubensis", "Siderastrea radians", "Siderastrea spp", "Tubastraea coccinea")  
  if(spp %in% other)
    est <- 4 * pi * (d / 2) ^ 2 

  return(est)
  
}

# functions for estimating area for individual species
# used in est_3d
estfun <- list(
  'Acropora palmata' = function(h, d){
    0.846 * log(h) + 0.723 * log(d / 2) + 0.510 * log(h + d / 2) + 0.656
    },
  
  'Briareum spp' = function(h, d){
    0.341 * d ^ 3 + 11.2 * h - 127 
    },
  
  'Colpophyllia natans' = function(h, d){
    2 * pi * (h + d / 2)
    }, 
  
  'Dichocoenia stokesii' = function(h, d){
    0.904 * log(h) + 1.165 * log(d / 2) + 0.610
    }, 
  
  'Diploria labyrinthiformis' = function(h, d){
    0.904 * log(h) + 1.165 * log(d / 2) + 0.610
    }, 
  
  'Erythopodium spp' = function(h, d){
    d * h
    },
  
  'Eunicea fusca' = function(h, d){
    0.0288 * h ^ 3 + 939
    },
  
  'Eunicea tourneforti' = function(h, d){
    76.4 * d - 806
    },
  
  'Gorgonia flabellum' = function(h, d){
    0.0113 * h ^ 3 + 106 * d - 1190
    },
  
  'Gorgonia ventalina' = function(h, d){
    0.68 * h ^ 2 + 0.66 * d ^ 2- 3.61
    },
  
  'Leptogorgia spp' = function(h, d){
    0.68 * h ^ 2 + 0.66 * d ^ 2- 3.61
    },
  
  'Meandrina meandrites' = function(h, d){
    0.904 * log(h) + 1.165 * log(d / 2) + 0.610
    },
  
  'Muriceopsis flavida' = function(h, d){
    4.77 * h ^ 2 - 2990  
    }, 
  
  'Plexaura spp' = function(h, d){
    1.46 * d ^ 2 + 399
    },
  
  'Porites astreoides' = function(h, d){
    0.846 * log(h) + 0.723 * log(d / 2) + 0.510 * log(h + d / 2) + 0.656
    }, 
  
  'Porites porites' = function(h, d){
    0.846 * log(h) + 0.723 * log(d / 2) + 0.510 * log(h + d / 2) + 0.656
    }, 
  
  'Pseudopterogorgia spp' = function(h, d){
    4.77 * h ^ 2 - 2990
    },
  
  'Pterogorgia spp' = function(h, d){
    -0.479 * h ^ 3 + 3.37 * h ^ 2 - 51.3 * h + 354
    },
  
  'Pterogorgia guadalupensi' = function(h, d){
    0.0672 * d ^ 3 + 1610
    },
  
  'Siderastrea siderea' = function(h, d){
    0.904 * log(h) + 1.165 * log(d / 2) + 0.610
    }, 
  
  'Stephanocoenia intersepta' = function(h, d){
    0.904 * log(h) + 1.165 * log(d / 2) + 0.610
    }
)
  
