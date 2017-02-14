# estimate 3d surface area of a coral colony
# uses size conversion equations from appendix J
#
# spp chr string of coral species to estimate
# h numeric of colony height (cm)
# d1 numeric of colony diameter (cm), diameter of major axis if ellipse
# d2 numeric of colony diameter (dm), diameter of minor axis if ellipse
#
# if both d1 and d2 are provided, they are converted to a single value for the diameter of a circle with the same area as the ellipse
#
# NA values will be all that don't match species in conv, juveniles marked as -1
est_3d <- function(spp, h, d1, d2 = d1){
  
  data(conv)

  # sanity check
  stopifnot(class(spp) %in% 'character')
  
  # convert diameter to single value
  # only use if value is relevant
  d <- ifelse(d1 == -1, NA, sqrt(d1 * d2)) 

  # use size conversions if not in estfun
  csa <- data.frame(spec = spp, h = h, d = d, stringsAsFactors = FALSE) %>% 
    left_join(., conv, by = 'spec') %>% 
    rename(M = conv) %>% 
    mutate(
      csa = pi * ((h + d / 2) / 2) ^ 2 * M
      )

  # juvies back to -1
  csa$csa[d1 == -1] <- -1
  
  out <- csa$csa
  return(out)
  
}
