#' Estimate 3d surface area of a coral colony,
#'
#' @param spp chr string of coral species to estimate
#' @param h numeric of colony height (cm)
#' @param d1 numeric of colony diameter (cm), diameter of major axis if ellipse
#' @param d2 numeric of colony diameter (dm), diameter of minor axis if ellipse
#'
#' @details If both d1 and d2 are provided, they are converted to a single value for the diameter of a circle with the same area as the ellipse
#'
#' NA values will be all values that do not match species in \code{\link{conv}}, juveniles marked as -1
#'  
#' Uses size conversion equations from appendix J
#' 
#' @return Vector of values with estimated surface area as cm2 for each species, -1 is juvenile
#' 
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' with(crl_dem, est_3d(species_name, Height, MaxDiam, PerpDiam)) 
est_3d <- function(spp, h, d1, d2 = d1){
  
  # sanity check
  stopifnot(class(spp) %in% 'character')
  
  # convert diameter to single value
  # only use if value is relevant
  d <- ifelse(d1 == -1, NA, sqrt(d1 * d2)) 

  # use size conversions
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