#' Percent live coral at all stations
#'
#' @param dat_in coral demographic data with estimated csa (from \code{\link{est_3d}})
#'
#' @return data frame with one estimate per station
#' 
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' crl_dem$csa <- with(crl_dem, est_3d(species_name, Height, MaxDiam, PerpDiam))
#' get_lcsa(crl_dem)
get_lcsa <- function(dat_in){
  
  # sanity check
  if(!'csa' %in% names(dat_in))
    stop('csa must be in data, use est_3d function')
  
  # coral live surface area
  out <- select(dat_in, station_code, species_name, ColonyID, MortOld, MortNew, csa) %>% 
    group_by(station_code, species_name, ColonyID) %>% 
    filter(!is.na(csa) & csa > -1) %>% 
    mutate(lcsa = csa * (1 - sum(MortOld + MortNew) / 100)) %>% 
    group_by(station_code) %>% 
    summarise(lcsa = sum(lcsa, na.rm = T) / sum(csa, na.rm = T)) %>% 
    gather('var', 'val', -station_code) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(out)
  
}