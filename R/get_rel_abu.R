#' Coral relative abundance
#'
#' @param dat_in raw coral demographic data
#'
#' @return data frame of species abundance and relative abundance by station
#' 
#' @details abundance is simply a count of species at a station
#' 
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' get_rel_abu(crl_dem)
get_rel_abu <- function(dat_in){
  
  # relative abundance
  out <- select(dat_in, station_code, species_name, ColonyID) %>% 
    group_by(station_code, species_name) %>% 
    summarise(
      spp_abu = length(species_name)
    ) %>% 
    group_by(station_code) %>% 
    mutate( 
      rel_abu = spp_abu/sum(spp_abu)
    ) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(out)

}