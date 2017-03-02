#' Get total alpha diversity
#'
#' @param dat_in coral demographic data, does not need estimated surface area
#'
#' @return data frame with one row per station, total richness, and estimated diversity for all species
#' 
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' get_tot_div(crl_dem)
get_tot_div <- function(dat_in){
  
  # get relative abundance of all
  rel_abu <- get_rel_abu(dat_in)
  
  # get rel_abu of sens/rare
  out <- group_by(rel_abu, station_code) %>% 
    summarise(
      tot_rich = sum(species_name %in% conv$spec),
      tot_div = vegan::diversity(spp_abu)
    ) %>% 
    gather('var', 'val', -station_code) %>% 
    data.frame(., stringsAsFactors = FALSE)

  return(out)
  
}

