#' Diversity of sensitive and rare species
#'
#' @param dat_in coral demographic data, does not need estimated surface area
#' @param gen chr string of genera for sensitive/rare species
#'
#' @return data frame with one row per station, richness of sens/rare species, relative abundance of sense/rare species, and estimated diversity
#' 
#' @details diversity is estimated with \code{\link[vegan]{diversity}}
#' 
#' @import dplyr tidyr
#' 
#' @export
#'
#' @examples
#' get_sr_div(crl_dem)
get_sr_div <- function(dat_in, gen = c('Eusmilia', 'Isophyllastrea', 'Isophyllia', 'Mycetophyllia', 'Scolymia')){
  
  # get relative abundance of all
  rel_abu <- get_rel_abu(dat_in)
  
  # collapse gen for regex
  gen <- paste0('^', gen) %>% 
    paste(., collapse = '|')

  # get rel_abu of sens/rare
  out <- group_by(rel_abu, station_code) %>% 
    summarise(
      sr_rich = sum(grepl(gen, species_name)),
      sr_rel_abu = sum(rel_abu[grepl(gen, species_name)]),
      sr_div = vegan::diversity(spp_abu[grepl(gen, species_name)])
    ) %>% 
    gather('var', 'val', -station_code) %>% 
    data.frame(., stringsAsFactors = FALSE)

  return(out)
  
}