#' Get dominance of acropora/orbicella genera
#'
#' @param dat_in coral demographic data with estimated csa (from \code{\link{est_3d}})
#' @param gen chr string of genera for sensitive/rare species
#'
#' @return data frame of relative abundance and csa of the genera for all stations
#' 
#' @import dplyr tidyr
#' 
#' @export
#'
#' @examples
#' crl_dem$csa <- with(crl_dem, est_3d(species_name, Height, MaxDiam, PerpDiam))
#' get_acrorb(crl_dem)
get_acrorb <- function(dat_in, gen = c('Acropora', 'Orbicella')){
  
  # sanity check
  if(!'csa' %in% names(dat_in))
    stop('csa must be in data, use est_3d function')
  
  # get relative abundance of all
  rel_abu <- get_rel_abu(dat_in)
  
  # collapse gen for regex
  gen <- paste0('^', gen) %>% 
    paste(., collapse = '|')
  
  # abundace of acrorb
  acrorb_abu <- group_by(rel_abu, station_code) %>% 
    summarise(
      acrorb_rel_abu = sum(rel_abu[grepl(gen, species_name)])
    )
  
  # 3d surf area of acrorb
  acrorb_csa <- mutate(dat_in, 
      csa = ifelse(csa == -1, 0, csa)
    ) %>% 
    group_by(station_code) %>% 
    summarise(
      acrorb_csa = sum(csa[grepl(gen, species_name)]),
      acrorb_csa = acrorb_csa/sum(csa), 
      acrorb_csa = ifelse(is.na(acrorb_csa), 0, acrorb_csa)
      )
  
  # Join the two  
  out <- full_join(acrorb_abu, acrorb_csa, by = 'station_code') %>% 
    gather('var', 'val', -station_code) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(out)
  
}