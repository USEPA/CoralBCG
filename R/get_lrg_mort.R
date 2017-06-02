#' Get mortality of large reef building genera
#'
#' @param dat_in coral demographic data with esimated csa (from \code{\link{est_3d}})
#' @param gen chr string of large genera
#'
#' @return data frame with counts of large genera (abundance as number of individuals/colonies) and one estimate of mortality per station estimated as average of individual mortality from 0 - 1
#' 
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' crl_dem$csa <- with(crl_dem, est_3d(species_name, Height, MaxDiam, PerpDiam))
#' get_lrg_mort(crl_dem)
get_lrg_mort <- function(dat_in, gen = c('Acropora', 'Colpophyllia', 'Dendrogyra', 'Orbicella', 'Pseudodiploria')){
  
  # sanity check
  if(!'csa' %in% names(dat_in))
    stop('csa must be in data, use est_3d function')
  
  # collapse gen for regex
  gen <- paste0('^', gen) %>% 
    paste(., collapse = '|')
  
  # mortality of large reef-building corals
  out <- select(dat_in, station_code, species_name, ColonyID, MortOld, MortNew, csa) %>%
    group_by(station_code, species_name, ColonyID) %>% 
    filter(!is.na(csa) & csa > 0) %>% 
    mutate(
      lcsa = csa * (1 - MortNew / 100),
      lrg_pa = ifelse(grepl(gen, species_name), 1, 0)
      ) %>% 
    group_by(station_code) %>% 
    summarize(
      lrg_abu = sum(lrg_pa),
      lrg_mort = 1 - mean(lcsa[lrg_pa == 1]/csa[lrg_pa == 1]),
      lrg_mort = ifelse(is.nan(lrg_mort), NA, lrg_mort)
    ) %>% 
    ungroup %>% 
    gather('var', 'val', -station_code) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(out)
  
}