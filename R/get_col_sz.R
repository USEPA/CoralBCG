#' Get colony size distributions
#' 
#' @param dat_in coral demographic data with esimated csa per species (from \code{\link{est_3d}})
#' @param raw_out logical if raw data are returned as an abbreviated version of \code{dat_in}
#' 
#' @details Frequency distribution of colony sizes is summarized as a single value that takes into account the number of colonies and the relative sizes.  This is analogous to the standard Shannon-Wiener Diversity index for species abundance data.  
#' 
#' @return data frame of station, size distribution estimates, and number of colonies as recruits, long format
#' 
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' crl_dem$csa <- with(crl_dem, est_3d(species_name, Height, MaxDiam, PerpDiam))
#' get_col_sz(crl_dem)
get_col_sz <- function(dat_in, raw_out = FALSE){
  
  # sanity check
  if(!'csa' %in% names(dat_in))
    stop('csa must be in data, use est_3d function')
  
  # get raw data
  out <- select(dat_in, station_code, species_name, ColonyID, MortOld, MortNew, csa) %>% 
    filter(!is.na(csa)) %>% 
    mutate(
      Recruits = ifelse(csa < 0, 1, 0)
    )  %>% 
    select(station_code, species_name, ColonyID, csa, Recruits) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  # return raw data
  if(raw_out) return(out)

  # summarise size distributions and recruits counts
  out <- tidyr::gather(out, 'var', 'val', csa:Recruits) %>% 
    filter(!(var %in% 'csa' & val == -1)) %>% 
    spread(var, val) %>% 
    group_by(station_code) %>% 
    summarise(
      sz_dist = vegan::diversity(na.omit(csa)), 
      recruits = sum(Recruits, na.rm = TRUE)
    ) %>% 
    gather('var', 'val', -station_code) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(out)
  
}