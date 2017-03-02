#' Get colony size distributions
#' 
#' @param dat_in coral demographic data with esimated csa per species (from \code{\link{est_3d}})
#'
#' @return data frame of station and colonies with indicator if recruits present and total csa (cm2)
#' 
#' @import dplyr
#' 
#' @export
#'
#' @examples
get_col_sz <- function(dat_in){
  
  # sanity check
  if(!'csa' %in% names(dat_in))
    stop('csa must be in data, use est_3d function')
  
  out <- select(dat_in, station_code, species_name, ColonyID, MortOld, MortNew, csa) %>% 
    filter(!is.na(csa)) %>% 
    mutate(
      Recruits = factor(ifelse(csa < 0, 1, 0))
    )  %>% 
    select(station_code, species_name, ColonyID, csa, Recruits) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(out)
  
}