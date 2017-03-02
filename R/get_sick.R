#' Get proportion of individuals at a station that are diseased or bleached
#'
#' @param dat_in coral demographic data, does not need estimated surface area
#'
#' @return data frame of bleached or diseased proportion at each station
#' 
#' @import dplyr tidyr
#' 
#' @export
#'
#' @examples
#' get_sick(crl_dem)
get_sick <- function(dat_in){
  
  out <- select(dat_in, station_code, species_name, Bleached, Diseased) %>% 
    mutate(
      Bleached = factor(Bleached), 
      Bleached = suppressWarnings(forcats::fct_recode(Bleached,
        '1' = 'T', 
        '0.5' = 'P',
        '0' = 'N'
      )), 
      Bleached = suppressWarnings(as.numeric(as.character(Bleached))),
      Diseased = factor(Diseased),
      Diseased = suppressWarnings(forcats::fct_recode(Diseased, 
        '1' = 'P', 
        '0' = 'A'
      )), 
      Diseased = suppressWarnings(as.numeric(as.character(Diseased)))
    ) %>% 
    group_by(station_code) %>% 
    summarise(
      bleached = sum(Bleached, na.rm = T)/length(Bleached), 
      diseased = sum(Diseased, na.rm = T)/length(Diseased) 
    ) %>% 
    gather('var', 'val', -station_code) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(out)
  
}
