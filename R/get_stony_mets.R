#' Get stony coral metrics
#' 
#' Get all stony coral metrics
#'
#' @param dat_in raw coral demographic data
#' @param rems chr string of species to remove, used to remove stations with no coral
#'
#' @return Data frame of coral metrics with one row per station
#' 
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' get_stony_mets(crl_dem)
get_stony_mets <- function(dat_in, rems = 'No Coral Observed'){
  
  # station master keys
  station_code <- dat_in$station_code %>% 
    unique %>% 
    sort
  
  # remove any trailing/leading spaces in species names
  dat_in$species_name <- gsub('^\\s+|\\s+$', '', dat_in$species_name)

  # 3d surface area of each species
  crl_dat <- dat_in %>% 
    mutate(
      csa = est_3d(species_name, Height, MaxDiam, PerpDiam)
    ) 

  # filter data to include sites with coral
  if(!is.null(rems)){
    
    # collapse for regex
    rems <- paste0('^', rems) %>% 
      paste(., collapse = '|')
      
      crl_dat <- filter(crl_dat, !grepl(rems, species_name))
      
  }

  # check if all species found, warning if not
  spp <- crl_dat$species_name %>% 
    unique %>% 
    sort
  if(any(!spp %in% conv$spec)){
    spp <- paste(spp[!spp %in% conv$spec], collapse = ', ')
    warning(spp, ' not found')
  }
  
  # total richness/diversity
  tot_div <- get_tot_div(crl_dat)
  
  # percent live cover
  lcsa <- get_lcsa(crl_dat)
  
  # mortality of large reef-building genera
  lrg_mort <- get_lrg_mort(crl_dat)
  
  # frequency distribution of colony sizes
  col_sz <- get_col_sz(crl_dat)
  
  # diversity of sensitive and rare species
  sr <- get_sr_div(crl_dat)
  
  # prportion of individuals as diseased or bleached
  sick <- get_sick(crl_dat)
  
  #  dominance of acropora/orbicella
  acrorb <- get_acrorb(crl_dat)

  # combine, back to wide
  out <- rbind(tot_div, lcsa, lrg_mort, col_sz, sr, sick, acrorb) %>% 
    spread(var, val)
  
  return(out)
  
}