# get stony coral metrics
#
# dat_in raw coral demographic data
# rems chr string of species to remove, used to remove stations with no coral
get_stony_mets <- function(dat_in, rems = 'No Coral Observed'){
  
  # 3d surface area of each species
  crl_dat <- crl_dem %>% 
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

  # percent live cover
  lcsa <- get_lcsa(crl_dat)
  
  # mortality of large reef-building genera
  lrg_mort <- get_lrg_mort(crl_dat)
  
  # frequency distribution of colony sizes
  col_sz <- get_col_sz(crl_dat)
  
  # diversity of sensitive and rate species
  sens_rare <- get_sens_rare(crl_dat)
  
  # prportion of individuals as diseased or bleached
  sick <- get_sick(crl_dat)
  
  #  dominance of acropora/orbicella
  acrorb <- get_acrorb(crl_dat)

  browser()
  # combine
  NULL
  
}

# estimate 3d surface area of a coral colony, cm2
# uses size conversion equations from appendix J
#
# spp chr string of coral species to estimate
# h numeric of colony height (cm)
# d1 numeric of colony diameter (cm), diameter of major axis if ellipse
# d2 numeric of colony diameter (dm), diameter of minor axis if ellipse
#
# if both d1 and d2 are provided, they are converted to a single value for the diameter of a circle with the same area as the ellipse
#
# NA values will be all that don't match species in conv, juveniles marked as -1
est_3d <- function(spp, h, d1, d2 = d1){
  
  data(conv)

  # sanity check
  stopifnot(class(spp) %in% 'character')
  
  # convert diameter to single value
  # only use if value is relevant
  d <- ifelse(d1 == -1, NA, sqrt(d1 * d2)) 

  # use size conversions if not in estfun
  csa <- data.frame(spec = spp, h = h, d = d, stringsAsFactors = FALSE) %>% 
    left_join(., conv, by = 'spec') %>% 
    rename(M = conv) %>% 
    mutate(
      csa = pi * ((h + d / 2) / 2) ^ 2 * M
      )

  # juvies back to -1
  csa$csa[d1 == -1] <- -1
  
  out <- csa$csa
  return(out)
  
}

# coral relative abundance function
#
# dat_in raw coral demographic data
#
# returns data frame of species abundance and relative abundance by station
# abundance is simply count of species at a station
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

# get percent live coral at all stations
#
# dat_in coral demographic data with esimated csa (from est_3d)
#
# returns data frame with one estimate per station
get_lcsa <- function(dat_in){
  
  # sanity check
  if(!'csa' %in% names(dat_in))
    stop('csa must be in data, use est_3d function')
  
  # coral live surface area
  out <- select(dat_in, station_code, species_name, ColonyID, MortOld, MortNew, csa) %>% 
    group_by(station_code, species_name, ColonyID) %>% 
    filter(!is.na(csa) & csa > -1) %>% 
    mutate(LCSA = csa * (1 - sum(MortOld + MortNew) / 100)) %>% 
    group_by(station_code, ColonyID) %>% 
    summarise(LCSA = sum(LCSA, na.rm = T) / sum(csa, na.rm = T)) %>% 
    group_by(station_code) %>% 
    summarise(LCSA = sum(LCSA) / length(unique(ColonyID))) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(out)
  
}

# get mortality of large reef building genera at all stations
#
# dat_in coral demographic data with esimated csa (from est_3d)
# gen chr string of large genera
#
# returns data frame with counts of large genera and one estimate of mortality per station
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
      LCSA = csa * (1 - MortNew / 100),
      lrg_sp = ifelse(grepl(gen, species_name), 1, 0)
      ) %>% 
    group_by(station_code, lrg_sp) %>% 
    summarize(
      mort = mean(LCSA/csa)
    ) %>% 
    ungroup %>% 
    complete(station_code, lrg_sp) %>% 
    filter(lrg_sp == 1) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(out)
  
}

# get colony size 3d surface areas
#
# dat_in coral demographic data with esimated csa per species (from est_3d)
#
# returns data frame of station and colonies with indicator if recruits present and total csa (cm2)
get_col_sz <- function(dat_in){
  
  # sanity check
  if(!'csa' %in% names(dat_in))
    stop('csa must be in data, use est_3d function')
  
  out <- select(dat_in, station_code, species_name, ColonyID, MortOld, MortNew, csa) %>% 
    group_by(station_code, ColonyID) %>% 
    filter(!is.na(csa)) %>% 
    summarise(
      Recruits = factor(ifelse(any(csa < 0), 1, 0)),
      csa = sum(pmax(0, csa))
    )  %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(out)
  
}


# diversity of sensitive rare species
#
# dat_in coral demographic data, does not need estimated surface area
# gen chr string of genera for sensitive/rare species
#
# returns data frame with one row per station, richness of sens/rare species, relative abundance of sense/rare species, and estimated diversity
get_sens_rare <- function(dat_in, gen = c('Eusmilia', 'Isophyllastrea', 'Isophyllia', 'Mycetophyllia', 'Scolymia')){
  
  # get relative abundance of all
  rel_abu <- get_rel_abu(dat_in)
  
  # collapse gen for regex
  gen <- paste0('^', gen) %>% 
    paste(., collapse = '|')

  # get rel_abu of sens/rare
  out <- group_by(rel_abu, station_code) %>% 
    summarise(
      rich = sum(grepl(gen, species_name)),
      rel_abu = sum(rel_abu[grepl(gen, species_name)]),
      div = vegan::diversity(spp_abu[grepl(gen, species_name)])
    ) %>% 
    data.frame(., stringsAsFactors = FALSE)

  return(out)
  
}

# get proportion of individuals at a station that are diseased or bleached
#
# dat_in coral demographic data, does not need estimated surface area
#
# returns data frame of bleached or diseased proportion at each station
get_sick <- function(dat_in){
  
  out <- select(dat_in, station_code, species_name, Bleached, Diseased) %>% 
    mutate(
      Bleached = factor(Bleached), 
      Bleached = fct_recode(Bleached,
        'NA' = 'N/A', 
        '1' = 'T', 
        '0.5' = 'P',
        '0' = 'N'
      ), 
      Bleached = suppressWarnings(as.numeric(as.character(Bleached))),
      Diseased = factor(Diseased),
      Diseased = fct_recode(Diseased, 
        'NA' = 'N/A',
        '1' = 'P', 
        '0' = 'A'
      ), 
      Diseased = suppressWarnings(as.numeric(as.character(Diseased)))
    ) %>% 
    group_by(station_code) %>% 
    summarise(
      Bleached = sum(Bleached, na.rm = T)/length(Bleached), 
      Diseased = sum(Diseased, na.rm = T)/length(Diseased) 
    ) %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(out)
  
}

# get dominance of acropora/orbicella genera
#
# dat_in coral demographic data
# gen chr string of genera for sensitive/rare species
#
# returns a data frame of relative abundance and csa of the genera for all stations
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
      rel_abu = sum(rel_abu[grepl(gen, species_name)])
    )
  
  # 3d surf area of acrorb
  acrorb_csa <- mutate(dat_in, 
      csa = ifelse(csa == -1, 0, csa)
    ) %>% 
    group_by(station_code) %>% 
    summarise(
      rel_csa = sum(csa[grepl(gen, species_name)]),
      rel_csa = rel_csa/sum(csa), 
      rel_csa = ifelse(is.na(rel_csa), 0, rel_csa)
      )
  
  # Join the two  
  out <- full_join(acrorb_abu, acrorb_csa, by = 'station_code') %>% 
    data.frame(., stringsAsFactors = FALSE)
  
  return(out)
  
}