######
# coral data processing

library(tidyverse)
library(readxl)
library(forcats)
soure('R/funcs.R')

######
# USEPA PR 2011 data

# transect data
crl_srv <- read_excel(
  'ignore/StonyCoral_PR2011_FNov062013.xlsx', 
  sheet = 'StonyCoral_PR2011', 
  col_types = c('date', 'numeric', 'text', 'text', 'text', 'numeric', 'numeric', 'numeric', 'text', 'text', 'text', 'numeric', 'text', 'text', 'blank')
  ) %>% 
  select(-`Data Collector`, -`Transect area (m2)`, -`Transect type`, -`Notes`) %>% 
  mutate(
    `Bleached` = ifelse(is.na(`Bleached`), 0, 1),
    `Diseased` = ifelse(is.na(`Diseased`), 0, 1),
    `Clionid` = ifelse(is.na(`Clionid`), 0, 1)
  )

# abundance
crl_abu <- read_excel(
  'ignore/StonyCoral_PR2011_FNov062013.xlsx', 
  sheet = 'abundance', 
  col_types = c('date', 'numeric', 'text', 'text', 'text', 'numeric', rep('blank', 8))
  ) %>% 
  select(-`Data Collector`)

# meta
crl_met <- read_excel(
  'ignore/StonyCoral_PR2011_FNov062013.xlsx', 
  sheet = 'StationInfo'
  ) %>% 
  select(Station, `Latitude (decimal deg)`, `Longitude (decimal deg)`) %>% 
  rename(
    lat = `Latitude (decimal deg)`, 
    lon = `Longitude (decimal deg)`
  )

######
# 2014 NOAA PR data

# transect data
crl_dem <- read.csv(
  'L:/lab/Coral_EcoSystems/BCG/D. Data/NOAA Data/2014/Data/PR data/Gibbs 081716/NCRMP_PR2014_CoralDemo_FINAL.csv', 
  stringsAsFactors = F
  ) %>%
  select(station_code, latitude, longitude, species_name, ColonyID, MaxDiam, PerpDiam, Height, MortOld, MortNew, Bleached, Diseased)

crl_dat <- crl_dem %>% 
  mutate(
    csa = est_3d(species_name, Height, MaxDiam, PerpDiam)
  ) 

get_lcsa(crl_dat)

get_lrg_mort(crl_dat)

get_col_sz(crl_dat)

get_sens_rare(crl_dat)

get_sick(crl_dat)

get_acrorb(crl_dat)

    

  