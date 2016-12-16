######
# coral data processing

library(tidyverse)
library(readxl)
library(ggmap)
soure('R/funcs.R')

# ######
# # get coral morph conversion factor table from pdf doc
# # colony surface area
# library(pdftools)
# 
# pth <- 'M:/docs/coral_bcg/ignore/Santavy etal 2012 field manual.pdf'
# raw <- pdf_text(pth)
# conv <- grep('^Table [3]-[2]', raw, value = T) %>% 
#   strsplit(., '\\r') %>% 
#   .[[1]] %>% 
#   .[5:50] %>% 
#   gsub('\\n', '', .) %>% 
#   strsplit(., '\\s\\s\\s*') %>% 
#   do.call('rbind', .) %>% 
#   data.frame(stringsAsFactors = F) %>% 
#   rename(
#     'spec' = X1, 
#     'code' = X2, 
#     'conv' = X3
#   ) %>% 
#   mutate(
#     conv = as.numeric(conv)
#   )
# save(conv, file = 'data/conv.RData', compress = 'xz')

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

# relative abundance
rel_abu <- select(crl_dem, station_code, species_name, ColonyID) %>% 
  group_by(station_code) %>% 
  mutate(
    abu = length(unique(ColonyID))
  ) %>% 
  group_by(station_code, species_name, abu) %>% 
  summarise(
    spp_abu = length(species_name)
  ) %>% 
  ungroup %>% 
  mutate( 
    rel_abu = spp_abu/abu
  )

# coral cover 
csa <- select(crl_dem, station_code, species_name, ColonyID, MaxDiam, PerpDiam, Height) %>% 
  group_by(station_code, species_name, ColonyID) %>% 
  mutate(
    csa = ifelse(MaxDiam != -1, est_3d(species_name, Height, MaxDiam, PerpDiam), -1)
  )

