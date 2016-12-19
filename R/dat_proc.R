######
# coral data processing

library(tidyverse)
library(readxl)
library(forcats)
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
  group_by(station_code, species_name) %>% 
  summarise(
    spp_abu = length(species_name)
  ) %>% 
  group_by(station_code) %>% 
  mutate( 
    rel_abu = spp_abu/sum(spp_abu)
  )

# coral cover 
csa <- select(crl_dem, station_code, species_name, ColonyID, MaxDiam, PerpDiam, Height) %>% 
  group_by(station_code, species_name, ColonyID) %>% 
  mutate(
    csa = ifelse(MaxDiam != -1, est_3d(species_name, Height, MaxDiam, PerpDiam), -1)
  ) %>% 
  ungroup %>% 
  select(station_code, species_name, ColonyID, csa)

# coral live surface area, colony averages by site
csa_live <- select(crl_dem, station_code, species_name, ColonyID, MortOld, MortNew) %>% 
  left_join(., csa, by = c('station_code', 'species_name', 'ColonyID')) %>% 
  group_by(station_code, species_name, ColonyID) %>% 
  mutate(LCSA = ifelse(csa != -1, csa * (1 - sum(MortOld + MortNew) / 100), -1)) %>% 
  group_by(station_code, ColonyID) %>% 
  summarise(LCSA = sum(pmax(0, LCSA)) / sum(pmax(0, csa))) %>% 
  group_by(station_code) %>% 
  summarise(LCSA = sum(LCSA) / length(unique(ColonyID)))

# mortality in large reef building genera
# coral live surface area, colony averages by site
lrg_mort <- select(crl_dem, station_code, species_name, ColonyID, MortOld, MortNew) %>% 
  left_join(., csa, by = c('station_code', 'species_name', 'ColonyID')) %>% 
  group_by(station_code, species_name, ColonyID) %>% 
  filter(csa != -1) %>% 
  mutate(
    LCSA = csa * (1 - MortNew / 100),
    lrg_sp = ifelse(grepl('^Acropora|^Colpophyllia|^Dendrogyra|^Orbicella|^Pseudodiploria', species_name), 1, 0)
    ) %>% 
  group_by(station_code, lrg_sp) %>% 
  summarize(
    mort = mean(LCSA/csa)
  ) %>% 
  ungroup %>% 
  complete(station_code, lrg_sp) %>% 
  filter(lrg_sp == 1)
  

# frequency distribution of colony sizes
col_sz <- select(crl_dem, station_code, species_name, ColonyID, MortOld, MortNew) %>% 
  left_join(., csa, by = c('station_code', 'species_name', 'ColonyID')) %>% 
  group_by(station_code, ColonyID) %>% 
  summarise(
    Recruits = factor(ifelse(any(csa < 0), 1, 0)),
    csa = sum(pmax(0, csa))
  ) %>% 
  group_by(station_code)
tmp <- summarise(col_sz, tot = sum(csa, na.rm = T)) %>% 
  .$tot %>% 
  order
col_sz <- ungroup(col_sz) %>% 
  mutate(
    station_code = as.character(station_code),
    station_code = factor(station_code, levels = unique(station_code)[tmp])
    ) %>% 
  filter(!is.na(csa)) %>% 
  arrange(station_code, csa) %>%
  unite('statcol', station_code, ColonyID, sep = '_', remove = F) %>% 
  mutate(statcol = factor(statcol, levels = as.character(statcol)))

# diversity of rare/sens species
senssp <- c('^Eusmilia|^Isophyllastrea|^Isophyllia|^Mycetophyllia|^Scolymia')
sens <- group_by(rel_abu, station_code) %>% 
  summarise(
    rich = sum(grepl(senssp, species_name)),
    rel_abu = sum(rel_abu[grepl(senssp, species_name)]),
    div = diversity(spp_abu[grepl(senssp, species_name)])
  )

# sick - disease, bleaching
sick <- select(crl_dem, station_code, species_name, Bleached, Diseased) %>% 
  mutate(
    Bleached = factor(Bleached), 
    Bleached = fct_recode(Bleached,
      'NA' = 'N/A', 
      '1' = 'T', 
      '0.5' = 'P',
      '0' = 'N'
    ), 
    Bleached = as.numeric(as.character(Bleached)),
    Diseased = factor(Diseased),
    Diseased = fct_recode(Diseased, 
      'NA' = 'N/A',
      '1' = 'P', 
      '0' = 'A'
    ), 
    Diseased = as.numeric(as.character(Diseased))
  ) %>% 
  group_by(station_code) %>% 
  summarise(
    Bleached = sum(Bleached, na.rm = T)/length(Bleached), 
    Diseased = sum(Diseased, na.rm = T)/length(Diseased) 
  )
    
# dominance of orbicella, acropora
gens <- '^Acropora|^Orbicella'
acrorb_abu <- summarise(rel_abu,
  rel_abu = sum(rel_abu[grepl(gens, species_name)])
  )
acrorb_csa <- mutate(csa, csa = ifelse(csa == -1, 0, csa)) %>% 
  group_by(station_code) %>% 
  summarise(
    rel_csa = sum(csa[grepl(gens, species_name)]),
    rel_csa = rel_csa/sum(csa), 
    rel_csa = ifelse(is.na(rel_csa), 0, rel_csa)
    )
acrorb <- full_join(acrorb_abu, acrorb_csa, by = 'station_code')
  