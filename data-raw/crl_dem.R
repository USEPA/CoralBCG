# transect data 
# select relevant columns, remove milipora, subset 5 random station, rename stations
crl_dem <- read_excel(
  'L:/lab/Coral_EcoSystems/BCG/D. Data/1_NOAA Data/2014/Data/NCRMP_PR2014_CoralDemo_FINAL.xlsx'
  ) %>%
  select(station_code, species_name, ColonyID, MaxDiam, PerpDiam, Height, MortOld, MortNew, Bleached, Diseased) %>% 
  filter(!grepl('Millepora', species_name)) %>% 
  filter(station_code %in% sample(unique(station_code), 5)) %>% 
  mutate(
    station_code = factor(station_code), 
    station_code = factor(station_code, labels = sample(1:length(levels(station_code)))),
    station_code = as.numeric(station_code)
  )

save(crl_dem, file = 'data/crl_dem.RData', compress = 'xz')
