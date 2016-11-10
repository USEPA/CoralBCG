######
# 2011 Puerto Rico data

library(tidyverse)
library(readxl)
library(ggmap)

######
# stony coral data

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
# 

crl_abu <- read_excel(
  'ignore/StonyCoral_PR2011_FNov062013.xlsx', 
  sheet = 'abundance', 
  col_types = c('date', 'numeric', 'text', 'text', 'text', 'numeric', rep('blank', 8))
  ) %>% 
  select(-`Data Collector`) %>% 
  filter(
    !Taxon %in% 'UNKNOWN'
  ) %>% 
  rename(
    abu = `Count of Taxon`
  ) %>% 
  select(-`Coral code`, -Date) %>% 
  na.omit %>% 
  spread(Taxon, abu, fill = 0) %>% 
  mutate(
    Richness = apply(., 1, function(x) sum(x != 0) - 1)
    ) %>% 
  left_join(., crl_met, by = 'Station')
  
ext <- make_bbox(crl_met$lon, crl_met$lat, f = 0.2)
ext[2] <- ext[2] - 0.3
ext[4] <- ext[4] + 0.5
map <- get_stamenmap(ext, zoom = 10, maptype = "toner-lite")

ggmap(map) + 
  geom_point(data = crl_abu, aes(x = lon, y = lat, size = `Agaricia fragilis`, fill = `Agaricia fragilis`), pch = 21, alpha = 0.8) + 
  scale_fill_distiller(palette = 'Spectral', direction = 1, guide = 'legend') +
  scale_size(range = c(0.5, 8), guide = 'legend') + 
  theme_bw() + 
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()
  )



