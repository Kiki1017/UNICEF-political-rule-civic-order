### Pandemic backsliding, ver. 2 ###

#### load packages and data ####
library(tidyverse)
library(wesanderson)
library(RColorBrewer)

## load data; downloaded from V-Dem website: https://www.v-dem.net/en/our-work/research-projects/pandemic-backsliding/
filename <- file.choose()
vod_pan <- readRDS(filename) 

#### prepare pandem data and regime type ####

## select countries for analysis
countries_selected <-c("Cambodia", "China", "Indonesia", "North Korea",
                       "Laos", "Malaysia", "Mongolia", "Burma/Myanmar", 
                       "Fiji","Vanuatu", "Philippines","Thailand", "Vietnam",
                       "Taiwan", "Singapore", "Hong Kong", "Japan", "South Korea")

pandemic_back <- vod_pan[vod_pan$country_name %in% countries_selected,]

## count and round
pandemic_back <- pandemic_back %>%
  group_by(country_name) %>%
  count(pandem) %>%
  mutate_at(2:3, round, 2)

## load Ver. 10 V-Dem 'episdoes' data, and select for regime type with Vdemdata package
# devtools::install_github("vdeminstitute/vdemdata")

library(vdemdata) # loads as 'episdoes'

## data cleaning; create new regime type variable (factor)
episodes.ea2 <- episodes[episodes$country_name %in% countries_selected,]
episodes.ea2 <- subset(episodes.ea2, year== 2019)
episodes.ea2$v2x_regime_f <- factor(episodes.ea2$v2x_regime, levels=c(0,1,2,3),
                                    labels=c("Closed Autocracy",
                                             "Electoral Autocracy",
                                             "Electoral Democracy",
                                             "Liberal Democracy"))
# group and count regime type by country
dem_ep19 <- episodes.ea2 %>%
  group_by(country_name) %>%
  count(v2x_regime_f)

# join data frames by country
pandemic_back <- left_join(pandemic_back, dem_ep19, by="country_name")


##### load V-Dem ver. 10 data from file
## download file and documentation here: https://www.v-dem.net/en/data/data-version-10/
filename <- file.choose()
vod <- readRDS(filename)

# subset for country, liberal democracy index, core civil soceity index, and year
vod_join <- vod %>% 
  select(country_name, v2x_libdem, v2xcs_ccsi, year)

vod_join <- vod_join[vod_join$country_f %in% countries_selected,] ## subset for selected countries

# take 10-year average of libdem index, round
libdem.avg10 <- subset(vod_join, year>2009) %>%
  group_by(country_f) %>%
  summarise(libdem10avg = mean(v2x_libdem, na.rm=T)) %>%
  mutate_at(2, round, 2)

# take 10-year average of core civil society index, round
civisociety.avg10 <- subset(vod_join, year>2009) %>%
  group_by(country_f) %>%
  summarise(civilsociety10avg = mean(v2xcs_ccsi, na.rm=T)) %>%
  mutate_at(2, round, 2)

## join pandem data by libdem and civil society indices (average)
pandemic_back <- left_join(pandemic_back,libdem.avg10, by="country_name")
pandemic_back <- left_join(pandemic_back,civisociety.avg10, by="country_name")

