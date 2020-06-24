## Varieties of Democracy (V-Dem) data, Ver. 10 
## for liberal democracy and civil society
## https://www.v-dem.net/en/data/data-version-10/
## 'Country-Year: V-Dem Core'
##########################################

#### load packages and data ####
library(tidyverse)
library(wesanderson)
library(RColorBrewer)

## load file
filename <- file.choose()
vod <- readRDS(filename)

## prepare V-Dem data
main_vod <- vod %>% 
  select(country_name, country_id, v2x_libdem_sd, v2x_libdem, v2x_libdem_codehigh, 
         v2x_libdem_codelow, v2xcs_ccsi, v2xcs_ccsi_sd, v2xcs_ccsi_codehigh, v2xcs_ccsi_codelow, year)

# subset by selected Asia Pacific countries
countries_selected <-c("Cambodia", "China", "Indonesia", "North Korea",
            "Laos", "Malaysia", "Mongolia", "Burma/Myanmar", "Fiji",
            "Solomon Islands", "Vanuatu", "Philippines",
            "Thailand", "Vietnam", # "Timor-Leste" removed for data availability concerns
            "Taiwan", "Singapore", "Hong Kong", "Japan", "South Korea")

main_vod <- main_vod[main_vod$country_name %in% countries_selected,]

#### Regime type ####
## from 'vdemdata' package
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

#### Libdem <> Civil society - high-level ####

main_vod <- subset(main_vod, year >= 1989)

libdem_civil_year <- main_vod %>% 
  group_by(year) %>%
  summarise_at(vars(v2x_libdem, v2xcs_ccsi), 
               funs(mean(.)), na.rm=T )

libdem_civil_year <- libdem_civil_year %>%
  select(year, v2x_libdem, v2xcs_ccsi) %>%
  gather(key = "variable", value = "value", -year)


#### Libdem scores 1989-2019 ####

libdem_year <- main_vod %>% 
  group_by(country_name, year) %>%
  summarise_at(vars(v2x_libdem, v2x_libdem_codehigh, v2x_libdem_codelow), 
             funs(mean(.)), na.rm=T )

libdem_year3 <- subset(libdem_year, year == 2019 | year == 2004 | year == 1989)
libdem_year3 <- libdem_year3 %>% mutate(year = factor(year, levels = c(2019,2004,1989))) # reorder year
libdem_year3$dum_2019 <- ifelse(libdem_year3$year==2019,1,0)
libdem_year3$value2019 <- ifelse(libdem_year3$dum_2019==1 & !is.na(libdem_year3$v2x_libdem),
                                 c(libdem_year3$v2x_libdem),c(0))
# join w/ regime type
libdem_year3 <- left_join(libdem_year3, dem_ep19, by="country_name")

#### Libdem change 89-19 ####

# avg. for first 15 years
libdem_avg.first15 <- libdem_year %>% subset(year <= 2004) %>%
  group_by(country_name) %>%
  summarise(libdem_first15.avg = mean(v2x_libdem, na.rm=T))

# avg. last 15 years
libdem_avg.last15 <- libdem_year %>% subset(year > 2004) %>%
  group_by(country_name) %>%
  summarise(libdem_last15.avg = mean(v2x_libdem, na.rm=T))

# combine and take difference
libdem_change <- left_join(libdem_avg.first15, libdem_avg.last15, by="country_name")
libdem_change <- libdem_change %>% mutate(libdem_diff30 = (libdem_last15.avg - libdem_first15.avg))

# add regime type
regime_counts_temp <- subset(episodes.ea2, year==1989) %>%
  group_by(country_name) %>%
  select(c(country_name, v2x_regime_f))

# combine diff and regime type DFs
libdem_change <- left_join(libdem_change, regime_counts_temp, by="country_name")

# diff
demo <- subset(episodes.ea, year==2004) %>%
  group_by(country_name) %>%
  select(country_name, v2x_regime) %>%
  mutate(demobi = if_else(v2x_regime >= 2, "Yes", "No"))

# demo combine
libdem_change <- left_join(libdem_change, demo, by="country_name")


#### Civil Society Core ####
civil_year <- main_vod %>% 
  group_by(country_name, year) %>%
  summarise_at(vars(v2xcs_ccsi, v2xcs_ccsi_codehigh, v2xcs_ccsi_codelow), 
               funs(mean(.)), na.rm=T )

civil_year3 <- subset(civil_year, year == 2019 | year == 2004 | year == 1989)
civil_year3 <- civil_year3 %>% mutate(year = factor(year, levels = c(2019,2004,1989))) # reorder year
civil_year3$dum_2019 <- ifelse(civil_year3$year==2019,1,0)
civil_year3$value2019 <- ifelse(civil_year3$dum_2019==1 & !is.na(civil_year3$v2xcs_ccsi),
                                 c(civil_year3$v2xcs_ccsi),c(0))

# join w/ regime type
civil_year3 <- left_join(civil_year3, dem_ep19, by="country_name")


#### Civil change 96-19 ####

# avg. for first 15 years
civil_avg.first15 <- civil_year %>% subset(year <= 2004) %>%
  group_by(country_name) %>%
  summarise(civil_first15.avg = mean(v2xcs_ccsi, na.rm=T))

# avg. last 15 years
civil_avg.last15 <- civil_year %>% subset(year > 2004) %>%
  group_by(country_name) %>%
  summarise(civil_last15.avg = mean(v2xcs_ccsi, na.rm=T))

# combine and take difference
civil_change <- left_join(civil_avg.first15, civil_avg.last15, by="country_name")
civil_change <- civil_change %>% mutate(civil_diff30 = (civil_last15.avg - civil_first15.avg))

# add regime type
regime_counts_temp <- subset(episodes.ea2, year==1989) %>%
  group_by(country_name) %>%
  select(c(country_name, v2x_regime_f))

# combine diff and regime type DFs
civil_change <- left_join(civil_change, regime_counts_temp, by="country_name")

# demo combine
civil_change <- left_join(civil_change, demo, by="country_name")
