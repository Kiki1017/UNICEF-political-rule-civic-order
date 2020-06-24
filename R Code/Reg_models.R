### Asian Barometer Survey, Wave 4 - all countries ###
# http://www.asianbarometer.org/survey
# for data, researcher is required to submit data request (http://www.asianbarometer.org/data/data-release)
##########################################

## load packages
library(tidyverse)
library(ggeffects)


## subset for variables used
## from main, used in "Asian_Barometer.R"

complete <- select(main, c("trust",
                     "trust_inst",
                     "demo_value",
                     "pol_part",
                     "qual_gov2",
                     "regime_pref",
                     "female", "age", "age_cohorts", "youth", "university", "religious", "married", "urban", "ses",
                     "w", "country", "country_name",
                     "abins", "abinsf"))
# impute
complete$regime_pref <- ifelse(is.na(complete$regime_pref), 
                                     c(median(na.omit(complete$regime_pref))), c((complete$regime_pref) ))
## new vars ##
complete$political_values <- ((complete$demo_value+complete$regime_pref) / 2)
complete$political_society <- ((complete$trust+main$trust_inst+complete$qual_gov2) / 3)


#### Political Values & Trust and Social Values ####

#lm models_all
alldf_m1 <- lm(scale(political_values) ~ abinsf + female + university + religious + married + urban + ses + country_name, data=complete, weights=w) # RC1
alldf_m2 <- lm(scale(political_society) ~ abinsf + female + university + religious + married + urban + ses + country_name, data=complete, weights=w) # RC2

#marginal effects_all
pr_political_values <- ggpredict(alldf_m1, c("country_name"))
pr_political_values$group <- rep("All",nrow(pr_political_values))
pr_political_society <- ggpredict(alldf_m2, c("country_name"))
pr_political_society$group <- rep("All",nrow(pr_political_society))

#lm models_youth
youthdf_m1 <- subset(complete, youth==1) %>% lm(scale(political_values) ~ abinsf + female + university + religious + married + urban + ses + country_name, data=., weights=w) # RC1
youthdf_m2 <- subset(complete, youth==1) %>% lm(scale(political_society) ~ abinsf + female + university + religious + married + urban + ses + country_name, data=., weights=w) # RC2

#marginal effects_youth
pr_political_values_youth <- ggpredict(youthdf_m1, c("country_name"))
pr_political_society_youth <- ggpredict(youthdf_m2, c("country_name"))

#marginal effects_all
pr_political_values_youth <- ggpredict(youthdf_m1, c("country_name"))
pr_political_values_youth$group <- rep("Youth",nrow(pr_political_values_youth))
pr_political_society_youth <- ggpredict(youthdf_m2, c("country_name"))
pr_political_society_youth$group <- rep("Youth",nrow(pr_political_society_youth))

## rbind it
pr_political_values_both <- rbind(pr_political_values, pr_political_values_youth)
pr_political_society_both <- rbind(pr_political_society, pr_political_society_youth)

## Political values: plotted ##
move_it_pol <- position_dodge(0.3)
pr_political_values_both$group <- factor(pr_political_values_both$group, levels = c("All", "Youth"))
pr_political_society_both$group <- factor(pr_political_society_both$group, levels = c("All", "Youth"))


## changes
pr_political_values_both <- pr_political_values_both %>% rename(country_name=x)
pr_political_values_both <- left_join(pr_political_values_both, dem_ep, by="country_name")
pr_political_values_both <- pr_political_values_both %>% mutate(country_name=recode(country_name, 
                                                                                 "Korea" = "South Korea",
                                                                                 "Myanmar" = "Burma/Myanmar"))

pr_political_society_both <- pr_political_society_both %>% rename(country_name=x)
pr_political_society_both <- left_join(pr_political_society_both, dem_ep, by="country_name")
pr_political_society_both <- pr_political_society_both %>% mutate(country_name=recode(country_name, 
                                                                                 "Korea" = "South Korea",
                                                                                 "Myanmar" = "Burma/Myanmar"))

#### Association, sys ~ publican opinion ####
pr_political_values_joined <- left_join(pr_political_values_both, libdem.avg10, by="country_name")
pr_political_values_joined <- left_join(pr_political_values_joined, libdem19, by="country_name")

pr_political_society_joined <- left_join(pr_political_society_both, libdem.avg10, by="country_name")
pr_political_society_joined <- left_join(pr_political_society_joined, libdem19, by="country_name")

pr_political_values_joined$libdem.avg10_s <- scale(pr_political_values_joined$libdem10avg)
pr_political_society_joined$libdem.avg10_s <- scale(pr_political_society_joined$libdem10avg)

## drop youth
pr_political_values_joined <- pr_political_values_joined %>% filter(group != "Youth")
pr_political_society_joined <- pr_political_society_joined %>% filter(group != "Youth")

# political values
pr_political_values_joined$congruence[pr_political_values_joined$predicted>0 & pr_political_values_joined$libdem.avg10_s>0]=1
pr_political_values_joined$congruence[pr_political_values_joined$predicted<0 & pr_political_values_joined$libdem.avg10_s>0]=2
pr_political_values_joined$congruence[pr_political_values_joined$predicted>0 & pr_political_values_joined$libdem.avg10_s<0]=3
pr_political_values_joined$congruence[pr_political_values_joined$predicted<0 & pr_political_values_joined$libdem.avg10_s<0]=4

pr_political_values_joined$congruence <- factor(pr_political_values_joined$congruence, levels=c(1,2,3,4),
                                                labels=c("Democratic Congruence",
                                                         "Non-Democratic Values, Democratic System",
                                                         "Democratic Values, Non-Democratic System",
                                                         "Authoritarian Congruence"))
# social values
pr_political_society_joined$congruence[pr_political_society_joined$predicted>0 & pr_political_society_joined$libdem.avg10_s>0]=1
pr_political_society_joined$congruence[pr_political_society_joined$predicted<0 & pr_political_society_joined$libdem.avg10_s>0]=2
pr_political_society_joined$congruence[pr_political_society_joined$predicted>0 & pr_political_society_joined$libdem.avg10_s<0]=3
pr_political_society_joined$congruence[pr_political_society_joined$predicted<0 & pr_political_society_joined$libdem.avg10_s<0]=4


pr_political_society_joined$congruence <- as.factor(pr_political_society_joined$congruence)
#pr_political_society_joined$congruence <- factor(pr_political_society_joined$congruence, levels=c(1,2,3,4),
                                               # labels=c("Democratic Congruence",
                                                         #"Non-Democratic Values, Democratic System",
                                                        #"Democratic Values, Non-Democratic System",
                                                         #"Authoritarian Congruence"))


#### Cohort Analysis Per Country / Political Values ####
# model fit
japan_m1 <- subset(complete, country_name=="Japan") %>%
  lm(political_values ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
taiwan_m1 <- subset(complete, country_name=="Taiwan") %>%
  lm(political_values ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
korea_m1 <- subset(complete, country_name=="South Korea") %>%
  lm(political_values ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
hk_m1 <- subset(complete, country_name=="Hong Kong") %>%
  lm(political_values ~ abinsf + female + university + religious + married + ses, weights=w, data=.)
cambodia_m1 <- subset(complete, country_name=="Cambodia") %>%
  lm(political_values ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
vietnam_m1 <- subset(complete, country_name=="Vietnam") %>%
  lm(political_values ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.) ## make sure input worked

phil_m1 <- subset(complete, country_name=="Philippines") %>%
  lm(political_values ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
sing_m1 <- subset(complete, country_name=="Singapore") %>%
  lm(political_values ~ abinsf + female + university + religious + married + ses, weights=w, data=.)
indo_m1 <- subset(complete, country_name=="Indonesia") %>%
  lm(political_values ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
malay_m1 <- subset(complete, country_name=="Malaysia") %>%
  lm(political_values ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
ch_m1 <- subset(complete, country_name=="China") %>%
  lm(political_values ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)

mong_m1 <- subset(complete, country_name=="Mongolia") %>%
  lm(political_values ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
thai_m1 <- subset(complete, country_name=="Thailand") %>%
  lm(political_values ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
myan_m1 <- subset(complete, country_name=="Burma/Myanmar") %>%
  lm(political_values ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)


# marginal effects
japan_pr1 <- ggpredict(japan_m1, c("abinsf"))
taiwan_pr1 <- ggpredict(taiwan_m1, c("abinsf"))
korea_pr1 <- ggpredict(korea_m1, c("abinsf"))
hk_pr1 <- ggpredict(hk_m1, c("abinsf"))
cambodia_pr1 <- ggpredict(cambodia_m1, c("abinsf"))
vietnam_pr1 <- ggpredict(vietnam_m1, c("abinsf"))

phil_pr1 <- ggpredict(phil_m1, c("abinsf"))
sing_pr1 <- ggpredict(sing_m1, c("abinsf"))
indo_pr1 <- ggpredict(indo_m1, c("abinsf"))
malay_pr1 <- ggpredict(malay_m1, c("abinsf"))
ch_pr1 <- ggpredict(ch_m1, c("abinsf"))

mong_pr1 <- ggpredict(mong_m1, c("abinsf"))
thai_pr1 <- ggpredict(thai_m1, c("abinsf"))
myan_pr1 <- ggpredict(myan_m1, c("abinsf"))



#### Cohort Analysis Per Country / Trust And Social Values ####

# model fit
japan_m2 <- subset(complete, country_name=="Japan") %>%
  lm(political_society ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
taiwan_m2 <- subset(complete, country_name=="Taiwan") %>%
  lm(political_society ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
korea_m2 <- subset(complete, country_name=="South Korea") %>%
  lm(political_society ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
hk_m2 <- subset(complete, country_name=="Hong Kong") %>%
  lm(political_society ~ abinsf + female + university + religious + married + ses, weights=w, data=.)
cambodia_m2 <- subset(complete, country_name=="Cambodia") %>%
  lm(political_society ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)

phil_m2 <- subset(complete, country_name=="Philippines") %>%
  lm(political_society ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
sing_m2 <- subset(complete, country_name=="Singapore") %>%
  lm(political_society ~ abinsf + female + university + religious + married + ses, weights=w, data=.)
indo_m2 <- subset(complete, country_name=="Indonesia") %>%
  lm(political_society ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
malay_m2 <- subset(complete, country_name=="Malaysia") %>%
  lm(political_society ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
ch_m2 <- subset(complete, country_name=="China") %>%
  lm(political_society ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)

mong_m2 <- subset(complete, country_name=="Mongolia") %>%
  lm(political_society ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
thai_m2 <- subset(complete, country_name=="Thailand") %>%
  lm(political_society ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
myan_m2 <- subset(complete, country_name=="Burma/Myanmar") %>%
  lm(political_society ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)
viet_m2 <- subset(complete, country_name=="Vietnam") %>%
  lm(political_society ~ abinsf + female + university + religious + married + urban + ses, weights=w, data=.)


# marginal effects
japan_pr2 <- ggpredict(japan_m2, c("abinsf"))
taiwan_pr2 <- ggpredict(taiwan_m2, c("abinsf"))
korea_pr2 <- ggpredict(korea_m2, c("abinsf"))
hk_pr2 <- ggpredict(hk_m2, c("abinsf"))
cambodia_pr2 <- ggpredict(cambodia_m2, c("abinsf"))
phil_pr2 <- ggpredict(phil_m2, c("abinsf"))
sing_pr2 <- ggpredict(sing_m2, c("abinsf"))
indo_pr2 <- ggpredict(indo_m2, c("abinsf"))
malay_pr2 <- ggpredict(malay_m2, c("abinsf"))
ch_pr2 <- ggpredict(ch_m2, c("abinsf"))
mong_pr2 <- ggpredict(mong_m2, c("abinsf"))
thai_pr2 <- ggpredict(thai_m2, c("abinsf"))
myan_pr2 <- ggpredict(myan_m2, c("abinsf"))
viet_pr2 <- ggpredict(viet_m2, c("abinsf"))



