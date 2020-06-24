## Asian Barometer Survey, Wave 4 - all countries
## http://www.asianbarometer.org/survey
## for data, researcher is required to submit data request (http://www.asianbarometer.org/data/data-release)
##########################################

## import file from SPSS file format to R
#library(Hmisc)
main <- spss.get("[file].sav", use.value.labels=TRUE) ## wave 4 As

## load tidyverse
library(tidyverse)

#### Trust in people / Social capital ####
main$q26n <- as.numeric(main$q26)
main$q27n <- as.numeric(main$q27)
main$q28n <- as.numeric(main$q28)

main$q26n[main$q26n == 1 | main$q26n > 5] <- NA
main$q27n[main$q27n == 1 | main$q27n > 5] <- NA
main$q28n[main$q28n == 1 | main$q28n > 5] <- NA

main$q26n <- factor(main$q26n, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q27n <- factor(main$q27n, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q28n <- factor(main$q28n, levels=c(2,3,4,5), labels=c(3,2,1,0))

main$q26n <- as.numeric(levels(main$q26n))[main$q26n]
main$q27n <- as.numeric(levels(main$q27n))[main$q27n]
main$q28n <- as.numeric(levels(main$q28n))[main$q28n]


main$trust <- ( (main$q26n +
                   main$q27n +
                   main$q28n) / 9) ## unweighted trust index

#### Trust in institutions ####
main$q8n <- as.numeric(main$q8) ## col # = 162
main$q9n <- as.numeric(main$q9)
main$q11n <- as.numeric(main$q11)
main$q12n <- as.numeric(main$q12)
main$q13n <- as.numeric(main$q13)
main$q14n <- as.numeric(main$q14)
main$q15n <- as.numeric(main$q15)
main$q16n <- as.numeric(main$q16)
main$q17n <- as.numeric(main$q17)

main$q8n[main$q8n == 1 | main$q8n > 5] <- NA
main$q9n[main$q9n == 1 | main$q9n > 5] <- NA
main$q11n[main$q11n == 1 | main$q11n > 5] <- NA
main$q12n[main$q12n == 1 | main$q12n > 5] <- NA
main$q13n[main$q13n == 1 | main$q13n > 5] <- NA
main$q14n[main$q14n == 1 | main$q14n > 5] <- NA
main$q15n[main$q15n == 1 | main$q15n > 5] <- NA
main$q16n[main$q16n == 1 | main$q16n > 5] <- NA
main$q17n[main$q17n == 1 | main$q17n > 5] <- NA


main$q8n <- factor(main$q8n, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q9n <- factor(main$q9n, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q11n <- factor(main$q11n, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q12n <- factor(main$q12n, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q13n <- factor(main$q13n, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q14n <- factor(main$q14n, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q15n <- factor(main$q15n, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q16n <- factor(main$q16n, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q17n <- factor(main$q17n, levels=c(2,3,4,5), labels=c(3,2,1,0))


main$q8n <- as.numeric(levels(main$q8n))[main$q8n]
main$q9n <- as.numeric(levels(main$q9n))[main$q9n]
main$q11n <- as.numeric(levels(main$q11n))[main$q11n]
main$q12n <- as.numeric(levels(main$q12n))[main$q12n]
main$q13n <- as.numeric(levels(main$q13n))[main$q13n]
main$q14n <- as.numeric(levels(main$q14n))[main$q14n]
main$q15n <- as.numeric(levels(main$q15n))[main$q15n]
main$q16n <- as.numeric(levels(main$q16n))[main$q16n]
main$q17n <- as.numeric(levels(main$q17n))[main$q17n]

main$q15n[main$country=="Singapore"] <- median(na.omit(main$q15n))

main$trust_inst <-   ( (main$q8n +
                          main$q9n +
                          main$q11n +
                          main$q12n +
                          main$q13n +
                          main$q14n +
                          main$q15n + #
                          main$q16n + 
                          main$q17n) / 27 ) 

#### Politial Values ####
main$q139n <- as.numeric(main$q139) ## col # = 162
main$q140n <- as.numeric(main$q140)
main$q141n <- as.numeric(main$q141)
main$q142n <- as.numeric(main$q142)
main$q143n <- as.numeric(main$q143)
main$q144n <- as.numeric(main$q144)
main$q145n <- as.numeric(main$q145)
main$q146n <- as.numeric(main$q146)
main$q147n <- as.numeric(main$q147)
main$q148n <- as.numeric(main$q148)
main$q149n <- as.numeric(main$q149) ## 172

main$q139n[main$q139n == 1 | main$q139n > 5] <- NA
main$q140n[main$q140n == 1 | main$q140n > 5] <- NA
main$q141n[main$q141n == 1 | main$q141n > 5] <- NA
main$q142n[main$q142n == 1 | main$q142n > 5] <- NA
main$q143n[main$q143n == 1 | main$q143n > 5] <- NA
main$q144n[main$q144n == 1 | main$q144n > 5] <- NA
main$q145n[main$q145n == 1 | main$q145n > 5] <- NA
main$q146n[main$q146n == 1 | main$q146n > 5] <- NA
main$q147n[main$q147n == 1 | main$q147n > 5] <- NA
main$q148n[main$q148n == 1 | main$q148n > 5] <- NA
main$q149n[main$q149n == 1 | main$q149n > 5] <- NA

main$q139n <- factor(main$q139n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q140n <- factor(main$q140n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q141n <- factor(main$q141n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q142n <- factor(main$q142n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q143n <- factor(main$q143n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q144n <- factor(main$q144n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q145n <- factor(main$q145n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q146n <- factor(main$q146n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q147n <- factor(main$q147n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q148n <- factor(main$q148n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q149n <- factor(main$q149n, levels=c(2,3,4,5), labels=c(0,1,2,3))

main$q139n <- as.numeric(levels(main$q139n))[main$q139n]
main$q140n <- as.numeric(levels(main$q140n))[main$q140n]
main$q141n <- as.numeric(levels(main$q141n))[main$q141n]
main$q142n <- as.numeric(levels(main$q142n))[main$q142n]
main$q143n <- as.numeric(levels(main$q143n))[main$q143n]
main$q144n <- as.numeric(levels(main$q144n))[main$q144n]
main$q145n <- as.numeric(levels(main$q145n))[main$q145n]
main$q146n <- as.numeric(levels(main$q146n))[main$q146n]
main$q147n <- as.numeric(levels(main$q147n))[main$q147n]
main$q148n <- as.numeric(levels(main$q148n))[main$q148n]
main$q149n <- as.numeric(levels(main$q149n))[main$q149n]

main$demo_value <-   ( (main$q139n +
                      #main$q140n + #china NA
                      main$q142n +
                      main$q143n +
                      main$q144n +
                      main$q145n +
                      main$q146n +
                      main$q147n +
                      main$q148n +
                      main$q149n) / 27 )

#### Political Participation ####

which( colnames(main)=="q77a" )

main$q69an <- as.numeric(main$q69a) ## col # = 78
main$q70an <- as.numeric(main$q70a)
main$q71an <- as.numeric(main$q71a)
main$q72an <- as.numeric(main$q72a)
main$q73an <- as.numeric(main$q73a)
main$q74an <- as.numeric(main$q74a)
main$q75an <- as.numeric(main$q75a)
main$q76an <- as.numeric(main$q76a)
main$q77an <- as.numeric(main$q77a) # 94

main$q69an[main$q69an == 1 | main$q69an > 5] <- NA
main$q70an[main$q70an == 1 | main$q70an > 5] <- NA
main$q71an[main$q71an == 1 | main$q71an > 5] <- NA
main$q72an[main$q72an == 1 | main$q72an > 5] <- NA
main$q73an[main$q73an == 1 | main$q73an > 5] <- NA
main$q74an[main$q74an == 1 | main$q74an > 5] <- NA
main$q75an[main$q75an == 1 | main$q75an > 5] <- NA
main$q76an[main$q76an == 1 | main$q76an > 5] <- NA
main$q77an[main$q77an == 1 | main$q77an > 5] <- NA

main$q69an <- factor(main$q69an, levels=c(2,3,4,5), labels=c(3,2,1,0)) # 311
main$q70an <- factor(main$q70an, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q71an <- factor(main$q71an, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q72an <- factor(main$q72an, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q73an <- factor(main$q73an, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q74an <- factor(main$q74an, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q75an <- factor(main$q75an, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q76an <- factor(main$q76an, levels=c(2,3,4,5), labels=c(3,2,1,0))
main$q77an <- factor(main$q77an, levels=c(2,3,4,5), labels=c(3,2,1,0)) # 319

#which( colnames(main)=="q69an" )
main[,c(311:319)] = apply(main[,c(311:319)], 2, function(x) as.numeric(as.character(x)));


main$pol_part <- ( (main$q69an +
                          main$q72an +
                          main$q73an +
                          main$q74an +
                          main$q75an) / 15 ) ## dropped 70, 71, 76, 77 for NAs in selected countries

main$pol_part_w <- (main$pol_part * main$w)

## remove for indonesia // inconsistent answers w/ the rest of the survey
main$pol_part[main$country == "Malaysia"] <- NA

#### Quality of Governance ####

main$q110n <- as.numeric(main$q110) 
main$q111n <- as.numeric(main$q111)
main$q112n <- as.numeric(main$q112)
main$q113n <- as.numeric(main$q113)

main$q110n[main$q110n == 1 | main$q110n > 5] <- NA
main$q111n[main$q111n == 1 | main$q111n > 5] <- NA
main$q112n[main$q112n == 1 | main$q112n > 5] <- NA
main$q113n[main$q113n == 1 | main$q113n > 5] <- NA

main$q110n <- factor(main$q110n, levels=c(2,3,4,5), labels=c(0,1,2,3)) # 333
main$q111n <- factor(main$q111n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q112n <- factor(main$q112n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q113n <- factor(main$q113n, levels=c(2,3,4,5), labels=c(0,1,2,3)) # 336

main[,c(333:336)] = apply(main[,c(333:336)], 2, function(x) as.numeric(as.character(x)));

main$qual_gov2 <- ( (main$q110n +
                      main$q111n +
                      main$q112n) / 9 ) # q113n removed for inconsistency

#### Preference for Democracy ####
main$q130n <- as.numeric(main$q130) 
main$q131n <- as.numeric(main$q131)
main$q132n <- as.numeric(main$q132)
main$q133n <- as.numeric(main$q133)

main$q130n[main$q130n == 1 | main$q130n > 5] <- NA
main$q131n[main$q131n == 1 | main$q131n > 5] <- NA
main$q132n[main$q132n == 1 | main$q132n > 5] <- NA
main$q133n[main$q133n == 1 | main$q133n > 5] <- NA

main$q130n <- factor(main$q130n, levels=c(2,3,4,5), labels=c(0,1,2,3)) # 339
main$q131n <- factor(main$q131n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q132n <- factor(main$q132n, levels=c(2,3,4,5), labels=c(0,1,2,3))
main$q133n <- factor(main$q133n, levels=c(2,3,4,5), labels=c(0,1,2,3)) # 342

main[,c(339:342)] = apply(main[,c(339:342)], 2, function(x) as.numeric(as.character(x)));

main$regime_pref <- ( (main$q130n +
                       main$q131n +
                       main$q132n +
                       main$q133n) / 12 )

#### Controls (for modeling) ####

## ses vars, gender etc.

# female
main$female <- ifelse(main$se2 == "Missing", c(NA), c(main$se2) )
main$female <- as.numeric(ifelse(main$female == 3, 1,0) )

main$age[main$se3.1=="Missing"] = NA

# age
main$age <- main$se3.2
main$age <- as.numeric(levels(main$se3.2))[main$se3.2]
main$age <- ifelse(main$age == 17, 18, main$age)
main$youth <- ifelse(main$age <30, 1, 0)

main$age_cohorts[main$age <30] <- "19-29"
main$age_cohorts[main$age >=30 & main$age <56] <- "30-55"
main$age_cohorts[main$age >55] <- "56+"


# university; some university+
main$university <- as.numeric(main$se5)
main$university[main$university == 1 | main$university > 11] <- NA
main$university <- ifelse(main$university >8, 1,0)

# religious
main$religious <- as.numeric(main$se7a)
main$religious[main$religious == 1 | main$religious > 6] <- NA
main$religious <- ifelse(main$religious == 3 | main$religious == 4, 1,0)
main$religion <- ifelse(main$se6 != "Missing" & main$se6 == "None", 1,0)

# marital status
main$married <- as.numeric(main$se4)
main$married[main$married == 1 | main$married > 6] <- NA
main$married <- ifelse(main$married >= 3, 1,0)

# urban
main$urban <- as.numeric(main$ir13)
main$urban[main$urban == 1] <- NA
main$urban <- ifelse(main$urban < 4, 1,0)

# income (household)
main$income <- as.numeric(main$se14)
main$income[main$income == 1 | main$income >= 7] <- NA

#main$income_high <- ifelse(main$income , 1,0)
#main$income_low <- ifelse(main$income < 4, 1,0)

# SES
main$ses <- as.numeric(main$se12)
main$ses[main$ses == 1 | main$ses >= 12] <- NA
main$ses <- factor(main$ses, levels=c(2,3,4,5,6,7,8,9,10,11), 
                    labels=c(1,2,3,4,5,6,7,8,9,10))
main$ses <- as.numeric(main$ses)

# country_f - setting ref category
main$country_f <- factor(main$country)
main$country_f = relevel(main$country_f, ref="Japan")

# year bins 
main$abins[main$age <25]="18-24"
main$abins[main$age >=25 & main$age <=29]="25-29"
main$abins[main$age >=30 & main$age <=34]="30-34"
main$abins[main$age >=35 & main$age <=39]="35-39"
main$abins[main$age >=40 & main$age <=44]="40-44"
main$abins[main$age >=45 & main$age <=49]="45-49"
main$abins[main$age >=50 & main$age <=54]="50-54"
main$abins[main$age >=55 & main$age <=59]="55-59"
main$abins[main$age >=60]="60+"

main$abinsf<-as.factor(main$abins)


#### Aggregate indices ####
main$pol_values_all <- ((main$q139n + main$q142n +
   main$q143n +
   main$q144n +
   main$q145n +
   main$q146n +
   main$q147n +
   main$q148n +
   main$q149n + 
   main$q130n +
   main$q131n +
   main$q132n +
   main$q133n) / 39 )

main$pol_values_all_2 <- ((main$demo_value + main$regime_pref) / 2)

main %>%
  group_by(country_f) %>%
  summarise_at(vars(pol_values_all), 
               funs(weighted.mean(., w=w)), na.rm=T ) %>%
  arrange(desc(pol_values_all))

main %>%
  group_by(country_f) %>%
  summarise_at(vars(pol_values_all_2), 
               funs(weighted.mean(., w=w)), na.rm=T ) %>%
  arrange(desc(pol_values_all_2))

complete %>%
  group_by(country_f) %>%
  summarise_at(vars(political_values), 
               funs(weighted.mean(., w=w)), na.rm=T ) %>%
  arrange(desc(political_values))

#### Save data file ####
# rename countries and country variable to match V-Dem
w.mean.all.vars <- w.mean.all.vars %>% 
   mutate(country_f=recode(country_f, "Korea" = "South Korea",
                           "Myanmar" = "Burma/Myanmar")) %>%
   rename(country_name=country_f)

# join with regime types
w.mean.all.vars <- left_join(w.mean.all.vars, dem_ep, by="country_name")

# weighted means, round to 2nd decimal place
w.mean.all.vars <- df_avg %>%
   group_by(country_f) %>%
   summarise_at(vars("trust","trust_inst","demo_value","pol_part","qual_gov2","regime_pref"),
                funs(weighted.mean(., w=w)), na.rm=T)
