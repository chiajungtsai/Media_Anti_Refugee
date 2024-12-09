# For access to GSOEP, please contact: https://www.diw.de/en/diw_01.c.615551.en/research_infrastructure__socio-economic_panel__soep.html
# Data Version: SOEP_v34_corrected_2019_03_18 
# File: pl.dta, hbrutto.dta

library(tidyverse)
library(haven)

#### GSOEP ####

setwd('path to GSOEP data')
pl <- read_dta("/SOEP_v34_corrected_2019_03_18/STATA_DEEN_v34/soep.v34/stata_de+en/pl.dta",col_select=c("pid","hid","syear","plj0046"))%>%
  filter(syear>=2010)

pla<- pl %>% filter(syear>=2014)

bula <- read_dta("/SOEP_v34_corrected_2019_03_18/STATA_DEEN_v34/soep.v34/stata_de+en/hbrutto.dta",col_select=c("hid","syear","bula"))%>%
  filter(syear>=2010)

dt<- merge(pl, bula, by=c("hid","syear"))
dt[dt$plj0046==-1]<-NA
dt[dt$plj0046==-5]<-NA

# code plj0046 into binary
dt$wrimm[dt$plj0046==1] <- 1
dt$wrimm[dt$plj0046==2] <- 0
dt$wrimm[dt$plj0046==3] <- 0

# code bula into Land (German States)
dt$Land[dt$bula==1] <- "Schleswig-Holstein"
dt$Land[dt$bula==2] <- "Hamburg"
dt$Land[dt$bula==3] <- "Niedersachsen"
dt$Land[dt$bula==4] <- "Bremen"
dt$Land[dt$bula==5] <- "Nordrhein-Westfalen"
dt$Land[dt$bula==6] <- "Hessen"
dt$Land[dt$bula==7] <- "Rheinland-Pfalz"
dt$Land[dt$bula==8] <- "Baden-Württemberg"
dt$Land[dt$bula==9] <- "Bayern"
dt$Land[dt$bula==10] <- "Saarland"
dt$Land[dt$bula==11] <- "Berlin"
dt$Land[dt$bula==12] <- "Brandenburg"
dt$Land[dt$bula==13] <- "Mecklenburg-Vorpommern"
dt$Land[dt$bula==14] <- "Sachsen"
dt$Land[dt$bula==15] <- "Sachsen-Anhalt"
dt$Land[dt$bula==16] <- "Thüringen"

save(dt,file = "/soep_dt.rda")


#### read in covariates EDU from GSOEP ####

pl_edu<- read_dta("/SOEP_v34_corrected_2019_03_18/STATA_DEEN_v34/soep.v34/stata_de+en/pl.dta",col_select=c("pid","hid","syear","plg0079_v3","plg0079_v4"))%>%
  filter(syear>=2010)
bula <- read_dta("/SOEP_v34_corrected_2019_03_18/STATA_DEEN_v34/soep.v34/stata_de+en/hbrutto.dta",col_select=c("hid","syear","bula"))%>%
  filter(syear>=2010)
dt_edu<- merge(pl_edu, bula, by=c("hid","syear")) 
# no pid in bula, using hid also okay

table(dt_edu$plg0079_v3)
#-8 (question not a part of the questionnaire this year)    
#-5 (does not include in the questionnaire version)     
#-2 (does not apply)  
#1 University of applied science (Fachhochschule)     
#2 Uni, college      
#3 Promotion (Dr PostDoc)

# code plg0079_v3 into higher-education
dt_edu$edu1[dt_edu$plg0079_v3==-8]<-NA
dt_edu$edu1[dt_edu$plg0079_v3==-5]<-NA
dt_edu$edu1[dt_edu$plg0079_v3==-2]<-NA 
dt_edu$edu1[dt_edu$plg0079_v3==1]<-1
dt_edu$edu1[dt_edu$plg0079_v3==2]<-1
dt_edu$edu1[dt_edu$plg0079_v3==3]<-1

table(dt_edu$plg0079_v4)
#-8 (question not a part of the questionnaire this year)    
#-5 (does not include in the questionnaire version)     
#-2 (does not apply)
#-1 (no information)
#1 University of applied science (Fachhochschule)
#2 Uni, Tech uni
#3 Promotion (Dr PostDoc)
#4 Double Uni, professional(Beruf) academy 
#5 other, higher education 

dt_edu$edu2[dt_edu$plg0079_v4==-8]<-NA
dt_edu$edu2[dt_edu$plg0079_v4==-5]<-NA
dt_edu$edu2[dt_edu$plg0079_v4==-2]<-NA 
dt_edu$edu2[dt_edu$plg0079_v4==-1]<-NA
dt_edu$edu2[dt_edu$plg0079_v4==1]<-1
dt_edu$edu2[dt_edu$plg0079_v4==2]<-1
dt_edu$edu2[dt_edu$plg0079_v4==3]<-1
dt_edu$edu2[dt_edu$plg0079_v4==4]<-1
dt_edu$edu2[dt_edu$plg0079_v4==5]<-1


#plg0079_v3 includes data until 2013, after 2014, it should be plg0079_v4
dt_edu <- dt_edu %>% mutate(higher_edu=coalesce(edu1,edu2))

# code bula into Land (German States)
dt_edu$Land[dt_edu$bula==1] <- "Schleswig-Holstein"
dt_edu$Land[dt_edu$bula==2] <- "Hamburg"
dt_edu$Land[dt_edu$bula==3] <- "Niedersachsen"
dt_edu$Land[dt_edu$bula==4] <- "Bremen"
dt_edu$Land[dt_edu$bula==5] <- "Nordrhein-Westfalen"
dt_edu$Land[dt_edu$bula==6] <- "Hessen"
dt_edu$Land[dt_edu$bula==7] <- "Rheinland-Pfalz"
dt_edu$Land[dt_edu$bula==8] <- "Baden-WÃ¼rttemberg"
dt_edu$Land[dt_edu$bula==9] <- "Bayern"
dt_edu$Land[dt_edu$bula==10] <- "Saarland"
dt_edu$Land[dt_edu$bula==11] <- "Berlin"
dt_edu$Land[dt_edu$bula==12] <- "Brandenburg"
dt_edu$Land[dt_edu$bula==13] <- "Mecklenburg-Vorpommern"
dt_edu$Land[dt_edu$bula==14] <- "Sachsen"
dt_edu$Land[dt_edu$bula==15] <- "Sachsen-Anhalt"
dt_edu$Land[dt_edu$bula==16] <- "ThÃ¼ringen"



############## another edu variable#################

pgen_edu<- read_dta("SOEP_v34_corrected_2019_03_18/STATA_DEEN_v34/soep.v34/stata_de+en/pgen.dta",col_select=c("pid","hid","syear","pgisced97","pgisced11","pgcasmin"))%>%
  filter(syear>=2010)

dt_edu<- merge(dt_edu,pgen_edu, by=c("pid","syear"))
# should use pid here

#pgisced97 (number below are from the SOEP data report, includes all year. here only 2010-17)
#-1 no answer 13506    
#0 [0] in school 16268
#1 [1] inadequately 23461
#2 [2] general elementary 101897
#3 [3] middle vocational 309103
#4 [4] vocational + Abi 35191
#5 [5] higher vocational 42574
#6 [6] higher education 115963

dt_edu$edu_is97[dt_edu$pgisced97==-1]<-NA
dt_edu$edu_is97[dt_edu$pgisced97==1]<-0
dt_edu$edu_is97[dt_edu$pgisced97==2]<-0 
dt_edu$edu_is97[dt_edu$pgisced97==3]<-0
dt_edu$edu_is97[dt_edu$pgisced97==4]<-0
dt_edu$edu_is97[dt_edu$pgisced97==5]<-0
dt_edu$edu_is97[dt_edu$pgisced97==6]<-1

#pgisced11
#-1 no answer     
#0 [0] in school 6511
#1 [1] Primary education 7893
#2 [2] Lower secondary education 28291
#3 [3] Upper secondary education 104808
#4 [4] Post-secondary non-tertiary education 18054
#5 [5] Short-cycle tertiary education 9784
#6 [6] Bachelors or equivalent level 33806
#7 [7] Masters or equivalent level 17334
#8 [8] Doctoral or equivalent level 1923

dt_edu$edu_is11[dt_edu$pgisced11==-1]<-NA
dt_edu$edu_is11[dt_edu$pgisced11==1]<-0
dt_edu$edu_is11[dt_edu$pgisced11==2]<-0 
dt_edu$edu_is11[dt_edu$pgisced11==3]<-0
dt_edu$edu_is11[dt_edu$pgisced11==4]<-0
dt_edu$edu_is11[dt_edu$pgisced11==5]<-0
dt_edu$edu_is11[dt_edu$pgisced11==6]<-1
dt_edu$edu_is11[dt_edu$pgisced11==7]<-1
dt_edu$edu_is11[dt_edu$pgisced11==8]<-1

#pgcasmin  CASMIN Classification
#0 [0] (0) In School 16171
#1 [1] (1a) Inadequately Completed 26233
#2 [2] (1b) General Elementary School 80648
#3 [3] (1c) Basic Vocational Qualification 168470
#4 [4] (2b) Intermediate General Qualification 22919
#5 [5] (2a) Intermediate Vocational 136176
#6 [6] (2c_gen) General Maturity Certificate 28295
#7 [7] (2c_voc) Vocational Maturity Certificate 45929
#8 [8] (3a) Lower Tertiary Education 42789
#9 [9] (3b) Higher Tertiary Education 73174
#-1 [-1] No Answer 17159
#-2 [-2] Does not apply 0
#-3 [-3] Answer improbable 0
#-4 [-4] Inadmissible multiple response 0
#-5 [-5] Not included in this version of the questionnaire 0
#-6 [-6] Version of questionnaire with modified filtering 0
#-8 [-8] Question this year not part of Survey program 0

dt_edu$edu_ismin[dt_edu$pgcasmin==-1]<-NA
dt_edu$edu_ismin[dt_edu$pgcasmin==1]<-0
dt_edu$edu_ismin[dt_edu$pgcasmin==2]<-0 
dt_edu$edu_ismin[dt_edu$pgcasmin==3]<-0
dt_edu$edu_ismin[dt_edu$pgcasmin==4]<-0
dt_edu$edu_ismin[dt_edu$pgcasmin==5]<-0
dt_edu$edu_ismin[dt_edu$pgcasmin==6]<-0
dt_edu$edu_ismin[dt_edu$pgcasmin==7]<-0
dt_edu$edu_ismin[dt_edu$pgcasmin==8]<-1
dt_edu$edu_ismin[dt_edu$pgcasmin==9]<-1

save(dt_edu, file = "dt_edu.rda")


#### read in covariates age, gender, and nationality from GSOEP ####

dt_sex_age_deu<- read_dta("/SOEP_v34_corrected_2019_03_18/STATA_DEEN_v34/soep.v34/stata_de+en/pbrutto.dta",col_select=c("pid","hid","syear","geburt_v1","geburt_v2","sex","pnat_v1","pnat2","pnat_v2"))%>%
  filter(syear>=2010)

#birth year
dt_sex_age_deu$geburt_v2[dt_sex_age_deu$geburt_v2==-1]<-NA

#sex
dt_sex_age_deu$sex_re[dt_sex_age_deu$sex==-1]<-NA
dt_sex_age_deu$sex_re[dt_sex_age_deu$sex==9]<-NA
dt_sex_age_deu$sex_re[dt_sex_age_deu$sex==1]<-1 #male
dt_sex_age_deu$sex_re[dt_sex_age_deu$sex==2]<-0 #female

# nationality
## pbrutto/pnat_v1: 1. Nationality [1985-1993, 1995] #all missings
## pbrutto/pnat2: 2nd Nationality
## pbrutto/pnat_v2: 1. Nationality [1994-2018]

dt_sex_age_deu <- dt_sex_age_deu %>% 
  mutate(
    nationality = case_when(
      pnat_v2>1 ~ 0 ,  # other countries
      pnat_v2==1 ~ 1,  # Germany
      pnat_v2<0 ~ NA_real_) # missings: NA_real_: numeric form of NA
  )


dt<- merge(dt_edu,dt_sex_age_deu, by=c("pid","syear"))
dt_soep_covariate <- dt %>% 
  select("pid","hid.x","syear","Land","sex_re","higher_edu",
                                   "edu_is97","edu_is11","edu_ismin", "geburt_v2",
                                   "nationality") 


save(dt_soep_covariate,file = "dt_soep_covariate.rda")



#### read in influx data and Gdelt media data ####

# influx data from Satistiches Bundesamt Deutschland: https://www.destatis.de/DE/Home/_inhalt.html 
load("Land_year_longformat.rda")

# cleaned and aggregated media data from the Gdelt project: https://www.gdeltproject.org/
load("U:/media_salience/Data/Media_by_Gdelt.rda")
load("U:/media_salience/Data/Media_by_Gdelt_national.rda")

# cleaned GSOEP data from above
load("U:/media_salience/Data/dt_soep.rda") # contain hid, syear, pid, plj0046, bula, Land 
load("U:/media_salience/Data/dt_soep_covariate.rda") # contain hid, syear, pid, sex_re, Land, higher_edu, 

# clean state labels (all states names in English, beware if all "ü" is cleaned)
dt$Land[dt$Land=="Niedersachsen"] <- "Lower Saxony"
dt$Land[dt$Land=="Nordrhein-Westfalen"] <- "North Rhein-WestPhalia"
dt$Land[dt$Land=="Hessen"] <- "Hesse"
dt$Land[dt$Land=="Rheinland-Pfalz"] <- "Rheinland-Palatinate"
dt$Land[dt$Land=="Bayern"] <- "Bavaria"
dt$Land[dt$Land=="Mecklenburg-Vorpommern"] <- "Mecklenburg-Western Pomerania"
dt$Land[dt$Land=="Sachsen"] <- "Saxony"
dt$Land[dt$Land=="Sachsen-Anhalt"] <- "Saxony-Anhalt"
dt$Land[dt$Land=="Thüringen"] <- "Thuringia"

Land_year_longformat$Land[Land_year_longformat$Land=="Niedersachsen"] <- "Lower Saxony"
Land_year_longformat$Land[Land_year_longformat$Land=="Nordrhein-Westfalen"] <- "North Rhein-WestPhalia"
Land_year_longformat$Land[Land_year_longformat$Land=="Hessen"] <- "Hesse"
Land_year_longformat$Land[Land_year_longformat$Land=="Rheinland-Pfalz"] <- "Rheinland-Palatinate"
Land_year_longformat$Land[Land_year_longformat$Land=="Bayern"] <- "Bavaria"
Land_year_longformat$Land[Land_year_longformat$Land=="Mecklenburg-Vorpommern"] <- "Mecklenburg-Western Pomerania"
Land_year_longformat$Land[Land_year_longformat$Land=="Sachsen"] <- "Saxony"
Land_year_longformat$Land[Land_year_longformat$Land=="Sachsen-Anhalt"] <- "Saxony-Anhalt"
Land_year_longformat$Land[Land_year_longformat$Land=="Thüringen"] <- "Thuringia"

Media$GE_state[Media$GE_state=="Niedersachsen"] <- "Lower Saxony"
Media$GE_state[Media$GE_state=="Nordrhein-Westfalen"] <- "North Rhein-WestPhalia"
Media$GE_state[Media$GE_state=="Hessen"] <- "Hesse"
Media$GE_state[Media$GE_state=="Rheinland-Pfalz"] <- "Rheinland-Palatinate"
Media$GE_state[Media$GE_state=="Bayern"] <- "Bavaria"
Media$GE_state[Media$GE_state=="Mecklenburg-Vorpommern"] <- "Mecklenburg-Western Pomerania"
Media$GE_state[Media$GE_state=="Sachsen"] <- "Saxony"
Media$GE_state[Media$GE_state=="Sachsen-Anhalt"] <- "Saxony-Anhalt"
Media$GE_state[Media$GE_state=="Thüringen"] <- "Thuringia"

dt_soep_covariate$Land[dt_soep_covariate$Land=="Niedersachsen"] <- "Lower Saxony"
dt_soep_covariate$Land[dt_soep_covariate$Land=="Nordrhein-Westfalen"] <- "North Rhein-WestPhalia"
dt_soep_covariate$Land[dt_soep_covariate$Land=="Hessen"] <- "Hesse"
dt_soep_covariate$Land[dt_soep_covariate$Land=="Rheinland-Pfalz"] <- "Rheinland-Palatinate"
dt_soep_covariate$Land[dt_soep_covariate$Land=="Bayern"] <- "Bavaria"
dt_soep_covariate$Land[dt_soep_covariate$Land=="Mecklenburg-Vorpommern"] <- "Mecklenburg-Western Pomerania"
dt_soep_covariate$Land[dt_soep_covariate$Land=="Sachsen"] <- "Saxony"
dt_soep_covariate$Land[dt_soep_covariate$Land=="Sachsen-Anhalt"] <- "Saxony-Anhalt"
dt_soep_covariate$Land[dt_soep_covariate$Land=="ThÃ¼ringen"] <- "Thuringia"
dt_soep_covariate$Land[dt_soep_covariate$Land=="Baden-WÃ¼rttemberg"] <-"Baden-Württemberg"


# Merge
dt_1<- merge(dt,Land_year_longformat,by.x=c("syear","Land"),by.y = c("Year","Land"))
Media_national <- Media_national %>% 
  setNames(paste0(names(.),"_national"))
Media_national<- na.omit(Media_national) #national delete "NA"
Media <- merge(Media, Media_national, by.x=c("Pub_Year"), by.y = c("Pub_Year_national"))
Media<- within(Media, rm(GE_state_national))
dt_1 <- merge(dt_1, Media, by.x=c("syear","Land"), by.y = c("Pub_Year","GE_state"))
dt_1 <- merge(dt_1, dt_soep_covariate, by.x=c("syear","Land","hid","pid"), by.y = c("syear","Land","hid.x","pid"))

# count age from the birth year
dt_1$age <- dt_1$syear-dt_1$geburt_v2+1


# select variables used in models
dt_1<- dt_1 %>%  mutate(logASLr=log(Asl_rate), logFrn=log(Frn_rate)) %>% rename(year=syear)
dt_2<- dt_1 %>% select(hid, pid, year, Land, W_E, plj0046, wrimm, 
                       logASLr, Asl_rate, logFrn, Frn_rate,  
                       Artl, Tone, Artl_national, Tone_national, 
                       edu_is97, nationality, sex_re, age) 

dt_2 <- dt_2 %>% drop_na() 
save(dt_2,file = "data_analysis.rda")
