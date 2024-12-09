library(tidyverse)
library(ggplot2)
library(lme4)
library(pscl)
library(jtools)
library(haven)
library(plm)
library(gplots)
library(sjPlot)
library(sjmisc)

options(scipen = 999)

#### load the data merged ####
dt <- load(file = "data_analysis.rda")

dt <- dt %>% rename (Log_Asylum_rate=logASLr, Amount_of_media_coverage=Artl, Amount_of_national_media = Artl_national, Edu=edu_is97,  
                         sex =sex_re)





# Random intercept model

#for revising the figure
dt_n <- dt_1 %>% rename (Log_Asylum_rate=logASLr,Amount_of_media_coverage=Artl)

# random intercept
logit_ri<-glmer(wrimm ~ 1+Log_Asylum_rate*Amount_of_media_coverage + factor(year) +(1|Land), 
                data=dt_n, family=binomial(link = "logit"), 
                control= glmerControl(optimizer="bobyqa"),nAGQ =0)
summary(logit_ri)

lattice::dotplot(ranef(logit_ri, which="Land", conVar=TRUE), scale= list(y=list(cex=0.5)))
exp(coef(logit_ri)$Land)

## Prediction for random intercept
# table for random intercept

tab_model(logit_ri)







# Random slope model with constrained intercept

## Original model : Log_Asylum_rate*Amount_of_media_coverage
### model1

# random slope with constrained intercept

model_1 <-glmer(wrimm ~ 1+Log_Asylum_rate*Amount_of_media_coverage + factor(year) +(Log_Asylum_rate*Amount_of_media_coverage-1|Land), data=dt, family=binomial(link = "logit"), 
                control= glmerControl(optimizer="bobyqa"),nAGQ =0)
summary(model_1)
exp(coef(model_1)$Land)

re_model1<-ranef(model_1, which="Land", conVar=TRUE)
names(re_model1)<-"The effect of asylum application rate and media salience across regions"
lattice::dotplot(re_model1, scales = list(x =list(relation = 'free')))

saveRDS(model_1, file = "U:/media_salience/Paper1_output_second_analysis/model_1.rds")

