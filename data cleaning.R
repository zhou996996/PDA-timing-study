if (!require("pacman")) install.packages("pacman"); library(pacman) 
p_load(skimr, tidyr, dplyr, lubridate, knitr, ggplot2, survival, tableone, here, survminer,ggsurvfit,xtable,splines,testthat,survey) ## added lubridate for year function

################################Start of the analysis########################################
###data cleaning
pda_bl_final <- read.csv(here("data","pda_updated_Nov4.csv")) 

pda_bl_final [pda_bl_final == ''] <- NA
names(pda_bl_final) <- tolower(names(pda_bl_final))
pda_intubated <- pda_bl_final %>% filter(intubated_ref==1)


pda_intubated$last_followup_date <- as.Date(pda_intubated$last_followup_date)
pda_intubated$date.of.birth <- as.Date(pda_intubated$date.of.birth)
pda_intubated$ref_date <- as.Date(pda_intubated$ref_date)

pda_intubated <- pda_intubated %>% mutate(ref_year=lubridate::year(ref_date)) ##10/1 z:create ref_year (year of referral) to include into models
pda_intubated$ref_year <- as.numeric(pda_intubated$ref_year)
pda_intubated <- pda_intubated %>% mutate(ref_days=ref_date - date.of.birth)
pda_intubated$ref_days <- as.numeric(pda_intubated$ref_days)
pda_intubated <- pda_intubated %>% mutate(lastfu_days=last_followup_date - date.of.birth)
pda_intubated$lastfu_days <- as.numeric(pda_intubated$lastfu_days)
pda_intubated <- pda_intubated %>% mutate(mv_since_ref=lastfu_days - ref_days)
pda_intubated <- pda_intubated %>% mutate(intervention_since_ref=daylife_pda - ref_days)
pda_intubated <- pda_intubated %>% mutate(mv_since_intervention=lastfu_days - daylife_pda)
pda_intubated <- pda_intubated %>% mutate(ega_days=7*gestational.age + gestational.age.day)
pda_intubated <- pda_intubated %>% mutate(ega_ref=floor((ega_days + ref_days)/7))
pda_intubated <- pda_intubated %>% filter(ega_ref<30) #only enroll <30 weeks
pda_intubated <- pda_intubated %>% mutate(age_ref=floor((ref_days + ega_days)/7))

##recode variables, sex, surfactant
##generate binary variables according to other variables
pda_intubated <- pda_intubated %>% mutate(female=case_when(sex=="Female" ~ 1,
                                                           sex=="Male" ~ 0))
pda_intubated <- pda_intubated %>% mutate(ref_med=case_when(no_courses_tx==0 | is.na(no_courses_tx) ~ 0,
                                                            TRUE ~ 1))
pda_intubated <- pda_intubated %>% mutate(surfactant=case_when(surfactant=="Yes" ~ 1,
                                                               TRUE ~0))
pda_intubated <- pda_intubated %>% mutate(tx_under_2= case_when(no_courses_tx<=2~ 1,
                                                                no_courses_tx>2 ~0)) #medical treatment course under 2

##Create event variable, where 1 is extubation within follow-up, 2 is lost to follow-up, 3 is death, 4 is administrative censoring       
pda_intubated <- pda_intubated %>% 
  mutate(event=case_when(extubation==1 & mv_since_ref <= 45 ~ 1,
                         extubation==0 & mv_since_ref <= 45 ~ 2,
                         death==1 ~ 3,
                         TRUE ~ 4)) 
