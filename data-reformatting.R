##convert to a long format
pda_intubated_long <- survSplit(Surv(time = mv_since_ref, 
                                     event = extubation
) ~ study.id + ega_under_28 + ega_days + ega_ref + age_ref +  ref_days + ref_year + female + gestational.age + weight +  
  apgar_1 + apgar_5 + race_ref + maternal_rfs + inotropes + daylife_pda + diameter_duct + length_duct + peak_syst_velo + ao_doppler_retro + la_dil + lv_dil +
  mv_since_ref + daylife_pda + event + ade_res_day + surfactant + ref_med + pda_intervention.type + intervention_since_ref + referral.center + chd.type + no_courses_tx + 
  mv_type + pip + peep + fio2 + paw_measured + rss + osi,
data = pda_intubated,
end="tstop", 
cut = seq(from=0, to=364, by=1)) 

#create the variable indicating whether or not an individual had the intervention
pda_intubated_long <- pda_intubated_long %>% mutate(intervention=case_when(tstart>=intervention_since_ref ~ 1,
                                                                           TRUE ~ 0))
pda_intubated_long <- pda_intubated_long %>% group_by(study.id) %>% mutate(intervention_lag=lag(intervention, n=1, default=0)) %>% ungroup()
pda_intubated_long <- pda_intubated_long %>% 
  mutate(sl=case_when(intervention==0 ~ 0,  #for sensitivity analysis 4 - surgical ligation
                      intervention==1 & pda_intervention.type==1 ~ 1,
                      TRUE ~0)) %>%
  mutate(dc=case_when(intervention==0 ~ 0, #for sensitivity analysis 4 - device closure
                      intervention==1 & pda_intervention.type==2 ~ 1,
                      TRUE ~0))

#Sensitivity analysis 1
pda_intubated$date_death <- as.Date(pda_intubated$date_death)
pda_intubated_sensitivity_1 <- pda_intubated %>%
  mutate(last_followup_date = if_else(death == 1, date_death, last_followup_date))

pda_intubated_sensitivity_1 <- pda_intubated_sensitivity_1 %>% mutate(lastfu_days=last_followup_date - date.of.birth)
pda_intubated_sensitivity_1$lastfu_days <- as.numeric(pda_intubated_sensitivity_1$lastfu_days)
pda_intubated_sensitivity_1 <- pda_intubated_sensitivity_1 %>% mutate(mv_since_ref=lastfu_days - ref_days)
pda_intubated_sensitivity_1 <- pda_intubated_sensitivity_1 %>% mutate(mv_since_intervention=lastfu_days - daylife_pda)

pda_intubated_long_sensitivity_1 <- survSplit(Surv(time = mv_since_ref, 
                                     event = extubation
) ~ study.id + ega_days + ref_year + ega_ref + female + weight + ade_res_day +intervention_since_ref +  
  ref_med,
data = pda_intubated_sensitivity_1,
end="tstop", 
cut = seq(from=0, to=364, by=1)) 

pda_intubated_long_sensitivity_1 <- pda_intubated_long_sensitivity_1 %>% mutate(intervention=case_when(tstart>=intervention_since_ref ~ 1,
                                                                           TRUE ~ 0)) 
pda_intubated_long_sensitivity_1 <- pda_intubated_long_sensitivity_1 %>% group_by(study.id) %>% mutate(intervention_lag=lag(intervention, n=1, default=0)) %>% ungroup()


#Sensitivity analysis 3
pda_intubated_sensitivity_3 <- pda_intubated %>% filter(ref_days <= 14)
pda_intubated_long_sensitivity_3 <- survSplit(Surv(time = mv_since_ref, 
                                                   event = extubation
) ~ study.id + ega_days + ref_year + ref_days + ega_ref + female + weight + ade_res_day +intervention_since_ref +  
  ref_med + referral.center,
data = pda_intubated_sensitivity_3,
end="tstop", 
cut = seq(from=0, to=364, by=1)) 

pda_intubated_long_sensitivity_3$age_days <- pda_intubated_long_sensitivity_3$ref_days + pda_intubated_long_sensitivity_3$tstart
pda_intubated_long_sensitivity_3 <- pda_intubated_long_sensitivity_3 %>% mutate(intervention=case_when(tstart>=intervention_since_ref ~ 1,
                                                                                                       TRUE ~ 0)) 
pda_intubated_long_sensitivity_3 <- pda_intubated_long_sensitivity_3 %>% group_by(study.id) %>% mutate(intervention_lag=lag(intervention, n=1, default=0)) %>% ungroup()


