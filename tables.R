source("data cleaning.R")
source("data-reformatting.R")
source("analysis-function.R")

#unweighted tables
tempdat <- pda_intubated_long %>% mutate(temp_early = case_when(intervention==1 & intervention_lag==0 & tstart< early_num ~ 1,
                                                                intervention==0 & extubation==1 & tstart< early_num ~ 1,
                                                                intervention==0 & ade_res_day < early_num ~ 1,
                                                                TRUE ~ 0),
                                         temp_late = case_when(intervention==1 & intervention_lag==0 & tstart>= early_num & tstart<=14 ~ 1,
                                                               intervention==0 & extubation==1 & tstart<=14 ~ 1,
                                                               intervention==0 & ade_res_day==tstart & ade_res_day<=14 ~ 1,
                                                               TRUE ~ 0))

.dat_for_table1_unweighted <- bind_rows(tempdat %>% filter(temp_early==1) %>% group_by(study.id) %>% filter(row_number()==1) %>%
                                          mutate(group="early") %>% select(-c(temp_early, temp_late)),
                                        tempdat %>% filter(temp_late==1) %>% group_by(study.id) %>% filter(row_number()==1) %>%
                                          mutate(group="late") %>% select(-c(temp_early, temp_late)))

fac_vars <- c("pda_intervention.type","female","ega_under_28","surfactant","maternal_rfs","inotropes",
              "ref_med","extubation","event","chd.type","mv_type")
.dat_for_table1_unweighted <- .dat_for_table1_unweighted %>% mutate_at(fac_vars,factor)

tab1 <- CreateTableOne(data = .dat_for_table1_unweighted,
                       vars = c("female","gestational.age","ega_ref","weight","ref_year","ref_med","chd.type"),
                       strata = "group", test = F)
kableone(tab1,nonnormal=c("gestational.age","weight","ega_ref","ref_year"),contDigits = 1,showAllLevels = TRUE)

tab2 <- CreateTableOne(data = .dat_for_table1_unweighted,
                       vars = c("ref_days","apgar_1","apgar_5","race_ref","surfactant","maternal_rfs",
                                "inotropes"),
                       strata = "group", test = F)
kableone(tab2,nonnormal=c("apgar_1","apgar_5","ref_days"),contDigits = 1, showAllLevels = TRUE)

tab3 <- CreateTableOne(data = .dat_for_table1_unweighted,
                       vars = c("daylife_pda","diameter_duct","peak_syst_velo","ao_doppler_retro","la_dil","lv_dil"),
                       strata = "group", test = F)
kableone(tab3,nonnormal=c("daylife_pda","diameter_duct","peak_syst_velo"),contDigits = 1, showAllLevels = TRUE)

tab4 <- CreateTableOne(data = .dat_for_table1_unweighted,
                       vars = c("pda_intervention.type","intervention_since_ref","mv_since_ref","daylife_pda","event"),
                       strata = "group",test = F)
kableone(tab4,nonnormal=c("intervention_since_ref","mv_since_ref","daylife_pda"),contDigits = 1, showAllLevels = TRUE)

tab5 <- CreateTableOne(data = .dat_for_table1_unweighted,
                       vars = c("mv_type","pip","peep","paw_measured","fio2"),
                       strata = "group",test = F)
kableone(tab5,nonnormal=c("pip","peep","paw_measured","fio2"),contDigits = 1, showAllLevels = TRUE)

############
.formula_d_e = "intervention==1 ~ ns(tstart, knots=c(3), Boundary.knots=c(1,4)) + 
                                 ns(ega_ref, knots=c(27), Boundary.knots=c(25,29)) +
                                 ns(ega_days, knots=171, Boundary.knots=c(165, 182)) + 
                                 ns(ref_year, knots=2017, Boundary.knots=c(2016, 2021)) +
                                 ns(weight, knots=620, Boundary.knots=c(530, 840)) +
                                 female + ref_med"
.formula_d_l = "intervention==1 ~ ns(tstart, knots=c(3), Boundary.knots=c(1,9)) + 
                                ns(ega_ref, knots=c(27), Boundary.knots=c(25,29)) +
                                ns(ega_days, knots=171, Boundary.knots=c(165, 182)) +
                                ns(ref_year, knots=2017, Boundary.knots=c(2016, 2021)) +
                                ns(weight, knots=620, Boundary.knots=c(530, 840)) +
                                female + ref_med"
pda_intubated_long_tables <- overall_function(.dat=pda_intubated_long, 
                                              formula_d_e=.formula_d_e,
                                              formula_n_e=.formula_n_e,
                                              formula_d_l=.formula_d_l,
                                              formula_n_l=.formula_n_l,
                                              formula_outcome=.formula_outcome,
                                              point_estimate_only=TRUE, dataset_for_tables=TRUE) ### 9/11: am: added "tables" option

dat2 <- rbind(pda_intubated_long_tables %>% group_by(study.id) %>% filter(sw_early_prod != 0) %>% filter(tstart == min(max(tstart), early_num)) %>% ungroup() %>% 
                mutate(early_group=1, w=sw_early_prod),
              pda_intubated_long_tables %>% group_by(study.id) %>% filter(sw_late_prod != 0) %>% filter(tstart == min(max(tstart), 14)) %>% ungroup() %>% 
                mutate(early_group=0, w=sw_late_prod)) %>% 
  dplyr::select(study.id, early_group, w) %>% left_join(pda_intubated, by= "study.id")

dat2 <- dat2 %>%
  mutate(across(c("gestational.age", "weight", "apgar_1", "apgar_5", "fio2", "map", "rss", "diameter_duct", "w"), as.numeric)) %>%
  mutate(across(c("female", "ega_cat", "bw_cat", "race_ref", "surfactant", "maternal_rfs", "pda_intervention.type", 
                  "ref_med", "inotropes", "early_group", "event","chd.type"), as.factor))

tab1_w_pre <- svydesign(id = ~ 1,
                        weights = ~ w, 
                        data = dat2)

##weighted tables
tab1_w <- svyCreateTableOne(vars = c("female","gestational.age","ega_ref","weight","ref_year","ref_med","chd.type"),
                            strata = "early_group",
                            data = tab1_w_pre,test=F)
kableone(tab1_w,nonnormal=c("gestational.age","weight","ega_ref","ref_year"),contDigits = 1,showAllLevels = T)

tab2_w <- svyCreateTableOne(vars = c("ref_days","apgar_1","apgar_5","race_ref","surfactant","maternal_rfs",
                                     "inotropes"),
                            strata = "early_group",
                            data = tab1_w_pre,test=F)
kableone(tab2_w,nonnormal=c("apgar_1","apgar_5","ref_days"),contDigits = 1)

tab3_w <- svyCreateTableOne(data = tab1_w_pre,
                            vars = c("daylife_pda","diameter_duct","peak_syst_velo","ao_doppler_retro","la_dil","lv_dil"),
                            strata = "early_group",test=F)
kableone(tab3_w,nonnormal=c("daylife_pda","diameter_duct","peak_syst_velo"),contDigits = 1)

