ipw_bootstrap_function_sensi3 <- function(formula_d_e, formula_n_e, formula_d_l, formula_n_l, .dat, 
                                          min_age=14, lower_age=21, upper_age=35){
  ipw_denominator_early <- glm(formula = as.formula(formula_d_e), 
                               data = .dat %>% filter(intervention_lag==0 & age_days < lower_age),
                               family = binomial(),
                               weights = Freq) 
  ipw_numerator_early <- glm(formula = as.formula(formula_n_e), 
                             data = .dat %>% filter(intervention_lag==0 & age_days < lower_age), 
                             family = binomial(),
                             weights = Freq)
  .dat$p_d_early <- NA
  .dat[.dat$intervention_lag==0 & .dat$age_days < lower_age,]$p_d_early <- predict(ipw_denominator_early, 
                                                                                 newdata = .dat %>% filter(intervention_lag==0 & age_days < lower_age), 
                                                                                 type = "response") 
  .dat$p_n_early <- NA
  .dat[.dat$intervention_lag==0 & .dat$age_days < lower_age,]$p_n_early <- predict(ipw_numerator_early, 
                                                                                 newdata = .dat %>% filter(intervention_lag==0 & age_days < lower_age), 
                                                                                 type = "response") 
  #dat$p_n_early <- predict(ipw_numerator_early, newdata = dat, type = "response") 
  
  ipw_denominator_late <- glm(formula = as.formula(formula_d_l), 
                              data = .dat %>% filter(intervention_lag==0 & age_days <= upper_age), 
                              family = binomial(),
                              weights = Freq) 
  
  ipw_numerator_late <- glm(formula = as.formula(formula_n_l),
                            data = .dat %>% filter(intervention_lag==0 & age_days <= upper_age), 
                            family = binomial(),
                            weights = Freq)
  
  .dat$p_d_late <- NA
  .dat[.dat$intervention_lag==0 & .dat$age_days <= upper_age,]$p_d_late <- predict(ipw_denominator_late, ### am 8/29: for "new-levels" issue
                                                                                                    newdata = .dat %>% filter(intervention_lag==0 & age_days <= upper_age), 
                                                                                                    type = "response") 
  .dat$p_n_late <- NA
  .dat[.dat$intervention_lag==0 & .dat$age_days <= upper_age,]$p_n_late <- predict(ipw_numerator_late, 
                                                                                                    newdata = .dat %>% filter(intervention_lag==0 & age_days <= upper_age), 
                                                                                                    type = "response")  
 .dat <- .dat %>% 
    mutate(sw_early=case_when(age_days < lower_age & (ade_res_day >= lower_age | is.na(ade_res_day)) & intervention_lag != 1 & intervention==0 ~ numerator_function(j=age_days - min_age, A=0, m=lower_age-min_age) / (1-p_d_early),
                              age_days < lower_age & ade_res_day< lower_age & intervention_lag != 1 & intervention==0 ~ 1, #added ade_res_day
                              age_days < lower_age & intervention_lag != 1 & intervention==1 ~ numerator_function(j=age_days - min_age, A=1, m=lower_age-min_age) / p_d_early,
                              age_days >= lower_age | intervention_lag==1 ~ 1)) %>%
    group_by(study.id) %>% mutate(sw_early_prod=cumprod(sw_early)) %>%
    ungroup()
  
  .dat <- .dat %>%
    mutate(sw_late=case_when(age_days < lower_age & intervention_lag == 1 ~ 0,
                             age_days < lower_age & ade_res_day == age_days &  ade_res_day < lower_age & intervention_lag != 1 & intervention==0 ~ 1,
                             age_days < lower_age & (ade_res_day >= lower_age | is.na(ade_res_day)) & intervention_lag != 1 & intervention==0 ~ (1 - p_n_late) / (1 - p_d_late),
                             age_days < lower_age & intervention_lag != 1 & intervention==1 ~ 0,
                             age_days >= lower_age & age_days <= upper_age & ade_res_day < upper_age & intervention_lag != 1 & intervention==0 ~ 1,
                             age_days >= lower_age & age_days <= upper_age & (ade_res_day>upper_age | is.na(ade_res_day)) & intervention_lag != 1 & intervention==0 ~ (1-p_n_late)*numerator_function(j=age_days-lower_age, A=0, m=upper_age-lower_age) / (1-p_d_late),
                             age_days >= lower_age & age_days <= upper_age & intervention_lag != 1 & intervention==1 ~ p_n_late*numerator_function(j=age_days-lower_age, A=1, m=upper_age-lower_age) / p_d_late,
                             age_days >= lower_age & age_days <= upper_age & intervention_lag == 1 ~ 1,
                             age_days > upper_age ~ 1)) %>%
    group_by(study.id) %>% mutate(sw_late_prod=cumprod(sw_late)) %>% ungroup()
  
  return(.dat) # this asks the function to give you "dat" once it is done running
}

#06/23 z: add analysis function for bootstraping
analysis_function_sensi3 <- function(.dat, 
                              formula_d_e,
                              formula_n_e,
                              formula_d_l,
                              formula_n_l,
                              formula_outcome, 
                              bootstrap, bs_num=NULL,
                              dataset_for_tables=FALSE) {
  
  #bootstrap
  if(bootstrap & bs_num != 0){
    unique.ids <- unique(.dat$study.id)
    N <- length(unique.ids)
    set.seed(bs_num) # keeps replicable
    print(bs_num)
    weighted.ids.table <- table(sample(unique.ids, size=N, replace=T)) %>% as.data.frame(., stringsAsFactors=FALSE) %>% 
      mutate(study.id=as.numeric(Var1)) %>% dplyr::select(-Var1) ### note that sum(weighted.ids.table$Freq) == N as expected
    .dat <- left_join(weighted.ids.table, .dat, by="study.id") ### now joins only what weighted.ids.table has (instead of left_join(date, weighted.ids.table,)
  } else if(!bootstrap | bs_num==0){ #point estimate only (no bootstrap)
    .dat <- .dat %>% mutate(Freq=1)
  }
  
  .dat <- ipw_bootstrap_function_sensi3(
    formula_d_e=formula_d_e,
    formula_n_e=formula_n_e,
    formula_d_l=formula_d_l,
    formula_n_l=formula_n_l, 
    .dat=.dat)
  
  if(dataset_for_tables){ ### 9/11: am: added this option for tables
    return(.dat)
  }
  
  #outcome models
  pooled_ipw_early <- glm(formula = as.formula(formula_outcome), 
                          data=.dat, 
                          family=quasibinomial(),
                          weights=sw_early_prod*Freq) #### look at this line/step and same in line 106 to understand why all bootstrap resample values were the same previously
  
  pooled_ipw_late <- glm(formula = as.formula(formula_outcome),    
                         data=.dat, 
                         family=quasibinomial(),
                         weights=sw_late_prod*Freq) 
  print("outcome models done")
  
  #p_early <- 1 - predict(pooled_ipw_early, newdata = data.frame(age_days=14:80),
  #                       type="response")
  #
  #p_late <- 1 - predict(pooled_ipw_late, newdata = data.frame(age_days=14:80),
  #                      type="response")
  #results <- data.frame(day=14:80,
  
  p_early <- 1 - predict(pooled_ipw_early, newdata = data.frame(tstart=0:45),  #x-axis should be days since referral?
                         type="response")
  p_late <- 1 - predict(pooled_ipw_late, newdata = data.frame(tstart=0:45),
                        type="response")
  results <- data.frame(day=0:45,
                        early= 1- cumprod(p_early),
                        late= 1- cumprod(p_late)) %>%
    mutate(rd=early-late) %>% mutate(rr=early/late) %>%
    mutate(across(everything(), ~ round(., 4)))
  return(results) 
}
