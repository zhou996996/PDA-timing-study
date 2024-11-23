ipw_bootstrap_function <- function(formula_d_e, formula_n_e, formula_d_l, formula_n_l, .dat){
 
  ipw_denominator_early <- glm(formula = as.formula(formula_d_e), 
                               data = .dat %>% filter(intervention_lag==0 & tstart <early_num),
                               family = binomial(),
                               weights = Freq) 
  ipw_numerator_early <- glm(formula = as.formula(formula_n_e), 
                             data = .dat %>% filter(intervention_lag==0 & tstart <early_num), 
                             family = binomial(),
                             weights = Freq)
  #am 6/23: try using dat. the reason this is also good idea in general is that your function shouldn't depend on anything in the "global" environment
  .dat$p_d_early <- NA
  .dat[.dat$intervention_lag==0 & .dat$tstart < early_num,]$p_d_early <- predict(ipw_denominator_early, 
                                                                                 newdata = .dat %>% filter(intervention_lag==0 & tstart < early_num), 
                                                                                 type = "response") 
  .dat$p_n_early <- NA
  .dat[.dat$intervention_lag==0 & .dat$tstart < early_num,]$p_n_early <- predict(ipw_numerator_early, 
                                                                                 newdata = .dat %>% filter(intervention_lag==0 & tstart < early_num), 
                                                                                 type = "response") 
  #dat$p_n_early <- predict(ipw_numerator_early, newdata = dat, type = "response") 
  
  ipw_denominator_late <- glm(formula = as.formula(formula_d_l), 
                              data = .dat %>% filter(intervention_lag==0 & tstart >= early_num & tstart <=14), 
                              family = binomial(),
                              weights = Freq) 
  
  ipw_numerator_late <- glm(formula = as.formula(formula_n_l),
                            data = .dat %>% filter(intervention_lag==0 & tstart >= early_num & tstart <=14), 
                            family = binomial(),
                            weights = Freq)
  
  .dat$p_d_late <- NA
  .dat[.dat$intervention_lag==0 & .dat$tstart >= early_num & .dat$tstart <=14,]$p_d_late <- predict(ipw_denominator_late, ### am 8/29: for "new-levels" issue
                                                                                                    newdata = .dat %>% filter(intervention_lag==0 & tstart >= early_num & tstart <=14), 
                                                                                                    type = "response") 
  .dat$p_n_late <- NA
  .dat[.dat$intervention_lag==0 & .dat$tstart >= early_num & .dat$tstart <=14,]$p_n_late <- predict(ipw_numerator_late, 
                                                                                                    newdata = .dat %>% filter(intervention_lag==0 & tstart >= early_num & tstart <=14), 
                                                                                                    type = "response")  
  print("treatment models done")
  
   .dat <- .dat %>% 
    mutate(sw_early=case_when(tstart <early_num & (ade_res_day >= early_num | is.na(ade_res_day)) & intervention_lag != 1 & intervention==0 ~ numerator_function(j=tstart, A=0, m=early_num-1) / (1-p_d_early),  ##8/22: the code for earlier group also got changed.
                              tstart <early_num & ade_res_day< early_num & intervention_lag != 1 & intervention==0 ~ 1, #added ade_res_day
                              tstart <early_num & intervention_lag != 1 & intervention==1 ~ numerator_function(j=tstart, A=1, m=early_num-1) / p_d_early,
                              tstart >=early_num | intervention_lag==1 ~ 1)) %>%
    group_by(study.id) %>% mutate(sw_early_prod=cumprod(sw_early)) %>%
    ungroup()
  
  .dat <- .dat %>%
    mutate(sw_late=case_when(tstart < early_num & intervention_lag == 1 ~ 0,    ##8/22: that's where the weight code for later group got changed.
                             tstart < early_num & ade_res_day==tstart & ade_res_day < early_num & intervention_lag != 1 & intervention==0 ~ 1, #added ade_res_day
                             tstart < early_num& intervention_lag != 1 & intervention_lag != 1 & intervention==0 ~ (1 - p_n_early) / (1 - p_d_early),
                             tstart < early_num & intervention_lag != 1 & intervention==1 ~ 0,
                             tstart >= early_num & tstart < 14 & ade_res_day < 14 & intervention_lag != 1 & intervention==0 ~ 1,
                             tstart >= early_num & tstart < 14 & (ade_res_day>= early_num | is.na(ade_res_day)) & intervention_lag != 1 & intervention==0 ~ (1-p_n_late)*numerator_function(j=tstart-early_num, A=0, m=early_num) / (1-p_d_late),
                             tstart >= early_num & tstart < 14 & intervention_lag != 1 & intervention==1 ~ p_n_late*numerator_function(j=tstart-early_num, A=1, m=early_num) / p_d_late,
                             tstart >= early_num & tstart < 14 & intervention_lag == 1 ~ 1,
                             tstart >= 14 ~ 1)) %>%
    group_by(study.id) %>% mutate(sw_late_prod=cumprod(sw_late)) %>% ungroup()
  
  return(.dat)} # this asks the function to give you "dat" once it is done running


analysis_function <- function(.dat, 
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
    weighted.ids.table <- table(sample(unique.ids, size=N, replace=T)) %>% as.data.frame(., stringsAsFactors=FALSE) %>% 
      mutate(study.id=as.numeric(Var1)) %>% dplyr::select(-Var1) ### note that sum(weighted.ids.table$Freq) == N as expected
    .dat <- left_join(weighted.ids.table, .dat, by="study.id") ### now joins only what weighted.ids.table has (instead of left_join(date, weighted.ids.table,)
  } else if(!bootstrap | bs_num==0){ #point estimate only (no bootstrap)
    .dat <- .dat %>% mutate(Freq=1)
  }
  
  .dat <- ipw_bootstrap_function(
    formula_d_e=.formula_d_e,
    formula_n_e=.formula_n_e,
    formula_d_l=.formula_d_l,
    formula_n_l=.formula_n_l, 
    .dat=.dat)
  
  if(dataset_for_tables){ ### 9/11: am: added this option for tables
    return(.dat)
  }
  
  #outcome models
  pooled_ipw_early <- glm(formula = as.formula(.formula_outcome), 
                          data=.dat, 
                          family=quasibinomial(),
                          weights=sw_early_prod*Freq) #### look at this line/step and same in line 106 to understand why all bootstrap resample values were the same previously
  
  pooled_ipw_late <- glm(formula = as.formula(.formula_outcome),    
                         data=.dat, 
                         family=quasibinomial(),
                         weights=sw_late_prod*Freq) 
  print("outcome models done")
  
  p_early <- 1 - predict(pooled_ipw_early, newdata = data.frame(tstart=0:45),
                         type="response")
  
  p_late <- 1 - predict(pooled_ipw_late, newdata = data.frame(tstart=0:45),
                        type="response")
  results <- data.frame(day=0:45,
                        early= 1- cumprod(p_early),
                        late= 1- cumprod(p_late)) %>%
    mutate(rd=early-late) %>% mutate(rr=early/late) %>%
    mutate(across(everything(), ~ round(., 4)))
  return(results) ### am: 6/24, kept as data.frame instead of kable for post-processing in a different step
}
