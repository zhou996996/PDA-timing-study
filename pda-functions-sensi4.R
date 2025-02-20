ipw_bootstrap_function_sensi6 <- function(formula_d_e, formula_n_e, formula_d_l, formula_n_l, .dat){
  
  ipw_denominator_sl <- glm(formula = as.formula(formula_d_e), 
                               data = .dat %>% filter(intervention_lag==0 & sl==1),
                               family = binomial(),
                               weights = Freq) 
  ipw_numerator_sl <- glm(formula = as.formula(formula_n_e), 
                             data = .dat %>% filter(intervention_lag==0 & sl==1), 
                             family = binomial(),
                             weights = Freq)

  .dat$p_d_sl <- NA
  .dat[.dat$intervention_lag==0 & .dat$sl==1,]$p_d_sl <- predict(ipw_denominator_sl,newdata = .dat %>% filter(intervention_lag==0 & sl==1),type = "response") 
  .dat$p_n_sl <- NA
  .dat[.dat$intervention_lag==0 & .dat$sl==1,]$p_n_sl <- predict(ipw_numerator_sl,newdata = .dat %>% filter(intervention_lag==0 & sl==1),type = "response") 

  ipw_denominator_dc <- glm(formula = as.formula(formula_d_l), 
                              data = .dat %>% filter(intervention_lag==0 & dc==1), 
                              family = binomial(),
                              weights = Freq) 
  
  ipw_numerator_dc <- glm(formula = as.formula(formula_n_l),
                            data = .dat %>% filter(intervention_lag==0 & dc==1), 
                            family = binomial(),
                            weights = Freq)
  
  .dat$p_d_dc <- NA
  .dat[.dat$intervention_lag==0 & .dat$dc==1,]$p_d_dc <- predict(ipw_denominator_dc, newdata = .dat %>% filter(intervention_lag==0 & dc==1),type = "response") 
  .dat$p_n_dc <- NA
  .dat[.dat$intervention_lag==0 & .dat$dc==1,]$p_n_dc <- predict(ipw_numerator_dc,newdata = .dat %>% filter(intervention_lag==0 & dc==1),type = "response")  
  print("treatment models done")
  
  .dat <- .dat %>% 
    mutate(sw_sl=case_when(intervention == 0 ~ 1,
                           intervention_lag==0 & intervention==1 & sl==1 ~ p_n_sl/p_d_sl,
                           intervention_lag==1 & sl==1 ~ 1,
                           intervention==1 & sl==0 & dc==1 ~ 0)) %>%
    group_by(study.id) %>% mutate(sw_sl_prod=cumprod(sw_sl)) %>%
    ungroup()
  
  .dat <- .dat %>%
    mutate(sw_dc=case_when(intervention== 0 ~ 1,
                           intervention_lag==0 & intervention==1 & dc==1 ~ p_n_dc/p_d_dc, 
                           intervention_lag==1 & dc==1 ~ 1,
                           intervention==1 & dc==0 & sl==1 ~ 0)) %>%
    group_by(study.id) %>% mutate(sw_dc_prod=cumprod(sw_dc)) %>% ungroup()
  
  return(.dat)} # this asks the function to give you "dat" once it is done running

#06/23 z: add analysis function for bootstraping
analysis_function_sensi6 <- function(.dat, 
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
  
  .dat <- ipw_bootstrap_function_sensi6(
    formula_d_e=.formula_d_e,
    formula_n_e=.formula_n_e,
    formula_d_l=.formula_d_l,
    formula_n_l=.formula_n_l, 
    .dat=.dat)
  
  if(dataset_for_tables){ ### 9/11: am: added this option for tables
    return(.dat)
  }
  
  #outcome models
  pooled_ipw_sl <- glm(formula = as.formula(.formula_outcome), 
                          data=.dat, 
                          family=quasibinomial(),
                          weights=sw_sl_prod*Freq) #### look at this line/step and same in line 106 to understand why all bootstrap resample values were the same previously
  
  pooled_ipw_dc <- glm(formula = as.formula(.formula_outcome),    
                         data=.dat, 
                         family=quasibinomial(),
                         weights=sw_dc_prod*Freq) 
  print("outcome models done")
  
  p_sl <- 1 - predict(pooled_ipw_sl, newdata = data.frame(tstart=0:45),
                         type="response")
  
  p_dc <- 1 - predict(pooled_ipw_dc, newdata = data.frame(tstart=0:45),
                        type="response")
  results <- data.frame(day=0:45,
                        sl= 1- cumprod(p_sl),
                        dc= 1- cumprod(p_dc)) %>%
    mutate(rd=sl - dc) %>% mutate(rr=sl/dc) %>%
    mutate(across(everything(), ~ round(., 4)))
  return(results) ### am: 6/24, kept as data.frame instead of kable for post-processing in a different step
}

overall_function_sensi6 <- function(.dat,    #9/8 z: should it be uniformed to dat rather than .dat, because for analysis_function it's dat?
                             formula_d_e,
                             formula_n_e,
                             formula_d_l,
                             formula_n_l,
                             formula_outcome,
                             point_estimate_only=FALSE,
                             bs_reps=NULL, ...
){
  if(!point_estimate_only & is.null(bs_reps)){return("please input a value for the number of bootstrap reamples (bs_reps)")}
  
  if(point_estimate_only){
    # point estimate
    results_point <- analysis_function_sensi6(.dat=.dat, formula_d_e=.formula_d_e, 
                                       formula_n_e=.formula_n_e, 
                                       formula_d_l=.formula_d_l,
                                       formula_n_l=.formula_n_l,
                                       formula_outcome=.formula_outcome, 
                                       bootstrap=FALSE, bs_num=0, ...) 
    return(results_point)
  } else if(!point_estimate_only){
    # bootstrap resampling estimates
    bs_results_all <- pblapply(setNames(0:bs_reps, 0:bs_reps), function(x){   
      set.seed(x) ## for reproducibility
      analysis_function_sensi6(.dat=.dat, 
                        formula_d_e=.formula_d_e,
                        formula_n_e=.formula_n_e,
                        formula_d_l=.formula_d_l,
                        formula_n_l=.formula_n_l,
                        formula_outcome=.formula_outcome,
                        bootstrap=TRUE, 
                        bs_num=x)
    })
    results_point <- bs_results_all[['0']]
    bs_results <- bs_results_all[names(bs_results_all) != "0"]
    
    results_ci <- do.call(rbind, bs_results) %>%
      group_by(day) %>%
      summarise(
        across(c(sl, dc, rd, rr), list(l = ~ quantile(., probs = 0.025),
                                            u = ~ quantile(., probs = 0.975)))) #compute CIs by day
  }
  
  results_com <- results_point %>% left_join(results_ci, by = "day") %>% dplyr::select(
    day, 
    sl, sl_l, sl_u,
    dc, dc_l, dc_u,
    rd, rd_l, rd_u,
    rr, rr_l, rr_u
  ) #create a combined dataset for point estimates and 95%CIs
  return(results_com) 
}
