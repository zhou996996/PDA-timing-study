p_load(splines, tidyr, pbapply) 
source("numerator-function.R")
source("pda-functions-sensi3.R")


overall_function_sensi3 <- function(.dat,    
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
    results_point <- analysis_function_sensi3(.dat=.dat, 
                                              formula_d_e=formula_d_e, 
                                              formula_n_e=formula_n_e, 
                                              formula_d_l=formula_d_l,
                                              formula_n_l=formula_n_l,
                                              formula_outcome=formula_outcome, 
                                              bootstrap=FALSE, bs_num=0, ...) 
    return(results_point)
  } else if(!point_estimate_only){
    # bootstrap resampling estimates
    bs_results_all <- pblapply(setNames(0:bs_reps, 0:bs_reps), function(x){   
      set.seed(x) ## for reproducibility
      analysis_function_sensi3(.dat=.dat, 
                               formula_d_e=formula_d_e,
                               formula_n_e=formula_n_e,
                               formula_d_l=formula_d_l,
                               formula_n_l=formula_n_l,
                               formula_outcome=formula_outcome, 
                               bootstrap=TRUE, 
                               bs_num=x)
    })
    results_point <- bs_results_all[['0']]
    bs_results <- bs_results_all[names(bs_results_all) != "0"]
    
    results_ci <- do.call(rbind, bs_results) %>%
      group_by(day) %>%
      summarise(
        across(c(early, late, rd, rr), list(l = ~ quantile(., probs = 0.025),
                                            u = ~ quantile(., probs = 0.975)))) 
  }
  
  results_com <- results_point %>% left_join(results_ci, by = "day") %>% dplyr::select(
    day, 
    early, early_l, early_u,
    late, late_l, late_u,
    rd, rd_l, rd_u,
    rr, rr_l, rr_u
  ) 
  return(results_com) 
}
