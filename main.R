source("data cleaning.R")
source("data-reformatting.R")
source("analysis-function.R")
source("analysis-function-sensi3.R")

#for main analysis
.formula_d_e = "intervention==1 ~ ns(tstart, knots=c(3), Boundary.knots=c(1,4)) + 
                                 ns(ega_ref, knots=c(27), Boundary.knots=c(25,29)) +
                                 ns(ega_days, knots=171, Boundary.knots=c(165, 182)) + 
                                 ns(ref_year, knots=2017, Boundary.knots=c(2016, 2021)) +
                                 ns(weight, knots=620, Boundary.knots=c(530, 840)) +
                                 female + ref_med"
.formula_n_e = "intervention==1 ~ ns(tstart, knots=c(3), Boundary.knots=c(1,4))"
.formula_d_l = "intervention==1 ~ ns(tstart, knots=c(3), Boundary.knots=c(1,9)) + 
                                ns(ega_ref, knots=c(27), Boundary.knots=c(25,29)) +
                                ns(ega_days, knots=171, Boundary.knots=c(165, 182)) +
                                ns(ref_year, knots=2017, Boundary.knots=c(2016, 2021)) +
                                ns(weight, knots=620, Boundary.knots=c(530, 840)) +
                                female + ref_med"
.formula_n_l = "intervention==1 ~ ns(tstart, knots=3, Boundary.knots = c(1,9))"
.formula_outcome = "extubation==1 ~ ns(tstart, knots = c(5,10), Boundary.knots = c(2, 40))"
early_num <- 5

main_analyis <- overall_function(.dat=pda_intubated_long, 
                                 formula_d_e=.formula_d_e,
                                 formula_n_e=.formula_n_e,
                                 formula_d_l=.formula_d_l,
                                 formula_n_l=.formula_n_l,
                                 formula_outcome=.formula_outcome,
                                 point_estimate_only=FALSE,bs_reps=1000)

#sensitivity analysis 1 (truncating follow-up at the time of death)
sensitivity_analyis1 <- overall_function(.dat=pda_intubated_long_sensitivity_1, 
                                 formula_d_e=.formula_d_e,
                                 formula_n_e=.formula_n_e,
                                 formula_d_l=.formula_d_l,
                                 formula_n_l=.formula_n_l,
                                 formula_outcome=.formula_outcome,
                                 point_estimate_only=FALSE,
                                 bs_reps=1000)

#sensitivity analysis 2 (including referral sites in confounder set)
.formula_d_e = "intervention==1 ~ ns(tstart, knots=c(3), Boundary.knots=c(1,4)) + 
                                 ns(ega_ref, knots=c(27), Boundary.knots=c(25,29)) +
                                 ns(ega_days, knots=171, Boundary.knots=c(165, 182)) +
                                 ns(ref_year, knots=2017, Boundary.knots=c(2016, 2021)) +
                                 ns(weight, knots=620, Boundary.knots=c(530, 840)) +
                                 female + ref_med + as.factor(referral.center)"
.formula_d_l = "intervention==1 ~ ns(tstart, knots=c(3), Boundary.knots=c(1,9)) + 
                                ns(ega_ref, knots=c(27), Boundary.knots=c(25,29)) +
                                ns(ega_days, knots=171, Boundary.knots=c(165, 182)) +
                                ns(ref_year, knots=2017, Boundary.knots=c(2016, 2021)) +
                                ns(weight, knots=620, Boundary.knots=c(530, 840)) +
                                female + ref_med + as.factor(referral.center)"

sensitivity_analyis2 <- overall_function(.dat=pda_intubated_long, 
                                         formula_d_e=.formula_d_e,
                                         formula_n_e=.formula_n_e,
                                         formula_d_l=.formula_d_l,
                                         formula_n_l=.formula_n_l,
                                         formula_outcome=.formula_outcome,
                                         point_estimate_only=FALSE,
                                         bs_reps=1000)

#sensitivity analysis 3 (younger vs. older group for actual age <= 14 days)
.formula_n_e_sensi3 = "intervention==1 ~ ns(age_days, knots=c(17), Boundary.knots=c(15,20))"
.formula_d_e_sensi3 = "intervention==1 ~ ns(age_days, knots=c(17), Boundary.knots=c(15,20)) + 
                                 ns(ega_ref, knots=c(27), Boundary.knots=c(25,29)) +
                                 ns(ega_days, knots=171, Boundary.knots=c(165, 182)) + 
                                 ns(ref_year, knots=2017, Boundary.knots=c(2016, 2021)) +
                                 ns(weight, knots=620, Boundary.knots=c(530, 840)) +
                                 female + ref_med"
.formula_n_l_sensi3 = "intervention==1 ~ ns(age_days, knots=25, Boundary.knots = c(15,34))"
.formula_d_l_sensi3 = "intervention==1 ~ ns(age_days, knots=25, Boundary.knots = c(15,34)) + 
                                ns(ega_ref, knots=c(27), Boundary.knots=c(25,29)) +
                                ns(ega_days, knots=171, Boundary.knots=c(165, 182)) +
                                ns(ref_year, knots=2017, Boundary.knots=c(2016, 2021)) +
                                ns(weight, knots=620, Boundary.knots=c(530, 840)) +
                                female + ref_med"
.formula_outcome_sensi3 = "extubation==1 ~ ns(tstart, knots = c(5,10), Boundary.knots = c(2, 40))"
sensitivity_analysis3 <- overall_function_sensi3(.dat=pda_intubated_long_sensitivity_3, 
                                                 formula_d_e=.formula_d_e_sensi3,
                                                 formula_n_e=.formula_n_e_sensi3,
                                                 formula_d_l=.formula_d_l_sensi3,
                                                 formula_n_l=.formula_n_l_sensi3,
                                                 formula_outcome=.formula_outcome_sensi3,
                                                 point_estimate_only=FALSE,
                                                 bs_reps=1000)
