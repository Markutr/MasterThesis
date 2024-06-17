library(dplyr)
library(tidyr)
library(INLA)
library(makemyprior)
library(matrixcalc)
library(bayestestR)
library(haven)
library(labelled)

options(dplyr.summarise.inform = FALSE)

# SET CORRECT WORKING DIRECTORY
setwd("/Users/MarkusTraetli/Desktop/Masteroppgave/Code") #MAKE SURE THIS IS CORRECT

if(!file.exists("./Data/processed_data.RData")){ #Run data preprocessing script if 
  source("data preprocessing.R")
}
source("./functions.R")
load("./Data/processed_data.RData")

# Settings for INLA
inla_compute_params = list(config = TRUE, dic = TRUE, cpo = TRUE, waic = TRUE)
inla_params = list(int.strategy = "ccd", lincomb.derived.correlation.matrix = TRUE, strategy = "simplified.laplace")

I = max(bp_data_f$age)
J = max(bp_data_f$period)
K = max(bp_data_f$cohort)

CS_prior = list("prec" = list("prior" = "pc.prec", param = c(10,0.01))) # probability 0.01 that the std.dev is greater than 10. Huge flat prior basically


###################################
# Component specific models
###################################

if(!file.exists("./Data/CS_models.RData")){

  formula_CS_aPc = n_backpain ~ -1 + education_1 + education_2 + education_3 + education_4 +
                               f(age_edu_1, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) + 
                               f(age_edu_2, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) + 
                               f(age_edu_3, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) + 
                               f(age_edu_4, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) + 
                               f(period, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) + 
                               f(cohort_edu_1, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) +
                               f(cohort_edu_2, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) +
                               f(cohort_edu_3, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) +
                               f(cohort_edu_4, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) +
                               f(random, model = "iid", hyper = CS_prior)
  
  #Trim res removes components of the inla result object we do not need to save memory
  res_CS_aPc_f = inla(formula_CS_aPc, data = bp_data_f, family = "binomial", Ntrials = bp_data_f$total_count, 
                       control.compute = inla_compute_params, control.inla = inla_params, lincomb = get_lincombs("aPc", I, J, K))
  
  res_CS_aPc_m = inla(formula_CS_aPc, data = bp_data_m, family = "binomial", Ntrials = bp_data_m$total_count, 
                       control.compute = inla_compute_params, control.inla = inla_params, lincomb = get_lincombs("aPc", I, J, K))
  
  formula_rw2_aPc = n_backpain ~ -1 + education_1 + education_2 + education_3 + education_4 +
    f(age_edu_1, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) + 
    f(age_edu_2, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) + 
    f(age_edu_3, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) + 
    f(age_edu_4, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) + 
    f(period, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) + 
    f(cohort_edu_1, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) +
    f(cohort_edu_2, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) +
    f(cohort_edu_3, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) +
    f(cohort_edu_4, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) +
    f(random, model = "iid", hyper = CS_prior)
  
  res_aPc_rw2_f = trim_res(inla(formula_rw2_aPc, data = bp_data_f, family = "binomial", Ntrials = bp_data_f$Ntrials, 
                     control.compute = inla_compute_params, control.inla = inla_params, lincomb = get_lincombs("aPc", I, J, K)))
  
  save(list = c("res_CS_aPc_f", "res_CS_aPc_m", "res_aPc_rw2_f"),
       file =  "./Data/CS_models.RData")
}else{
  load("./Data/CS_models.RData")
}

#######################################
# Joint priors
########################################
if(!file.exists("./Data/joint_models.RData")){
  
  #Top 4 models by female
  res_expert_f_aPc = inlaAPC("aPc", bp_data_f, "n_backpain", trim = F) #Trim res enabled by default, can be disabled. Has to be disabled for sampling of model!
  res_expert_f_apC = inlaAPC("apC", bp_data_f, "n_backpain")
  res_expert_f_aPC = inlaAPC("aPC", bp_data_f, "n_backpain")
  res_expert_f_APc = inlaAPC("APc", bp_data_f, "n_backpain")
  
  #top4 models by male
  res_expert_m_aPc = inlaAPC("aPc", bp_data_m, "n_backpain", trim = F)
  res_expert_m_apC = inlaAPC("apC", bp_data_m, "n_backpain")
  res_expert_m_aPC = inlaAPC("aPC", bp_data_m, "n_backpain")
  res_expert_m_APc = inlaAPC("APc", bp_data_m, "n_backpain")
  
  #Alternative formats for comparison
  res_dir_f = inlaAPC("aPc", bp_data_f, "n_backpain", prior_mode = "Dirichlet", trim = F)
  res_dir_m = inlaAPC("aPc", bp_data_m, "n_backpain", prior_mode = "Dirichlet", trim = F)
  
  res_anti_f = inlaAPC("aPc", bp_data_f, "n_backpain", prior_mode = "Anti-EK", trim = F)
  res_anti_m = inlaAPC("aPc", bp_data_m, "n_backpain", prior_mode = "Anti-EK", trim = F)
  
  res_semi_f = inlaAPC("aPc", bp_data_f, "n_backpain", prior_mode = "Semi", trim = F) 
  res_semi_m = inlaAPC("aPc", bp_data_m, "n_backpain", prior_mode = "Semi", trim = F)

  save(list = c("res_expert_f_aPc", "res_expert_f_apC", "res_expert_f_aPC", "res_expert_f_APc",
                "res_expert_m_aPc", "res_expert_m_apC", "res_expert_m_aPC","res_expert_m_APc",
                "res_dir_f", "res_dir_m",
                "res_anti_f","res_anti_m",
                "res_semi_f","res_semi_m"),
       file =  "./Data/joint_models.RData")
}else{
  load("./Data/joint_models.RData")
}

##################################
#Individum specific
#################################
if(!file.exists("./Data/individual_models.RData")){
  res_expert_indi_f = inlaAPC("aPc", bp_ind_f, "backpain", prior_mode = "EK", trim = F)
  res_expert_indi_m = inlaAPC("aPc", bp_ind_m, "backpain", prior_mode = "EK", trim = F)

  #res_anti_indi_f = inlaAPC("aPc", bp_ind_f, "backpain", prior_mode = "Anti-EK", trim = F)
  #res_anti_indi_m = inlaAPC("aPc", bp_ind_m, "backpain", prior_mode = "Anti-EK", trim = F)

  #res_dir_indi_f = inlaAPC("aPc", bp_ind_f, "backpain", prior_mode = "Dirichlet", trim = F)
  #res_dir_indi_m = inlaAPC("aPc", bp_ind_m, "backpain", prior_mode = "Dirichlet", trim = F)

  save(list = c("res_expert_indi_m", "res_expert_indi_f"),
       file =  "./Data/individual_models.RData")
}else{
  load("./Data/individual_models.RData")
}

#########################
#Survey design
#####################

#With survey weight adjustment
if(!file.exists("./Data/survey_models.RData")){
  
  #Top4 female
  res_survey_aPc_f = inlaAPC("aPc", survey_data_f, "logit_phat", family = "gaussian", do_scale = T, trim = F)
  res_survey_aPC_f = inlaAPC("aPC", survey_data_f, "logit_phat", family = "gaussian", do_scale = T)
  res_survey_APc_f = inlaAPC("APc", survey_data_f, "logit_phat", family = "gaussian", do_scale = T)
  res_survey_apC_f = inlaAPC("apC", survey_data_f, "logit_phat", family = "gaussian", do_scale = T)

  #Top4 male
  res_survey_aPc_m = inlaAPC("aPc", survey_data_m, "logit_phat", family = "gaussian", do_scale = T, trim = F)
  res_survey_aPC_m = inlaAPC("aPC", survey_data_m, "logit_phat", family = "gaussian", do_scale = T)
  res_survey_APc_m = inlaAPC("APc", survey_data_m, "logit_phat", family = "gaussian", do_scale = T)
  res_survey_apC_m = inlaAPC("apC", survey_data_m, "logit_phat", family = "gaussian", do_scale = T)

  #Component specific models
  form_joint = logit_phat ~ -1 + education_1 + education_2 + education_3 + education_4 + 
    f(age_edu_1, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) + 
    f(age_edu_2, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) + 
    f(age_edu_3, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) + 
    f(age_edu_4, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) +
    f(period, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) + 
    f(cohort_edu_1, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) + 
    f(cohort_edu_2, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) + 
    f(cohort_edu_3, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) + 
    f(cohort_edu_4, model = "rw1", constr = T, scale.model = T, hyper = CS_prior) +
    f(random, model = "iid", constr = T, hyper = CS_prior)
  
  
  res_survey_cs_aPc_f = inla(form_joint, data = survey_data_f, family = "gaussian", lincomb = get_lincombs("aPc", I, J, K), 
                             control.compute = inla_compute_params, control.inla = inla_params,
                             control.family = list(hyper = list(prec = list(initial = log(1), fixed = T))), scale = scale)
  
  res_survey_cs_aPc_m = inla(form_joint, data = survey_data_m, family = "gaussian", lincomb = get_lincombs("aPc", I, J, K), 
                             control.compute = inla_compute_params, control.inla = inla_params,
                             control.family = list(hyper = list(prec = list(initial = log(1), fixed = T))), scale = scale)
  
  
  save(list = c("res_survey_aPc_f", "res_survey_aPC_f", "res_survey_APc_f", "res_survey_apC_f",
                "res_survey_aPc_m", "res_survey_aPC_m", "res_survey_APc_m","res_survey_apC_m",
                "res_survey_cs_aPc_f","res_survey_cs_aPc_m"),
       file =  "./Data/survey_models.RData")
}else{
  load("./Data/survey_models.RData")
}

if(!file.exists("./Data/survey_model_filtered.RData")){
  res_survey_aPc_f_filtered = inlaAPC("aPc", survey_data_f_filter, "logit_phat", family = "gaussian", do_scale = T, trim = F)
  
  res_survey_aPc_m_filtered = inlaAPC("aPc", survey_data_m_filter, "logit_phat", family = "gaussian", do_scale = T, trim = F)

  formula_rw2 = n_backpain ~ -1 + education_1 + education_2 + education_3 + education_4 +
    f(age_edu_1, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) + 
    f(age_edu_2, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) + 
    f(age_edu_3, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) + 
    f(age_edu_4, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) +  
    f(period, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) + 
    f(cohort_edu_1, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) +
    f(cohort_edu_2, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) +
    f(cohort_edu_3, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) +
    f(cohort_edu_4, model = "rw2", constr = T, scale.model = T, hyper = CS_prior) +
    f(random, model = "iid", hyper = CS_prior)
  
  res_survey_aPc_rw2_f = trim_res(inla(formula_rw2, data = survey_data_f_filter, family = "binomial", Ntrials = survey_data_f_filter$Ntrials, 
                     control.compute = inla_compute_params, control.inla = inla_params, lincomb = get_lincombs("aPc", I, J, K)))
  
  res_survey_aPc_rw2_m = trim_res(inla(formula_rw2, data = survey_data_m_filter, family = "binomial", Ntrials = survey_data_m_filter$Ntrials, 
                     control.compute = inla_compute_params, control.inla = inla_params, lincomb = get_lincombs("aPc", I, J, K)))
  
  

  save(list=c("res_survey_aPc_m_filtered", "res_survey_aPc_f_filtered",
              "res_survey_aPc_rw2_f","res_survey_aPc_rw2_m"),
       file = "./Data/survey_model_filtered.RData")
}else{
  load("./Data/survey_model_filtered.RData")
}





