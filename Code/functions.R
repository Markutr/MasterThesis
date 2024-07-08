library(INLA)
library(makemyprior)
library(stringr)
library(plyr)
library(rlang)

##################
# Functions for utility
##################
file_Exists <- function(filename, path = "./Data", cs = F){#Case sensitive
  if(cs){
    return(filename %in% list.files(path))
  }else{
    return(tolower(filename) %in% tolower(list.files(path)))
  }
}

time_diff <- function(start_time, end_time){
  diff_h = floor(as.double(difftime(end_time, start_time, units = "hours")))
  diff_m = round((floor(as.double(difftime(end_time, start_time, units = "mins")))/60 - diff_h)*60)
  diff_s =  round((floor(as.double(difftime(end_time, start_time, units = "secs")))/(60) - diff_m - 60*diff_h)*60)
  return(paste(diff_h,":",diff_m,":",diff_s, sep =""))
  
}

diff_format_to_sec <- function(format){
  split_format = as.numeric(str_split(format, ":")[[1]])
  return(sum(split_format %*% c(60*60, 60, 1)))
}

apc_to_save_format <- function(apc_name, capitalized_is_shared = T){
  returned_text = ""
  for (letter in str_split(apc_name,"")[[1]]) {
    if(letter != tolower(letter)){
      returned_text = paste(returned_text, tolower(letter), ifelse(capitalized_is_shared, "1", "2"), sep = "")
    }else{
      returned_text = paste(returned_text, tolower(letter), ifelse(!capitalized_is_shared, "1","2"), sep="") 
    }
  }
  return(returned_text)
}

save_format_to_apc <- function(save_format, capitalized_is_shared = T){
  returned_text = ""
  chars = str_split(save_format,"")[[1]]
  for (i in 1:length(chars)) {
    if(suppressWarnings(is.na(as.numeric(chars[i])))){
      if(chars[i+1] == 1){
        returned_text = paste(returned_text, ifelse(!capitalized_is_shared, tolower(chars[i]), toupper(chars[i])), sep="")
      }else{
        returned_text = paste(returned_text, ifelse(!capitalized_is_shared, toupper(chars[i]), tolower(chars[i])), sep="")
      }
      
    }
  }
  return(returned_text)
}

trim_res <- function(inla_object, skip = F){
  
  if(skip){
    return(inla_object)
  }
  
  if("inla" %in% names(inla_object)){
    summary_keys = names(inla_object$inla)[startsWith(names(inla_object$inla), "summary")]
    keys = c(summary_keys, "cpo","waic","dic")
    inla_object$inla = inla_object$inla[keys]

  }else{
    summary_keys = names(inla_object)[startsWith(names(inla_object), "summary")]
    keys = c(summary_keys, "cpo","waic","dic")
    inla_object = inla_object[keys]
  }
  
  return(inla_object)
}

generate_priors <- function(data, y_name, prior_folder_path = "./Priors", special_priors = F){
  
  models = rev(c("APc", "ApC", "aPC", "Apc", "aPc","apC"))
  prior_structures = c("EK","Anti-EK","Dir","Semi")
  
  cat("Warning! Generating all the priors may take a long time (~6 hours). If you only need a single prior, call makePriorAPC(data, y_name, apc_format = 'your_apc_format') instead.\n")
  cat("Starting computations...\n")
  for(model in models){ #Generate all EK priors
    prior = makePriorAPC(data, y_name, prior_mode = "EK", apc_format = model, path = prior_folder_path)
  }
  if(special_priors){ #Special priors used to make some figures
    prior = makePriorAPC(data, y_name, prior_mode = "Anti-EK", apc_format = "aPc", path = prior_folder_path)
    prior = makePriorAPC(data, y_name, prior_mode = "Dirichlet", apc_format = "aPc", path = prior_folder_path)
    prior = makePriorAPC(data, y_name, prior_mode = "Semi", apc_format = "aPc", path = prior_folder_path)
  }
  
  cat(paste("All priors computed! They are saved in ",prior_folder_path," \n", sep = ""))
}

#####################
# APC functions
####################

#Makes/loads prior for the age-period-cohort models
makePriorAPC <- function(data, y_name, education_colname = "education", prior_mode = "EK" , rw_effects = list(age = "age", period = "period", cohort = "cohort"), 
                         rw_order = "111", apc_format = "aPc", filename = "auto", iid_colname = "random", override = F, path = "./Priors", family = "binomial", skip_family = F,
                         age_strat_cols = c("age_edu_1", "age_edu_2", "age_edu_3", "age_edu_4"), 
                         period_strat_cols = c("period_edu_1", "period_edu_2", "period_edu_3", "period_edu_4"), 
                         cohort_strat_cols = c("cohort_edu_1", "cohort_edu_2", "cohort_edu_3", "cohort_edu_4"), skip_load = F){
  
  # Make sure some input is valid
  stopifnot(paste(sort(unlist(strsplit(tolower(apc_format), ""))), collapse = "") == "acp") #Check valid format
  stopifnot(ldply(str_match_all(apc_format,"[a-p]"),length)["V1"] >= 1) #Require at least one stratum specific effect
  stopifnot(ldply(str_match_all(apc_format,"[A-P]"),length)["V1"] >= 1) #Require at least one shared effect
  stopifnot(!is.null(data[[y_name]])) # Cant find y
  stopifnot(prior_mode %in% c("EK","Dirichlet","Anti-EK","Semi")) #Needs to be valid
  stopifnot(education_colname %in% colnames(data)) #Make sure it present
  
  #infer filename. Try to load precomputed prior
  if(filename == "auto"){
    filename = paste(prior_mode,"_", apc_to_save_format(apc_format), "_rw", rw_order,".rds", sep = "")
  }
  if(family == "gaussian" & !skip_family){
    iid_colname = "eps"
  }


  I = max(data[[rw_effects$age]], na.rm = T)
  J = max(data[[rw_effects$period]], na.rm = T)
  K = max(data[[rw_effects$cohort]], na.rm = T)
  
  #Make the formula used in practice
  returned_text = paste0(as.character(y_name), paste0(" ~ -1 + ", paste(paste("education", 1:length(unique(data[[education_colname]])), sep = "_"), collapse = " + "), sep = " + "), sep = "")
  for (effect in str_split(apc_format, "")[[1]]) {
    if(str_detect(effect, "[[:lower:]]")){
      returned_text = paste(returned_text, paste("f(",ifelse(rep(effect != "a", length(unique(data[[education_colname]]))), ifelse(rep(effect == "p", length(unique(data[[education_colname]]))), period_strat_cols, cohort_strat_cols), age_strat_cols),", model = ",ifelse(rw_order == "111", "'rw1'", "'rw2'"),", constr = T, scale.model = T)", collapse = " + "), " + ", sep = "")
    }else{
      returned_text = paste(returned_text, paste("f(", ifelse(effect != "A", ifelse(effect == "P", rw_effects$period, rw_effects$cohort), rw_effects$age),", model = ",ifelse(rw_order == "111", "'rw1'", "'rw2'"),", constr = T, scale.model = T) + ", sep = ""), sep = "")
    }
  }
  returned_text = paste(returned_text, " f(", iid_colname, ",model = 'iid', constr = T)", sep = "")
  returned_formula = as.formula(returned_text)

  if(file_Exists(filename, path = path) & !override){
    cat(paste("Prior ", "'", filename, "' already found. Skipping computations...\n", sep=""))
    if(!skip_load){
    return(list(prior = readRDS(paste(path, "/",filename, sep = "")), formula = returned_formula))
    }else{
      return()
    }
  }
  
  data_new = data #Make copy
  A_mat = NA
  P_mat = NA
  C_mat = NA
  
  if(ldply(str_match_all(apc_format,"A"),length)[[1]]==0){#Age is group specific effect
    if(str_split(rw_order, "")[[1]][1] == "1"){
      A_mat <<- kronecker(diag(4), INLA:::inla.rw1(I))
    }else if(str_split(rw_order, "")[[1]][1] == "2"){
      A_mat <<- kronecker(diag(4), INLA:::inla.rw2(I))
    }
    data_new$age_new = as.integer(data[[rw_effects$age]] + (as.numeric(data[[education_colname]])-1)*I)
  }
  if(ldply(str_match_all(apc_format,"P"),length)[[1]]==0){#Period is group specific effect
    if(str_split(rw_order, "")[[1]][2] == "1"){
      P_mat <<- kronecker(diag(4), INLA:::inla.rw1(J))
    }else if(str_split(rw_order, "")[[1]][2] == "2"){
      P_mat <<- kronecker(diag(4), INLA:::inla.rw2(J))
    }
    data_new$period_new = as.integer(data[[rw_effects$period]] + (as.numeric(data[[education_colname]])-1)*J)
  }
  if(ldply(str_match_all(apc_format,"C"),length)[[1]]==0){#Cohort is group specific effect
    if(str_split(rw_order, "")[[1]][3] == "1"){
      C_mat <<- kronecker(diag(4), INLA:::inla.rw1(K))
    }else if(str_split(rw_order, "")[[1]][3] == "2"){
      C_mat <<- kronecker(diag(4), INLA:::inla.rw2(K))
    }
    data_new$cohort_new = as.integer(data[[rw_effects$cohort]] + (as.numeric(data[[education_colname]])-1)*K)
  }

  #Make the formula used in mmp
  prior_formula_text = paste0(as.character(y_name), paste0(" ~ -1 + ", paste(paste("education", 1:length(unique(data[[education_colname]])), sep = "_"), collapse = " + "), sep = " + "), sep = "")
  for (effect in str_split(apc_format, "")[[1]]) {
    if(str_detect(effect, "[[:lower:]]")){
      prior_formula_text = paste(prior_formula_text, paste("mc(", ifelse(effect != "a", ifelse(effect == "p", "period_new", "cohort_new"), "age_new"),", model = 'generic0', constr = T, scale.model = T, Cmatrix = ", ifelse(effect != "a", ifelse(effect == "p", "P_mat", "C_mat"), "A_mat"),ifelse(rw_order == "111","", "") ,") + ", sep = ""), sep ="")
      }else{
      prior_formula_text = paste(prior_formula_text, paste("mc(", ifelse(effect != "A", ifelse(effect == "P", rw_effects$period, rw_effects$cohort), rw_effects$age),", model = ", ifelse(rw_order == "111", "'rw1'", "'rw2', diagonal = 1e-10"),", constr = T, scale.model = T) + ", sep = ""), sep = "")
    }
  }
  prior_formula_text = paste(prior_formula_text, "mc(", iid_colname, ", model = 'iid', constr = T)", sep = "")
  prior_formula <- as.formula(prior_formula_text)

  
  if(prior_mode == "EK"){
    prior_tree = list(tree = paste("s1 = (s2,", iid_colname,"); s2 = (s3,", ifelse("P" %in% str_split(apc_format, "")[[1]], rw_effects[["period"]], "period_new"), "); s3 = (",ifelse("A" %in% str_split(apc_format, "")[[1]], rw_effects[["age"]], "age_new"),",", ifelse("C" %in% str_split(apc_format, "")[[1]], rw_effects[["cohort"]], "cohort_new"),");", sep = ""),
                    V = list(s1 = list(prior = "pc", param = c(1.6, 0.05))),
                    w = list(s1 = list(prior = "pc0", param = c(0.4)),
                             s2 = list(prior = "pcM", param = c(35/40, 0.5)),
                             s3 = list(prior = "pcM", param = c(25/35, 0.5))))
  }else if (prior_mode == "Dirichlet"){
    prior_tree = list(tree = paste("s1 = (random,", 
                                   ifelse("A" %in% str_split(apc_format, "")[[1]], rw_effects[["age"]], "age_new"),
                                   ",", 
                                   ifelse("P" %in% str_split(apc_format, "")[[1]], rw_effects[["period"]], "period_new"),
                                   ",", 
                                   ifelse("C" %in% str_split(apc_format, "")[[1]], rw_effects[["cohort"]], "cohort_new"),
                                   ");", sep=""),
                      V = list(s1 = list(prior = "pc", param = c(1.6, 0.05))),
                      w = list(s1 = list(prior = "dirichlet")))
  } else if(prior_mode == "Anti-EK"){
    prior_tree = list(tree = paste("s1 = (s2,", 
                                   iid_colname,
                                   "); s2 = (s3,", 
                                   ifelse("P" %in% str_split(apc_format, "")[[1]], rw_effects[["period"]], "period_new"), 
                                   "); s3 = (",
                                   ifelse("A" %in% str_split(apc_format, "")[[1]], rw_effects[["age"]], "age_new"),
                                   ",", 
                                   ifelse("C" %in% str_split(apc_format, "")[[1]], rw_effects[["cohort"]], "cohort_new"),
                                   ");", sep = ""),
                      V = list(s1 = list(prior = "pc", param = c(1.6, 0.05))),
                      w = list(s1 = list(prior = "pc1", param = c(0.6)),
                               s2 = list(prior = "pcM", param = c(5/40, 0.5)),
                               s3 = list(prior = "pcM", param = c(10/35, 0.5))))
  } else if(prior_mode=="Semi"){
    prior_tree = list(tree = paste("s1 = (", iid_colname,", s2); s2 = (", ifelse("P" %in% str_split(apc_format, "")[[1]], rw_effects[["period"]], "period_new"), "," ,ifelse("A" %in% str_split(apc_format, "")[[1]], rw_effects[["age"]], "age_new"),",", ifelse("C" %in% str_split(apc_format, "")[[1]], rw_effects[["cohort"]], "cohort_new"),");", sep = ""),
                      V = list(s1 = list(prior = "pc", param = c(1.6, 0.05))),
                      w = list(s1 = list(prior = "pc1", param = c(0.6)),
                               s2 = list(prior = "Dirichlet")))
  }else{
    stop()
  }
  cat(paste("Starting computations for prior ", filename, "\n", sep = ""))
  start_time = Sys.time()
  
  #Make the prior
  prior <- makemyprior::make_prior(prior_formula, data = data_new, family = family, 
                                   prior = prior_tree)
  
  saveRDS(prior, paste(path, "/",filename, sep = ""))

  cat(paste("Computations finished. Elapsed time: ", time_diff(start_time, Sys.time()), "\n", sep=""))

  return(list(prior = prior, formula = returned_formula))
}

#Creates the lincombs for inla to compute cross strata differences for stratum specific effects over the relevant indices
get_lincombs <- function(apc_format, I = 60, J = 22, K = 81){
  
  all_lincombs = c()
  listed_format = str_split(apc_format, "")[[1]]
  
  
  if("a" %in% listed_format){
    age_edu_lincomb = c()
    for(l in 1:I){ #For each age 
      
      idx = rep(NA, I)
      idx2 = rep(NA, I)
      idx[l] = 1
      idx2[l] = -1
      
      #Make 3 linear combinations
      lc1 <- inla.make.lincomb(age_edu_4 = idx2, age_edu_1 = idx, education_4 = -1, education_1 = 1)#, cohort_edu_1 = idx_cohorts, cohort_edu_2 = idx_cohorts, cohort_edu_3 = idx_cohorts, cohort_edu_4 = idx_cohorts)
      lc2 <- inla.make.lincomb(age_edu_4 = idx2, age_edu_2 = idx, education_4 = -1, education_2 = 1)#, cohort_edu_1 = idx_cohorts, cohort_edu_2 = idx_cohorts, cohort_edu_3 = idx_cohorts, cohort_edu_4 = idx_cohorts)
      lc3 <-inla.make.lincomb(age_edu_4 = idx2, age_edu_3 = idx, education_4 = -1, education_3 = 1)#, cohort_edu_1 = idx_cohorts, cohort_edu_2 = idx_cohorts, cohort_edu_3 = idx_cohorts, cohort_edu_4 = idx_cohorts)
      names(lc1) <- paste("Less than HS vs BA+: Age : ", as.character(data_extras$min_age + (l-1)), sep="")
      names(lc2) <- paste("HS vs BA+: Age : ", as.character(data_extras$min_age + (l-1)),sep="")
      names(lc3) <- paste("Some college/AA vs BA+: Age : ", as.character(data_extras$min_age + (l-1)), sep="")
      
      age_edu_lincomb <- c(age_edu_lincomb, lc1, lc2, lc3)
    }
    age_edu_lincomb = age_edu_lincomb[order(names(age_edu_lincomb))]
    all_lincombs = c(all_lincombs, age_edu_lincomb)
  }
  if("p" %in% listed_format){
    period_edu_lincomb = c()
    for(l in 1:J){ #For each age 
      
      idx = rep(NA, J)
      idx2 = rep(NA, J)
      idx[l] = 1
      idx2[l] = -1
      
      #Make 3 linear combinations
      lc1 <- inla.make.lincomb(period_edu_4 = idx2, period_edu_1 = idx, education_4 = -1, education_1 = 1)#, cohort_edu_1 = idx_cohorts, cohort_edu_2 = idx_cohorts, cohort_edu_3 = idx_cohorts, cohort_edu_4 = idx_cohorts)
      lc2 <- inla.make.lincomb(period_edu_4 = idx2, period_edu_2 = idx, education_4 = -1, education_2 = 1)#, cohort_edu_1 = idx_cohorts, cohort_edu_2 = idx_cohorts, cohort_edu_3 = idx_cohorts, cohort_edu_4 = idx_cohorts)
      lc3 <-inla.make.lincomb(period_edu_4 = idx2, period_edu_3 = idx, education_4 = -1, education_3 = 1)#, cohort_edu_1 = idx_cohorts, cohort_edu_2 = idx_cohorts, cohort_edu_3 = idx_cohorts, cohort_edu_4 = idx_cohorts)
      names(lc1) <- paste("Less than HS vs BA+: Period : ", as.character(data_extras$min_year + (l-1)), sep="")
      names(lc2) <- paste("HS vs BA+: Period : ", as.character(data_extras$min_year + (l-1)), sep="")
      names(lc3) <- paste("Some college/AA vs BA+: Period : ", as.character(data_extras$min_year + (l-1)), sep="")
      
      period_edu_lincomb <- c(period_edu_lincomb, lc1, lc2, lc3)
    }
    period_edu_lincomb = period_edu_lincomb[order(names(period_edu_lincomb))]
    all_lincombs = c(all_lincombs, period_edu_lincomb)
  }
  if("c" %in% listed_format){
    cohort_edu_lincomb = c()
    for (l in 1:K) {
      
      idx = rep(NA, K)
      idx2 = rep(NA, K)
      idx[l] = 1
      idx2[l] = -1
      
      lc1 <- inla.make.lincomb(cohort_edu_4 = idx2, cohort_edu_1 = idx, education_4 = -1, education_1 = 1)
      lc2 <- inla.make.lincomb(cohort_edu_4 = idx2, cohort_edu_2 = idx, education_4 = -1, education_2 = 1)
      lc3 <-inla.make.lincomb(cohort_edu_4 = idx2, cohort_edu_3 = idx, education_4 = -1, education_3 = 1)
      names(lc1) <- paste("Less than HS vs BA+: Cohort : ", as.character(l), sep="")
      names(lc2) <- paste("HS vs BA+: Cohort : ", as.character(l) ,sep="")
      names(lc3) <- paste("Some college/AA vs BA+: Cohort : ", as.character(l), sep="")
      
      cohort_edu_lincomb = c(cohort_edu_lincomb, lc1, lc2, lc3)
    }
    cohort_edu_lincomb = cohort_edu_lincomb[order(names(cohort_edu_lincomb))]
    all_lincombs = c(all_lincombs, cohort_edu_lincomb)
  }
  
  return(all_lincombs)
}

# Prior function to be passed to inla using the sum of the four variances. The indices 1:4 refer to age_education, and 6:9 to the cohort_education
prior_func_APC <- function(logprec){
  logprec_a <- logprec[1]
  logprec_p <- logprec[2]
  logprec_c <- logprec[3]
  logprec_z <- logprec[4]
  
  return(
    eval_joint_prior(-c(logprec_a, logprec_p, logprec_c, logprec_z), prior_data)
  )
} #Not used

prior_func_aPC <- function(logprec){
  logprec_a <- log(sum(exp(logprec[1:4])))
  logprec_p <- logprec[5]
  logprec_c <- logprec[6]
  logprec_z <- logprec[7]
  
  return(
    eval_joint_prior(-c(logprec_a, logprec_p, logprec_c, logprec_z), prior_data)
  )
}

prior_func_apC <- function(logprec){
  logprec_a <- log(sum(exp(logprec[1:4])))
  logprec_p <- log(sum(exp(logprec[5:8])))
  logprec_c <- logprec[9]
  logprec_z <- logprec[10]
  
  return(
    eval_joint_prior(-c(logprec_a, logprec_p, logprec_c, logprec_z), prior_data)
  )
}

prior_func_apc <- function(logprec){
  logprec_a <- log(sum(exp(logprec[1:4])))
  logprec_p <- log(sum(exp(logprec[5:8])))
  logprec_c <- log(sum(exp(logprec[9:12])))
  logprec_z <- logprec[13]
  
  return(
    eval_joint_prior(-c(logprec_a, logprec_p, logprec_c, logprec_z), prior_data)
  )
}# Not used

prior_func_Apc <- function(logprec){
  logprec_a <- logprec[1]
  logprec_p <- log(sum(exp(logprec[2:5])))
  logprec_c <- log(sum(exp(logprec[6:9])))
  logprec_z <- logprec[10]
  
  return(
    eval_joint_prior(-c(logprec_a, logprec_p, logprec_c, logprec_z), prior_data)
  )
}

prior_func_APc <- function(logprec){
  logprec_a <- logprec[1]
  logprec_p <- logprec[2]
  logprec_c <- log(sum(exp(logprec[3:6])))
  logprec_z <- logprec[7]
  
  return(
    eval_joint_prior(-c(logprec_a, logprec_p, logprec_c, logprec_z), prior_data)
  )
}

prior_func_aPc <- function(logprec){
  logprec_a <- log(sum(exp(logprec[1:4])))
  logprec_p <- logprec[5]
  logprec_c <- log(sum(exp(logprec[6:9])))
  logprec_z <- logprec[10]
  
  return(
    eval_joint_prior(-c(logprec_a, logprec_p, logprec_c, logprec_z), prior_data)
  )
}

prior_func_ApC <- function(logprec){
  logprec_a <- logprec[1]
  logprec_p <- log(sum(exp(logprec[2:5])))
  logprec_c <- logprec[6]
  logprec_z <- logprec[7]
  
  return(
    eval_joint_prior(-c(logprec_a, logprec_p, logprec_c, logprec_z), prior_data)
  )
}

#Returns the correct prior evaluation function based on format
pick_prior_func <- function(apc_format){
  models = c("APc", "ApC", "aPC", "Apc", "aPc","apC", "apc","APC")
  priors_funcs = c(prior_func_APc, prior_func_ApC, prior_func_aPC, prior_func_Apc, prior_func_aPc, prior_func_apC, prior_func_apc, prior_func_APC)
  return(priors_funcs[[which(models == apc_format)]])
}

#Fits the APC model for the given format using the given data with the given y_name
inlaAPC <- function(apc_format, data, y_name, I = 60, J = 22, K = 81, family = "binomial", do_scale = F, trim = T, ...){
  
  inla_compute_params = control.compute = list(config = TRUE, dic = TRUE, cpo = TRUE, waic = TRUE, return.marginals.predictor=TRUE)
  inla_params = list(int.strategy = "ccd", lincomb.derived.correlation.matrix = TRUE, strategy = "simplified.laplace")
  control.predictor = list(compute=T)

  res = makePriorAPC(data, y_name, apc_format = apc_format, ...)
  res$data = make_eval_prior_data(res$prior)
  lincombs_acq = get_lincombs(apc_format, I, J, K)
  prior_func = pick_prior_func(apc_format)
  if(do_scale){
    fam_ctrl = NULL
  }else{  
    fam_ctrl = list(hyper = list(prec = list(initial = log(1), fixed = T)))
  }

  inla_start_time = Sys.time()
  if(do_scale){
    inla_res <- inla(res$formula, data = data, family = family, Ntrials = data$total_count,
                     control.inla = inla_params,
                     control.compute = inla_compute_params,
                     control.family = fam_ctrl,
                     scale = scale,
                     lincomb = lincombs_acq,
                     control.predictor = control.predictor,
                     control.expert = list(jp = inla.jp.define(
                       prior_func, prior_data = res$data,
                       eval_joint_prior = makemyprior::eval_joint_prior,
                       hd_prior_joint_lpdf = makemyprior:::hd_prior_joint_lpdf, 
                       calc_jac_logdet = makemyprior:::calc_jac_logdet, 
                       choose_prior_lpdf = makemyprior:::choose_prior_lpdf, 
                       cw_priors_lpdf = makemyprior:::cw_priors_lpdf, 
                       expit = makemyprior:::expit, 
                       get_dirichlet_parameter = makemyprior:::get_dirichlet_parameter, 
                       get_indexes = makemyprior:::get_indexes, 
                       get_indexes2 = makemyprior:::get_indexes2, 
                       hd_dirichlet_prior_lpdf = makemyprior:::hd_dirichlet_prior_lpdf, 
                       hd_pc_prior_lpdf = makemyprior:::hd_pc_prior_lpdf, 
                       eval_spline_lpdf = makemyprior:::eval_spline_lpdf
                     )))
  }else{
  inla_res <- inla(res$formula, data = data, family = family, Ntrials = data$total_count,
                   control.inla = inla_params,
                   control.compute = inla_compute_params,
                   lincomb = lincombs_acq,
                   control.predictor = control.predictor,
                   control.expert = list(jp = inla.jp.define(
                     prior_func, prior_data = res$data,
                     eval_joint_prior = makemyprior::eval_joint_prior,
                     hd_prior_joint_lpdf = makemyprior:::hd_prior_joint_lpdf, 
                     calc_jac_logdet = makemyprior:::calc_jac_logdet, 
                     choose_prior_lpdf = makemyprior:::choose_prior_lpdf, 
                     cw_priors_lpdf = makemyprior:::cw_priors_lpdf, 
                     expit = makemyprior:::expit, 
                     get_dirichlet_parameter = makemyprior:::get_dirichlet_parameter, 
                     get_indexes = makemyprior:::get_indexes, 
                     get_indexes2 = makemyprior:::get_indexes2, 
                     hd_dirichlet_prior_lpdf = makemyprior:::hd_dirichlet_prior_lpdf, 
                     hd_pc_prior_lpdf = makemyprior:::hd_pc_prior_lpdf, 
                     eval_spline_lpdf = makemyprior:::eval_spline_lpdf
                   )))
  }
  inla_end_time = Sys.time()

  res$inla = inla_res
  res$time = time_diff(inla_start_time, inla_end_time)
  
  if(trim){
    res = trim_res(res)
  }
  
  return(res)
}


