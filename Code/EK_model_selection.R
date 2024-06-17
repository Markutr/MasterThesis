source("functions.R")
load("./Data/processed_data.RData") 

# Settings for INLA
inla_compute_params = list(config = TRUE, dic = TRUE, cpo = TRUE, waic = TRUE)
inla_params = list(int.strategy = "grid", lincomb.derived.correlation.matrix = TRUE)

# Settings for the scan
models = c("APc", "ApC", "aPC", "Apc", "aPc","apC")
walks = c("111")
genders = c("m","f")
skip_inla = F
time_in_s = F

#The model selection
res_list_m = list()
time_list_m = list()
res_list_f = list()
time_list_f = list()
for(walk in walks){
  for (model in models) {
    if(!skip_inla){
      for (gender in genders) {
        start_time = Sys.time()
        cat(paste("Model: ", model,walk, ". Starting up...\n", sep =""))
        if(gender == "m"){
          bp_data = bp_data_m
        }else if(gender == "f"){
          bp_data = bp_data_f
        }
        
        inla_res = inlaAPC(model, bp_data, "n_backpain")
        
        ident = paste(model, walk, sep = "")
        if(gender == "m"){
          res_list_m[ident] = list(ident = inla_res$inla)
          time_list_m[ident] = diff_format_to_sec(inla_res$time)
        }else if(gender == "f"){
          res_list_f[ident] = list(ident = inla_res$inla)
          time_list_f[ident] = diff_format_to_sec(inla_res$time)
        }
      }
      cat(paste("INLA finished. Time elapsed: ", time_diff(start_time, Sys.time()), "\n", sep = ""))
    }else{
      inla_res_m = NA
      inla_res_f = NA
    }
  }
}

#Compute matrix to be used for table
if(!skip_inla){
  score_matrix_m = data.frame(matrix(NA, nrow = 3, ncol = length(models)))
  score_matrix_f = data.frame(matrix(NA, nrow = 3, ncol = length(models)))
  colnames(score_matrix_m) = paste(models, "111", sep = "")
  rownames(score_matrix_m) = c("Mean log score", "WAIC", "DIC")
  colnames(score_matrix_f) = paste(models, "111", sep = "")
  rownames(score_matrix_f) = c("Mean log score", "WAIC", "DIC")
  for (model in names(res_list_f)) {
    score_matrix_m[model] = c(mean(-log(res_list_m[[model]]$cpo$cpo)), res_list_m[[model]]$waic$waic, res_list_m[[model]]$dic$dic)
    score_matrix_f[model] = c(mean(-log(res_list_f[[model]]$cpo$cpo)), res_list_f[[model]]$waic$waic, res_list_f[[model]]$dic$dic)
  }
  score_matrix_f[1, ] = round(score_matrix_f[1,], 3)
  score_matrix_f[2, ] = round(score_matrix_f[2,])
  score_matrix_f[3, ] = round(score_matrix_f[3,])
  score_matrix_m[1, ] = round(score_matrix_m[1,], 3)
  score_matrix_m[2, ] = round(score_matrix_m[2,])
  score_matrix_m[3, ] = round(score_matrix_m[3,])
  
  #save(list = c("res_list_m","res_list_f", "score_matrix_f","score_matrix_m"), file = "./Data/EK_model_selection.RData")
  save(list = c("score_matrix_f","score_matrix_m"), file = "./Data/EK_model_selection.RData")
  
}

cat("Finished.\n")

