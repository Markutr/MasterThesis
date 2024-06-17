library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(INLA)
library(sf)
library(sp)
library(spdep)
library(geosphere)
library(ggspatial)
library(makemyprior)
library(matrixcalc)
library(bayestestR)
library(haven)
library(ggnewscale)
library(labelled)
library(ggpubr)
library(latex2exp)
library(reshape2)
library(xtable)
library(scales)
library(visNetwork)
library(MCMCpack)
library(ggbreak)
library(grid)
library(tables)
library(Epi)
library(stringi)
library(dvmisc)

options(dplyr.summarise.inform = FALSE)
#################################
#Plotting functions and data post processing functions
#################################

#Extract samples on the posterior of the weight parameters according to the EK prior tree structure
extract_res_inla = function(result, n_samples = 10000, return_hist_data = T, sum_age = T, sum_period = T, sum_cohort = T, compute_tree = T){
  
  n_fixed = ifelse(is.na(nrow(result$summary.fixed)), 0, nrow(result$summary.fixed))
  stopifnot(n_fixed > 0)
  
  posterior_samples = inla.hyperpar.sample(n_samples, result) #Sample the posterior
  effect_names = word(colnames(posterior_samples), start = -1) # Extract names
  stopifnot(length(effect_names) > 0) # Need random effect to work
  rho_samples = data.frame(matrix(ncol = length(effect_names), nrow = n_samples))
  colnames(rho_samples) = effect_names
  
  #Transform precision to variance
  for (i in 1:n_samples) {
    rho_samples[i,] = as.numeric(1/posterior_samples[i,])
  }
  if (sum_age){
    cols = startsWith(colnames(rho_samples), "age")
    if(sum(cols) > 1){
      age = rowSums(rho_samples[cols])
      rho_samples = rho_samples[!cols]
      rho_samples$age = age
      effect_names = c(effect_names[!cols],"age")
    }
  }
  if (sum_cohort){
    cols = startsWith(colnames(rho_samples), "cohort")
    if(sum(cols) > 1){
      cohort = rowSums(rho_samples[cols])
      rho_samples = rho_samples[!cols]
      rho_samples$cohort = cohort
      effect_names = c(effect_names[!cols],"cohort")
    }
  }
  if (sum_period){
    cols = startsWith(colnames(rho_samples), "period")
    if(sum(cols) > 1){
      period = rowSums(rho_samples[cols])
      rho_samples = rho_samples[!cols]
      rho_samples$period = period
      effect_names = c(effect_names[!cols],"period")
    }
  }
  sigma = sqrt(rowSums(rho_samples)) # Square root total marginal variance
  rho_samples = rho_samples/rowSums(rho_samples) #Calculate proportion of variance in each effect
  
  #Create dataframe to store the results
  inference_results = data.frame(matrix(ncol = 3, nrow = 1 + n_fixed + length(effect_names)))
  colnames(inference_results) = c("median", "0.025quant", "0.975quant")
  
  #These first rows are the fixed effects
  for (i in 1:n_fixed) {
    inference_results[i, ] = result$summary.fixed[c("0.5quant", "0.025quant", "0.975quant")][i,]
  }
  
  rownames(inference_results)[1:n_fixed] = rownames(result$summary.fixed)
  
  for (i in 1:length(effect_names)) {
    effect = effect_names[i]
    cred_int = ci(rho_samples[effect], method = "ETI")
    inference_results[n_fixed + i,] = c(median(rho_samples[effect][,1]), cred_int$CI_low, cred_int$CI_high)
  }
  
  rownames(inference_results)[(n_fixed + 1):(n_fixed + length(effect_names))] = paste0("rho_", effect_names)
  
  
  #Create equi-tailed CI
  cred_int_sigma = ci(sigma, method ="ETI")
  inference_results[n_fixed + length(effect_names) + 1,] = c(median(sigma), cred_int_sigma$CI_low, cred_int_sigma$CI_high)
  rownames(inference_results)[n_fixed + length(effect_names) + 1] = "sigma"
  
  if(compute_tree){
    rho_samples$age_cohort = rho_samples$age/(rho_samples$age + rho_samples$cohort)
    rho_samples$period_age_cohort = rho_samples$period /(rho_samples$period + rho_samples$age + rho_samples$cohort)
    rho_samples$random_age_period_cohort = rho_samples$random /(rho_samples$random + rho_samples$period + rho_samples$age + rho_samples$cohort)
    effect_names = colnames(rho_samples)
  }
  
  # Include the samples in the returned object. For making posterior densities
  if (return_hist_data){
    hist_data = data.frame(matrix(ncol = length(effect_names) + 1, nrow = n_samples))
    colnames(hist_data)[1:length(effect_names)] = effect_names
    colnames(hist_data)[length(effect_names)+1] = "sigma"
    for (i in 1:length(effect_names)) {
      hist_data[effect_names[i]][,1] = rho_samples[,i]
    }
    colnames(hist_data)[1:length(effect_names)] = paste0("rho_", effect_names)
    hist_data$sigma = as.numeric(sigma)
    inference_results = list(inference_results = inference_results, hist_data = hist_data)
  }
  
  # Calculate posterior probabilities 
  inference_results$posterior_prob = c(paste("Posterior probability of", effect_names[1], ">",effect_names[2], sep =" "), mean(ifelse(rho_samples[,1] > rho_samples[,2], 1, 0)))
  
  return(inference_results)
}

#Helping function used in extract_res_inla
unpack_summary = function(inla_res){
  res_frame = inla_res$summary.lincomb.derived
  effect_frame = read.table(text = rownames(res_frame), sep=":", strip.white = T)
  colnames(effect_frame) = c("compare","effect","value")
  unique_frame = unique(effect_frame[1:2])
  stopifnot(ncol(unique_frame) == 2)
  n_elem = nrow(unique_frame)
  return_list = vector(mode = "list", length = n_elem)
  for (i in 1:n_elem) {
    name = paste(unique_frame[i,2], unique_frame[i,1], sep = " ")
    return_list[[i]] = res_frame[effect_frame$compare == unique_frame[i,1] & effect_frame$effect == unique_frame[i,2],]
    return_list[[i]]$ID = effect_frame$value[effect_frame$compare == unique_frame[i,1] & effect_frame$effect == unique_frame[i,2]]
    names(return_list)[i] = name
    rownames(return_list[[i]]) = 1:length(return_list[[i]][,1])
  }
  return(return_list)
}

#Not in use
plot_random_effects_separate = function(random_effect_list, nrow = NULL, ncol = NULL, n_plots = NULL, verbose = T){
  if(class(random_effect_list) == "inla"){ #Unpack inla res object
    random_effect_list = unpack_summary(random_effect_list)
  }
  n_effects = length(random_effect_list)
  
  do_grid = F
  if(!is.null(nrow) & !is.null(ncol)){
    if(is.null(n_plots)){ #auto infer
      nplots = n_effects%/%(nrow*ncol) + 1*n_effects%%(nrow*ncol)
    }else{
      nplots = n_plots
      nrow = 1
      ncol = 1
    }
    stopifnot(n_effects <= ncol*nrow*nplots) #Cant fit all plots
    plot_list = list()
    do_grid = T
  }
  else{
    nrow = 1
    ncol = 1
    nplots = n_effects
  }
  for (i in 1:n_effects) {
    p = ggplot(aes(x=ID, y=`0.5quant`), data = random_effect_list[[i]]) + 
      geom_line(aes(group=1)) + 
      geom_line(aes(x=ID, y=`0.025quant`, group=1), lty=2) + 
      geom_line(aes(x=ID, y=`0.975quant`, group=1), lty=2) + 
      ggtitle(names(random_effect_list)[i]) + 
      ylab("random effect") + 
      xlab(paste(word(names(random_effect_list)[i],1))) +
      theme(strip.text.x = element_blank(),
            legend.background = element_rect(fill = "transparent"),
            legend.key = element_rect(colour = "black"),
            legend.position=c(0.3, 0.85),
            legend.title = element_blank(),
            axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 17),
            axis.text = element_text(size = 17),
            legend.text = element_text(size = 17))
    if(!do_grid){
      print(p)
    }else{
      plot_list = c(plot_list, list( i = p))
    }
  }
  if(do_grid){
    n_whole = n_effects%/%(nrow*ncol)
    n_leftover = n_effects%%(nrow*ncol)
    if(n_whole > 0){
      for (j in 1:n_whole) {
        print(ggarrange(plotlist = plot_list[(nrow*ncol*(j - 1) + 1):(nrow*ncol*j)], ncol = ncol, nrow = nrow))
      }
    }
    if(n_leftover > 0){
      print(ggarrange(plotlist = plot_list[(n_whole*nrow*ncol+1):(n_whole*nrow*ncol+n_leftover)], ncol = ncol, nrow = nrow))
    }
  }
  if(verbose & !do_grid) print(paste("Produced",as.character(nplots),"plots."))
}

#Not in use
plot_random_effects_joint = function(random_effect_list){
  if(class(random_effect_list) == "inla"){ #Unpack inla res object
    random_effect_list = unpack_summary(random_effect_list)
  }
  
  if(any("Age" == substr(names(temp), 1, 3))){#Age effects
    age_list = temp["Age" == substr(names(temp), 1, 3)]
    age_list$`Age BA+ vs HS`$ident = "Age BA+ vs HS"
    age_list$`Age BA+ vs less than HS`$ident = "Age BA+ vs less than HS"
    age_list$`Age BA+ vs some college/AA`$ident = "Age BA+ vs some college/AA"
    age_df = bind_rows(age_list)
    colnames(age_df)[1] = "Age"
    colnames(age_df)[5] = "Effect"
    ggplot(data = age_df) + 
      geom_line(aes(y = `0.5quant`, x = Age, color = ident)) +
      geom_line(aes(y = `0.025quant`, x = Age, color = ident), linetype = 2) +
      geom_line(aes(y = `0.975quant`, x = Age, color = ident), linetype = 2) +
      theme_bw() + 
      theme(strip.text.x = element_blank(),
            legend.background = element_rect(fill = "transparent"),
            legend.key = element_rect(colour = "black"),
            legend.position=c(0.3, 0.85),
            legend.title = element_blank(),
            axis.title.x = element_text(size = 25),
            axis.title.y = element_text(size = 17),
            axis.text = element_text(size = 17),
            legend.text = element_text(size = 17)) +
      scale_color_manual(values = c("steelblue","magenta", "orange"))
    
    
  }
  if(any("Coh" == substr(names(temp), 1, 3))){#cohort effects
    
  }
  if(any("Per" == substr(names(temp), 1, 3))){#period effects
    #Unused
    tempp = 1
  }
  
  
  
  
  
  
  
}

#Takes prior objects from makemyprior and extract_res_inla in lists to compare posteriors and priors.
compare_posterior_prior = function(posterior_list, prior_list, replace_na = F, prior_dir_at_split = c(T,F,F,F), 
                                   generate_dir = T, ylabels = NA, dir_to_blind = T, dir_samp = 10000, squash_list = NA,
                                   no_y_label = T, hide_y_ticks = F, posterior_left = c(T,F,F,T)){
  #Color blind pallete
  #cbPalette <- c("#D55E00", "#009E73","#56B4E9")# "#F0E442", "#CC79A7", "#0072B2", "#E69F00", "#999999")
  cbPalette = viridis::viridis(length(names(posterior_list)))
  
  #Process prior data
  if(!any(names(prior_list) == "covmats")){
    prior_data = data.frame(matrix(ncol = 4, nrow = 0))
    colnames(prior_data) = c("x","y","param","model")
    for (model in names(prior_list)) {
      temp_prior_data = as.data.frame(plot_prior(prior_list[[model]])$data)
      temp_prior_data$model = model
      for (split in 1:4) {
        temp_prior_data$param[temp_prior_data$param == unique(temp_prior_data$param)[split]] = as.character(split-1)
      }
      prior_data = rbind(prior_data, temp_prior_data)
    }
  }else{
    prior_data = as.data.frame(plot_prior(prior)$data)
  }
  
  #Process posterior sample data
  if(any(names(posterior_list) == "hist_data")){#More than one posterior to compare
    column_names = colnames(posterior_list[[names(posterior_list)[1]]][["hist_data"]])
    posterior_data = data.frame(matrix(ncol = length(column_names) + 1, nrow = 0))
    colnames(posterior_data) = c(column_names, "Model")
  }else{
    column_names = colnames(posterior_list$hist_data)
    posterior_data = data.frame(matrix(ncol = length(column_names) + 1, nrow = 0))
    colnames(posterior_data) = c(column_names, "Model")
  }
  for (posterior_name in names(posterior_list)){
    temp = posterior_list[[posterior_name]][["hist_data"]]
    temp$model = posterior_name
    posterior_data = rbind(posterior_data, temp)
  }
  legend_positions_priors = list("1" = c(0.5, 0.80), 
                                 "2" = c(0.5, 0.80), 
                                 "3" = c(0.5,0.80),
                                 "4" = c(0.5, 0.80))
  left_pos = c(0.25, 0.75)
  right_pos = c(0.75, 0.75)
  legend_positions_posteriors = list("1" = ifelse(rep(posterior_left[1], 2), left_pos, right_pos), 
                                     "2" = ifelse(rep(posterior_left[2], 2), left_pos, right_pos), 
                                     "3" = ifelse(rep(posterior_left[3], 2), left_pos, right_pos),
                                     "4" = ifelse(rep(posterior_left[4], 2), left_pos, right_pos))
  break_pos = list("1" = NA,
                   "2" = c(0,0.15,0.80, 1),
                   "3" = c(0,0.22,0.9, 1),
                   "4" = c(0,0.1,0.9, 1))
  
  x_labels = list("1" = NA,
                  "2" = c("","Purely\nstructured","Purely\nunstructured",""),
                  "3" = c("","Purely\nAge & Cohort","Purely\nPeriod",""),
                  "4" = c("","Purely\nCohort","Purely\nAge",""))
  
  x_lim_list = list("1" = c(0.1, 0.5),
                    "2" = c(0,0.15),
                    "3" = c(0,1),
                    "4" = c(0,1))
  
  x_labels_bot = list("1" = latex2exp::TeX("Standard deviation $\\sigma$"),
                      "2" = latex2exp::TeX("$\\omega_{top}$"),
                      "3" = latex2exp::TeX("$\\omega_{mid}$"),
                      "4" = latex2exp::TeX("$\\omega_{bot}$"))
  
  #Handle X-labels
  if(is.na(ylabels)){
    if(no_y_label){
      ylabels = c("","","","")
    }else{
      ylabels = c(latex2exp::TeX("$\\pi(\\sigma)$"), rep(latex2exp::TeX("$\\pi(\\rho)$"), 3))
    }
  }else if(length(ylabels) != 4){
    ylabels = c(latex2exp::TeX("$\\pi(\\sigma)$"), rep(latex2exp::TeX("$\\pi(\\rho)$"), 3))
  }
  
  
  plot_list = list()
  for (split in 1:4) { #For each split
    split_data = prior_data[prior_data$param == as.character(split-1),]
    inference_query = c("sigma", "rho_random_age_period_cohort", "rho_period_age_cohort", "rho_age_cohort")[split]
    if(generate_dir & prior_dir_at_split[split] & split != 1){
      split_data = split_data[split_data$model != "Dirichlet", ]
      samps = MCMCpack::rdirichlet(n = dir_samp, alpha = c(1,1,1,1))
      samps = as.data.frame(samps)
      colnames(samps) = c("Age","Period","Cohort","Random")
      if(inference_query == "rho_random_age_period_cohort"){
        temp = density(samps$Random, from = 0, to = 1)
      }
      if(inference_query == "rho_period_age_cohort"){
        temp = density(samps$Period/rowSums(samps[c("Period","Age","Cohort")]), from = 0, to = 1)
      }
      if(inference_query == "rho_age_cohort"){
        temp = density(samps$Age/rowSums(samps[c("Age","Cohort")]), from = 0, to = 1)
      }
      temp = as.data.frame(temp[c("x","y")])
      temp$model = "Dirichlet"
      temp$param = as.character(split-1)
      split_data = rbind(split_data, temp)
    }
    sample_data = data.frame(matrix(ncol = 3, nrow = 0))
    colnames(sample_data) = c("x","y","model")
    for (model in unique(posterior_data$model)) {
      density_data = density(posterior_data[posterior_data$model == model,inference_query], from = 0, to = 1)
      density_data = as.data.frame(density_data[c("x","y")])
      density_data$model = model
      sample_data = rbind(sample_data, density_data)
    }
    split_data = split_data[c("x","y","model")]
    if(!prior_dir_at_split[split]){
      split_data = split_data[split_data$model != "Dirichlet",]
    }
    
    split_data = split_data[!is.na(split_data$y), ]
    sample_data = sample_data[!is.na(sample_data$y), ]
    
    tops = sample_data%>%dplyr::group_by(model)%>%dplyr::summarize(max = max(y))
    if(max(tops$max) > 2*min(tops$max)){
      tops = tops[-which.max(tops$max),]
      sample_ylim = c(0, 1.1*max(ifelse(is.infinite(tops$max), NA, tops$max[] ), na.rm = T))
    }else{
      sample_ylim = c(0, 1.1*max(ifelse(is.infinite(sample_data$y), NA, sample_data$y ), na.rm = T))
      }
    split_ylim = c(0, 1.1*max(ifelse(is.infinite(split_data$y), NA, split_data$y ), na.rm = T))
    
    split_data$y[is.infinite(split_data$y)] = 999
    sample_data$y[is.infinite(sample_data$y)] = 999
    
    levels = names(posterior_list)
    split_data$model2 <- factor(split_data$model, levels = levels)
    sample_data$model2 <- factor(sample_data$model, levels = levels)
    
    #colors_prior = cbPalette[c("EK"%in%unique(split_data$model2), "Dirichlet"%in%unique(split_data$model2), "Anti-EK"%in%unique(split_data$model2))]
    #colors_posterior = cbPalette[c("EK"%in%unique(sample_data$model2), "Dirichlet"%in%unique(sample_data$model2), "Anti-EK"%in%unique(sample_data$model2))]
    colors_prior = cbPalette
    colors_posterior = cbPalette
    
    
    if(dir_to_blind){
      split_data$model[split_data$model == "Dirichlet"] = "Blind"
      sample_data$model[sample_data$model == "Dirichlet"] = "Blind"
      levels_samp = unique(sample_data$model)
      levels_split = unique(split_data$model)
      split_data$model2 <- factor(split_data$model, levels = levels_split)
      sample_data$model2 <- factor(sample_data$model, levels = levels_samp)
    }
    
    if(split == 1){
      p_posteriors = ggplot(data = sample_data, mapping = aes(x = x, y = y, fill = model2)) + 
        geom_line() + 
        coord_cartesian(ylim = sample_ylim, xlim = x_lim_list[[split]]) +
        scale_x_continuous(position = "bottom", lim = x_lim_list[[split]]) +
        ylim(sample_ylim) + 
        geom_area(alpha = 0.4, position = "identity") +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              legend.background = element_rect(fill = "transparent"),
              legend.key = element_rect(colour = "black"),
              legend.position=legend_positions_posteriors[[as.character(split)]],
              legend.title = element_blank(),
              axis.title.x = element_text(size = 20),
              axis.text.y = element_text(size = ifelse(hide_y_ticks, 0, 17)),
              axis.ticks.y = element_line(color = ifelse(hide_y_ticks, NA, "black")),
              axis.title.y = element_text(size = 20),
              axis.text = element_text(size = 17),
              legend.text = element_text(size = 17)) +
        ylab(ylabels[[split]]) + 
        xlab(x_labels_bot[[split]]) +
        scale_fill_manual(values = colors_posterior) 
      
      p_priors = ggplot(data = split_data, mapping = aes(x = x, y = y, fill = model2)) + 
        geom_line() + 
        coord_cartesian(ylim = split_ylim, xlim = c(0,4)) +
        scale_x_continuous(position = "bottom", lim = c(0,4)) +
        geom_area(alpha = 0.4, position = "identity") +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              legend.background = element_rect(fill = "transparent"),
              legend.key = element_rect(colour = "black"),
              legend.position=legend_positions_priors[[as.character(split)]],
              legend.title = element_blank(),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              axis.text = element_text(size = 17),
              axis.text.y = element_text(size = ifelse(hide_y_ticks, 0, 17)),
              axis.ticks.y = element_line(color = ifelse(hide_y_ticks, NA, "black")),
              legend.text = element_text(size = 17)) +
        ylab(ylabels[[split]]) + 
        xlab(x_labels_bot[[split]]) +
        scale_fill_manual(values = colors_prior)
    }else{
      p_posteriors = ggplot(data = sample_data, mapping = aes(x = x, y = y, fill = model2)) + 
        geom_line() + 
        coord_cartesian(ylim = sample_ylim, xlim = x_lim_list[[split]]) +
        scale_x_continuous(position = "bottom", lim = x_lim_list[[split]], sec.axis = sec_axis(~., breaks = break_pos[[split]], labels = x_labels[[split]])) +
        geom_area(alpha = 0.4, position = "identity") +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              legend.background = element_rect(fill = "transparent"),
              legend.key = element_rect(colour = "black"),
              legend.position=legend_positions_posteriors[[as.character(split)]],
              legend.title = element_blank(),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              axis.text = element_text(size = 17),
              axis.text.y = element_text(size = ifelse(hide_y_ticks, 0, 17)),
              axis.ticks.x.top = element_line(color = c("black",NA, NA, "black")),
              axis.ticks.y = element_line(color = ifelse(hide_y_ticks, NA, "black")),
              legend.text = element_text(size = 17)) +
        ylab(ylabels[[split]]) + 
        xlab(x_labels_bot[[split]]) +
        scale_fill_manual(values = colors_posterior)
      
      if(split == 2){
        grob = grobTree(textGrob("=", x = 0.8, y = 0.05, rot = 90, gp = gpar(fontsize = 30)))
        
        p_posteriors = ggplot(data = sample_data, mapping = aes(x = x, y = y, fill = model2)) + 
          geom_line() + 
          coord_cartesian(ylim = sample_ylim, xlim = x_lim_list[[split]]) +
          scale_x_continuous(position = "bottom", lim = x_lim_list[[split]], sec.axis = sec_axis(~., breaks = c(0,0.023,0.12, 0.15), labels = x_labels[[split]]),
                             breaks = c(0, 0.05, 0.10, 0.15), labels = c("0", "0.05", "0.10", "1")) +
          geom_area(alpha = 0.4, position = "identity") +
          theme_bw() +
          theme(strip.text.x = element_blank(),
                legend.background = element_rect(fill = "transparent"),
                legend.key = element_rect(colour = "black"),
                legend.position=legend_positions_posteriors[[as.character(split)]],
                legend.title = element_blank(),
                axis.title.x = element_text(size = 20),
                axis.title.y = element_text(size = 20),
                axis.text = element_text(size = 17),
                axis.text.y = element_text(size = ifelse(hide_y_ticks, 0, 17)),
                axis.ticks.x.top = element_line(color = c("black",NA, NA, "black")),
                axis.ticks.y = element_line(color = ifelse(hide_y_ticks, NA, "black")),
                legend.text = element_text(size = 17)) +
          ylab(ylabels[[split]]) + 
          xlab(x_labels_bot[[split]]) +
          scale_fill_manual(values = colors_posterior) +
          annotation_custom(grob)
      }
      
      
      p_priors = ggplot(data = split_data, mapping = aes(x = x, y = y, fill = model2)) + 
        geom_line() + 
        coord_cartesian(ylim = split_ylim, xlim = c(0,1)) +
        scale_x_continuous(position = "bottom", lim = c(0,1), sec.axis = sec_axis(~., breaks = break_pos[[split]], labels = x_labels[[split]])) +
        geom_area(alpha = 0.4, position = "identity") +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              legend.background = element_rect(fill = "transparent"),
              legend.key = element_rect(colour = "black"),
              legend.position=legend_positions_priors[[as.character(split)]],
              legend.title = element_blank(),
              axis.title.x = element_text(size = 20),
              axis.title.y = element_text(size = 20),
              axis.text = element_text(size = 17),
              axis.text.y = element_text(size = ifelse(hide_y_ticks, 0, 17)),
              axis.ticks.x.top = element_line(color = c("black",NA, NA, "black")),
              axis.ticks.y = element_line(color = ifelse(hide_y_ticks, NA, "black")),
              legend.text = element_text(size = 17)) +
        ylab(ylabels[[split]]) + 
        xlab(x_labels_bot[[split]]) +
        scale_fill_manual(values = colors_prior) 
    }
    
    plot_list = c(plot_list, list(prior = p_priors, posterior = p_posteriors))
    
  }
  return(plot_list)
}

#Plots the cross strata differences of the inla-objects given in list
plot_lincombs_joint = function(res_list, do_exp = T, dir_to_blind = T, pos_list = NA, boost = 0, def_pos = c(0.25, 0.85)){
  
  #Color blind pallete
  #cbPalette <- c("#D55E00", "#009E73","#56B4E9", "#F0E442", "#CC79A7", "#0072B2", "#E69F00", "#999999")
  
  cbPalette = viridis::viridis(length(res_list))
  
  plot_list = list()
  res_df = as.data.frame(matrix(nrow = 0, ncol = 7))
  colnames(res_df) = c("ID", "0.5quant", "0.025quant", "0.975quant", "model", "effect", "level")
  
  for (result in names(res_list)) {
    temp_res = res_list[[result]]$summary.lincomb.derived
    temp_res$model = result
    temp_res[c("level", "effect","ID")] = t(as.data.frame(strsplit(rownames(temp_res), ":")))
    temp_res$effect = gsub(" ", "", temp_res$effect)
    temp_res = temp_res[colnames(res_df)]
    res_df = rbind(res_df, temp_res)
  }
  res_df$ID = as.numeric(res_df$ID)
  res_df$ID[res_df$effect == "Cohort"] = res_df$ID[res_df$effect == "Cohort"] + 1912 #First cohort is 1913
  
  if(do_exp){
    res_df$`0.5quant` = exp(res_df$`0.5quant`)
    res_df$`0.975quant` = exp(res_df$`0.975quant`)
    res_df$`0.025quant` = exp(res_df$`0.025quant`)
  }
  
  #If unspecified, then do over
  if(is.na(pos_list)){
    pos_list = list()
    for (i in 1:(length(unique(res_df$effect))*length(unique(res_df$level)))) {
      pos_list = c(pos_list, list(pos = def_pos))
    }
  }else if(length(pos_list) != length(unique(res_df$effect))*length(unique(res_df$level))){ #partially specified list
    pos_indices = names(pos_list)
    pos_list_new = list()
    for (i in 1:(length(unique(res_df$effect))*length(unique(res_df$level)))) {
      if(any(as.character(i) %in% pos_indices)){
        pos_list_new = c(pos_list_new, list(pos = pos_list[[as.character(i)]]))
      }else{
        pos_list_new = c(pos_list_new, list(pos = c(0.3, 0.75)))
      }
    }
    pos_list = pos_list_new
  }
  
  
  i = 1
  for (effect in unique(res_df$effect)) {
    max_y = max(res_df[res_df$effect == effect,]$`0.975quant`) + 0.05 + boost
    min_y = min(res_df[res_df$effect == effect,]$`0.025quant`) - 0.05
    for (level in unique(res_df$level)) {
      
      sub_df = res_df[res_df$effect == effect & res_df$level == level,]
      if(dir_to_blind){
        sub_df$model[sub_df$model == "Dirichlet"] = "Blind"
      }
      levels = names(res_list)
      levels = ifelse(rep(dir_to_blind, length(names(res_list))), sub("Dirichlet", "Blind", levels) , levels)
      sub_df$model2 <- factor(sub_df$model, levels = levels)
      fac = 1.3
      titl = level
      titl = gsub("Less than HS", "LHS/GED", titl, fixed = T)
      titl = gsub("Some college/AA", "SC/AA", titl, fixed = T)
      p = ggplot(aes(x=ID, y=`0.5quant`, fill = model2), data = sub_df) + 
        geom_line() + 
        ylim(c(min_y, max_y)) + 
        geom_ribbon(aes(ymax = `0.975quant`, ymin = `0.025quant`, fill = model2), alpha = 0.4) + 
        ylab(ifelse(do_exp, "Odds ratio", "Effect")) + 
        theme_bw() + 
        theme(strip.text.x = element_blank(),
              legend.background = element_rect(fill = "transparent"),
              legend.key = element_rect(colour = "black"),
              legend.position = pos_list[[i]],
              legend.title = element_blank(),
              axis.title.x = element_text(size = fac*24),
              axis.title.y = element_text(size = fac*20),
              axis.text.x = element_text(size = fac*20*ifelse(effect != "Age",0.93,1)),
              axis.text = element_text(size = fac*20),
              legend.text = element_text(size = fac*20),
              plot.title = element_text(hjust = 0.5, size = fac*20)) +
        xlab(effect) + 
        scale_fill_manual(values = cbPalette[names(res_list) %in% sub_df$model]) +
        ggtitle(titl)
      if(effect == "Period"){
        p = p + scale_x_continuous(breaks = c(2000,2005,2010,2015), labels = c("2000 ", " 2005 "," 2010 ", " 2015"))
      }else{
        p = p + scale_x_continuous(breaks = scales::pretty_breaks(n = 4))
      }
      plot_list = c(plot_list, list(heh = p))
      i = i + 1
    }
  }
  return(plot_list)
}

