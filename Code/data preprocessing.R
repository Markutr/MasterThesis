library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(INLA)
library(makemyprior)
library(matrixcalc)
library(haven)
library(labelled)
library(fastDummies)
library(survey)

options(dplyr.summarise.inform = FALSE)

# SET CORRECT WORKING DIRECTORY
#setwd("/Users/MarkusTraetli/Desktop/Masteroppgave/Code") #MAKE SURE THIS IS CORRECT


NHIS_data_name = "CODED-NHIS-1997-2018-24-05-09.dta"

if(!file.exists("./Data/processed_data.RData")){
  if(!file.exists(paste("./Data/", NHIS_data_name, sep=""))){
    cat("NHIS data file not found.\n")
    stop()
  }
  cat("Processed data not found. Processing data...\n")
  
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
    
  data = read_dta(paste("./Data/", NHIS_data_name, sep = ""))
  data = as.data.frame(data)
  data = labelled::remove_labels(data)
  data$education = as.factor(data$education)
  
  data = data[!is.na(data$backpain),] #Remove NA
  
  #Fix indices
  min_age = min(data$age)
  max_age = max(data$age)
  data$person_age = data$age
  data$age = data$age - min_age + 1  #Age indices start at 1
  min_year = min(data$year)
  max_year = max(data$year)
  data$period = data$year - min_year + 1
  data$cohort = max(data$age) - data$age + data$period
  data$birth_cohort = data$year - data$person_age
  data_extras = list("min_year" = min_year, "min_age" = min_age, "max_age" = max(data$person_age), "max_year" = max_year)
  
  #Transform to integer
  data$period = as.integer(data$period)
  data$age = as.integer(data$age)
  data$cohort = as.integer(data$cohort)
  data$backpain = as.integer(data$backpain)
  data$race = as.factor(data$race)
  
  #Separate by gender
  data_f = data[data$female==1,] 
  data_m = data[data$female==0,] 
  
  #Filter for only white, non foreign born
  data_f_filter = data_f[data_f$race == 1 & !data_f$immigrant, ]
  data_m_filter = data_m[data_m$race == 1 & !data_m$immigrant, ]
  
  ##########################
  #Female data
  ##########################
  
  bp_aggregated = data_f%>%dplyr::group_by(age, period, education)%>%dplyr::summarise(total_count = n(), n_backpain = sum(backpain), tot_weight = sum(sampweight))
  bp_aggregated$random = 1:length(bp_aggregated$age)
  bp_aggregated$cohort = max(bp_aggregated$age) - bp_aggregated$age + bp_aggregated$period
  
  #Make NA structure
  edu_dummy = dummy_cols(bp_aggregated["education"])
  edu_dummy$age_edu_1 = edu_dummy$education_1*bp_aggregated$age
  edu_dummy$age_edu_2 = edu_dummy$education_2*bp_aggregated$age
  edu_dummy$age_edu_3 = edu_dummy$education_3*bp_aggregated$age
  edu_dummy$age_edu_4 = edu_dummy$education_4*bp_aggregated$age
  edu_dummy$cohort_edu_1 = edu_dummy$education_1*bp_aggregated$cohort
  edu_dummy$cohort_edu_2 = edu_dummy$education_2*bp_aggregated$cohort
  edu_dummy$cohort_edu_3 = edu_dummy$education_3*bp_aggregated$cohort
  edu_dummy$cohort_edu_4 = edu_dummy$education_4*bp_aggregated$cohort
  edu_dummy$period_edu_1 = edu_dummy$education_1*bp_aggregated$period
  edu_dummy$period_edu_2 = edu_dummy$education_2*bp_aggregated$period
  edu_dummy$period_edu_3 = edu_dummy$education_3*bp_aggregated$period
  edu_dummy$period_edu_4 = edu_dummy$education_4*bp_aggregated$period
  edu_dummy[edu_dummy == 0] = NA
  bp_aggregate_2 = bind_cols(edu_dummy, bp_aggregated)
  bp_aggregate_2$Ntrials = bp_aggregate_2$total_count
  bp_aggregate_2$education = bp_aggregate_2$education...1
  bp_data = bp_aggregate_2[,-c(1, 20)]
  bp_data = bind_rows(bp_data[!is.na(bp_data$education_1),], #Order
                      bp_data[!is.na(bp_data$education_2),], 
                      bp_data[!is.na(bp_data$education_3),], 
                      bp_data[!is.na(bp_data$education_4),]) 
  
  bp_data = as.data.frame(bp_data)
  
  #Make an individium specific version
  bp_ind = data_f[c("age","period","cohort", "education", "backpain", "race", "immigrant","marital","sampweight", "strata", "psu")]
  edu_dummy = dummy_cols(bp_ind["education"])
  edu_dummy$age_edu_1 = edu_dummy$education_1*bp_ind$age
  edu_dummy$age_edu_2 = edu_dummy$education_2*bp_ind$age
  edu_dummy$age_edu_3 = edu_dummy$education_3*bp_ind$age
  edu_dummy$age_edu_4 = edu_dummy$education_4*bp_ind$age
  edu_dummy$cohort_edu_1 = edu_dummy$education_1*bp_ind$cohort
  edu_dummy$cohort_edu_2 = edu_dummy$education_2*bp_ind$cohort
  edu_dummy$cohort_edu_3 = edu_dummy$education_3*bp_ind$cohort
  edu_dummy$cohort_edu_4 = edu_dummy$education_4*bp_ind$cohort
  edu_dummy[edu_dummy == 0] = NA
  race_dummy = dummy_cols(bp_ind["race"])[,c(2,3,4,5)]
  bp_ind = bind_cols(edu_dummy, bp_ind, race_dummy)
  bp_ind = bp_ind[,-c(17)]
  colnames(bp_ind)[1] = "edu4"
  bp_ind$education = as.numeric(bp_ind$edu4)
  bp_ind$Ntrials = 1
  bp_ind = bind_rows(bp_ind[!is.na(bp_ind$education_1),], #Order
                     bp_ind[!is.na(bp_ind$education_2),], 
                     bp_ind[!is.na(bp_ind$education_3),], 
                     bp_ind[!is.na(bp_ind$education_4),]) 
  
  
  bp_ind$random = rep(NA, length(bp_ind$education_1)) #assign random index
  i = 1
  for (age in 1:max(bp_ind$age)) {
    for (period in 1:max(bp_ind$period)) {
      for (education in 1:4) {
        bp_ind$random[bp_ind$age == age & bp_ind$period == period & bp_ind$edu4 == education] = i
        i = i + 1
      }
    }  
  }
  
  bp_data_f = bp_data
  bp_ind_f = bp_ind
  
  
  #Survey design
  des <- svydesign(ids = ~psu, strata = ~ strata,
                   weights = ~sampweight, data = bp_ind_f, nest = T)
  
  prevs = svyby(~backpain, 
                by = ~education + age + period,
                design = des, FUN = svymean)
  
  prevs$cohort = max(prevs$age) - prevs$age + prevs$period
  bp_data_f$education = as.numeric(bp_data_f$education)
  prevs = left_join(prevs, bp_data_f, by = c("age","period","cohort","education"))
  temp = prevs[prevs$backpain == 0 | prevs$backpain == "0", c("backpain","se")]
  if(nrow(temp) > 0){
    cat(paste(nrow(temp), "rows replaced by p=0.01 and se=1\n"))
    temp[,1] = 0.01
    temp[,2] = 1
    prevs[prevs$backpain == 0 | prevs$backpain == "0", c("backpain","se")] = temp
  }
  temp = prevs[prevs$backpain == 1 | prevs$backpain == "1" , c("backpain","se")]
  if(nrow(temp) > 0){
    cat(paste(nrow(temp), "rows replaced by p=0.99 and se=1\n"))
    temp[,1] = 0.99
    temp[,2] = 1
    prevs[prevs$backpain == 1 | prevs$backpain == "1", c("backpain","se")] = temp
  }
  prevs$logit_phat = logit(prevs$backpain)
  prevs$logit_prec = prevs$backpain**2*(1-prevs$backpain)**2/(prevs$se**2)
  prevs$scale = prevs$logit_prec
  
  survey_data_f = prevs
  
  cat("Female data complete.")
  
  ##########################
  #Male data
  ##########################
  
  
  #Aggregate
  bp_aggregated = data_m%>%dplyr::group_by(age, period, education)%>%dplyr::summarise(total_count = n(), n_backpain = sum(backpain), tot_weight = sum(sampweight))
  bp_aggregated$random = 1:length(bp_aggregated$age)
  bp_aggregated$cohort = max(bp_aggregated$age) - bp_aggregated$age + bp_aggregated$period
  
  #Make NA structure
  edu_dummy = dummy_cols(bp_aggregated["education"])
  edu_dummy$age_edu_1 = edu_dummy$education_1*bp_aggregated$age
  edu_dummy$age_edu_2 = edu_dummy$education_2*bp_aggregated$age
  edu_dummy$age_edu_3 = edu_dummy$education_3*bp_aggregated$age
  edu_dummy$age_edu_4 = edu_dummy$education_4*bp_aggregated$age
  edu_dummy$cohort_edu_1 = edu_dummy$education_1*bp_aggregated$cohort
  edu_dummy$cohort_edu_2 = edu_dummy$education_2*bp_aggregated$cohort
  edu_dummy$cohort_edu_3 = edu_dummy$education_3*bp_aggregated$cohort
  edu_dummy$cohort_edu_4 = edu_dummy$education_4*bp_aggregated$cohort
  edu_dummy$period_edu_1 = edu_dummy$education_1*bp_aggregated$period
  edu_dummy$period_edu_2 = edu_dummy$education_2*bp_aggregated$period
  edu_dummy$period_edu_3 = edu_dummy$education_3*bp_aggregated$period
  edu_dummy$period_edu_4 = edu_dummy$education_4*bp_aggregated$period
  edu_dummy[edu_dummy == 0] = NA
  bp_aggregate_2 = bind_cols(edu_dummy, bp_aggregated)
  bp_aggregate_2$Ntrials = bp_aggregate_2$total_count
  bp_aggregate_2$education = bp_aggregate_2$education...1
  bp_data = bp_aggregate_2[,-c(1, 20)]
  bp_data = bind_rows(bp_data[!is.na(bp_data$education_1),], #Order
                      bp_data[!is.na(bp_data$education_2),], 
                      bp_data[!is.na(bp_data$education_3),], 
                      bp_data[!is.na(bp_data$education_4),]) 
  
  bp_data = as.data.frame(bp_data)
  
  
  #Make an individium specific version
  bp_ind = data_m[c("age","period","cohort", "education", "backpain", "race", "immigrant","marital", "sampweight","psu","strata")]
  edu_dummy = dummy_cols(bp_ind["education"])
  edu_dummy$age_edu_1 = edu_dummy$education_1*bp_ind$age
  edu_dummy$age_edu_2 = edu_dummy$education_2*bp_ind$age
  edu_dummy$age_edu_3 = edu_dummy$education_3*bp_ind$age
  edu_dummy$age_edu_4 = edu_dummy$education_4*bp_ind$age
  edu_dummy$cohort_edu_1 = edu_dummy$education_1*bp_ind$cohort
  edu_dummy$cohort_edu_2 = edu_dummy$education_2*bp_ind$cohort
  edu_dummy$cohort_edu_3 = edu_dummy$education_3*bp_ind$cohort
  edu_dummy$cohort_edu_4 = edu_dummy$education_4*bp_ind$cohort
  edu_dummy[edu_dummy == 0] = NA
  race_dummy = dummy_cols(bp_ind["race"])[,c(2,3,4,5)]
  bp_ind = bind_cols(edu_dummy, bp_ind, race_dummy)
  bp_ind = bp_ind[,-c(17)]
  colnames(bp_ind)[1] = "edu4"
  bp_ind$education = as.numeric(bp_ind$edu4)
  bp_ind$Ntrials = 1
  bp_ind = bind_rows(bp_ind[!is.na(bp_ind$education_1),], #Order
                     bp_ind[!is.na(bp_ind$education_2),], 
                     bp_ind[!is.na(bp_ind$education_3),], 
                     bp_ind[!is.na(bp_ind$education_4),]) 
  
  
  bp_ind$random = rep(NA, length(bp_ind$education_1)) #assign random index
  i = 1
  for (age in 1:max(bp_ind$age)) {
    for (period in 1:max(bp_ind$period)) {
      for (education in 1:4) {
        bp_ind$random[bp_ind$age == age & bp_ind$period == period & bp_ind$edu4 == education] = i
        i = i + 1
      }
    }  
  }
  
  bp_data_m = bp_data
  bp_ind_m = bp_ind
  
  #Survey design
  des <- svydesign(ids = ~psu, strata = ~strata,
                   weights = ~sampweight, data = bp_ind_m, nest = T)
  
  
  prevs = svyby(~backpain, 
                by = ~education + age + period,
                design = des, FUN = svymean, weights = ~sampweight)
  
  prevs$cohort = max(prevs$age) - prevs$age + prevs$period
  bp_data_m$education = as.numeric(bp_data_m$education)
  prevs = left_join(prevs, bp_data_m, by = c("age","period","cohort","education"))
  temp = prevs[prevs$backpain == 0 | prevs$backpain == "0" , c("backpain","se")]
  if(nrow(temp) > 0){
    cat(paste(nrow(temp), "rows replaced by p=0.01 and se=1\n"))
    temp[,1] = 0.01
    temp[,2] = 1
    prevs[prevs$backpain == 0 | prevs$backpain == "0" , c("backpain","se")] = temp
  }
  temp = prevs[prevs$backpain == 1 | prevs$backpain == "1", c("backpain","se")]
  if(nrow(temp) > 0){
    cat(paste(nrow(temp), "rows replaced by p=0.99 and se=1\n"))
    temp[,1] = 0.99
    temp[,2] = 1
    prevs[prevs$backpain == 1 | prevs$backpain == "1", c("backpain","se")] = temp
  }
  prevs$logit_phat = logit(prevs$backpain)
  prevs$logit_prec = prevs$backpain**2*(1-prevs$backpain)**2/(prevs$se**2)
  prevs$scale = prevs$logit_prec
  
  survey_data_m = prevs
  
  cat("Male data complete.")
  
  ##########################
  #Female filtered
  ##########################
  
  
  #Aggregate
  bp_aggregated = data_f_filter%>%dplyr::group_by(age, period, education)%>%dplyr::summarise(total_count = n(), n_backpain = sum(backpain), tot_weight = sum(sampweight))
  bp_aggregated$random = 1:length(bp_aggregated$age)
  bp_aggregated$cohort = max(bp_aggregated$age) - bp_aggregated$age + bp_aggregated$period
  
  #Make NA structure
  edu_dummy = dummy_cols(bp_aggregated["education"])
  edu_dummy$age_edu_1 = edu_dummy$education_1*bp_aggregated$age
  edu_dummy$age_edu_2 = edu_dummy$education_2*bp_aggregated$age
  edu_dummy$age_edu_3 = edu_dummy$education_3*bp_aggregated$age
  edu_dummy$age_edu_4 = edu_dummy$education_4*bp_aggregated$age
  edu_dummy$cohort_edu_1 = edu_dummy$education_1*bp_aggregated$cohort
  edu_dummy$cohort_edu_2 = edu_dummy$education_2*bp_aggregated$cohort
  edu_dummy$cohort_edu_3 = edu_dummy$education_3*bp_aggregated$cohort
  edu_dummy$cohort_edu_4 = edu_dummy$education_4*bp_aggregated$cohort
  edu_dummy$period_edu_1 = edu_dummy$education_1*bp_aggregated$period
  edu_dummy$period_edu_2 = edu_dummy$education_2*bp_aggregated$period
  edu_dummy$period_edu_3 = edu_dummy$education_3*bp_aggregated$period
  edu_dummy$period_edu_4 = edu_dummy$education_4*bp_aggregated$period
  edu_dummy[edu_dummy == 0] = NA
  bp_aggregate_2 = bind_cols(edu_dummy, bp_aggregated)
  bp_aggregate_2$Ntrials = bp_aggregate_2$total_count
  bp_aggregate_2$education = bp_aggregate_2$education...1
  bp_data = bp_aggregate_2[,-c(1, 20)]
  bp_data = bind_rows(bp_data[!is.na(bp_data$education_1),], #Order
                      bp_data[!is.na(bp_data$education_2),], 
                      bp_data[!is.na(bp_data$education_3),], 
                      bp_data[!is.na(bp_data$education_4),]) 
  
  bp_data = as.data.frame(bp_data)
  
  #Make an individium specific version
  bp_ind = data_f_filter[c("age","period","cohort", "education", "backpain", "race", "immigrant","marital","sampweight","psu","strata")]
  edu_dummy = dummy_cols(bp_ind["education"])
  edu_dummy$age_edu_1 = edu_dummy$education_1*bp_ind$age
  edu_dummy$age_edu_2 = edu_dummy$education_2*bp_ind$age
  edu_dummy$age_edu_3 = edu_dummy$education_3*bp_ind$age
  edu_dummy$age_edu_4 = edu_dummy$education_4*bp_ind$age
  edu_dummy$cohort_edu_1 = edu_dummy$education_1*bp_ind$cohort
  edu_dummy$cohort_edu_2 = edu_dummy$education_2*bp_ind$cohort
  edu_dummy$cohort_edu_3 = edu_dummy$education_3*bp_ind$cohort
  edu_dummy$cohort_edu_4 = edu_dummy$education_4*bp_ind$cohort
  edu_dummy[edu_dummy == 0] = NA
  bp_ind = bind_cols(edu_dummy, bp_ind)
  bp_ind = bp_ind[,-c(17)]
  colnames(bp_ind)[1] = "edu4"
  bp_ind$education = as.numeric(bp_ind$edu4)
  bp_ind$Ntrials = 1
  bp_ind = bind_rows(bp_ind[!is.na(bp_ind$education_1),], #Order
                     bp_ind[!is.na(bp_ind$education_2),], 
                     bp_ind[!is.na(bp_ind$education_3),], 
                     bp_ind[!is.na(bp_ind$education_4),]) 
  
  
  bp_ind$random = rep(NA, length(bp_ind$education_1)) #assign random index
  i = 1
  for (age in 1:max(bp_ind$age)) {
    for (period in 1:max(bp_ind$period)) {
      for (education in 1:4) {
        bp_ind$random[bp_ind$age == age & bp_ind$period == period & bp_ind$edu4 == education] = i
        i = i + 1
      }
    }  
  }
  
  bp_data_f_filter = bp_data
  bp_ind_f_filter = bp_ind
  
  #Survey design
  des <- svydesign(ids = ~psu, strata = ~ strata,
                   weights = ~sampweight, data = bp_ind_f_filter, nest = T)
  
  prevs = svyby(~backpain, 
                by = ~education + age + period,
                design = des, FUN = svymean)
  
  prevs$cohort = max(prevs$age) - prevs$age + prevs$period
  bp_data_f_filter$education = as.numeric(bp_data_f_filter$education)
  prevs = left_join(prevs, bp_data_f_filter, by = c("age","period","cohort","education"))
  temp = prevs[prevs$backpain == 0 | prevs$backpain == "0", c("backpain","se")]
  if(nrow(temp) > 0){
    cat(paste(nrow(temp), "rows replaced by p=0.01 and se=1\n"))
    temp[,1] = 0.01
    temp[,2] = 1
    prevs[prevs$backpain == 0 | prevs$backpain == "0", c("backpain","se")] = temp
  }
  temp = prevs[prevs$backpain == 1 | prevs$backpain == "1" , c("backpain","se")]
  if(nrow(temp) > 0){
    cat(paste(nrow(temp), "rows replaced by p=0.99 and se=1\n"))
    temp[,1] = 0.99
    temp[,2] = 1
    prevs[prevs$backpain == 1 | prevs$backpain == "1", c("backpain","se")] = temp
  }
  prevs$logit_phat = logit(prevs$backpain)
  prevs$logit_prec = prevs$backpain**2*(1-prevs$backpain)**2/(prevs$se**2)
  prevs$scale = prevs$logit_prec
  
  survey_data_f_filter = prevs
  
  cat("Female filtered data complete.")
  
  ##########################
  #Male filtered
  ##########################
  
  
  #Aggregate
  bp_aggregated = data_m_filter%>%dplyr::group_by(age, period, education)%>%dplyr::summarise(total_count = n(), n_backpain = sum(backpain), tot_weight = sum(sampweight))
  bp_aggregated$random = 1:length(bp_aggregated$age)
  bp_aggregated$cohort = max(bp_aggregated$age) - bp_aggregated$age + bp_aggregated$period
  
  #Make NA structure
  edu_dummy = dummy_cols(bp_aggregated["education"])
  edu_dummy$age_edu_1 = edu_dummy$education_1*bp_aggregated$age
  edu_dummy$age_edu_2 = edu_dummy$education_2*bp_aggregated$age
  edu_dummy$age_edu_3 = edu_dummy$education_3*bp_aggregated$age
  edu_dummy$age_edu_4 = edu_dummy$education_4*bp_aggregated$age
  edu_dummy$cohort_edu_1 = edu_dummy$education_1*bp_aggregated$cohort
  edu_dummy$cohort_edu_2 = edu_dummy$education_2*bp_aggregated$cohort
  edu_dummy$cohort_edu_3 = edu_dummy$education_3*bp_aggregated$cohort
  edu_dummy$cohort_edu_4 = edu_dummy$education_4*bp_aggregated$cohort
  edu_dummy$period_edu_1 = edu_dummy$education_1*bp_aggregated$period
  edu_dummy$period_edu_2 = edu_dummy$education_2*bp_aggregated$period
  edu_dummy$period_edu_3 = edu_dummy$education_3*bp_aggregated$period
  edu_dummy$period_edu_4 = edu_dummy$education_4*bp_aggregated$period
  edu_dummy[edu_dummy == 0] = NA
  bp_aggregate_2 = bind_cols(edu_dummy, bp_aggregated)
  bp_aggregate_2$Ntrials = bp_aggregate_2$total_count
  bp_aggregate_2$education = bp_aggregate_2$education...1
  bp_data = bp_aggregate_2[,-c(1, 20)]
  bp_data = bind_rows(bp_data[!is.na(bp_data$education_1),], #Order
                      bp_data[!is.na(bp_data$education_2),], 
                      bp_data[!is.na(bp_data$education_3),], 
                      bp_data[!is.na(bp_data$education_4),]) 
  
  bp_data = as.data.frame(bp_data)
  
  
  #Make an individium specific version
  bp_ind = data_m_filter[c("age","period","cohort", "education", "backpain", "race", "immigrant","marital", "sampweight","strata","psu")]
  edu_dummy = dummy_cols(bp_ind["education"])
  edu_dummy$age_edu_1 = edu_dummy$education_1*bp_ind$age
  edu_dummy$age_edu_2 = edu_dummy$education_2*bp_ind$age
  edu_dummy$age_edu_3 = edu_dummy$education_3*bp_ind$age
  edu_dummy$age_edu_4 = edu_dummy$education_4*bp_ind$age
  edu_dummy$cohort_edu_1 = edu_dummy$education_1*bp_ind$cohort
  edu_dummy$cohort_edu_2 = edu_dummy$education_2*bp_ind$cohort
  edu_dummy$cohort_edu_3 = edu_dummy$education_3*bp_ind$cohort
  edu_dummy$cohort_edu_4 = edu_dummy$education_4*bp_ind$cohort
  edu_dummy[edu_dummy == 0] = NA
  bp_ind = bind_cols(edu_dummy, bp_ind)
  bp_ind = bp_ind[,-c(17)]
  colnames(bp_ind)[1] = "edu4"
  bp_ind$education = as.numeric(bp_ind$edu4)
  bp_ind$Ntrials = 1
  bp_ind = bind_rows(bp_ind[!is.na(bp_ind$education_1),], #Order
                     bp_ind[!is.na(bp_ind$education_2),], 
                     bp_ind[!is.na(bp_ind$education_3),], 
                     bp_ind[!is.na(bp_ind$education_4),]) 
  
  
  bp_ind$random = rep(NA, length(bp_ind$education_1)) #assign random index
  i = 1
  for (age in 1:max(bp_ind$age)) {
    for (period in 1:max(bp_ind$period)) {
      for (education in 1:4) {
        bp_ind$random[bp_ind$age == age & bp_ind$period == period & bp_ind$edu4 == education] = i
        i = i + 1
      }
    }  
  }
  
  bp_data_m_filter = bp_data
  bp_ind_m_filter = bp_ind
  
  #Survey design
  des <- svydesign(ids = ~psu, strata = ~strata,
                   weights = ~sampweight, data = bp_ind_m_filter, nest = T)
  
  
  prevs = svyby(~backpain, 
                by = ~education + age + period,
                design = des, FUN = svymean, weights = ~sampweight)
  
  prevs$cohort = max(prevs$age) - prevs$age + prevs$period
  bp_data_m_filter$education = as.numeric(bp_data_m_filter$education)
  prevs = left_join(prevs, bp_data_m_filter, by = c("age","period","cohort","education"))
  temp = prevs[prevs$backpain == 0 | prevs$backpain == "0" , c("backpain","se")]
  if(nrow(temp) > 0){
    cat(paste(nrow(temp), "rows replaced by p=0.01 and se=1\n"))
    temp[,1] = 0.01
    temp[,2] = 1
    prevs[prevs$backpain == 0 | prevs$backpain == "0" , c("backpain","se")] = temp
  }
  temp = prevs[prevs$backpain == 1 | prevs$backpain == "1", c("backpain","se")]
  if(nrow(temp) > 0){
    cat(paste(nrow(temp), "rows replaced by p=0.99 and se=1\n"))
    temp[,1] = 0.99
    temp[,2] = 1
    prevs[prevs$backpain == 1 | prevs$backpain == "1", c("backpain","se")] = temp
  }
  prevs$logit_phat = logit(prevs$backpain)
  prevs$logit_prec = prevs$backpain**2*(1-prevs$backpain)**2/(prevs$se**2)
  prevs$scale = prevs$logit_prec
  
  
  survey_data_m_filter = prevs
  
  cat("Male filtered data complete.")
  
  
  
  ######################
  #Saving
  
  #Save to RData object to avoid doing preprocessing again
  cat("Saving...")
  save(list = c("data", "data_extras", "bp_data_f", "bp_data_m", "bp_ind_f", "bp_ind_m", "survey_data_m", "survey_data_f",
                "survey_data_f_filter", "survey_data_m_filter"), file = "./Data/processed_data.RData")
  cat("Data preprocessing completed.")
}else{
  cat("Processed data found, loading...\n")
  load("./Data/processed_data.RData")
}
