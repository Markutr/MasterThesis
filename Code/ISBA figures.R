setwd("/Users/MarkusTraetli/Desktop/Masteroppgave/Code") #MAKE SURE THIS IS CORRECT

source("plotting functions.R")

if(!file.exists("./Data/processed_data.RData")){ #Run data preprocessing script if 
  source("data preprocessing.R")
}
source("./functions.R")
load("./Data/processed_data.RData")

# Settings for INLA
inla_compute_params = list(config = TRUE, dic = TRUE, cpo = TRUE, waic = TRUE)
inla_params = list(int.strategy = "ccd", lincomb.derived.correlation.matrix = TRUE, strategy = "simplified.laplace")
control.predictor = list(compute=T)

I = max(bp_data_f$age)
J = max(bp_data_f$period)
K = max(bp_data_f$cohort)

CS_prior = list("prec" = list("prior" = "pc.prec", param = c(10,0.01))) # probability 0.01 that the std.dev is greater than 10. Huge flat prior basically


#Fit models that are needed and save them
if(!file.exists("./Data/ISBA.RData")){
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
  
  res_CS_aPc_f = inla(formula_CS_aPc, data = bp_data_f, family = "binomial", Ntrials = bp_data_f$total_count, 
                      control.compute = inla_compute_params, control.inla = inla_params, lincomb = get_lincombs("aPc", I, J, K))
  
  res_expert_f_aPc = inlaAPC("aPc", survey_data_f, "n_backpain", trim = F)
  res_expert_m_aPc = inlaAPC("aPc", survey_data_m, "n_backpain", trim = F)
  
  res_expert_f_aPc_filter = inlaAPC("aPc", survey_data_f_filter, "n_backpain", trim = F)
  res_expert_m_aPc_filter = inlaAPC("aPc", survey_data_m_filter, "n_backpain", trim = F)
  
  
  res_dir_f = inlaAPC("aPc", bp_data_f, "n_backpain", prior_mode = "Dirichlet", trim = F)
  res_anti_f = inlaAPC("aPc", bp_data_f, "n_backpain", prior_mode = "Anti-EK", trim = F)
  res_semi_f = inlaAPC("aPc", bp_data_f, "n_backpain", prior_mode = "Semi", trim = F) 
  
  res_survey_aPc_f = inlaAPC("aPc", survey_data_f, "logit_phat", family = "gaussian", do_scale = T, trim = F)
  res_survey_aPc_m = inlaAPC("aPc", survey_data_m, "logit_phat", family = "gaussian", do_scale = T, trim = F)
  
  res_survey_aPc_f_filtered = inlaAPC("aPc", survey_data_f_filter, "logit_phat", family = "gaussian", do_scale = T, trim = F)
  res_survey_aPc_m_filtered = inlaAPC("aPc", survey_data_m_filter, "logit_phat", family = "gaussian", do_scale = T, trim = F)

  save(list=c("res_expert_f_aPc", "res_expert_m_aPc","res_expert_f_aPc_filter","res_expert_m_aPc_filter", "res_dir_f", "res_anti_f", "res_semi_f",
              "res_survey_aPc_f","res_survey_aPc_m", "res_survey_aPc_f_filtered", "res_survey_aPc_m_filtered", "res_CS_aPc_f"),
       file = "./Data/ISBA.RData")
  }else{
  load("./Data/ISBA.RData")
}

#Where the saved figures are placed
output_dir = "../ISBA figures/"

#Head of data. Needs to be screenshotted...
backpain_data = data[,c("backpain","person_age","year","birth_cohort","education","female","immigrant","race","strata","psu","sampweight")]
colnames(backpain_data) = c("backpain","age","period","birth_cohort","education","female","immigrant","race","strata","psu","sampweight")
set.seed(1)
backpain_data[sample(nrow(backpain_data),6),]

#########################
# Figures 2.1 and 2.2 using filtered data
########################

fac = 1
#Female data
data_sub = data[data$female == 1 & data$immigrant ==0 & data$race == 1,]

p = data_sub%>%
  dplyr::group_by(person_age, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=person_age, color=education))+
  geom_line(size = 10)+
  theme_bw() +
  ylim(c(0, 10000)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        legend.title = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  ylab("Number") + 
  xlab("Age") + guides(color=guide_legend(title="Education")) +
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

p = as_ggplot(get_legend(p))

#Counts
p_a_f = data_sub%>%
  dplyr::group_by(person_age, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=person_age, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0, 1750)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  annotate(geom="text", x=75, y=1630, label="Female", size = fac*19/.pt) +
  xlab("Age") + guides(color = "none") + 
  ylab("Number") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

p_p_f = data_sub%>%
  dplyr::group_by(year, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=year, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0, 4000)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.26,0.25),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  xlab("Period") + guides(color = "none") + 
  ylab("Number") + 
  annotate(geom="text", x=2001, y=3750, label="Female", size = fac*19/.pt) +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

p_c_f = data_sub%>%
  dplyr::group_by(birth_cohort, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=birth_cohort, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0, 1750)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  annotate(geom="text", x=1927, y=1630, label="Female", size = fac*19/.pt) +
  scale_x_continuous(labels = c("1920","1940","1960","1980"), breaks = c(1920,1940,1960,1980)) +
  xlab("Birth cohort") + guides(color = "none") + 
  ylab("Number") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pdf(paste(output_dir,"1-3 legend.pdf", sep = ""), width = 3, height = 5)
p
dev.off()

#Male data
data_sub = data[data$female==0 & data$immigrant==0 & data$race==1,]

#Counts
p_a_m = data_sub%>%
  dplyr::group_by(person_age, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=person_age, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0, 1750)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  annotate(geom="text", x=75, y=1630, label="Male", size = fac*19/.pt) +
  xlab("Age") + guides(color = "none") + 
  ylab("Number") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

p_p_m = data_sub%>%
  dplyr::group_by(year, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=year, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0, 4000)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.26,0.25),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  annotate(geom="text", x=2001, y=3750, label="Male", size = fac*19/.pt) +
  xlab("Period") + guides(color = "none") + 
  ylab("Number") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

p_c_m = data_sub%>%
  dplyr::group_by(birth_cohort, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=birth_cohort, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0, 1750)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  scale_x_continuous(labels = c("1920","1940","1960","1980"), breaks = c(1920,1940,1960,1980)) +
  annotate(geom="text", x=1927, y=1630, label="Male", size = fac*19/.pt) +
  xlab("Birth cohort") + guides(color = "none") + 
  ylab("Number") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

data_sub = data[data$female == 1 & data$immigrant==0 & data$race == 1,]


pp_a_f = data_sub%>%
  dplyr::group_by(person_age, education)%>%
  dplyr::summarise(n_backpain = mean(backpain))%>%
  ggplot(aes(y=n_backpain, x=person_age, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0.1, 0.55)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  annotate(geom="text", x=54.5, y=0.15, label="Female", size = fac*19/.pt) +
  ylab("Rate") + 
  xlab("Age") +guides(color = "none") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pp_p_f = data_sub%>%
  dplyr::group_by(year, education)%>%
  dplyr::summarise(n_backpain = mean(backpain))%>%
  ggplot(aes(y=n_backpain, x=year, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0.1, 0.55)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  ylab("Rate") +   xlab("Period") +guides(color = "none") + 
  annotate(geom="text", x=2007.5, y=0.15, label="Female", size = fac*19/.pt) +
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pp_c_f = data_sub%>%
  dplyr::group_by(birth_cohort, education)%>%
  dplyr::summarise(n_backpain = mean(backpain))%>%
  ggplot(aes(y=n_backpain, x=birth_cohort, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0.1, 0.55)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  ylab("Rate") +   annotate(geom="text", x=1953, y=0.15, label="Female", size = fac*19/.pt) +
  scale_x_continuous(labels = c("1920","1940","1960","1980"), breaks = c(1920,1940,1960,1980)) +
  xlab("Birth cohort") +guides(color = "none") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

data_sub = data[data$female == 0 & data$immigrant==0 & data$race==1,]

# Male
pp_a_m = data_sub%>%
  dplyr::group_by(person_age, education)%>%
  dplyr::summarise(n_backpain = mean(backpain))%>%
  ggplot(aes(y=n_backpain, x=person_age, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0.1, 0.55)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  annotate(geom="text", x=54.5, y=0.15, label="Male", size = fac*19/.pt) +
  ylab("Rate") +   xlab("Age") +guides(color = "none") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pp_p_m = data_sub%>%
  dplyr::group_by(year, education)%>%
  dplyr::summarise(n_backpain = mean(backpain))%>%
  ggplot(aes(y=n_backpain, x=year, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0.1, 0.55)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  annotate(geom="text", x=2007.5, y=0.15, label="Male", size = fac*19/.pt) +
  ylab("Rate") +   xlab("Period") +guides(color = "none") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pp_c_m = data_sub%>%
  dplyr::group_by(birth_cohort, education)%>%
  dplyr::summarise(n_backpain = mean(backpain))%>%
  ggplot(aes(y=n_backpain, x=birth_cohort, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0.1, 0.55)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  annotate(geom="text", x=1953, y=0.15, label="Male", size = fac*19/.pt) +
  ylab("Rate") +   scale_x_continuous(labels = c("1920","1940","1960","1980"), breaks = c(1920,1940,1960,1980)) +
  xlab("Birth cohort") +guides(color = "none") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pdf(paste(output_dir,"1-1 numbers.pdf", sep = ""), width = 12, height = 8)
ggarrange(plotlist = list(p_a_f, p_p_f, p_c_f, p_a_m, p_p_m, p_c_m), nrow = 2, ncol = 3)
dev.off()

pdf(paste(output_dir,"1-2 proportions.pdf", sep = ""), width = 12, height = 8)
ggarrange(plotlist = list(pp_a_f, pp_p_f, pp_c_f, pp_a_m, pp_p_m, pp_c_m), nrow = 2, ncol = 3)
dev.off()


#########################
# Figures 2.1 and 2.2 using all data
########################

fac = 1
#Female data
data_sub = data[data$female == 1,]

p = data_sub%>%
  dplyr::group_by(person_age, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=person_age, color=education))+
  geom_line(size = 10)+
  theme_bw() +
  ylim(c(0, 10000)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        legend.title = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  ylab("Number") + 
  xlab("Age") + guides(color=guide_legend(title="Education")) +
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

p = as_ggplot(get_legend(p))

#Counts
p_a_f = data_sub%>%
  dplyr::group_by(person_age, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=person_age, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0, 2500)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  annotate(geom="text", x=75, y=2250, label="Female", size = fac*19/.pt) +
  xlab("Age") + guides(color = "none") + 
  ylab("Number") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

p_p_f = data_sub%>%
  dplyr::group_by(year, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=year, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(1000, 6000)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.26,0.25),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  xlab("Period") + guides(color = "none") + 
  ylab("Number") + 
  annotate(geom="text", x=2000, y=5500, label="Female", size = fac*19/.pt) +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

p_c_f = data_sub%>%
  dplyr::group_by(birth_cohort, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=birth_cohort, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0, 2500)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  annotate(geom="text", x=1925, y=2350, label="Female", size = fac*19/.pt) +
  scale_x_continuous(labels = c("1920","1940","1960","1980"), breaks = c(1920,1940,1960,1980)) +
  xlab("Birth cohort") + guides(color = "none") + 
  ylab("Number") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pdf(paste(output_dir,"9-3 legend.pdf", sep = ""), width = 3, height = 5)
p
dev.off()

#Male data
data_sub = data[data$female==0,]

#Counts
p_a_m = data_sub%>%
  dplyr::group_by(person_age, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=person_age, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0, 2500)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  annotate(geom="text", x=75, y=2250, label="Male", size = fac*19/.pt) +
  xlab("Age") + guides(color = "none") + 
  ylab("Number") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

p_p_m = data_sub%>%
  dplyr::group_by(year, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=year, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(1000, 6000)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.26,0.25),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  annotate(geom="text", x=2000, y=5500, label="Male", size = fac*19/.pt) +
  xlab("Period") + guides(color = "none") + 
  ylab("Number") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

p_c_m = data_sub%>%
  dplyr::group_by(birth_cohort, education)%>%
  dplyr::summarise(n_backpain = n())%>%
  ggplot(aes(y=n_backpain, x=birth_cohort, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0, 2500)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  scale_x_continuous(labels = c("1920","1940","1960","1980"), breaks = c(1920,1940,1960,1980)) +
  annotate(geom="text", x=1925, y=2350, label="Male", size = fac*19/.pt) +
  xlab("Birth cohort") + guides(color = "none") + 
  ylab("Number") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

data_sub = data[data$female == 1,]


pp_a_f = data_sub%>%
  dplyr::group_by(person_age, education)%>%
  dplyr::summarise(n_backpain = mean(backpain))%>%
  ggplot(aes(y=n_backpain, x=person_age, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0.1, 0.55)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  annotate(geom="text", x=54.5, y=0.15, label="Female", size = fac*19/.pt) +
  ylab("Rate") + 
  xlab("Age") +guides(color = "none") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pp_p_f = data_sub%>%
  dplyr::group_by(year, education)%>%
  dplyr::summarise(n_backpain = mean(backpain))%>%
  ggplot(aes(y=n_backpain, x=year, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0.1, 0.55)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  ylab("Rate") +   xlab("Period") +guides(color = "none") + 
  annotate(geom="text", x=2007.5, y=0.15, label="Female", size = fac*19/.pt) +
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pp_c_f = data_sub%>%
  dplyr::group_by(birth_cohort, education)%>%
  dplyr::summarise(n_backpain = mean(backpain))%>%
  ggplot(aes(y=n_backpain, x=birth_cohort, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0.1, 0.55)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  ylab("Rate") +   annotate(geom="text", x=1953, y=0.15, label="Female", size = fac*19/.pt) +
  scale_x_continuous(labels = c("1920","1940","1960","1980"), breaks = c(1920,1940,1960,1980)) +
  xlab("Birth cohort") +guides(color = "none") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

data_sub = data[data$female == 0,]

# Male
pp_a_m = data_sub%>%
  dplyr::group_by(person_age, education)%>%
  dplyr::summarise(n_backpain = mean(backpain))%>%
  ggplot(aes(y=n_backpain, x=person_age, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0.1, 0.55)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  annotate(geom="text", x=54.5, y=0.15, label="Male", size = fac*19/.pt) +
  ylab("Rate") +   xlab("Age") +guides(color = "none") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pp_p_m = data_sub%>%
  dplyr::group_by(year, education)%>%
  dplyr::summarise(n_backpain = mean(backpain))%>%
  ggplot(aes(y=n_backpain, x=year, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0.1, 0.55)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  annotate(geom="text", x=2007.5, y=0.15, label="Male", size = fac*19/.pt) +
  ylab("Rate") +   xlab("Period") +guides(color = "none") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pp_c_m = data_sub%>%
  dplyr::group_by(birth_cohort, education)%>%
  dplyr::summarise(n_backpain = mean(backpain))%>%
  ggplot(aes(y=n_backpain, x=birth_cohort, color=education))+
  geom_line(size = 1.3)+
  theme_bw() +
  ylim(c(0.1, 0.55)) + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.position = c(0.75,0.75),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20*0.95),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  annotate(geom="text", x=1953, y=0.15, label="Male", size = fac*19/.pt) +
  ylab("Rate") +   scale_x_continuous(labels = c("1920","1940","1960","1980"), breaks = c(1920,1940,1960,1980)) +
  xlab("Birth cohort") +guides(color = "none") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pdf(paste(output_dir,"9-1 numbers.pdf", sep = ""), width = 12, height = 8)
ggarrange(plotlist = list(p_a_f, p_p_f, p_c_f, p_a_m, p_p_m, p_c_m), nrow = 2, ncol = 3)
dev.off()

pdf(paste(output_dir,"9-2 proportions.pdf", sep = ""), width = 12, height = 8)
ggarrange(plotlist = list(pp_a_f, pp_p_f, pp_c_f, pp_a_m, pp_p_m, pp_c_m), nrow = 2, ncol = 3)
dev.off()


################
# Priors
################

break_pos = list("1" = NA,
                 "2" = c(0,0.15,0.80, 1),
                 "3" = c(0,0.22,0.9, 1),
                 "4" = c(0,0.1,0.9, 1))

x_labels = list("1" = NA,
                "2" = c("","Purely\nstructured","Purely\nunstructured",""),
                "3" = c("","Purely\nAge & Cohort","Purely\nPeriod",""),
                "4" = c("","Purely\nCohort","Purely\nAge",""))

prior_data = as.data.frame(plot_prior(res_expert_f_aPc$prior)$data)
prior_data_s1 = prior_data[202:302,]
prior_data_s2 = prior_data[303:403,]
prior_data_s3 = prior_data[404:504,]
colnames(prior_data_s1) = c("rho", "Density", "param")
colnames(prior_data_s2) = c("rho", "Density", "param")
colnames(prior_data_s3) = c("rho", "Density", "param")
fac = 1.3

p1 = ggplot(prior_data_s1, aes(x = rho, y = Density)) +
  geom_path() +
  theme_bw() +
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "black"),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*22),
        axis.title.y = element_text(size = fac*12),
        axis.text = element_text(size = fac*20),
        axis.text.x.top = element_text(size = 20),
        legend.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        plot.title = element_text(hjust = 0.5, size = fac*20),
        axis.ticks.x = element_line(colour = c("black",NA,NA,NA,"black")),
        axis.ticks.x.top = element_line(colour = c("black",NA,NA,"black")))  +
  scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c("0","","","","1"), sec.axis = sec_axis(~., breaks = break_pos[["2"]], labels = x_labels[["2"]])) + 
  xlab(latex2exp::TeX("$\\omega_{top}$"))

p2 = ggplot(prior_data_s2, aes(x = rho, y = Density)) +
  geom_path() +
  theme_bw() +
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "black"),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*22),
        axis.title.y = element_text(size = fac*12),
        axis.text = element_text(size = fac*20),
        axis.text.x.top = element_text(size = 20),
        legend.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        plot.title = element_text(hjust = 0.5, size = fac*20),
        axis.ticks.x = element_line(colour = c("black",NA,NA,NA,"black")),
        axis.ticks.x.top = element_line(colour = c("black",NA,NA,"black")))  +
  scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c("0","","","","1"), sec.axis = sec_axis(~., breaks = break_pos[["3"]], labels = x_labels[["3"]])) + 
  xlab(latex2exp::TeX("$\\omega_{mid}$"))

p3 = ggplot(prior_data_s3, aes(x = rho, y = Density)) +
  geom_path() +
  theme_bw() +
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "black"),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*22),
        axis.title.y = element_text(size = fac*12),
        axis.text = element_text(size = fac*20),
        axis.text.x.top = element_text(size = 20),
        legend.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        plot.title = element_text(hjust = 0.5, size = fac*20),
        axis.ticks.x = element_line(colour = c("black",NA,NA,NA,"black")),
        axis.ticks.x.top = element_line(colour = c("black",NA,NA,"black")))  +
  scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c("0","","","","1"), sec.axis = sec_axis(~., breaks = break_pos[["4"]], labels = x_labels[["4"]])) + 
  xlab(latex2exp::TeX("$\\omega_{bot}$"))

pdf(paste(output_dir,"7 prior_rest.pdf", sep = ""), width = 12, height = 4)
ggarrange(plotlist = list(p1 = p1, p2 = p2, p3 = p3), nrow = 1, ncol = 3)
dev.off()

#######################
#Lincombs
#######################

#Female
acc_vs_unacc_f = plot_lincombs_joint(list("Unacc." = res_expert_f_aPc_filter$inla, "Acc." = res_survey_aPc_f_filtered$inla))
pdf(paste(output_dir,"2-1-1 lincomb_age_unacc_vs_acc_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = acc_vs_unacc_f[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"2-1-2 lincomb_cohort_unacc_vs_acc_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = acc_vs_unacc_f[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()

all_vs_fil_f = plot_lincombs_joint(list("All" = res_survey_aPc_f$inla, "Fil." = res_survey_aPc_f_filtered$inla), override_maxmin = T)
pdf(paste(output_dir,"3-1-1 lincomb_age_all_vs_fil_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = all_vs_fil_f[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"3-1-2 lincomb_cohort_all_vs_fil_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = all_vs_fil_f[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()

fil_only_f = plot_lincombs_joint(list("Fil" = res_survey_aPc_f_filtered$inla))
pdf(paste(output_dir,"4-1-1 lincomb_age_fil_only_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = fil_only_f[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"4-1-2 lincomb_cohort_fil_only_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = fil_only_f[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()


#Male
acc_vs_unacc_m = plot_lincombs_joint(list("Unacc." = res_expert_m_aPc_filter$inla, "Acc." = res_survey_aPc_m_filtered$inla))
pdf(paste(output_dir,"2-2-1 lincomb_age_unacc_vs_acc_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = acc_vs_unacc_m[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"2-2-2 lincomb_cohort_unacc_vs_acc_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = acc_vs_unacc_m[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()

all_vs_fil_m = plot_lincombs_joint(list("All" = res_survey_aPc_m$inla, "Fil." = res_survey_aPc_m_filtered$inla), override_maxmin = T)
pdf(paste(output_dir,"3-2-1 lincomb_age_all_vs_fil_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = all_vs_fil_m[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"3-2-2 lincomb_cohort_all_vs_fil_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = all_vs_fil_m[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()

fil_only_m = plot_lincombs_joint(list("Fil" = res_survey_aPc_m_filtered$inla))
pdf(paste(output_dir,"4-2-1 lincomb_age_fil_only_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = fil_only_m[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"4-2-2 lincomb_cohort_fil_only_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = fil_only_m[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()





#######################
#Lincombs entire population
#######################

#Female
acc_vs_unacc_f = plot_lincombs_joint(list("Unacc." = res_expert_f_aPc$inla, "Acc." = res_survey_aPc_f$inla))
pdf(paste(output_dir,"10-1-1 lincomb_age_unacc_vs_acc_all_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = acc_vs_unacc_f[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"10-1-2 lincomb_cohort_unacc_vs_acc_all_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = acc_vs_unacc_f[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()

fil_only_f = plot_lincombs_joint(list("Fil" = res_survey_aPc_f$inla), override_maxmin = T, set = 2)
pdf(paste(output_dir,"11-1-1 lincomb_age_all_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = fil_only_f[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"11-1-2 lincomb_cohort_all_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = fil_only_f[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()


#Male
acc_vs_unacc_m = plot_lincombs_joint(list("Unacc." = res_expert_m_aPc$inla, "Acc." = res_survey_aPc_m$inla))
pdf(paste(output_dir,"10-2-1 lincomb_age_unacc_vs_acc_all_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = acc_vs_unacc_m[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"10-2-2 lincomb_cohort_unacc_vs_acc_all_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = acc_vs_unacc_m[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()

fil_only_m = plot_lincombs_joint(list("Fil" = res_survey_aPc_m$inla), override_maxmin = T, set = 2)
pdf(paste(output_dir,"11-2-1 lincomb_age_all_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = fil_only_m[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"11-2-2 lincomb_cohort_all_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = fil_only_m[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()


#####################
#Prior sensitivity
####################
expert_res_f = extract_res_inla(res_expert_f_aPc$inla)
anti_res_f = extract_res_inla(res_anti_f$inla)
dir_res_f = extract_res_inla(res_dir_f$inla)
comp_res_f = extract_res_inla(res_CS_aPc_f)
semi_res_f = extract_res_inla(res_semi_f$inla)

compare_plots_f = compare_posterior_prior(prior_dir_at_split = c(T,T,T,T), prior_list = list(Dirichlet = res_dir_f$prior, EK = res_expert_f_aPc$prior, "Anti-EK" = res_anti_f$prior), 
                                          posterior_list = list(EK = expert_res_f, "Anti-EK" = anti_res_f, Dirichlet = dir_res_f, CS = comp_res_f), dir_samp = 1000000, no_y_label = T,
                                          hide_y_ticks = F, dir_to_blind = F, posterior_left = c(F,F,F,T))

pdf(paste(output_dir,"8 compare_plots_f.pdf", sep = ""), width = 12, height = 8)
ggarrange(plotlist = compare_plots_f[c(3,5,7,4,6,8)], ncol = 3, nrow = 2)
dev.off()


###################
# Scatterplot
##################

expit2 = function(x){
  return(exp(x)/(1+exp(x)))
}

#Empirical probabilities
survey_data_f_filter$fitted_values_nodesign = res_expert_f_aPc_filter$inla$summary.fitted.values$`0.5quant`
survey_data_f_filter$fitted_values_design = res_survey_aPc_f_filtered$inla$summary.fitted.values$`0.5quant`
survey_data_f_filter$fitted_values_design = expit(survey_data_f_filter$fitted_values_design)

survey_data_f_filter$var_design = NA
for (i in 1:5280) {
  temp = inla.tmarginal(fun = expit2, marginal = res_survey_aPc_f_filtered$inla$marginals.linear.predictor[[i]])
  temp2 = inla.zmarginal(temp, silent =T )
  survey_data_f_filter$var_design[i] = temp2$sd
}
survey_data_f_filter$var_design = survey_data_f_filter$var_design**2
survey_data_f_filter$education = as.factor(survey_data_f_filter$education)
survey_data_f_filter$var_nodesign = res_expert_f_aPc_filter$inla$summary.fitted.values$sd**2

pdf(paste(output_dir,"5-1-1 scatter_phat_f.pdf", sep = ""), width = 8, height = 8)
ggplot(data = survey_data_f_filter, aes(x=fitted_values_nodesign, y = fitted_values_design, colour = education)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype ="dashed") +
  scale_color_manual(values = palette.colors(palette = "Classic Tableau", n = 4), labels = c("LHS/GED","HS","SC/AA","BA+")) + 
  xlim(c(0.15,0.65)) + ylim(c(0.15,0.65)) + theme_bw() +
  xlab("Posterior median (unadjusted)") + 
  ylab("Posterior median (adjusted)") + 
  guides(color=guide_legend(title="Education")) +
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_blank(),
        legend.position = c(0.20, 0.80),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*20),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*20),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20))
dev.off()


pdf(paste(output_dir,"5-1-2 scatter_var_f.pdf", sep = ""), width = 9, height = 8)
ggplot(data = survey_data_f_filter, aes(x=var_nodesign, y = var_design, colour = education)) + 
  geom_abline(intercept = 0, slope = 1, linetype ="dashed") +
  geom_point() + 
  scale_color_manual(values = palette.colors(palette = "Classic Tableau", n = 4), labels = c("LHS/GED","HS","SC/AA","BA+")) + 
  theme_bw() +
  xlab("Posterior variance (unadjusted)") + 
  ylab("Posterior variance (adjusted)") + 
  guides(color=guide_legend(title="Education")) +
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_blank(),
        legend.position = c(0.20, 0.80),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*20),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*13),
        axis.text.y = element_text(size = fac*13),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(1e-4,1e-3)) +
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(1e-4,1e-3))
dev.off()

survey_data_m_filter$fitted_values_nodesign = res_expert_m_aPc_filter$inla$summary.fitted.values$`0.5quant`
survey_data_m_filter$fitted_values_design = res_survey_aPc_m_filtered$inla$summary.fitted.values$`0.5quant`
survey_data_m_filter$fitted_values_design = expit(survey_data_m_filter$fitted_values_design)

survey_data_m_filter$var_design = NA
for (i in 1:5280) {
  temp = inla.tmarginal(fun = expit2, marginal = res_survey_aPc_m_filtered$inla$marginals.linear.predictor[[i]])
  temp2 = inla.zmarginal(temp, silent = T)
  survey_data_m_filter$var_design[i] = temp2$sd
}
survey_data_m_filter$var_design = survey_data_m_filter$var_design**2
survey_data_m_filter$education = as.factor(survey_data_m_filter$education)
survey_data_m_filter$var_nodesign = res_expert_m_aPc_filter$inla$summary.fitted.values$sd**2

pdf(paste(output_dir,"5-2-1 scatter_phat_m.pdf", sep = ""), width = 8, height = 8)
ggplot(data = survey_data_m_filter, aes(x=fitted_values_nodesign, y = fitted_values_design, colour = education)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype ="dashed") +
  scale_color_manual(values = palette.colors(palette = "Classic Tableau", n = 4), labels = c("LHS/GED","HS","SC/AA","BA+")) + 
  xlim(c(0.15,0.65)) + ylim(c(0.15,0.65)) + theme_bw() +
  xlab("Posterior median (unadjusted)") + 
  ylab("Posterior median (adjusted)") + 
  guides(color=guide_legend(title="Education")) +
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_blank(),
        legend.position = c(0.20, 0.80),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*20),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*20),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20))
dev.off()

pdf(paste(output_dir,"5-2-2 scatter_var_m.pdf", sep = ""), width = 8, height = 8)
ggplot(data = survey_data_m_filter, aes(x=var_nodesign, y = var_design, colour = education)) + 
  geom_abline(intercept = 0, slope = 1, linetype ="dashed") +
  geom_point() + 
  scale_color_manual(values = palette.colors(palette = "Classic Tableau", n = 4), labels = c("LHS/GED","HS","SC/AA","BA+")) + 
  theme_bw() +
  xlab("Posterior variance (unadjusted)") + 
  ylab("Posterior variance (adjusted)") + 
  guides(color=guide_legend(title="Education")) +
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_blank(),
        legend.position = c(0.20, 0.80),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*20),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*13),
        axis.text.y = element_text(size = fac*13),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  scale_y_continuous(labels = function(x) x, limits = c(5e-5,2.5e-3)) +
  scale_x_continuous(labels = function(x) x, limits = c(5e-5,2.5e-3))
dev.off()

###################
# Scatterplot all data
##################

#Empirical probabilities
survey_data_f$fitted_values_nodesign = res_expert_f_aPc$inla$summary.fitted.values$`0.5quant`
survey_data_f$fitted_values_design = res_survey_aPc_f$inla$summary.fitted.values$`0.5quant`
survey_data_f$fitted_values_design = expit(survey_data_f$fitted_values_design)

survey_data_f$var_design = NA
for (i in 1:5280) {
  temp = inla.tmarginal(fun = expit2, marginal = res_survey_aPc_f$inla$marginals.linear.predictor[[i]])
  temp2 = inla.zmarginal(temp, silent =T )
  survey_data_f$var_design[i] = temp2$sd
}
survey_data_f$var_design = survey_data_f$var_design**2
survey_data_f$education = as.factor(survey_data_f$education)
survey_data_f$var_nodesign = res_expert_f_aPc$inla$summary.fitted.values$sd**2

pdf(paste(output_dir,"12-1-1 scatter_phat_f.pdf", sep = ""), width = 8, height = 8)
ggplot(data = survey_data_f, aes(x=fitted_values_nodesign, y = fitted_values_design, colour = education)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype ="dashed") +
  scale_color_manual(values = palette.colors(palette = "Classic Tableau", n = 4), labels = c("LHS/GED","HS","SC/AA","BA+")) + 
  xlim(c(0.15,0.65)) + ylim(c(0.15,0.65)) + theme_bw() +
  xlab("Posterior median (unadjusted)") + 
  ylab("Posterior median (adjusted)") + 
  guides(color=guide_legend(title="Education")) +
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_blank(),
        legend.position = c(0.20, 0.80),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*20),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*20),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20))
dev.off()


pdf(paste(output_dir,"12-1-2 scatter_var_f.pdf", sep = ""), width = 9, height = 8)
ggplot(data = survey_data_f, aes(x=var_nodesign, y = var_design, colour = education)) + 
  geom_abline(intercept = 0, slope = 1, linetype ="dashed") +
  geom_point() + 
  scale_color_manual(values = palette.colors(palette = "Classic Tableau", n = 4), labels = c("LHS/GED","HS","SC/AA","BA+")) + 
  theme_bw() +
  xlab("Posterior variance (unadjusted)") + 
  ylab("Posterior variance (adjusted)") + 
  guides(color=guide_legend(title="Education")) +
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_blank(),
        legend.position = c(0.80, 0.20),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*20),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*13),
        axis.text.y = element_text(size = fac*13),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  scale_y_continuous(labels = function(x) x, limits = c(1e-5,0.00125)) +
  scale_x_continuous(labels = function(x) x, limits = c(1e-5,0.00125))
dev.off()

survey_data_m$fitted_values_nodesign = res_expert_m_aPc$inla$summary.fitted.values$`0.5quant`
survey_data_m$fitted_values_design = res_survey_aPc_m$inla$summary.fitted.values$`0.5quant`
survey_data_m$fitted_values_design = expit(survey_data_m$fitted_values_design)

survey_data_m$var_design = NA
for (i in 1:5280) {
  temp = inla.tmarginal(fun = expit2, marginal = res_survey_aPc_m$inla$marginals.linear.predictor[[i]])
  temp2 = inla.zmarginal(temp, silent = T)
  survey_data_m$var_design[i] = temp2$sd
}
survey_data_m$var_design = survey_data_m$var_design**2
survey_data_m$education = as.factor(survey_data_m$education)
survey_data_m$var_nodesign = res_expert_m_aPc$inla$summary.fitted.values$sd**2

pdf(paste(output_dir,"12-2-1 scatter_phat_m.pdf", sep = ""), width = 8, height = 8)
ggplot(data = survey_data_m, aes(x=fitted_values_nodesign, y = fitted_values_design, colour = education)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, linetype ="dashed") +
  scale_color_manual(values = palette.colors(palette = "Classic Tableau", n = 4), labels = c("LHS/GED","HS","SC/AA","BA+")) + 
  xlim(c(0.1,0.6)) + ylim(c(0.1,0.6)) + theme_bw() +
  xlab("Posterior median (unadjusted)") + 
  ylab("Posterior median (adjusted)") + 
  guides(color=guide_legend(title="Education")) +
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_blank(),
        legend.position = c(0.20, 0.80),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*20),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*20),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20))
dev.off()

pdf(paste(output_dir,"12-2-2 scatter_var_m.pdf", sep = ""), width = 8, height = 8)
ggplot(data = survey_data_m_filter, aes(x=var_nodesign, y = var_design, colour = education)) + 
  geom_abline(intercept = 0, slope = 1, linetype ="dashed") +
  geom_point() + 
  scale_color_manual(values = palette.colors(palette = "Classic Tableau", n = 4), labels = c("LHS/GED","HS","SC/AA","BA+")) + 
  theme_bw() +
  xlab("Posterior variance (unadjusted)") + 
  ylab("Posterior variance (adjusted)") + 
  guides(color=guide_legend(title="Education")) +
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_blank(),
        legend.position = c(0.80, 0.20),
        legend.title = element_blank(),
        axis.title.x = element_text(size = fac*20),
        axis.title.y = element_text(size = fac*20),
        axis.text.x = element_text(size = fac*13),
        axis.text.y = element_text(size = fac*13),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  scale_y_continuous(labels = function(x) x, limits = c(5e-5,2.5e-3)) +
  scale_x_continuous(labels = function(x) x, limits = c(5e-5,2.5e-3))
dev.off()

