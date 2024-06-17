source("plotting functions.R")
# SET CORRECT WORKING DIRECTORY
# setwd("C:/Users/marku/Desktop/Masteroppgave/Masteroppgave/Code"") #For my windows pc
#setwd("/Users/MarkusTraetli/Desktop/Masteroppgave/Code") #MAKE SURE THIS IS CORRECT

LaTeX_build = F #Change to F if not compiling for latex
if(LaTeX_build){
  output_dir = "../LaTeX/Figures/"
}else{
  output_dir = "./Figures/"
}
#Load data
load("./Data/processed_data.RData")

##############################
# Explorative analysis
#############################

########################
#Count plots
fac = 1.3
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
  annotate(geom="text", x=75, y=2250, label="Females", size = 25/.pt) +
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
  annotate(geom="text", x=2000, y=5500, label="Females", size = 25/.pt) +
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
  annotate(geom="text", x=1925, y=2350, label="Females", size = 25/.pt) +
  scale_x_continuous(labels = c("1920","1940","1960","1980"), breaks = c(1920,1940,1960,1980)) +
  xlab("Birth cohort") + guides(color = "none") + 
  ylab("Number") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pdf(paste(output_dir,"number_prop_legend_m.pdf", sep = ""), width = 3, height = 5)
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
  annotate(geom="text", x=75, y=2250, label="Males", size = 25/.pt) +
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
  annotate(geom="text", x=2000, y=5500, label="Males", size = 25/.pt) +
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
  annotate(geom="text", x=1925, y=2350, label="Males", size = 25/.pt) +
  xlab("Birth cohort") + guides(color = "none") + 
  ylab("Number") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))



##########################
#Proportions

#Female
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
  annotate(geom="text", x=35, y=0.5, label="Females", size = 25/.pt) +
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
  annotate(geom="text", x=2000, y=0.5, label="Females", size = 25/.pt) +
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
  ylab("Rate") +   annotate(geom="text", x=1980, y=0.5, label="Females", size = 25/.pt) +
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
  annotate(geom="text", x=35, y=0.5, label="Males", size = 25/.pt) +
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
  annotate(geom="text", x=2000, y=0.5, label="Males", size = 25/.pt) +
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
  annotate(geom="text", x=1980, y=0.5, label="Males", size = 25/.pt) +
  ylab("Rate") +   scale_x_continuous(labels = c("1920","1940","1960","1980"), breaks = c(1920,1940,1960,1980)) +
  xlab("Birth cohort") +guides(color = "none") + 
  scale_color_manual(labels = c("LHS/GED","HS","SC/AA","BA+"), values = palette.colors(palette = "Classic Tableau"))

pdf(paste(output_dir,"numbers.pdf", sep = ""), width = 12, height = 14)
ggarrange(plotlist = list(p_a_f, p_a_m, p_p_f, p_p_m, p_c_f, p_c_m), nrow = 3, ncol = 2)
dev.off()

pdf(paste(output_dir,"proportions.pdf", sep = ""), width = 12, height = 14)
ggarrange(plotlist = list(pp_a_f, pp_a_m, pp_p_f, pp_p_m, pp_c_f, pp_c_m), nrow = 3, ncol = 2)
dev.off()



#######################
#Rateplots

rateplot_options = theme(strip.text.x = element_blank(),
                         legend.background = element_rect(fill = "transparent"),
                         legend.key = element_rect(color = "black"),
                         legend.title = element_blank(),
                         axis.title.x = element_text(size = fac*24),
                         axis.title.y = element_text(size = fac*20),
                         axis.text = element_text(size = fac*20),
                         axis.text.y = element_text(size = fac*20),
                         axis.text.x = element_text(size = fac*20*0.95),
                         legend.text = element_text(size = fac*20),
                         plot.title=element_text(hjust = 0.5,margin=margin(t=40,b=-30), size = fac*20))

#Age groups ovr periods
n_groups = 6

#Female
bp_rate_f = as.data.frame(bp_ind_f%>%dplyr::mutate(ints = cut(age, breaks = n_groups))%>%dplyr::group_by(ints, period, education)%>%dplyr::summarize(rate = mean(backpain)))
bp_rate_f$ints = as.character(bp_rate_f$ints)
for (i in 1:n_groups) {
  bp_rate_f$ints[bp_rate_f$ints == unique(bp_rate_f$ints)[i]] = i
}
bp_rate_f$period = bp_rate_f$period + 1996


bp_rate_f_e1 = bp_rate_f[bp_rate_f$education == 1,c(1,2,4)]
bp_rate_f_e2 = bp_rate_f[bp_rate_f$education == 2,c(1,2,4)]
bp_rate_f_e3 = bp_rate_f[bp_rate_f$education == 3,c(1,2,4)]
bp_rate_f_e4 = bp_rate_f[bp_rate_f$education == 4,c(1,2,4)]

temp_plot = ggplot(bp_rate_f_e1, aes(x = period, y = rate, color = ints)) + 
  geom_line(size = 10) + theme_bw() + ggtitle("LHS") + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.title = element_text(size = fac*20),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  ylim(c(0.15,0.55)) +
  ylab("Rate") + 
  scale_color_manual(labels = c("25-34","35-44","45-54","55-64","65-74","75-84"), values = palette.colors(palette = "Classic Tableau")) +
  guides(color=guide_legend(title="Age group"))

p1 = ggplot(bp_rate_f_e1, aes(x = period, y = rate, color = ints)) + 
  geom_line(size = 1.3) + theme_bw() + ggtitle("LHS/GED") + 
  rateplot_options +
  ylab("Rate") + 
  ylim(c(0.15,0.55)) + guides(color="none") + xlab("Period") +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  scale_color_manual(labels = c("25-34","35-44","45-54","55-64","65-74","75-84"), values = palette.colors(palette = "Classic Tableau")) 

p2 = ggplot(bp_rate_f_e2, aes(x = period, y = rate, color = ints)) + 
  geom_line(size = 1.3) + theme_bw() + ggtitle("HS") + 
  rateplot_options +
  ylab("Rate") + 
  ylim(c(0.15,0.55))  + guides(color="none") + xlab("Period") +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  scale_color_manual(labels = c("25-34","35-44","45-54","55-64","65-74","75-84"), values = palette.colors(palette = "Classic Tableau")) 
p3 = ggplot(bp_rate_f_e3, aes(x = period, y = rate, color = ints)) + 
  geom_line(size = 1.3) + theme_bw() + ggtitle("SC/AA")+ 
  rateplot_options+
  ylab("Rate") + 
  ylim(c(0.15,0.55)) +  guides(color="none") + xlab("Period") +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  scale_color_manual(labels = c("25-34","35-44","45-54","55-64","65-74","75-84"), values = palette.colors(palette = "Classic Tableau")) 
p4 = ggplot(bp_rate_f_e4, aes(x = period, y = rate, color = ints)) + 
  geom_line(size = 1.3) + theme_bw() + ggtitle("BA+")+ 
  rateplot_options+
  ylab("Rate") + 
  ylim(c(0.15,0.55)) +  guides(color="none") + xlab("Period") + 
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  scale_color_manual(labels = c("25-34","35-44","45-54","55-64","65-74","75-84"), values = palette.colors(palette = "Classic Tableau")) 

pdf(paste(output_dir,"rateplot_age_f.pdf", sep = ""), width = 12, height = 10)
ggarrange(plotlist = list(p1,p2,p3,p4), nrow = 2, ncol = 2)
dev.off()

pdf(paste(output_dir,"rateplot_age_legend_f.pdf", sep = ""), width = 3, height = 7)
as_ggplot(get_legend(temp_plot))
dev.off()

#Male
bp_rate_m = as.data.frame(bp_ind_m%>%dplyr::mutate(ints = cut(age, breaks = n_groups))%>%dplyr::group_by(ints, period, education)%>%dplyr::summarize(rate = mean(backpain)))
bp_rate_m$ints = as.character(bp_rate_m$ints)
for (i in 1:n_groups) {
  bp_rate_m$ints[bp_rate_m$ints == unique(bp_rate_m$ints)[i]] = i
}

bp_rate_m$period = bp_rate_m$period + 1996

bp_rate_m_e1 = bp_rate_m[bp_rate_m$education == 1,c(1,2,4)]
bp_rate_m_e2 = bp_rate_m[bp_rate_m$education == 2,c(1,2,4)]
bp_rate_m_e3 = bp_rate_m[bp_rate_m$education == 3,c(1,2,4)]
bp_rate_m_e4 = bp_rate_m[bp_rate_m$education == 4,c(1,2,4)]

temp_plot = ggplot(bp_rate_m_e1, aes(x = period, y = rate, color = ints)) + 
  geom_line(size = 10) + theme_bw() + ggtitle("LHS") + 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.title = element_text(size = fac*20),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) +
  ylim(c(0.15,0.55)) +
  ylab("Rate") + 
  scale_color_manual(labels = c("25-34","35-44","45-54","55-64","65-74","75-84"), values = palette.colors(palette = "Classic Tableau")) +
  guides(color=guide_legend(title="Age group"))


p1 = ggplot(bp_rate_m_e1, aes(x = period, y = rate, color = ints)) + 
  geom_line(size = 1.3) + theme_bw() + ggtitle("LHS/GED") + 
  rateplot_options +
  ylab("Rate") + 
  ylim(c(0.07,0.55)) + guides(color="none") + xlab("Period") +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  scale_color_manual(labels = c("25-34","35-44","45-54","55-64","65-74","75-84"), values = palette.colors(palette = "Classic Tableau")) 

p2 = ggplot(bp_rate_m_e2, aes(x = period, y = rate, color = ints)) + 
  geom_line(size = 1.3) + theme_bw() + ggtitle("HS") + 
  rateplot_options +ylab("Rate") + 
  ylim(c(0.07,0.55))  + guides(color="none") + xlab("Period") +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  scale_color_manual(labels = c("25-34","35-44","45-54","55-64","65-74","75-84"), values = palette.colors(palette = "Classic Tableau")) 
p3 = ggplot(bp_rate_m_e3, aes(x = period, y = rate, color = ints)) + 
  geom_line(size = 1.3) + theme_bw() + ggtitle("SC/AA")+ 
  rateplot_options+ylab("Rate") + 
  ylim(c(0.07,0.55)) +  guides(color="none") + xlab("Period") +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  scale_color_manual(labels = c("25-34","35-44","45-54","55-64","65-74","75-84"), values = palette.colors(palette = "Classic Tableau")) 
p4 = ggplot(bp_rate_m_e4, aes(x = period, y = rate, color = ints)) + 
  geom_line(size = 1.3) + theme_bw() + ggtitle("BA+")+ 
  rateplot_options+ylab("Rate") + 
  ylim(c(0.07,0.55)) +  guides(color="none") + xlab("Period") +
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  scale_color_manual(labels = c("25-34","35-44","45-54","55-64","65-74","75-84"), values = palette.colors(palette = "Classic Tableau")) 

pdf(paste(output_dir,"rateplot_age_m.pdf", sep = ""), width = 12, height = 10)
ggarrange(plotlist = list(p1,p2,p3,p4), nrow = 2, ncol = 2)
dev.off()

pdf(paste(output_dir,"rateplot_age_legend_m.pdf", sep = ""), width = 3, height = 7)
as_ggplot(get_legend(temp_plot))
dev.off()

#Cohort groups over period

#Female
n_groups = 5
bp_rate_f = as.data.frame(bp_ind_f%>%dplyr::mutate(ints = cut(cohort, breaks = c(0,16,33,52,68,82)))%>%dplyr::group_by(ints, period, education)%>%dplyr::summarize(rate = mean(backpain)))
bp_rate_f$ints = as.character(bp_rate_f$ints)
for (i in 1:n_groups) {
  bp_rate_f$ints[bp_rate_f$ints == unique(bp_rate_f$ints)[i]] = i
}
bp_rate_f$period = bp_rate_f$period + 1996


bp_rate_f_e1 = bp_rate_f[bp_rate_f$education == 1,c(1,2,4)]
bp_rate_f_e2 = bp_rate_f[bp_rate_f$education == 2,c(1,2,4)]
bp_rate_f_e3 = bp_rate_f[bp_rate_f$education == 3,c(1,2,4)]
bp_rate_f_e4 = bp_rate_f[bp_rate_f$education == 4,c(1,2,4)]

temp_plot = ggplot(bp_rate_f_e1, aes(x = period, y = rate, color = ints)) + geom_line(size = 10) + theme_bw()+ ggtitle("LHS")+ 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.title = element_text(size = fac*20),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) + xlab("Period") + ylab("Rate") + 
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  ylim(c(0.15,0.55)) + scale_color_manual(labels = c("1913-1927","1928-1945","1946-1964","1965-1980","1981-1993"), values = palette.colors(palette = "Classic Tableau")) +
  guides(color=guide_legend(title="Birth cohorts"))

p1 = ggplot(bp_rate_f_e1, aes(x = period, y = rate, color = ints)) + geom_line(size = 1.3) + theme_bw()+ ggtitle("LHS")+ 
  rateplot_options +  guides(color="none") + xlab("Period") + ylab("Rate") + 
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  ylim(c(0.15,0.55)) +scale_color_manual(labels = c("1913-1927","1928-1945","1946-1964","1965-1980","1981-1993"), values = palette.colors(palette = "Classic Tableau"))
p2 = ggplot(bp_rate_f_e2, aes(x = period, y = rate, color = ints)) + geom_line(size = 1.3) + theme_bw()+ ggtitle("HS")+ 
  rateplot_options +guides(color="none") + xlab("Period") + ylab("Rate") + 
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  ylim(c(0.15,0.55)) + scale_color_manual(labels = c("1913-1927","1928-1945","1946-1964","1965-1980","1981-1993"), values = palette.colors(palette = "Classic Tableau"))
p3 = ggplot(bp_rate_f_e3, aes(x = period, y = rate, color = ints)) + geom_line(size = 1.3) + theme_bw()+ ggtitle("SC/AA")+ 
  rateplot_options +guides(color="none") + xlab("Period") + ylab("Rate") + 
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  ylim(c(0.15,0.55)) + scale_color_manual(labels = c("1913-1927","1928-1945","1946-1964","1965-1980","1981-1993"), values = palette.colors(palette = "Classic Tableau"))
p4 = ggplot(bp_rate_f_e4, aes(x = period, y = rate, color = ints)) + geom_line(size = 1.3) + theme_bw() + ggtitle("BA+")+ 
  rateplot_options + guides(color="none") + xlab("Period") + ylab("Rate") + 
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  ylim(c(0.15,0.55)) +scale_color_manual(labels = c("1913-1927","1928-1945","1946-1964","1965-1980","1981-1993"), values = palette.colors(palette = "Classic Tableau"))

pdf(paste(output_dir,"rateplot_cohort_f.pdf", sep = ""), width = 12, height = 10)
ggarrange(plotlist = list(p1,p2,p3,p4), nrow = 2, ncol = 2)
dev.off()

pdf(paste(output_dir,"rateplot_cohort_legend_f.pdf", sep = ""), width = 3, height = 7)
as_ggplot(get_legend(temp_plot))
dev.off()

#Male
n_groups = 5
bp_rate_m = as.data.frame(bp_ind_m%>%dplyr::mutate(ints = cut(cohort, breaks = c(0,16,33,52,68,82)))%>%dplyr::group_by(ints, period, education)%>%dplyr::summarize(rate = mean(backpain)))
bp_rate_m$ints = as.character(bp_rate_m$ints)
for (i in 1:n_groups) {
  bp_rate_m$ints[bp_rate_m$ints == unique(bp_rate_m$ints)[i]] = i
}
bp_rate_m$period = bp_rate_m$period + 1996


bp_rate_m_e1 = bp_rate_m[bp_rate_m$education == 1,c(1,2,4)]
bp_rate_m_e2 = bp_rate_m[bp_rate_m$education == 2,c(1,2,4)]
bp_rate_m_e3 = bp_rate_m[bp_rate_m$education == 3,c(1,2,4)]
bp_rate_m_e4 = bp_rate_m[bp_rate_m$education == 4,c(1,2,4)]

temp_plot = ggplot(bp_rate_m_e1, aes(x = period, y = rate, color = ints)) + geom_line(size = 10) + theme_bw()+ ggtitle("LHS")+ 
  theme(strip.text.x = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(color = "black"),
        legend.title = element_text(size = fac*20),
        axis.title.x = element_text(size = fac*24),
        axis.title.y = element_text(size = fac*20),
        axis.text = element_text(size = fac*20),
        axis.text.y = element_text(size = fac*15),
        legend.text = element_text(size = fac*20),
        plot.title = element_text(hjust = 0.5, size = fac*20)) + xlab("Period") + ylab("Rate") + 
  scale_x_continuous(labels = c("00","05","10","15"), breaks = c(2000,2005,2010,2015)) +
  ylim(c(0.07,0.55)) + scale_color_manual(labels = c("1913-1927","1928-1945","1946-1964","1965-1980","1981-1993"), values = palette.colors(palette = "Classic Tableau")) +
  guides(color=guide_legend(title="Birth cohorts"))

p1 = ggplot(bp_rate_m_e1, aes(x = period, y = rate, color = ints)) + geom_line(size = 1.3) + theme_bw()+ ggtitle("LHS")+ 
  rateplot_options +  guides(color="none") + xlab("Period") + ylab("Rate") + 
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  ylim(c(0.07,0.55)) +scale_color_manual(labels = c("1913-1927","1928-1945","1946-1964","1965-1980","1981-1993"), values = palette.colors(palette = "Classic Tableau"))
p2 = ggplot(bp_rate_m_e2, aes(x = period, y = rate, color = ints)) + geom_line(size = 1.3) + theme_bw()+ ggtitle("HS")+ 
  rateplot_options +guides(color="none") + xlab("Period") + ylab("Rate") + 
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  ylim(c(0.07,0.55)) + scale_color_manual(labels = c("1913-1927","1928-1945","1946-1964","1965-1980","1981-1993"), values = palette.colors(palette = "Classic Tableau"))
p3 = ggplot(bp_rate_m_e3, aes(x = period, y = rate, color = ints)) + geom_line(size = 1.3) + theme_bw()+ ggtitle("SC/AA")+ 
  rateplot_options +guides(color="none") + xlab("Period") + ylab("Rate") + 
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  ylim(c(0.07,0.55)) + scale_color_manual(labels = c("1913-1927","1928-1945","1946-1964","1965-1980","1981-1993"), values = palette.colors(palette = "Classic Tableau"))
p4 = ggplot(bp_rate_m_e4, aes(x = period, y = rate, color = ints)) + geom_line(size = 1.3) + theme_bw() + ggtitle("BA+")+ 
  rateplot_options + guides(color="none") + xlab("Period") + ylab("Rate") + 
  scale_x_continuous(labels = c("2000","2005","2010","2015"), breaks = c(2000,2005,2010,2015)) +
  ylim(c(0.07,0.55)) +scale_color_manual(labels = c("1913-1927","1928-1945","1946-1964","1965-1980","1981-1993"), values = palette.colors(palette = "Classic Tableau"))

pdf(paste(output_dir,"rateplot_cohort_m.pdf", sep = ""), width = 12, height = 10)
ggarrange(plotlist = list(p1,p2,p3,p4), nrow = 2, ncol = 2)
dev.off()

pdf(paste(output_dir,"rateplot_cohort_legend_m.pdf", sep = ""), width = 3, height = 7)
as_ggplot(get_legend(temp_plot))
dev.off()





##############################################
# Priors
##############################################

load("./Data/joint_models.RData") #Load the join models

break_pos = list("1" = NA,
                 "2" = c(0,0.15,0.80, 1),
                 "3" = c(0,0.22,0.9, 1),
                 "4" = c(0,0.1,0.9, 1))

x_labels = list("1" = NA,
                "2" = c("","Purely\nstructured","Purely\nunstructured",""),
                "3" = c("","Purely\nAge & Cohort","Purely\nPeriod",""),
                "4" = c("","Purely\nCohort","Purely\nAge",""))

#Dirichlet prior
prior_data = as.data.frame(plot_prior(res_dir_f$prior)$data)

# Plot of the priors used full model
prior_data_V = prior_data[1:201,]
prior_data_s1 = prior_data[202:302,]
colnames(prior_data_s1) = c("rho", "Density", "param")
colnames(prior_data_V) = c("V", "Density", "param")
fac = 1.3

#Gamma and phi are identical, plot only one
pdf(paste(output_dir,"prior_dir4.pdf", sep = ""), width = 5, height = 5)
print(ggplot(prior_data_s1, aes(x = rho, y = Density)) +
        geom_path() +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              legend.background = element_rect(fill = "transparent"),
              legend.key = element_rect(colour = "black"),
              legend.title = element_blank(),
              axis.title.x = element_text(size = fac*24),
              axis.title.y = element_text(size = fac*15),
              axis.text = element_text(size = fac*20),
              axis.text.y = element_text(size = fac*15),
              legend.text = element_text(size = fac*20),
              plot.title = element_text(hjust = 0.5, size = fac*20),
              axis.ticks.x = element_line(colour = c("black",NA,NA,NA,"black")))  +
        scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c("0","","","","1")) + 
        xlab(latex2exp::TeX("$\\rho$"))) 
dev.off()

pdf(paste(output_dir,"prior_V.pdf", sep = ""), width = 5, height = 5)
print(ggplot(prior_data_V, aes(x = V, y = Density)) +
        geom_path() +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              legend.background = element_rect(fill = "transparent"),
              legend.key = element_rect(colour = "black"),
              legend.title = element_blank(),
              axis.title.x = element_text(size = fac*24),
              axis.title.y = element_text(size = fac*15),
              axis.text = element_text(size = fac*20),
              axis.text.y = element_text(size = fac*15),
              legend.text = element_text(size = fac*20),
              plot.title = element_text(hjust = 0.5, size = fac*20))  +
        xlab(latex2exp::TeX("$\\sigma$")) + xlim(c(0,5)))
dev.off()

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

pdf(paste(output_dir,"prior_rest.pdf", sep = ""), width = 12, height = 4)
ggarrange(plotlist = list(p1 = p1, p2 = p2, p3 = p3), nrow = 1, ncol = 3)
dev.off()

prior_data = as.data.frame(plot_prior(res_anti_f$prior)$data)
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

pdf(paste(output_dir,"prior_rest_anti.pdf", sep = ""), width = 12, height = 4)
ggarrange(plotlist = list(p1 = p1, p2 = p2, p3 = p3), nrow = 1, ncol = 3)
dev.off()

#Visualizations of the prior tree in MMP. Equivalent to figures 5.2a, 5.2b and 5.3a. Difficult to save so its not run but have a look to make sure the priors look correct!
if(F){ 
  prior_expert = makePriorAPC(bp_data_f, "n_backpain", prior_mode = "EK")$prior #can be used to load prior
  prior_anti = makePriorAPC(bp_data_f, "n_backpain", prior_mode = "Anti-EK")$prior
  prior_dir = makePriorAPC(bp_data_f, "n_backpain", prior_mode = "Dirichlet")$prior
  x_axis = seq(from = 0, to = 10, by = 1000)
  
  #EK prior
  plot_tree_structure(prior_expert, nodenames = list(
    random_period_age_new_cohort_new = "Observed standard deviation",
    period_age_new_cohort_new = "Structured",
    age_new_cohort_new = "Age & Cohort",
    cohort_new = "Cohort",
    age_new = "Age",
    period = "Period",
    random = "Unstructured"
  ))
  
  #Anti-EK prior
  plot_tree_structure(prior_anti, nodenames = list(
    random_period_age_new_cohort_new = "Observed standard deviation",
    period_age_new_cohort_new = "Structured",
    age_new_cohort_new = "Age & Cohort",
    cohort_new = "Cohort",
    period = "Period",
    age_new = "Age",
    random = "Unstructured"
  ))
  
  #Dirichlet prior
  plot_tree_structure(prior_dir, nodenames = list(
    age_new_period_cohort_new_random = "Observed standard deviation",
    cohort_new = "Cohort",
    age_new = "Age",
    period = "Period",
    random = "Unstructured"
  ))
}
###################################################
# Prior & posterior plots
#################################################

#These models are required for these parts
load("./Data/CS_models.RData")
load("./Data/individual_models.RData")

# Plots that compare the linear combination effects of the models
expert_res_f = extract_res_inla(res_expert_f_aPc$inla)
anti_res_f = extract_res_inla(res_anti_f$inla)
dir_res_f = extract_res_inla(res_dir_f$inla)
comp_res_f = extract_res_inla(res_CS_aPc_f)
semi_res_f = extract_res_inla(res_semi_f$inla)

expert_res_m = extract_res_inla(res_expert_m_aPc$inla)
anti_res_m = extract_res_inla(res_anti_m$inla)
dir_res_m = extract_res_inla(res_dir_m$inla)
comp_res_m = extract_res_inla(res_CS_aPc_m)
semi_res_m = extract_res_inla(res_semi_m$inla)

ind_res_f = extract_res_inla(res_expert_indi_f$inla) #For later plots
ind_res_m = extract_res_inla(res_expert_indi_m$inla)

# Plots that compare the prior and posteriors side by side
compare_plots_f = compare_posterior_prior(prior_dir_at_split = c(T,T,T,T), prior_list = list(Dirichlet = res_dir_f$prior, EK = res_expert_f_aPc$prior, "Anti-EK" = res_anti_f$prior), 
                                        posterior_list = list(EK = expert_res_f, "Anti-EK" = anti_res_f, Dirichlet = dir_res_f, CS = comp_res_f), dir_samp = 1000000, no_y_label = T,
                                        hide_y_ticks = F, dir_to_blind = F, posterior_left = c(F,F,F,T))

pdf(paste(output_dir,"compare_plots_s0_f.pdf", sep = ""), width = 8, height = 4)
ggarrange(plotlist = compare_plots_f[1:2], ncol = 2, nrow = 1)
dev.off()
pdf(paste(output_dir,"compare_plots_s1_f.pdf", sep = ""), width = 8, height = 4)
ggarrange(plotlist = compare_plots_f[3:4], ncol = 2, nrow = 1)
dev.off()
pdf(paste(output_dir,"compare_plots_s2_f.pdf", sep = ""), width = 8, height = 4)
ggarrange(plotlist = compare_plots_f[5:6], ncol = 2, nrow = 1)
dev.off()
pdf(paste(output_dir,"compare_plots_s3_f.pdf", sep = ""), width = 8, height = 4)
ggarrange(plotlist = compare_plots_f[7:8], ncol = 2, nrow = 1)
dev.off()

compare_plots_m = compare_posterior_prior(prior_dir_at_split = c(T,T,T,T), prior_list = list(Dirichlet = res_dir_m$prior, EK = res_expert_m_aPc$prior, "Anti-EK" = res_anti_m$prior), 
                                        posterior_list = list(EK = expert_res_m, "Anti-EK" = anti_res_m, Dirichlet = dir_res_m, CS = comp_res_m), dir_samp = 1000000, no_y_label = T,
                                        hide_y_ticks = F, dir_to_blind = F, posterior_left = c(T,F,F,T))

pdf(paste(output_dir,"compare_plots_s0_m.pdf", sep = ""), width = 8, height = 4)
ggarrange(plotlist = compare_plots_m[1:2], ncol = 2, nrow = 1)
dev.off()
pdf(paste(output_dir,"compare_plots_s1_m.pdf", sep = ""), width = 8, height = 4)
ggarrange(plotlist = compare_plots_m[3:4], ncol = 2, nrow = 1)
dev.off()
pdf(paste(output_dir,"compare_plots_s2_m.pdf", sep = ""), width = 8, height = 4)
ggarrange(plotlist = compare_plots_m[5:6], ncol = 2, nrow = 1)
dev.off()
pdf(paste(output_dir,"compare_plots_s3_m.pdf", sep = ""), width = 8, height = 4)
ggarrange(plotlist = compare_plots_m[7:8], ncol = 2, nrow = 1)
dev.off()
########################
#Individual level plots
########################

compare_plots_indi_f = compare_posterior_prior(prior_dir_at_split = c(T,T,T,T), prior_list = list(Dirichlet = res_dir_f$prior, EK = res_expert_f_aPc$prior, "Anti-EK" = res_anti_f$prior, "Individual" = res_expert_f_aPc$prior), 
                                               posterior_list = list(EK = expert_res_f, "Anti-EK" = anti_res_f, Dirichlet = dir_res_f, "Individual" = ind_res_f), dir_samp = 1000000, no_y_label = T,
                                               hide_y_ticks = F, dir_to_blind = F, posterior_left = c(F,F,F,T))

pdf(paste(output_dir,"compare_plots_indi_s0_f.pdf", sep = ""), width = 8, height = 4)
ggarrange(plotlist = compare_plots_indi_f[1:2], ncol = 2, nrow = 1)
dev.off()
pdf(paste(output_dir,"compare_plots_indi_s1_f.pdf", sep = ""), width = 8, height = 4)
ggarrange(plotlist = compare_plots_indi_f[3:4], ncol = 2, nrow = 1)
dev.off()
pdf(paste(output_dir,"compare_plots_indi_s2_f.pdf", sep = ""), width = 8, height = 4)
ggarrange(plotlist = compare_plots_indi_f[5:6], ncol = 2, nrow = 1)
dev.off()
pdf(paste(output_dir,"compare_plots_indi_s3_f.pdf", sep = ""), width = 8, height = 4)
ggarrange(plotlist = compare_plots_indi_f[7:8], ncol = 2, nrow = 1)
dev.off()

ind_agg_joint = plot_lincombs_joint(list("Indi." = res_expert_indi_f$inla, "Agg." = res_expert_f_aPc$inla))

pdf(paste(output_dir,"lincomb_ind_agg_m_age.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = ind_agg_joint[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_ind_agg_m_cohort.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = ind_agg_joint[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()

# the equivalent plots for individual specif data in males are omitted as the female case proves the point

#Remove and save some memory
rm(list = c("res_dir_indi_f", "res_dir_indi_m", "res_anti_indi_m",
            "res_anti_indi_f", "res_expert_indi_m", "res_expert_indi_f"))


#######################
# Lincombs
########################

# RW2
rw2_plots = plot_lincombs_joint(list("RW2" = res_aPc_rw2_f))
pdf(paste(output_dir,"rw2_age_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = rw2_plots[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"rw2_cohort_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = rw2_plots[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()

#Model selection best models

#Top4 female
top4_f_lincombs = plot_lincombs_joint(list(aPc = res_expert_f_aPc$inla, apC = res_expert_f_apC$inla, aPC = res_expert_f_aPC$inla, APc = res_expert_f_APc$inla))
pdf(paste(output_dir,"lincomb_age_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top4_f_lincombs[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_cohort_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top4_f_lincombs[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_period_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top4_f_lincombs[c(8,7,9)], nrow = 1, ncol = 3)
dev.off()

#Top4 male
top4_m_lincombs = plot_lincombs_joint(list(aPc = res_expert_m_aPc$inla, apC = res_expert_m_apC$inla, aPC = res_expert_m_aPC$inla, APc = res_expert_m_APc$inla))
pdf(paste(output_dir,"lincomb_age_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top4_m_lincombs[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_cohort_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top4_m_lincombs[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_period_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top4_m_lincombs[c(8,7,9)], nrow = 1, ncol = 3)
dev.off()

#Top both
top_cross_lincombs = plot_lincombs_joint(list(Female = res_expert_f_aPc$inla, Male = res_expert_m_aPc$inla))
pdf(paste(output_dir,"best_lincomb_age.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top_cross_lincombs[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"best_lincomb_cohort.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top_cross_lincombs[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()


#################
#model selection figures and tables
#################
load("./Data/EK_model_selection.RData")

#Make table
mydf = data.frame(criterion = rep(c("log-score","WAIC","DIC"), 12),
                  gender = rep(c(rep("Male",3), rep("Female", 3)), 6),
                  value = 1:36,
                  model = c(rep("2",6),rep("3",6),rep("4",6),rep("5",6),rep("6",6),rep("7",6)))

mydf$value[mydf$criterion == "log-score" & mydf$gender == "Male"] = score_matrix_m[1,1:6]
mydf$model[mydf$criterion == "log-score" & mydf$gender == "Male"] = substr(colnames(score_matrix_m), 1,3)
mydf$value[mydf$criterion == "WAIC" & mydf$gender == "Male"] = score_matrix_m[2,1:6]
mydf$model[mydf$criterion == "WAIC" & mydf$gender == "Male"] = substr(colnames(score_matrix_m), 1,3)
mydf$value[mydf$criterion == "DIC" & mydf$gender == "Male"] = score_matrix_m[3,1:6]
mydf$model[mydf$criterion == "DIC" & mydf$gender == "Male"] = substr(colnames(score_matrix_m), 1,3)

mydf$value[mydf$criterion == "log-score" & mydf$gender == "Female"] = score_matrix_f[1,1:6]
mydf$model[mydf$criterion == "log-score" & mydf$gender == "Female"] = substr(colnames(score_matrix_f), 1,3)
mydf$value[mydf$criterion == "WAIC" & mydf$gender == "Female"] = score_matrix_f[2,1:6]
mydf$model[mydf$criterion == "WAIC" & mydf$gender == "Female"] = substr(colnames(score_matrix_f), 1,3)
mydf$value[mydf$criterion == "DIC" & mydf$gender == "Female"] = score_matrix_f[3,1:6]
mydf$model[mydf$criterion == "DIC" & mydf$gender == "Female"] = substr(colnames(score_matrix_f), 1,3)

mydf$value = as.character(mydf$value)
mydf$value[mydf$criterion == "log-score"] = stri_pad_right(mydf$value[mydf$criterion == "log-score"], 5, 0)

bold_best = T

if(bold_best){
  male_waics = as.numeric(mydf$value[mydf$gender == "Male" & mydf$criterion == "WAIC"])
  male_min_index = which.min(male_waics)
  mydf$value[mydf$gender == "Male" & mydf$criterion == "WAIC"][male_min_index] = paste("\\textbf{", as.character(male_waics[male_min_index]) ,"}")

  male_dics = as.numeric(mydf$value[mydf$gender == "Male" & mydf$criterion == "DIC"])
  male_min_index = which.min(male_dics)
  mydf$value[mydf$gender == "Male" & mydf$criterion == "DIC"][male_min_index] = paste("\\textbf{", as.character(male_dics[male_min_index]) ,"}")

  male_cpos = as.numeric(mydf$value[mydf$gender == "Male" & mydf$criterion == "log-score"])
  male_min_index = which.min(male_cpos)
  mydf$value[mydf$gender == "Male" & mydf$criterion == "log-score"][male_min_index] = paste("\\textbf{", as.character(male_cpos[male_min_index]) ,"}")


  female_waics = as.numeric(mydf$value[mydf$gender == "Female" & mydf$criterion == "WAIC"])
  female_min_index = which.min(female_waics)
  mydf$value[mydf$gender == "Female" & mydf$criterion == "WAIC"][female_min_index] = paste("\\textbf{", as.character(female_waics[female_min_index]) ,"}")

  female_dics = as.numeric(mydf$value[mydf$gender == "Female" & mydf$criterion == "DIC"])
  female_min_index = which.min(female_dics)
  mydf$value[mydf$gender == "Female" & mydf$criterion == "DIC"][female_min_index] = paste("\\textbf{", as.character(female_dics[female_min_index]) ,"}")

  female_cpos = as.numeric(mydf$value[mydf$gender == "Female" & mydf$criterion == "log-score"])
  female_min_index = which.min(female_cpos)
  mydf$value[mydf$gender == "Female" & mydf$criterion == "log-score"][female_min_index] = paste("\\textbf{", as.character(female_cpos[female_min_index]) ,"}")
  
}

mydf$criterion[mydf$criterion == "log-score"] = "1log-score"
mydf$criterion[mydf$criterion == "WAIC"] = "2WAIC"
mydf$criterion[mydf$criterion == "DIC"] = "3DIC"

caption = "The achieved logarithmic score, WAIC, and DIC for each of the 6 candidate MAPC models, specified by different combinations of stratum-specific and shared age, period, and cohort effects. Shared effects are denoted with uppercase letters, and stratum-specific effects are denoted with lowercase letters. For each sex, the best model by each criterion is highlighted in bold."
label = "fig:model_selection_score"



latex_tabular <- latexTable(tabular((
  Heading()*RowFactor(criterion, space = 0.25, spacing = 1)*Heading()*RowFactor(gender, space = 1, spacing = 2)) ~ (Heading()*Factor(model)*Heading()*value*Heading()*identity),
  data = mydf), table.envir = "table"
)

latex_tabular = gsub("\\begin{tabular}{llcccccc}", "\\begin{tabularx}{\\textwidth}{llXXXXXX}", latex_tabular, fixed=T)
latex_tabular = gsub("\\end{tabular}", "\\end{tabularx}\\endgroup", latex_tabular, fixed=T)
latex_tabular = gsub("1log-score", "log-score", latex_tabular, fixed=T)
latex_tabular = gsub("2WAIC", "WAIC", latex_tabular, fixed=T)
latex_tabular = gsub("3DIC", "DIC", latex_tabular, fixed=T)
latex_tabular = gsub("\\begin{table}", "\\begin{table}[h!]\n\\centering", latex_tabular, fixed=T)
latex_tabular = gsub("\\centering", "\\centering\n\\begingroup\\footnotesize\\setstretch{1.2}", latex_tabular, fixed=T)
latex_tabular = gsub("\\endgroup", paste("\\endgroup\n \\caption{",caption,"} \\label{", label,"}", sep =""), latex_tabular, fixed = T)

#save table
cat(latex_tabular, file = paste(output_dir,"model_selection.tex", sep = ""))

###########################
# Complex survey data plots and tables
############################

load("./Data/survey_model_selection.RData")

#Model selection table
mydf = data.frame(criterion = rep(c("log-score","WAIC","DIC"), 12),
                  gender = rep(c(rep("Male",3), rep("Female", 3)), 6),
                  value = 1:36,
                  model = c(rep("2",6),rep("3",6),rep("4",6),rep("5",6),rep("6",6),rep("7",6)))

mydf$value[mydf$criterion == "log-score" & mydf$gender == "Male"] = score_matrix_m_survey[1,1:6]
mydf$model[mydf$criterion == "log-score" & mydf$gender == "Male"] = substr(colnames(score_matrix_m_survey), 1,3)
mydf$value[mydf$criterion == "WAIC" & mydf$gender == "Male"] = score_matrix_m_survey[2,1:6]
mydf$model[mydf$criterion == "WAIC" & mydf$gender == "Male"] = substr(colnames(score_matrix_m_survey), 1,3)
mydf$value[mydf$criterion == "DIC" & mydf$gender == "Male"] = score_matrix_m_survey[3,1:6]
mydf$model[mydf$criterion == "DIC" & mydf$gender == "Male"] = substr(colnames(score_matrix_m_survey), 1,3)

mydf$value[mydf$criterion == "log-score" & mydf$gender == "Female"] = score_matrix_f_survey[1,1:6]
mydf$model[mydf$criterion == "log-score" & mydf$gender == "Female"] = substr(colnames(score_matrix_f_survey), 1,3)
mydf$value[mydf$criterion == "WAIC" & mydf$gender == "Female"] = score_matrix_f_survey[2,1:6]
mydf$model[mydf$criterion == "WAIC" & mydf$gender == "Female"] = substr(colnames(score_matrix_f_survey), 1,3)
mydf$value[mydf$criterion == "DIC" & mydf$gender == "Female"] = score_matrix_f_survey[3,1:6]
mydf$model[mydf$criterion == "DIC" & mydf$gender == "Female"] = substr(colnames(score_matrix_f_survey), 1,3)

mydf$value = as.character(mydf$value)
mydf$value[mydf$criterion == "log-score"] = stri_pad_right(mydf$value[mydf$criterion == "log-score"], 5, 0)

bold_best = T

if(bold_best){
  male_waics = as.numeric(mydf$value[mydf$gender == "Male" & mydf$criterion == "WAIC"])
  male_min_index = which.min(male_waics)
  mydf$value[mydf$gender == "Male" & mydf$criterion == "WAIC"][male_min_index] = paste("\\textbf{", as.character(male_waics[male_min_index]) ,"}")

  male_dics = as.numeric(mydf$value[mydf$gender == "Male" & mydf$criterion == "DIC"])
  male_min_index = which.min(male_dics)
  mydf$value[mydf$gender == "Male" & mydf$criterion == "DIC"][male_min_index] = paste("\\textbf{", as.character(male_dics[male_min_index]) ,"}")

  
  male_cpos = as.numeric(mydf$value[mydf$gender == "Male" & mydf$criterion == "log-score"])
  male_min_index = which.min(male_cpos)
  mydf$value[mydf$gender == "Male" & mydf$criterion == "log-score"][male_min_index] = paste("\\textbf{", as.character(male_cpos[male_min_index]) ,"}")


  female_waics = as.numeric(mydf$value[mydf$gender == "Female" & mydf$criterion == "WAIC"])
  female_min_index = which.min(female_waics)
  mydf$value[mydf$gender == "Female" & mydf$criterion == "WAIC"][female_min_index] = paste("\\textbf{", as.character(female_waics[female_min_index]) ,"}")

  female_dics = as.numeric(mydf$value[mydf$gender == "Female" & mydf$criterion == "DIC"])
  female_min_index = which.min(female_dics)
  mydf$value[mydf$gender == "Female" & mydf$criterion == "DIC"][female_min_index] = paste("\\textbf{", as.character(female_dics[female_min_index]) ,"}")

  female_cpos = as.numeric(mydf$value[mydf$gender == "Female" & mydf$criterion == "log-score"])
  female_min_index = which.min(female_cpos)
  mydf$value[mydf$gender == "Female" & mydf$criterion == "log-score"][female_min_index] = paste("\\textbf{", as.character(female_cpos[female_min_index]) ,"}")

  
  }
mydf$criterion[mydf$criterion == "log-score"] = "1log-score"
mydf$criterion[mydf$criterion == "WAIC"] = "2WAIC"
mydf$criterion[mydf$criterion == "DIC"] = "3DIC"


caption = "The achieved logarithmic score, WAIC, and DIC for each of the 6 candidate MAPC models accounting for the survey design, specified by different combinations of stratum-specific and shared age, period, and cohort effects. Shared effects are denoted with uppercase letters, and stratum-specific effects are denoted with lowercase letters. For each sex, the best model by each criterion is highlighted in bold."
label = "fig:model_selection_survey"

latex_tabular <- latexTable(tabular((
  Heading()*RowFactor(criterion, space = 0.25, spacing = 1)*Heading()*RowFactor(gender, space = 1, spacing = 2)) ~ (Heading()*Factor(model)*Heading()*value*Heading()*identity),
  data = mydf), table.envir = "table"
)

latex_tabular = gsub("\\begin{tabular}{llcccccc}", "\\begin{tabularx}{\\textwidth}{llXXXXXX}", latex_tabular, fixed=T)
latex_tabular = gsub("\\end{tabular}", "\\end{tabularx}\\endgroup", latex_tabular, fixed=T)
latex_tabular = gsub("\\begin{table}", "\\begin{table}[h!]\n\\centering", latex_tabular, fixed=T)
latex_tabular = gsub("1log-score", "log-score", latex_tabular, fixed=T)
latex_tabular = gsub("2WAIC", "WAIC", latex_tabular, fixed=T)
latex_tabular = gsub("3DIC", "DIC", latex_tabular, fixed=T)
latex_tabular = gsub("\\centering", "\\centering\n\\begingroup\\footnotesize\\setstretch{1.2}", latex_tabular, fixed=T)
latex_tabular = gsub("\\endgroup", paste("\\endgroup\n \\caption{",caption,"} \\label{", label,"}", sep =""), latex_tabular, fixed = T)

#save table
cat(latex_tabular, file = paste(output_dir,"model_selection_survey.tex", sep = ""))

#Lincombs comparing adjusted and unadjusted ApC models
survey_comb = plot_lincombs_joint(list("Acc." = res_survey_aPc_f$inla, "Unacc." = res_expert_f_aPc$inla), boost = 0.05)
pdf(paste(output_dir,"lincomb_hajek_f_age.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = survey_comb[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_hajek_f_cohort.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = survey_comb[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()

survey_comb = plot_lincombs_joint(list("Acc." = res_survey_aPc_m$inla, "Unacc." = res_expert_m_aPc$inla), boost = 0.05)
pdf(paste(output_dir,"lincomb_hajek_m_age.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = survey_comb[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_hajek_m_cohort.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = survey_comb[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()

#Plot lincombs
top4_f_lincombs_survey = plot_lincombs_joint(list(aPc = res_survey_aPc_f$inla, apC = res_survey_apC_f$inla, aPC = res_survey_aPC_f$inla, APc = res_survey_APc_f$inla), boost = 0.1, def_pos = c(0.2,0.85))
pdf(paste(output_dir,"lincomb_age_f_survey.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top4_f_lincombs_survey[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_cohort_f_survey.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top4_f_lincombs_survey[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_period_f_survey.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top4_f_lincombs_survey[c(8,7,9)], nrow = 1, ncol = 3)
dev.off()

top4_m_lincombs_survey = plot_lincombs_joint(list(aPc = res_survey_aPc_m$inla, apC = res_survey_apC_m$inla, aPC = res_survey_aPC_m$inla, APc = res_survey_APc_m$inla), boost = 0.05, def_pos = c(0.2,0.85))
pdf(paste(output_dir,"lincomb_age_m_survey.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top4_m_lincombs_survey[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_cohort_m_survey.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top4_m_lincombs_survey[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_period_m_survey.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = top4_m_lincombs_survey[c(8,7,9)], nrow = 1, ncol = 3)
dev.off()

# survey_res_f = extract_res_inla(res_survey_aPc_f$inla)
# survey_res_m = extract_res_inla(res_survey_aPc_m$inla)
# 
# compare_plots_survey_f = compare_posterior_prior(prior_dir_at_split = c(T,T,T,T), prior_list = list(Dirichlet = res_dir_f$prior, EK = res_expert_f_aPc$prior, "Anti-EK" = res_anti_f$prior, "Individual" = res_expert_f_aPc$prior), 
#                                                posterior_list = list(EK = expert_res_f, "Anti-EK" = anti_res_f, Dirichlet = dir_res_f, "Adjusted" = survey_res_f), dir_samp = 1000000, no_y_label = T,
#                                                hide_y_ticks = F, dir_to_blind = F, posterior_left = c(F,F,F,T))
# pdf(paste(output_dir,"compare_plots_survey_s0_f.pdf", sep = ""), width = 8, height = 4)
# ggarrange(plotlist = compare_plots_survey_f[1:2], ncol = 2, nrow = 1)
# dev.off()
# pdf(paste(output_dir,"compare_plots_survey_s1_f.pdf", sep = ""), width = 8, height = 4)
# ggarrange(plotlist = compare_plots_survey_f[3:4], ncol = 2, nrow = 1)
# dev.off()
# pdf(paste(output_dir,"compare_plots_survey_s2_f.pdf", sep = ""), width = 8, height = 4)
# ggarrange(plotlist = compare_plots_survey_f[5:6], ncol = 2, nrow = 1)
# dev.off()
# pdf(paste(output_dir,"compare_plots_survey_s3_f.pdf", sep = ""), width = 8, height = 4)
# ggarrange(plotlist = compare_plots_survey_f[7:8], ncol = 2, nrow = 1)
# dev.off()
# 
# 
# compare_plots_survey_m = compare_posterior_prior(prior_dir_at_split = c(T,T,T,T), prior_list = list(Dirichlet = res_dir_m$prior, EK = res_expert_m_aPc$prior, "Anti-EK" = res_anti_m$prior, "Individual" = res_expert_m_aPc$prior), 
#                                                  posterior_list = list(EK = expert_res_m, "Anti-EK" = anti_res_m, Dirichlet = dir_res_m, "Adjusted" = survey_res_m), dir_samp = 1000000, no_y_label = T,
#                                                  hide_y_ticks = F, dir_to_blind = F, posterior_left = c(F,F,F,T))
# pdf(paste(output_dir,"compare_plots_survey_s0_m.pdf", sep = ""), width = 8, height = 4)
# ggarrange(plotlist = compare_plots_survey_m[1:2], ncol = 2, nrow = 1)
# dev.off()
# pdf(paste(output_dir,"compare_plots_survey_s1_m.pdf", sep = ""), width = 8, height = 4)
# ggarrange(plotlist = compare_plots_survey_m[3:4], ncol = 2, nrow = 1)
# dev.off()
# pdf(paste(output_dir,"compare_plots_survey_s2_m.pdf", sep = ""), width = 8, height = 4)
# ggarrange(plotlist = compare_plots_survey_m[5:6], ncol = 2, nrow = 1)
# dev.off()
# pdf(paste(output_dir,"compare_plots_survey_s3_m.pdf", sep = ""), width = 8, height = 4)
# ggarrange(plotlist = compare_plots_survey_m[7:8], ncol = 2, nrow = 1)
# dev.off()


##################################
#Filtered results
#################################

aPc_survey_filtered_lincombs_f = plot_lincombs_joint(list("All" = res_survey_aPc_f$inla, "Fil." = res_survey_aPc_f_filtered$inla), boost = 0.05, def_pos = c(0.2,0.85))
pdf(paste(output_dir,"lincomb_survey_filtered_age_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = aPc_survey_filtered_lincombs_f[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_survey_filtered_cohort_f.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = aPc_survey_filtered_lincombs_f[c(5,4,6)], nrow = 1, ncol = 3)
dev.off() 

aPc_survey_filtered_lincombs_m = plot_lincombs_joint(list("All" = res_survey_aPc_m$inla, "Fil." = res_survey_aPc_m_filtered$inla), boost = 0.05, def_pos = c(0.2,0.85))
pdf(paste(output_dir,"lincomb_survey_filtered_age_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = aPc_survey_filtered_lincombs_m[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_survey_filtered_cohort_m.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = aPc_survey_filtered_lincombs_m[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()

aPc_survey_filtered_lincombs_f_rw2 = plot_lincombs_joint(list("RW2" = res_survey_aPc_rw2_f, "RW1" = res_survey_aPc_f_filtered$inla), def_pos = c(0.2,0.85),boost = 0.05)
pdf(paste(output_dir,"lincomb_survey_filtered_age_f_rw2.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = aPc_survey_filtered_lincombs_f_rw2[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_survey_filtered_cohort_f_rw2.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = aPc_survey_filtered_lincombs_f_rw2[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()

aPc_survey_filtered_lincombs_m_rw2 = plot_lincombs_joint(list("RW2" = res_survey_aPc_rw2_m, "RW1" = res_survey_aPc_m_filtered$inla), def_pos = c(0.2,0.85),boost = 0.05)
pdf(paste(output_dir,"lincomb_survey_filtered_age_m_rw2.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = aPc_survey_filtered_lincombs_m_rw2[c(2,1,3)], nrow = 1, ncol = 3)
dev.off()
pdf(paste(output_dir,"lincomb_survey_filtered_cohort_m_rw2.pdf", sep = ""), height = 5, width = 15)
ggarrange(plotlist = aPc_survey_filtered_lincombs_m_rw2[c(5,4,6)], nrow = 1, ncol = 3)
dev.off()
