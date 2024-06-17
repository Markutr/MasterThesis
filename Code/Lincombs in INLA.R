library(INLA)

#############
#Below follows a simple example of MAPC models in inla. We use 3 age groups, 3 periods and 2 education levels.
#The MAPC model will use an aPC model, i.e. only the age is education-specific.
#We formulate lincombs for the age effect.
#We create data with IID noise and add some structure over period and cohort
#We also add different trends with age based on education.
#The end goal is to be able to recongize the differences in the age effects based on education.
#############

#
set.seed(123)

#First we setup our data format.
I = 3; J = 3; E = 2 #Age, periods and education groups
data = as.data.frame(matrix(NA, nrow = I*J*E, ncol = 7))
colnames(data) = c("Value", "Age","Period","Cohort","Education1","Education2","IID")
data$Age = rep(c(1:I), E*J)
data$Period = rep(c(rep(1,I),rep(2,I),rep(3,I)),E)
data$Cohort = I - data$Age + data$Period
data$Education1 = c(rep(1, I*J), rep(NA,I*J))
data$Education2 = c(rep(NA, I*J), rep(1,I*J))
data$Age_Education1 = data$Age*data$Education1
data$Age_Education2 = data$Age*data$Education2
data$IID = 1:(I*J*E)

#Now we insert structure into the periods and cohorts
data$Value = rnorm(I*J*E, mean = 0, sd = 0.05) #IID component for each
data$Value[data$Period == 3] = data$Value[data$Period == 3] + 0.9 #Add shock to the last period
data$Value[data$Cohort == 1] = data$Value[data$Cohort == 1] - 0.5 #Add some differences over some cohorts 
data$Value[data$Cohort == 2] = data$Value[data$Cohort == 2] - 0.3
data$Value[data$Cohort == 3] = data$Value[data$Cohort == 3] - 0.1

#Insert different scaling with age based on education.
data$Value[!is.na(data$Age_Education1)] = data$Value[!is.na(data$Age_Education1)] + data$Age_Education1[!is.na(data$Age_Education1)]*0.3
data$Value[!is.na(data$Age_Education2)] = data$Value[!is.na(data$Age_Education2)] + data$Age_Education2[!is.na(data$Age_Education2)]*0.1

#Add different baseline levels
data$Value[!is.na(data$Education1)] = data$Value[!is.na(data$Education1)] + 0.9
data$Value[!is.na(data$Education2)] = data$Value[!is.na(data$Education2)] + 0.2 #Different baseline based on education

#Formula to use with INLA
inla_formula = Value ~ -1 + Education1 + Education2 + 
  f(Age_Education1, model = "rw1") +
  f(Age_Education2, model = "rw1") +
  f(Period, model = "rw1") +
  f(Cohort, model = "rw1") + 
  f(IID, model= "iid")

#Creation of the linear combinations. Random effects require the coefficient (here its 1 and -1) at the correct index. Fixed effects just the coefficients
lc1 = inla.make.lincomb(Age_Education1 = c(1,NA,NA), Age_Education2 = c(-1, NA, NA), Education1 = 1, Education2 = -1)
lc2 = inla.make.lincomb(Age_Education1 = c(NA,1,NA), Age_Education2 = -c(NA, -1, NA), Education1 = 1, Education2 = -1)
lc3 = inla.make.lincomb(Age_Education1 = c(NA,NA,1), Age_Education2 = -c(NA, NA, -1), Education1 = 1, Education2 = -1)
names(lc1) = "Age 1"
names(lc2) = "Age 2"
names(lc3) = "Age 3"

##########
# Due to our construction, we expect to see a baseline difference of 0.9 - 0.2 = 0.7 due to the mean.
# Then we expect to see differences with age as 0.2, 0.4 and 0.6.
# In total, over age we expect to see estimates in the trends as 0.9, 1.1 and 1.3.
##########

res = inla(inla_formula, data = data, lincomb = c(lc1, lc2, lc3))

lincomb_extract = res$summary.lincomb.derived[,c("ID","0.5quant")]
colnames(lincomb_extract) = c("Age", "Value")
lincomb_extract[4:6,] = NA
lincomb_extract$Value[4:6] = c(0.9, 1.1, 1.3)
lincomb_extract$Age = rep(1:3,2)
lincomb_extract$Difference = c(rep("Estimated", 3), rep("Real", 3))

ggplot(lincomb_extract, aes(x = Age, y = Value, color = Difference)) + 
  geom_line() + 
  theme_bw()

#########
#And that is what we see in the estimated trends (approximately, run a few times to see it). Thus showing that the combinations work.
########
