# This simple script generates all the prior objects used in the analysis. 
# Running this code will take alot of resources and is a lengthy process.
# Luckily it only needs to be run once.
#######################################
source("functions.R")
if(!file.exists("./Data/processed_data.RData")){
  source("./data preprocessing.R")
}
load("./Data/processed_data.RData")

models = rev(c("APc", "ApC", "aPC", "Apc", "aPc","apC"))
prior_structures = c("EK","Anti-EK","Dir","Semi")

for(model in models){ #Generate all EK priors
  prior = makePriorAPC(bp_data_f, "n_backpain", prior_mode = "EK", apc_format = model)
}

prior = makePriorAPC(bp_data_f, "n_backpain", prior_mode = "Anti-EK", apc_format = "ApC")
prior = makePriorAPC(bp_data_f, "n_backpain", prior_mode = "Dirichlet", apc_format = "ApC")
prior = makePriorAPC(bp_data_f, "n_backpain", prior_mode = "Semi", apc_format = "ApC")


