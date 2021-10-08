# Preparation -------------------------------------------------------------
if(!("MASS" %in% installed.packages())) install.packages("MASS", dependencies = T)
if(!("mvtnorm" %in% installed.packages())) install.packages("mvtnorm", dependencies = T)
if(!("copula" %in% installed.packages())) install.packages("copula", dependencies = T)
if(!("multcomp" %in% installed.packages())) install.packages("multcomp", dependencies = T)

source("util.R")
source("dat_gen.R")
source("stats.R")
source("settings.R")
source("sim_fun.R")


# Simulation --------------------------------------------------------------

nsim  = 1e4 # Number of Simulation Runs
dists = c("norm", "pois", "beta", "binom")
param_list = list(list(mean = 0, sd = 1), list(lambda = 3), list(shape1 = 2, shape2 = 5), list(size = 1, prob = 0.5))

for(u in 1:length(dists)){
    sim_fun(nsim, dists[u], param_list[u], samples, NULL, "Dunnett")
}