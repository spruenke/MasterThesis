# Preparation -------------------------------------------------------------
rm(list = ls())
if(!("MASS" %in% installed.packages())) install.packages("MASS", dependencies = T)
if(!("mvtnorm" %in% installed.packages())) install.packages("mvtnorm", dependencies = T)
if(!("copula" %in% installed.packages())) install.packages("copula", dependencies = T)
if(!("multcomp" %in% installed.packages())) install.packages("multcomp", dependencies = T)
if(!("Rcpp" %in% installed.packages())) install.packages("Rcpp", dependencies = T)
if(!("RcppArmadillo" %in% installed.packages())) install.packages("RcppArmadillo", dependencies = T)
if(!("doParallel" %in% installed.packages())) install.packages("doParallel", dependencies = T)
if(!("foreach" %in% installed.packages())) install.packages("foreach")
if(!("rankCluster" %in% installed.packages())) devtools::install_github("spruenke/rankCluster")

#source("util.R")
#source("dat_gen.R")
#source("stats.R")
source("settings.R")
source("sim_fun.R")
#source("utilcpp_wrap.R")




# Simulation --------------------------------------------------------------


nsim  = 10000 # Number of Simulation Runs
dists = c("norm", "pois", "beta", "binom")
param_list = list(list(mean = 0, sd = 1), list(lambda = 3), list(shape1 = 2, shape2 = 5), list(size = 1, prob = 0.5))

# Unweighted
start_time = Sys.time()
for(u in 1:length(dists)){
    sim_fun(nsim, dists[u], param_list[[u]], samples, NULL, "Dunnett", w_type = "unweighted")
}
stop_time = Sys.time()
dur = stop_time - start_time

# Weighted
start_time = Sys.time()
for(u in 1:length(dists)){
  sim_fun(nsim, dists[u], param_list[[u]], samples, NULL, "Dunnett", w_type = "weighted")
}
stop_time = Sys.time()
dur_w = stop_time - start_time
