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
source("utilcpp_wrap.R")

# Simulation --------------------------------------------------------------

nsim  = 1e4 # Number of Simulation Runs
dists = c("norm", "pois", "beta", "binom")
param_list = list(list(mean = 0, sd = 1), list(lambda = 3), list(shape1 = 2, shape2 = 5), list(size = 1, prob = 0.5))
dist_c = "norm"
dist_params = param_list[[1]]
c_type = "Dunnett"
results = list()

# No Correlation ----------------------------------------------------------

settings = samples

settings$rho  = 0
settings$dist = dist_c
settings$f_2  = 0
settings$wald = 0
settings$anv  = 0
settings$maxt = 0

z = 80

sets = settings[z,]
sizes = nm_gen(nn = sets$nn, n_i = sets$n_i, m_ij = sets$m_ij, each_s = sets$each_s, both_s = sets$both_s, identical_s = sets$identical_s, identical_c = sets$identical_c)

f2 = sum(sizes[[1]]) - length(sizes[[1]])

c_mat = contrMat(sizes[[1]], type = c_type) %*% diag(1, length(sizes[[1]]))
dec = matrix(0, nrow = 3, ncol = nsim)
theta = rep(1/sets$nn, length(sizes[[1]]))

data_n = h_0_f(sizes[[1]], sizes[[2]], dist = dist_c, corstruct = "independent", rho = settings$rho[z], params = dist_params)

f_theta_p(c(0.2, 0.4), data_n)
f_theta_p2(c(0.2, 0.4), data_n)

rel_eff(data_n)
rel_eff_p(data_n)
rel_eff_p2(data_n)

A_1 = sigma_est(sizes[[1]], data_n, theta = theta, psi = NULL)
A_2 = sigma_est_p2(sizes[[1]], data_n, theta = theta, psi = NULL)
# C++ vs R ----------------------------------------------------------------

q_anova(sizes[[1]], data_n, c_mat, f2, theta = NULL, psi = NULL, alpha = 0.05)
q_wald(sizes[[1]], data_n, c_mat, theta = NULL, psi = NULL, alpha = 0.05)

Q_anova(rel_eff(data_n), A_1, c_mat, f2, sizes[[1]], 0.05)
Q_wald(rel_eff(data_n), A_1, c_mat, sizes[[1]], 0.05)
# sigma_est
# Q_Wald
# Q_Anova
# max_t
