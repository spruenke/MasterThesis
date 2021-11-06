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

z = 10

sets = settings[z,]
sizes = rankCluster::nm_gen(nn = sets$nn, n_i = sets$n_i, m_ij = sets$m_ij, each_s = sets$each_s, both_s = sets$both_s, identical_s = sets$identical_s, identical_c = sets$identical_c)

f2 = sqrt(sum(sizes[[1]]) - length(sizes[[1]]))

c_mat = multcomp::contrMat(sizes[[1]], type = c_type) %*% diag(1, length(sizes[[1]]))
dec = matrix(0, nrow = 3, ncol = nsim)
theta = rep(1/sets$nn, length(sizes[[1]]))
psi = NULL

data_n = h_0_f(sizes[[1]], sizes[[2]], dist = dist_c, corstruct = "independent", rho = settings$rho[z], params = dist_params)

# f_theta_p(c(0.2, 0.4), data_n)
# f_theta_p2(c(0.2, 0.4), data_n)
# if(is.null(psi)){
#   psi = list()
#   for(i in 1:length(data_n)){
#     psi[[i]] = round(rep(1 / length(data_n[[i]]), length(data_n[[i]])), 7)
#   }
# }

max_T2  = function(n, data, p_null = 0.5, cont, normal = FALSE, theta = NULL, psi = NULL, alpha){
  Sigma = rankCluster::sigma_est(n, data, theta = theta, psi = psi)
  p = rankCluster::rel_eff(data, theta, psi)
  R = cov2cor(Sigma)
  R_c = cov2cor(cont%*%Sigma%*%t(cont))
  stat = sqrt(g(n)) * (p - p_null) / sqrt(diag(Sigma))
  #if(normal == TRUE) crit = mvtnorm::qmvnorm(1-alpha, tail = "lower.tail", mean = rep(0, length(p)), corr = R)$quantile
  #if(normal == FALSE) crit = mvtnorm::qmvt(1-alpha, tail = "lower.tail", df = g(n) - 1, corr = R)$quantile
  if(normal == FALSE) rej = 1-mvtnorm::pmvt(upper = abs(stat), df = g(n) - 1, corr = R)
  #dec = max(abs(stat)) > crit
  #return(list(Statistic = stat, df = g(n) - 1, reject = dec))
  
  return(rej < alpha/2)
}

rankCluster::rel_eff(data_n)

ab = replicate(1000, expr = {
  data_b = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = dist_c, corstruct = "independent", rho = settings$rho[z], params = dist_params)
  
  # f_theta_p(c(0.2, 0.4), data_n)
  # f_theta_p2(c(0.2, 0.4), data_n)
  # if(is.null(psi)){
  #   psi = list()
  #   for(i in 1:length(data_n)){
  #     psi[[i]] = round(rep(1 / length(data_n[[i]]), length(data_n[[i]])), 7)
  #   }
  # }
  
  c(rankCluster::q_wald(sizes[[1]], data_b, cont=c_mat, alpha = 0.05)$reject,
    rankCluster::q_anova(sizes[[1]], data_b, cont = c_mat, f_2 = f2, alpha = 0.05)$reject)
})
rel_eff_p(data_n)
rel_eff_p2(data_n)

sizes[[1]]
nn = rep(3, 5)
A_1 = sigma_est(sizes[[1]], data_n, theta = theta, psi = NULL)
A_2 = sigma_est_p2(sizes[[1]], data_n, theta = NULL, psi = NULL)
A_3 = debug_p(sizes[[1]], data_n, theta = NULL, psi = NULL)

st = Sys.time()
rankCluster::max_T(sizes[[1]], data_n, cont = c_mat, alpha = 0.05)$reject
Sys.time() - st
st = Sys.time()
max_T2(sizes[[1]], data_n, cont = c_mat, alpha = 0.05)
Sys.time() - st

microbenchmark::microbenchmark(rankCluster::max_T(sizes[[1]], data_n, cont=c_mat, alpha = 0.05)$reject,
                               rankCluster::max_T2(sizes[[1]], data_n, cont = c_mat, alpha = 0.05)$reject)
ab = replicate(1000, expr = {
  data_b = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = dist_c, corstruct = "independent", rho = settings$rho[z], params = dist_params)
  
  # f_theta_p(c(0.2, 0.4), data_n)
  # f_theta_p2(c(0.2, 0.4), data_n)
  # if(is.null(psi)){
  #   psi = list()
  #   for(i in 1:length(data_n)){
  #     psi[[i]] = round(rep(1 / length(data_n[[i]]), length(data_n[[i]])), 7)
  #   }
  # }
  
  c(rankCluster::max_T(sizes[[1]], data_b, cont=c_mat, alpha = 0.05)$reject,
    rankCluster::max_T2(sizes[[1]], data_b, cont = c_mat, alpha = 0.05)$reject)
})
# C++ vs R ----------------------------------------------------------------

sim_fun2 = function(){
  data_t = h_0_f(sizes[[1]], sizes[[2]], dist = dist_c, corstruct = "independent", rho = settings$rho[z], params = dist_params)
  r1 = q_anova(sizes[[1]], data_t, c_mat, f2, theta = NULL, psi = NULL, alpha = 0.05)$reject
  r2 = Q_anova(rel_eff(data_n), sigma_est(sizes[[1]], data_t, theta = theta, psi = NULL), c_mat, f2, sizes[[1]], 0.05)$reject
  return(c(r1, r2))
}

rowMeans(replicate(1000, sim_fun2()))

decc = foreach(1:1000, .combine = "cbind") %dopar% {
  #data_t = h_0_f(sizes[[1]], sizes[[2]], dist = dist_c, corstruct = "independent", rho = settings$rho[z], params = dist_params)
  #   psi = list()
  #   for(i in 1:length(data)){
  #     psi[[i]] = rep(1 / length(data[[i]]), length(data[[i]]))
  #   }
  # r1 = q_anova(sizes[[1]], data_t, c_mat, f2, theta = theta, psi = psi, alpha = 0.05)$reject
  # r2 = Q_anova(rel_eff(data_n), sigma_est(sizes[[1]], data_t, theta = theta, psi = psi), c_mat, f2, sizes[[1]], 0.05)$reject
  # #c(r1, r2)
  #c(mean(data_t[[1]][[1]]), r1, r2)
  sim_fun2()
}

q_anova(sizes[[1]], data_n, c_mat, f2, theta = NULL, psi = NULL, alpha = 0.05)$reject
q_wald(sizes[[1]], data_n, c_mat, theta = NULL, psi = NULL, alpha = 0.05)$reject

Q_anova(rel_eff(data_n), A_1, c_mat, f2, sizes[[1]], 0.05)
Q_wald(rel_eff(data_n), A_1, c_mat, sizes[[1]], 0.05)

Q_An_R = function(data, n, theta = NULL, psi = NULL, c_mat){
  if(is.null(psi)){
    psi = list()
    for(i in 1:length(data)){
      psi[[i]] = rep(1 / length(data[[i]]), length(data[[i]]))
    }
  }
  if(is.null(theta)) theta = rep(1/length(data), length(data))

  Sigma = sigma_est(sizes[[1]], data, theta = theta, psi = NULL)
  p = rel_eff(data)
  Q_anova(p, Sigma, c_mat, f2, sizes[[1]], 0.05)
}

Q_Wa_R = function(data, n, theta = NULL, psi = NULL, c_mat){
  if(is.null(psi)){
    psi = list()
    for(i in 1:length(data)){
      psi[[i]] = rep(1 / length(data[[i]]), length(data[[i]]))
    }
  }
  if(is.null(theta)) theta = rep(1/length(data), length(data))

  Sigma = sigma_est(sizes[[1]], data, theta = theta, psi = NULL)
  p = rel_eff(data)
  Q_wald(p, Sigma, c_mat, sizes[[1]], 0.05)
}

microbenchmark::microbenchmark("C++:" = q_anova(sizes[[1]], data_n, c_mat, f2, theta = NULL, psi = NULL, alpha = 0.05),
                               "R:" = Q_An_R(data_n, sizes[[1]], c_mat = c_mat))
microbenchmark::microbenchmark("C++:" = q_wald(sizes[[1]], data_n, c_mat, theta = NULL, psi = NULL, alpha = 0.05),
                               "R:" = Q_Wa_R(data_n, sizes[[1]], c_mat = c_mat))

stat1 = function(p, M, Sigma){

  t(p)%*%M%*%p / sum(diag(M%*%Sigma))
}

stat2 = function(p, M, Sigma){
  res(p, M) / nen(M, Sigma)

}

stat3 = function(p, M, Sigma){
  stat_pp(p, M, Sigma)
}
# sigma_est
# Q_Wald
# Q_Anova
# max_t
abb = replicate(1000, expr = {
data_n = h_0_f(sizes[[1]], sizes[[2]], dist = dist_c, corstruct = "independent", rho = settings$rho[z], params = dist_params)
pnull = 0.5
alpha = 0.05
n = sizes[[1]]
sig = rankCluster::sigma_est(n, data_n, theta)
phat = rankCluster::rel_eff(data_n, theta)
Rhat = cov2cor(sig)
R_c  = cov2cor(c_mat%*%sig%*%t(c_mat))
stat = sqrt(rankCluster::g(n)) * (phat - pnull) / sqrt(diag(sig))
crit = mvtnorm::qmvt(1-alpha/2, tail = "lower.tail", df = rankCluster::g(n) - 1, corr = Rhat)$quantile
c(1-mvtnorm::pmvt(upper=rep(max(abs(stat)), nrow(Rhat)), df = rankCluster::g(n) - 1, corr = Rhat, keepAttr = F),
max(abs(stat)) > crit)

})

abb1 = which(abb[1,] <= alpha/2)
abb2 = which(abb[2,] == T)
all(abb1 == abb2)