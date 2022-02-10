library(devtools)
install_github("spruenke/rankCluster", force = T)
library(multcomp)
library(doParallel)
library(rankCluster)
library(foreach)

#---------------------------------------------------------------------------

c_type = "Tukey"
nsim = 1e4
res_list = list()
# Type-I Error ------------------------------------------------------------

      # Norm 1
      settings = subset(samples, grp == 1)
      settings$type = "unweighted"
      settings$rho = runif(1, 0.35, 0.8)
      settings$corstruct = "exchangeable"
      
      dist_type = "norm"
      dist_params = list(mean = 0, sd = 1)
      
      settings$wald = 0
      settings$anv = 0
      settings$maxt = 0
      
      for(i in 1:nrow(settings)){
         sets = settings[i,]
         sizes = rankCluster::nm_gen(nn = sets$nn, n_i = sets$n_i, m_ij = sets$m_ij, each_s = sets$each_s, both_s = sets$both_s, identical_s = sets$identical_s, identical_c = sets$identical_c)
         f2 = sum(sizes[[1]]) - length(sizes[[1]])
         c_mat = multcomp::contrMat(sizes[[1]], type = c_type) %*% diag(1, length(sizes[[1]]))
         dec = matrix(0, nrow = 3, ncol = nsim)
         dec = foreach(a = 1:nsim, .combine = "cbind", .packages = c("rankCluster")) %dopar% {
            
            .GlobalEnv$sizes <- sizes
            .GlobalEnv$f2 <- f2
            .GlobalEnv$c_mat <- c_mat
            
            data_n = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = dist_type, corstruct = sets$corstruct, rho = sets$rho, params = list(mean = 0, sd = 1))
            w = rankCluster::weight_fun(data_n, type = sets$type)
            
            rankCluster::q_comb(sizes[[1]], data_n, p_null = 0.5, cont = c_mat, normal = F, theta = w$theta, psi = w$psi, alpha = 0.05)
            
            # c(rankCluster::q_wald(sizes[[1]], data_n, c_mat, w$theta, w$psi, alpha = 0.05)$reject,
            #   rankCluster::q_anova(sizes[[1]], data_n, c_mat, f2, w$theta, w$psi, alpha = 0.05)$reject,
            #   rankCluster::max_T(sizes[[1]], data_n, cont = c_mat, theta = w$theta, psi = w$psi, alpha = 0.05)$reject)
         }
         settings[i, which(colnames(settings) %in% c("wald", "anv", "maxt"))] = rowMeans(dec)
      }
      
      res_list[[1]] = settings
      
      # Pois 1
      settings = subset(samples, grp == 1)
      settings$type = "weighted"
      settings$rho = runif(1, 0.05, 0.35)
      settings$corstruct = "exchangeable"
      
      dist_type = "pois"
      dist_params = list(lambda = 5)
      
      settings$wald = 0
      settings$anv = 0
      settings$maxt = 0
      
      for(i in 1:nrow(settings)){
         sets = settings[i,]
         sizes = rankCluster::nm_gen(nn = sets$nn, n_i = sets$n_i, m_ij = sets$m_ij, each_s = sets$each_s, both_s = sets$both_s, identical_s = sets$identical_s, identical_c = sets$identical_c)
         f2 = sum(sizes[[1]]) - length(sizes[[1]])
         c_mat = multcomp::contrMat(sizes[[1]], type = c_type) %*% diag(1, length(sizes[[1]]))
         dec = matrix(0, nrow = 3, ncol = nsim)
         dec = foreach(a = 1:nsim, .combine = "cbind", .packages = c("rankCluster")) %dopar% {
            
            .GlobalEnv$sizes <- sizes
            .GlobalEnv$f2 <- f2
            .GlobalEnv$c_mat <- c_mat
            
            data_n = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = dist_type, corstruct = sets$corstruct, rho = sets$rho, params = list(mean = 0, sd = 1))
            w = rankCluster::weight_fun(data_n, type = sets$type)
            
            rankCluster::q_comb(sizes[[1]], data_n, p_null = 0.5, cont = c_mat, normal = F, theta = w$theta, psi = w$psi, alpha = 0.05)
            
            # c(rankCluster::q_wald(sizes[[1]], data_n, c_mat, w$theta, w$psi, alpha = 0.05)$reject,
            #   rankCluster::q_anova(sizes[[1]], data_n, c_mat, f2, w$theta, w$psi, alpha = 0.05)$reject,
            #   rankCluster::max_T(sizes[[1]], data_n, cont = c_mat, theta = w$theta, psi = w$psi, alpha = 0.05)$reject)
         }
         settings[i, which(colnames(settings) %in% c("wald", "anv", "maxt"))] = rowMeans(dec)
      }
      
      res_list[[2]] = settings
      
      # Beta 4
      settings = subset(samples, grp == 4)
      settings$type = "unweighted"
      settings$rho = 0
      settings$corstruct = "exchangeable"
      
      dist_type = "beta"
      dist_params = list(shape1 = 2, shape2 = 5)
      
      settings$wald = 0
      settings$anv = 0
      settings$maxt = 0
      
      for(i in 1:nrow(settings)){
         sets = settings[i,]
         sizes = rankCluster::nm_gen(nn = sets$nn, n_i = sets$n_i, m_ij = sets$m_ij, each_s = sets$each_s, both_s = sets$both_s, identical_s = sets$identical_s, identical_c = sets$identical_c)
         f2 = sum(sizes[[1]]) - length(sizes[[1]])
         c_mat = multcomp::contrMat(sizes[[1]], type = c_type) %*% diag(1, length(sizes[[1]]))
         dec = matrix(0, nrow = 3, ncol = nsim)
         dec = foreach(a = 1:nsim, .combine = "cbind", .packages = c("rankCluster")) %dopar% {
            
            .GlobalEnv$sizes <- sizes
            .GlobalEnv$f2 <- f2
            .GlobalEnv$c_mat <- c_mat
            
            data_n = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = dist_type, corstruct = sets$corstruct, rho = sets$rho, params = list(mean = 0, sd = 1))
            w = rankCluster::weight_fun(data_n, type = sets$type)
            
            rankCluster::q_comb(sizes[[1]], data_n, p_null = 0.5, cont = c_mat, normal = F, theta = w$theta, psi = w$psi, alpha = 0.05)
            
            # c(rankCluster::q_wald(sizes[[1]], data_n, c_mat, w$theta, w$psi, alpha = 0.05)$reject,
            #   rankCluster::q_anova(sizes[[1]], data_n, c_mat, f2, w$theta, w$psi, alpha = 0.05)$reject,
            #   rankCluster::max_T(sizes[[1]], data_n, cont = c_mat, theta = w$theta, psi = w$psi, alpha = 0.05)$reject)
         }
         settings[i, which(colnames(settings) %in% c("wald", "anv", "maxt"))] = rowMeans(dec)
      }
      
      res_list[[3]] = settings
      
      # Norm 5
      settings = subset(samples2, grp == 5)
      settings$type = "unweighted"
      settings$rho = runif(1, 0.05, 0.35)
      settings$corstruct = "exchangeable"
      
      dist_type = "norm"
      dist_params = list(mean = 0, sd = 1)
      
      settings$wald = 0
      settings$anv = 0
      settings$maxt = 0
      
      for(i in 1:nrow(settings)){
         sets = settings[i,]
         sizes = rankCluster::nm_gen(nn = sets$nn, n_i = sets$n_i, m_ij = sets$m_ij, each_s = sets$each_s, both_s = sets$both_s, identical_s = sets$identical_s, identical_c = sets$identical_c)
         f2 = sum(sizes[[1]]) - length(sizes[[1]])
         c_mat = multcomp::contrMat(sizes[[1]], type = c_type) %*% diag(1, length(sizes[[1]]))
         dec = matrix(0, nrow = 3, ncol = nsim)
         dec = foreach(a = 1:nsim, .combine = "cbind", .packages = c("rankCluster")) %dopar% {
            
            .GlobalEnv$sizes <- sizes
            .GlobalEnv$f2 <- f2
            .GlobalEnv$c_mat <- c_mat
            
            data_n = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = dist_type, corstruct = sets$corstruct, rho = sets$rho, params = list(mean = 0, sd = 1))
            w = rankCluster::weight_fun(data_n, type = sets$type)
            
            rankCluster::q_comb(sizes[[1]], data_n, p_null = 0.5, cont = c_mat, normal = F, theta = w$theta, psi = w$psi, alpha = 0.05)
            
            # c(rankCluster::q_wald(sizes[[1]], data_n, c_mat, w$theta, w$psi, alpha = 0.05)$reject,
            #   rankCluster::q_anova(sizes[[1]], data_n, c_mat, f2, w$theta, w$psi, alpha = 0.05)$reject,
            #   rankCluster::max_T(sizes[[1]], data_n, cont = c_mat, theta = w$theta, psi = w$psi, alpha = 0.05)$reject)
         }
         settings[i, which(colnames(settings) %in% c("wald", "anv", "maxt"))] = rowMeans(dec)
      }
      
      res_list[[4]] = settings
names(res_list) = c("Norm1", "Pois1", "Beta4", "Norm5")      
save(res_list, file = "results_t1_spec.RData")


# Type-II Error -----------------------------------------------------------
   # Norm 1
   pow_list_norm = list()
   settings = subset(samples, grp == 1)
   settings$type = "unweighted"
   settings$rho = runif(1, 0.35, 0.8)
   settings$corstruct = "exchangeable"
   
   dist_type = "norm"
   dist_params = list(mean = 0, sd = 1)
   
   settings$wald = 0
   settings$anv = 0
   settings$maxt = 0
   
   mu = c(0.25, 0.4, 0.5, 0.6, 0.75, 1)
   for(j in 1:length(mu)){
      param_norm = list(list(mean = 0, sd = 1), list(mean = mu[j], sd = 1))
      for(i in 1:nrow(nm)){
         sets = settings[i, ]
         sizes = rankCluster::nm_gen(nn = sets$nn, n_i = sets$n_i, m_ij = sets$m_ij, each_s = sets$each_s, both_s = sets$both_s, identical_s = sets$identical_s, identical_c = sets$identical_c)
         
         d = sets$nn
         par_list = param_norm
         if(d > 2) par_list = c(param_norm, rep(list(list(mean = 0, sd = 1)), (d-2)))
         
         c_mat = multcomp::contrMat(sizes[[1]], type = c_type) %*% diag(1, length(sizes[[1]]))
         #microbenchmark::microbenchmark(rankCluster::q_comb(sizes[[1]], data_n, p_null = 0.5, cont = c_mat, normal = F, theta = w$theta, psi = w$psi, alpha = 0.05))
         
         dec = foreach(a = 1:nsim, .combine = "cbind", .packages = c("rankCluster")) %dopar% {
            
            .GlobalEnv$sizes <- sizes
            .GlobalEnv$c_mat <- c_mat
            .GlobalEnv$mu <- mu
            .GlobalEnv$par_list <- par_list
            
            data_n = rankCluster::h_1_f(sizes[[1]], sizes[[2]], dist = dist_type, corstruct = sets$corstruct, rho = sets$rho, params = par_list)
            w = rankCluster::weight_fun(data_n, type = sets$type)
            
            rankCluster::q_comb(sizes[[1]], data_n, p_null = 0.5, cont = c_mat, normal = F, theta = w$theta, psi = w$psi, alpha = 0.05)
            
            # c(rankCluster::q_wald(sizes[[1]], data_n, c_mat, w$theta, w$psi, alpha = 0.05)$reject,
            #   rankCluster::q_anova(sizes[[1]], data_n, c_mat, f2, w$theta, w$psi, alpha = 0.05)$reject,
            #   rankCluster::max_T(sizes[[1]], data_n, cont = c_mat, theta = w$theta, psi = w$psi, alpha = 0.05)$reject)
         }
         settings[i, which(colnames(settings) %in% c("wald", "anv", "maxt"))] = rowMeans(dec)
      }
      pow_list_norm[[j]] = settings
   }
   
   
   # Pois 5
   pow_list_pois = list()
   settings = subset(samples2, grp == 5)
   settings$type = "unweighted"
   settings$rho = runif(1, 0.05, 0.35)
   settings$corstruct = "exchangeable"
   
   dist_type = "pois"
   dist_params = list(lambda = 5)
   
   settings$wald = 0
   settings$anv = 0
   settings$maxt = 0
   
   lambda = c(6:11)
   for(j in 1:length(mu)){
      param_pois = list(list(mean = 0, sd = 1), list(lambda = lambda[j]))
      for(i in 1:nrow(nm)){
         sets = settings[i, ]
         sizes = rankCluster::nm_gen(nn = sets$nn, n_i = sets$n_i, m_ij = sets$m_ij, each_s = sets$each_s, both_s = sets$both_s, identical_s = sets$identical_s, identical_c = sets$identical_c)
         
         d = sets$nn
         par_list = param_pois
         if(d > 2) par_list = c(param_pois, rep(list(list(lambda = 5)), (d-2)))
         
         c_mat = multcomp::contrMat(sizes[[1]], type = c_type) %*% diag(1, length(sizes[[1]]))
         #microbenchmark::microbenchmark(rankCluster::q_comb(sizes[[1]], data_n, p_null = 0.5, cont = c_mat, normal = F, theta = w$theta, psi = w$psi, alpha = 0.05))
         
         dec = foreach(a = 1:nsim, .combine = "cbind", .packages = c("rankCluster")) %dopar% {
            
            .GlobalEnv$sizes <- sizes
            .GlobalEnv$c_mat <- c_mat
            .GlobalEnv$mu <- mu
            .GlobalEnv$par_list <- par_list
            
            data_n = rankCluster::h_1_f(sizes[[1]], sizes[[2]], dist = dist_type, corstruct = sets$corstruct, rho = sets$rho, params = par_list)
            w = rankCluster::weight_fun(data_n, type = sets$type)
            
            rankCluster::q_comb(sizes[[1]], data_n, p_null = 0.5, cont = c_mat, normal = F, theta = w$theta, psi = w$psi, alpha = 0.05)
            
            # c(rankCluster::q_wald(sizes[[1]], data_n, c_mat, w$theta, w$psi, alpha = 0.05)$reject,
            #   rankCluster::q_anova(sizes[[1]], data_n, c_mat, f2, w$theta, w$psi, alpha = 0.05)$reject,
            #   rankCluster::max_T(sizes[[1]], data_n, cont = c_mat, theta = w$theta, psi = w$psi, alpha = 0.05)$reject)
         }
         settings[i, which(colnames(settings) %in% c("wald", "anv", "maxt"))] = rowMeans(dec)
      }
      pow_list_pois[[j]] = settings
   }
   
   save(pow_list_norm, pow_list_pois, file = "results_t2_spec.RData")
