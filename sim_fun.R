library(multcomp)
library(doParallel)
library(foreach)
library(rankCluster)

sim_fun = function(nsim, dist_c, dist_params, samples, f_2 = NULL, c_type){

    # Prepare Parallelization -------------------------------------------------

    cores=detectCores()
    cl <- makeCluster(cores-1) #not to overload your computer

    registerDoParallel(cl)

      results = list()

    # No Correlation ----------------------------------------------------------

    settings = samples

    settings$rho  = 0
    settings$dist = dist_c
    settings$f_2  = 0
    settings$wald = 0
    settings$anv  = 0
    settings$maxt = 0

    for(z in 1:nrow(settings)){
        sets = settings[z,]
        sizes = nm_gen(nn = sets$nn, n_i = sets$n_i, m_ij = sets$m_ij, each_s = sets$each_s, both_s = sets$both_s, identical_s = sets$identical_s, identical_c = sets$identical_c)

        f2 = sum(sizes[[1]]) - length(sizes[[1]])

        c_mat = contrMat(sizes[[1]], type = c_type) %*% diag(1, length(sizes[[1]]))
        dec = matrix(0, nrow = 3, ncol = nsim)
        theta = rep(1/sets$nn, length(sizes[[1]]))
        #for(a in 1:nsim){
        dec = foreach(a = 1:nsim, .combine = "cbind", .packages = c("rankCluster")) %dopar% {
          data_n = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = dist_c, corstruct = "independent", rho = settings$rho[z], params = dist_params)
          #p_hat  = rel_eff(data_n)
          #sigma_hat = sigma_est(sizes[[1]], data_n, theta = theta, psi = NULL)
          # dec[1,a] = q_wald(sizes[[1]], data_n, c_mat, theta = theta, psi = NULL, alpha = 0.05)$reject
          # dec[2,a] = q_anova(sizes[[1]], data_n, c_mat, f2, theta = theta, psi = NULL, alpha = 0.05)$reject
          # dec[3,a] = max_T(data_n, p_null = 0.5, c_mat, sizes[[1]], normal = F, 0.05, theta = theta, psi = NULL)$reject
          c(rankCluster::q_wald(sizes[[1]], data_n, c_mat, theta = theta, psi = NULL, alpha = 0.05)$reject,
          rankCluster::q_anova(sizes[[1]], data_n, c_mat, f2, theta = theta, psi = NULL, alpha = 0.05)$reject,
          rankCluster::max_T(sizes[[1]], data_n, p_null = 0.5, c_mat, normal = F, 0.05, theta = theta, psi = NULL)$reject)
        }
        settings[z,which(colnames(settings) %in% c("wald", "anv", "maxt"))] = rowMeans(dec)
        settings$f_2[z] = f2
        print(paste0("No: ", z))
      }

    results[[1]] = settings


    # Mild Correlation --------------------------------------------------------

    settings = samples

    rho           = runif(1, 0.05, 0.35)
    settings$rho  = rho
    settings$dist = dist_c
    settings$f_2  = 0
    settings$wald = 0
    settings$anv  = 0
    settings$maxt = 0

    for(z in 1:nrow(settings)){
      sets = settings[z,]
      sizes = nm_gen(nn = sets$nn, n_i = sets$n_i, m_ij = sets$m_ij, each_s = sets$each_s, both_s = sets$both_s, identical_s = sets$identical_s, identical_c = sets$identical_c)

      f2 = sum(sizes[[1]]) - length(sizes[[1]])

      c_mat = contrMat(sizes[[1]], type = c_type) %*% diag(1, length(sizes[[1]]))
      dec = matrix(0, nrow = 3, ncol = nsim)
      theta = rep(1/sets$nn, length(sizes[[1]]))
      #for(a in 1:nsim){
      dec = foreach(a = 1:nsim, .combine = "cbind", .packages = c("rankCluster")) %dopar% {
        data_n = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = dist_c, corstruct = "exchangeable", rho = settings$rho[z], params = dist_params)
        #p_hat  = rel_eff(data_n)
        #sigma_hat = sigma_est(sizes[[1]], data_n, theta = theta, psi = NULL)
        # dec[1,a] = q_wald(sizes[[1]], data_n, c_mat, theta = theta, psi = NULL, alpha = 0.05)$reject
        # dec[2,a] = q_anova(sizes[[1]], data_n, c_mat, f2, theta = theta, psi = NULL, alpha = 0.05)$reject
        # dec[3,a] = max_T(data_n, p_null = 0.5, c_mat, sizes[[1]], normal = F, 0.05, theta = theta, psi = NULL)$reject
        c(q_wald(sizes[[1]], data_n, c_mat, theta = theta, psi = NULL, alpha = 0.05)$reject,
          q_anova(sizes[[1]], data_n, c_mat, f2, theta = theta, psi = NULL, alpha = 0.05)$reject,
          max_T(sizes[[1]], data_n, p_null = 0.5, c_mat, normal = F, 0.05, theta = theta, psi = NULL)$reject)
      }
      settings[,which(colnames(settings) %in% c("wald", "anv", "maxt"))] = rowMeans(dec)
      settings$f_2[z] = f2
      print(paste0("Mild ", z))
    }

    results[[2]] = settings

    # Severe Correlation ------------------------------------------------------


    settings = samples

    rho           = runif(1, 0.35, 0.8)
    settings$rho  = rho
    settings$dist = dist_c
    settings$f_2  = 0
    settings$wald = 0
    settings$anv  = 0
    settings$maxt = 0

    for(z in 1:nrow(settings)){
        sets = settings[z,]
        sizes = nm_gen(nn = sets$nn, n_i = sets$n_i, m_ij = sets$m_ij, each_s = sets$each_s, both_s = sets$both_s, identical_s = sets$identical_s, identical_c = sets$identical_c)

        f2 = sum(sizes[[1]]) - length(sizes[[1]])

        c_mat = contrMat(sizes[[1]], type = c_type) %*% diag(1, length(sizes[[1]]))
        dec = matrix(0, nrow = 3, ncol = nsim)
        theta = rep(1/sets$nn, length(sizes[[1]]))
        #for(a in 1:nsim){
        dec = foreach(a = 1:nsim, .combine = "cbind", .packages = c("rankCluster")) %dopar% {
          data_n = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = dist_c, corstruct = "exchangeable", rho = settings$rho[z], params = dist_params)
          #p_hat  = rel_eff(data_n)
          #sigma_hat = sigma_est(sizes[[1]], data_n, theta = theta, psi = NULL)
          # dec[1,a] = q_wald(sizes[[1]], data_n, c_mat, theta = theta, psi = NULL, alpha = 0.05)$reject
          # dec[2,a] = q_anova(sizes[[1]], data_n, c_mat, f2, theta = theta, psi = NULL, alpha = 0.05)$reject
          # dec[3,a] = max_T(data_n, p_null = 0.5, c_mat, sizes[[1]], normal = F, 0.05, theta = theta, psi = NULL)$reject
          c(q_wald(sizes[[1]], data_n, c_mat, theta = theta, psi = NULL, alpha = 0.05)$reject,
            q_anova(sizes[[1]], data_n, c_mat, f2, theta = theta, psi = NULL, alpha = 0.05)$reject,
            max_T(sizes[[1]], data_n, p_null = 0.5, c_mat, normal = F, 0.05, theta = theta, psi = NULL)$reject)
        }
        settings[,which(colnames(settings) %in% c("wald", "anv", "maxt"))] = rowMeans(dec)
        settings$f_2[z] = f2
        print(paste0("Severe: ", z))

      }

    results[[3]] = settings


    # Save --------------------------------------------------------------------

    names(results) = c("no_cor", "mild_cor", "sev_cor")
    save(results, file = paste0(dist_c, "_results.RData"))

    stopCluster(cl)

    return(results)

}



