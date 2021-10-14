library(Rcpp)
library(RcppArmadillo)
sourceCpp("util2.cpp")
sourceCpp("util_arma.cpp")
# Pendant to f_psi

    f_psi_p = function(x, i, data, psi = NULL){
      #m = numeric(length(data[[i]])) #empty vector for means of clusters
      # If psi is not provided create it
      if(is.null(psi)) psi = rep(1/length(data[[i]]), length(data[[i]]))
      m = f_psi_cpp(x, data[[i]], psi)
      return(m)
    }

    f_psi_p2 = function(x, i, data, psi = NULL){
      #m = numeric(length(data[[i]])) #empty vector for means of clusters
      # If psi is not provided create it
      if(is.null(psi)) psi = rep(1/length(data[[i]]), length(data[[i]]))
      m = f_psi_cpp3(x, data[[i]], psi)
      return(m)
    }

    f_theta_p = function(x, data, theta = NULL, psi = NULL){
      if(is.null(theta)) theta = rep(1/length(data), length(data))
      if(is.null(psi)){
        psi = list()
        for(i in 1:length(data)){
          psi[[i]] = rep(1 / length(data[[i]]), length(data[[i]]))
        }
      }
      res = f_theta_cpp(x, data, theta, psi)
      return(res)
    }

    f_theta_p2 = function(x, data, theta = NULL, psi = NULL){
      if(is.null(theta)) theta = rep(1/length(data), length(data))
      if(is.null(psi)){
        psi = list()
        for(i in 1:length(data)){
          psi[[i]] = rep(1 / length(data[[i]]), length(data[[i]]))
        }
      }
      res = f_theta_cpp2(x, data, theta, psi)
      return(c(res))
    }

    rel_eff_p = function(data, theta = NULL, psi = NULL){
      if(is.null(psi)){
        psi = list()
        for(i in 1:length(data)){
          psi[[i]] = rep(1 / length(data[[i]]), length(data[[i]]))
        }
      }
      if(is.null(theta)) theta = rep(1/length(data), length(data))
      return (rel_eff_cpp(data, theta, psi))
    }

    rel_eff_p2 = function(data, theta = NULL, psi = NULL){
      if(is.null(psi)){
        psi = list()
        for(i in 1:length(data)){
          psi[[i]] = rep(1 / length(data[[i]]), length(data[[i]]))
        }
      }
      if(is.null(theta)) theta = rep(1/length(data), length(data))
      return (c(rel_eff_cpp2(data, theta, psi)))
    }

    g = function(n){
      sum(sapply(n, length))
    }

    # sigma_est_p = function(n, data, theta = NULL, psi = NULL){
    #   if(is.null(psi)){
    #     psi = list()
    #     for(i in 1:length(data)){
    #       psi[[i]] = rep(1 / length(data[[i]]), length(data[[i]]))
    #     }
    #   }
    #   if(is.null(theta)) theta = rep(1/length(data), length(data))
    #   return(g(n) * sigma_est_cpp(n, data, theta, psi))
    # }

    sigma_est_p = function(n, data, theta = NULL, psi = NULL){
      if(is.null(psi)){
        psi = list()
        for(i in 1:length(data)){
          psi[[i]] = rep(1 / length(data[[i]]), length(data[[i]]))
        }
      }
      if(is.null(theta)) theta = rep(1/length(data), length(data))
      return( sigma_est_cpp2(n, data, theta, psi))
    }

    sigma_est_p2 = function(n, data, theta = NULL, psi = NULL){
      if(is.null(psi)){
        psi = list()
        for(i in 1:length(data)){
          psi[[i]] = rep(1 / length(data[[i]]), length(data[[i]]))
        }
      }
      if(is.null(theta)) theta = rep(1/length(data), length(data))
      return( g(n) * sigma_est_cpp3(n, data, theta, psi))
    }


    q_wald = function(n, data, cont, theta = NULL, psi = NULL, alpha = 0.05){
      if(is.null(psi)){
        psi = list()
        for(i in 1:length(data)){
          psi[[i]] = rep(1 / length(data[[i]]), length(data[[i]]))
        }
      }
      if(is.null(theta)) theta = rep(1/length(data), length(data))
      st   = q_wald_arma(n, data, theta, psi, cont)
      stat = g(n) * st[[1]]
      #df   = Matrix::rankMatrix(cont%*%Sigma)
      df   = st[[2]]
      pv   = 1 - pchisq(stat, df)
      dec  = pv < alpha
      return(list(Statistic = stat, df = df, p.value = pv, reject = dec))
    }

    q_anova = function(n, data, cont, f_2, theta = NULL, psi = NULL, alpha = 0.05){
      if(is.null(psi)){
        psi = list()
        for(i in 1:length(data)){
          psi[[i]] = rep(1 / length(data[[i]]), length(data[[i]]))
        }
      }
      if(is.null(theta)) theta = rep(1/length(data), length(data))
      st   = q_anova_arma(n, data, theta, psi, cont)
      stat = g(n) * st[[1]]
      #df   = c(sum(diag(M%*%Sigma))^2 / sum(diag(M%*%Sigma%*%M%*%Sigma)), f_2)
      df   = c(st[[2]], f_2)
      pv   = 1 - pf(stat, df[1], df[2])
      dec  = pv < alpha
      return(list(Statistic = stat, df = df, p.value = pv, reject = dec, nen = st[[3]]))
    }
