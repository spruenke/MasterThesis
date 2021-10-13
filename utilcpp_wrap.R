library(Rcpp)
library(RcppArmadillo)
sourceCpp("util2.cpp")
# Pendant to f_psi

    f_psi_p = function(x, i, data, psi = NULL){
      #m = numeric(length(data[[i]])) #empty vector for means of clusters
      # If psi is not provided create it
      if(is.null(psi)) psi = rep(1/length(data[[i]]), length(data[[i]]))
      m = f_psi_cpp(x, data[[i]], psi)
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
      return(g(n) * sigma_est_cpp2(n, data, theta, psi))
    }

