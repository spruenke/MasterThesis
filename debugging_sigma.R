y_abc = function(x_ab, c, data){
  subdata = data[[c]]
  Fcx = numeric(length(x_ab))
  for(k in 1:length(x_ab)){
    x_k = x_ab[k]
    for(i in 1:length(subdata)){
      subsubdata = subdata[[i]]
      Fcx[k] = Fcx[k] + ((length(which(subsubdata < x_k)) + 0.5 * length(which(subsubdata == x_k))) / length(subsubdata))
    }
    Fcx[k] = Fcx[k] / length(subdata)
  
  }
  return(mean(Fcx))
}

kappa_r = function(psi, i, j){
  return( 1 - 2*psi[[i]][j] + sum(psi[[i]]^2))
}

g = function(n){
  sum(n)
}

sigma_est_r = function(n, data, theta, psi){
  A_i_list = list()
  for(i in 1:length(data)){
    A_ij_list = list()
   
      for(j in 1:length(data[[i]])){
      A_ij = numeric(length(data))
      for(h in 1:length(data)){
       
          if(h == i){
            ### Create vectors of necessary Y's and theta's
            ind = c(1:length(data))[-i]
            y = numeric(length(ind))
            for(s in ind){
              y[s] = y_abc(data[[i]][[j]], s, data) * theta[s]
            }
            A_ij[h] = sum(y)
            
          } else if (h != i){
            A_ij[h] = -1 * theta[i] * y_abc(data[[i]][[j]], h, data) 
          }
         
      }
        A_ij_list[[j]] = A_ij
      
    }
    #A_ibar[[i]] = rowMeans(do.call("cbind", A_ij_list)) # Mean of A_ij's <=> Careful: Potentially weighted mean with psi instead
    A_i_list[[i]] = A_ij_list
  }
  
  #####################################
  ##### Berechnung von den Sigmas #####
  
  sigma = matrix(0, length(data), length(data))
  for(i in 1:length(data)){
    A_imat = do.call("cbind", A_i_list[[i]])
    A_ibar = numeric(length(data))
    for(zz in 1:ncol(A_imat)){
        A_ibar = A_ibar + psi[[i]][zz] * A_imat[,zz]
    }
    for(j in 1:length(data[[i]])){
      sigma = sigma + ((A_i_list[[i]][[j]] - A_ibar) %*% t(A_i_list[[i]][[j]] - A_ibar)) /  (kappa_r(psi, i, j))  * psi[[i]][j]^2 
    }
  }
  return( sigma * g(n))
}

wald_debug = function(n, data, theta, psi, c_mat, alpha = 0.05){
  phat = rankCluster::rel_eff(data, theta, psi)
  sigma = sigma_est_r(n, data, theta, psi)
  stat = (t(phat) %*% t(c_mat) %*% MASS::ginv(c_mat %*% sigma %*% t(c_mat)) %*% c_mat %*% phat)  * g(sizes[[1]])
  df = Matrix::rankMatrix(c_mat %*% sigma)
  pv   = 1 - pchisq(stat, df)
  dec  = pv < alpha
  return(list("rej" = dec, "Statistic" = stat))
  #return(stat)
}

anova_debug = function(n, data, theta, psi, c_mat, f_2, alpha = 0.05){
  phat = rankCluster::rel_eff(data, theta, psi)
  sigma = sigma_est_r(n, data, theta, psi)
  M = t(c_mat) %*% MASS::ginv(c_mat %*% t(c_mat)) %*% c_mat
  nen = sum(diag(M %*% sigma))
  stat = t(phat) %*% M %*% phat / nen * g(n)
 df_1  = sum(diag(M * sigma))^2 / sum(diag(M * sigma * M * sigma))
 df   = c(df_1, f_2)
 crit = qf(1-alpha, df[1], df[2])
 pv   = 1 - pf(stat, df[1], df[2])
 dec = stat > crit
 return(list("rej" = dec, "Statistic" = stat))
  #return(stat)
}
