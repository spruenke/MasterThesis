

f_psi = function(x, i, data, psi = NULL){
  m = numeric(length(data[[i]])) #empty vector for means of clusters
  # If psi is not provided create it
  if(is.null(psi)) psi = 1/length(data[[i]])
  res = numeric(length(x))
  if(length(x) > 1){
    for(b in 1:length(x)){
      for(j in 1:length(data[[i]])){
        m[j] =  (sum(data[[i]][[j]] < x[b]) + sum(0.5 * (data[[i]][[j]] == x[b]))) / length(data[[i]][[j]])
      }
      res[b] = sum(psi * m)
    }
  } else if(length(x) == 1) {
    for(j in 1:length(data[[i]])){
      m[j] =  (sum(data[[i]][[j]] < x) + sum(0.5 * (data[[i]][[j]] == x))) / length(data[[i]][[j]])
    }
    res = sum(psi * m)
  }

  return(res)
}



f_theta = function(x, data, theta = NULL, psi = NULL){
  if(is.null(theta)) theta = 1/length(data)
  if(is.null(psi)){
    psi = list()
    for(i in 1:length(data)){
      psi[[i]] = 1 / length(data[[i]])
    }
  }
  res = numeric(length(x))
  if(length(x) > 1){
      for(j in 1:length(x)){
        ab = numeric(length(data))
        for(i in 1:length(data)){
          ab[i] = f_psi(x[j], i, data, psi[[i]])
        }
        res[j] = sum(theta * ab)
      }
  } else if(length(x) == 1){
    ab = numeric(length(theta))
    for(i in 1:length(data)){
      ab[i] = f_psi(x, i, data, psi[[i]])
    }
    res = sum(theta * ab)
  }
  return(res)
}

rel_eff = function(data, theta = NULL, psi = NULL){
  p = numeric(length(data))
  if(is.null(psi)){
    psi = list()
    for(i in 1:length(data)){
        psi[[i]] = 1 / length(data[[i]])
    }
  }
  for(i in 1:length(data)){
      mm = numeric(length(data[[i]]))
      for(j in 1:length(data[[i]])){
        mm[j] = mean(f_theta(data[[i]][[j]], data, theta, psi))
      }
      p[i] = sum(psi[[i]] * mm)
  }
  return(p)
}


Y = function(a, b, s, data, psi){
      mean(f_psi(data[[a]][[b]], s, data, psi = psi))
}

kappa = function(psi, i, j){
  return(1 - 2*psi[[i]][j] + sum(psi[[i]]^2))
}

g = function(n){
  sum(sapply(n, length))
}

sigma_est = function(n, data, theta, psi = NULL){
    A = list()
    d = length(data)
    ind = c(1:d)
    if(is.null(psi)){
      psi = list()
      for(i in 1:length(data)){
        psi[[i]] = rep(length(data[[i]]), length(data[[i]]))^(-1)
      }
    }
    for(i in 1:d){
      A[[i]] = list()
      for(j in 1:n[[i]]){
        A[[i]][[j]] = numeric(d)
          for(s in 1:d){
            if(s == i){
              cc = which(ind == s)
              ind_new = ind[-cc]
              for(h in ind_new){
                A[[i]][[j]][s] = A[[i]][[j]][s] + theta[h]*mean(f_psi(data[[i]][[j]], h, data, psi = psi[[i]]))
              }
            } else if(s != i){
              A[[i]][[j]][s] = -1 * theta[i] * mean(f_psi(data[[i]][[j]], s, data, psi = psi[[i]]))
            }
          }
      }
    }

    A_bar = list()
    for(i in 1:d){
      A_bar[[i]] = numeric(d)
      for(s in 1:d){
        for(j in 1:length(A[[i]])){
            A_bar[[i]][s] = A_bar[[i]][s] + A[[i]][[j]][s] * psi[[i]][j]
        }
      }
    }

    sigma_list = list()
    for(i in 1:d){
      sigma_list[[i]] = list()
      for(j in 1:length(A[[i]])){
          sigma_list[[i]][[j]] = (A[[i]][[j]] - A_bar[[i]])%*%t(A[[i]][[j]] - A_bar[[i]])
      }
    }



    sigma = matrix(0, nrow = d, ncol = d)
    for(i in 1:d){
      for(j in 1:length(A[[i]])){
          sigma = sigma + sigma_list[[i]][[j]] * psi[[i]][j]^2 / kappa(psi, i, j)
      }
    }
    return(sigma * g(n))
}



