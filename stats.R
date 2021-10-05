if(!("MASS" %in% installed.packages())) install.packages("MASS", dependencies = T)
if(!("mvtnorm" %in% installed.packages())) install.packages("mvtnorm", dependencies = T)
library(MASS)
library(mvtnorm)

Q_wald = function(p, Sigma, cont, n, alpha){
  stat = g(n) * t(p)%*%t(cont) %*% MASS::ginv(cont%*%Sigma%*%t(cont)) %*% cont %*% p
  df   = Matrix::rankMatrix(cont%*%Sigma)
  pv   = 1 - pchisq(stat, df)
  dec  = pv < alpha
  return(list(Statistic = stat, df = df, p.value = pv, reject = dec))
}

Q_anova = function(p, Sigma, cont, f_2, n, alpha){
  M    = t(cont)%*% MASS::ginv(cont%*%t(cont))%*%cont
  stat = g(n) / sum(diag(M%*%Sigma)) * t(p)%*%M%*%p
  df   = c(sum(diag(M%*%Sigma))^2 / sum(diag(M%*%Sigma%*%M%*%Sigma)), f_2)
  pv   = 1 - pf(stat, df[1], df[2])
  dec  = pv < alpha
  return(list(Statistic = stat, df = df, p.value = pv, reject = dec))
}

max_T  = function(p, Sigma, p_null = 0.5, cont, n, normal = FALSE, alpha){
  R = cov2cor(Sigma)
  R_c = cov2cor(cont%*%Sigma%*%t(cont))
  stat = sqrt(g(n)) * (p - p_null) / sqrt(diag(Sigma))
  if(normal == TRUE) crit = mvtnorm::qmvnorm(1-alpha, tail = "lower.tail", mean = rep(0, length(p)), corr = R)$quantile
  if(normal == FALSE) crit = mvtnorm::qmvt(1-alpha, tail = "lower.tail", df = g(n) - 1, corr = R)$quantile
  dec = max(abs(stat)) > crit
  return(list(Statistic = stat, df = g(n) - 1, reject = dec))
}
