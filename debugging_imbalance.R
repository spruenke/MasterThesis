library(devtools)
install_github("spruenke/rankCluster")
library(rankCluster)
library(multcomp)
library(rankFD)
library(nparcomp)

# Eigenschaften für Datenerstellung <-> Unwichtig zum Lesen ---------------


  nn = 2 # 3 Gruppen
  n_i = 2 # Größenordnung Sample size je Gruppe (Abweichung durch Imbalancing)
  m_ij = 1# 1 Observation je Cluster
  
  each_s = T
  both_s = T
  identical_s = T
  identical_c = T
  
  # Erstellen der Sample- und Clustersizes
  sizes = rankCluster::nm_gen2(nn, n_i, m_ij, each_s, both_s, identical_s, identical_c)
  
  # Kontrastmatrix
  c_mat = contrMat(sizes[[1]], type = "GrandMean") %*% diag(1, length(sizes[[1]]))
  #c_mat = diag(1,3) - 1/3
  rownames(c_mat) = NULL
  

# Daten wie in "Severely Imbalanced" --------------------------------------

  

  # Generieren von Daten
  dat = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = "norm", corstruct = "independent", rho = 0, params = list(mean = 0, sd = 1))

  # Datensatz in ein Format, das rankFD lesen kann
  grp = rep(c("a", "b", "d"), times = lapply(sizes[[2]], length))
  dat_fd = data.frame(val = unlist(dat), grp = grp)
  
  # Teststatistiken und Freiheitsgrade 
  rfd = rankFD::rankFD(val ~ grp, dat_fd, effect = "unweighted", hypothesis = "H0p", rounds = 10)
  p_hat = rankCluster::rel_eff(dat)
  qw = rankCluster::q_wald(sizes[[1]], dat, c_mat, type = "unweighted")
  qa = rankCluster::q_anova(sizes[[1]], dat, c_mat, type = "unweighted", f_2 = (sum(sizes[[1]]) - length(sizes[[1]])))

  npc = nparcomp::mctp(val ~ grp, dat_fd, "GrandMean", correlation = T)  
  
  # Abs. Differenzen zwischen rankFD und Masterarbeit (rankCluster)
  diff_sev = data.frame("Wald" = abs(rfd$Wald.Type.Statistic[1] - qw$Statistic),
             "ANOVA" = abs(rfd$ANOVA.Type.Statistic[1] - qa$Statistic),
             "ANOVA df_1" = abs(rfd$ANOVA.Type.Statistic[2] - qa$df[1]),
             "Rel.Eff" =  sum(abs(rfd$Descriptive$Rel.Effect - p_hat)))
  print(diff_sev)

# Daten wie in "Mildly Imbalanced" ----------------------------------------


  # Erstellen der Sample- und Clustersizes
  sizes = rankCluster::nm_gen2(nn, n_i, m_ij, each_s, both_s, identical_s, identical_c)
  
  # Generieren von Daten
  dat = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = "norm", corstruct = "independent", rho = 0, params = list(mean = 0, sd = 1))
  
  # Datensatz in ein Format, das rankFD lesen kann
  grp = rep(c("a", "b", "d"), times = lapply(sizes[[2]], length))
  dat_fd = data.frame(val = unlist(dat), grp = grp)
  
  # Teststatistiken und Freiheitsgrade 
  rfd = rankFD::rankFD(val ~ grp, dat_fd, effect = "unweighted", hypothesis = "H0p", rounds = 10)
  p_hat = rankCluster::rel_eff(dat)
  qw = rankCluster::q_wald(sizes[[1]], dat, c_mat, type = "unweighted")
  qa = rankCluster::q_anova(sizes[[1]], dat, c_mat, type = "unweighted", f_2 = (sum(sizes[[1]]) - length(sizes[[1]])))
  
  # Abs. Differenzen zwischen rankFD und Masterarbeit (rankCluster)
  diff_mild = data.frame("Wald" = abs(rfd$Wald.Type.Statistic[1] - qw$Statistic),
             "ANOVA" = abs(rfd$ANOVA.Type.Statistic[1] - qa$Statistic),
             "ANOVA df_1" = abs(rfd$ANOVA.Type.Statistic[2] - qa$df[1]),
             "Rel.Eff" =  sum(abs(rfd$Descriptive$Rel.Effect - p_hat)))
  
  print(diff_mild)
  

# Simulation d. Kovarianzmatrix für "Severely Imbalanced" -----------------

  # Erstellen der Sample- und Clustersizes
  sizes = rankCluster::nm_gen(nn, n_i, m_ij, each_s, both_s, identical_s, identical_c)
  
  n_sim = 1e4
  
  p_hat = matrix(0, nrow = 3, ncol = n_sim)
  var_hat = list()
  
  for(i in 1:n_sim){
    
    # Generieren von Daten
    dat = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = "norm", corstruct = "independent", rho = 0, params = list(mean = 0, sd = 1))
    p_hat[,i] = sqrt(rankCluster::g(sizes[[1]])) * rankCluster::rel_eff(dat) # Rel. Effekt multipliziert mit Inflationsterm
    var_hat[[i]] = rankCluster::sigma_est(sizes[[1]], dat) # Varianzmatrix
    
  }
  
  mean_var = Reduce("+", var_hat) / n_sim # Durchschnittliche Varianzmatrix
  emp_var  =  cov(t(p_hat)) # Empirische Kovarianzmatrix
  diff_var_sev = abs(mean_var - emp_var)
  
# Simulation d. Kovarianzmatrix für "Mildly Imbalanced" -----------------
  
  # Erstellen der Sample- und Clustersizes
  sizes = rankCluster::nm_gen2(nn, n_i, m_ij, each_s, both_s, identical_s, identical_c)
  
  n_sim = 1e4
  
  p_hat = matrix(0, nrow = 3, ncol = n_sim)
  var_hat = list()
  
  for(i in 1:n_sim){
    
    # Generieren von Daten
    dat = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = "norm", corstruct = "independent", rho = 0, params = list(mean = 0, sd = 1))
    p_hat[,i] = sqrt(rankCluster::g(sizes[[1]])) * rankCluster::rel_eff(dat) # Rel. Effekt multipliziert mit Inflationsterm
    var_hat[[i]] = rankCluster::sigma_est(sizes[[1]], dat) # Varianzmatrix
    
  }
  
  mean_var = Reduce("+", var_hat) / n_sim # Durchschn. Varianzmatrix
  emp_var =  cov(t(p_hat)) # Empirische Kovarianzmatrix
  diff_var_mild = abs(mean_var - emp_var)
  
  

# Alle Resultate ----------------------------------------------------------

  # Differenzen Teststatistiken, 1. Freiheitsgrad u. Rel. Effekt:
      # Mild Unbalanciert:
      print(diff_mild)
      # Stark Unbalanciert:
      print(diff_sev)
      
  # Differenzen Varianz-Matrix (Berechnung vs. Empirisch)
      # Mild Unbalanciert:
      print(diff_var_mild)
      # Stark Unbalanciert:
      print(diff_var_sev)
  
      
      

# Testdaten Frank ---------------------------------------------------------

load("testdata2.Rdata")
sizes2 = c(3, 3, 3)
dat_fd2 = dat_fd[c(1:3, 16:21),]
dat2 = list(as.list(dat_fd2[1:3,1]), as.list(dat_fd2[4:6,1]), as.list(dat_fd2[7:9,1]))

sigma_est(sizes2, dat2)
test_var_c = c_mat %*% sigma_est(sizes2, dat2) %*% t(c_mat) #/ g(sizes2) * sqrt(g(sizes2))
test_var_r = c_mat %*% sigma_est_r(sizes2, dat2, weight_fun(dat2)$theta, weight_fun(dat2)$psi) %*% t(c_mat) #/ g(sizes2) * sqrt(g(sizes2))
test_var_npc = nparcomp::mctp(val ~ grp, dat_fd, "GrandMean", correlation = T)$Covariance

w = rankCluster::weight_fun(dat)
rankCluster::sigma_est(sizes[[1]], dat, w$theta, w$psi)
sigma_est_r(sizes[[1]], dat, w$theta, w$psi)

phat = rankCluster::rel_eff(dat)
wald_debug(sizes[[1]], dat, w$theta, w$psi, c_mat)
anova_debug(sizes[[1]], dat, w$theta, w$psi, c_mat, 3)
rankCluster::q_wald(sizes[[1]], dat, c_mat, w$theta, w$psi)
g(sizes[[1]]) * t(phat) %*% t(c_mat) %*% MASS::ginv(c_mat %*% emp_var %*% t(c_mat)) %*% c_mat %*% phat
rfd$Wald.Type.Statistic

nsim = 1e3
diff_man = diff_pak = diff_anman = diff_anpak= numeric(nsim)
for(i in 1:nsim){
  # Generieren von Daten
  c_mat = contrMat(sizes[[1]], type = "GrandMean")
  dat = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = "norm", corstruct = "independent", rho = 0, params = list(mean = 0, sd = 1))
  w = rankCluster::weight_fun(dat)
  # Datensatz in ein Format, das rankFD lesen kann
  grp = rep(c("a", "b", "d"), times = lapply(sizes[[2]], length))
  dat_fd = data.frame(val = unlist(dat), grp = grp)
  
  # Teststatistiken und Freiheitsgrade 
  rfd = rankFD::rankFD(val ~ grp, dat_fd, effect = "unweighted", hypothesis = "H0p", rounds = 10) #$Wald.Type.Statistic[1]
  phat = rankCluster::rel_eff(dat)
  w_man = wald_debug(sizes[[1]], dat, w$theta, w$psi, c_mat)$Statistic
  w_pak = rankCluster::q_wald(sizes[[1]], dat, c_mat, w$theta, w$psi)$Statistic
  a_man = anova_debug(sizes[[1]], dat, w$theta, w$psi, c_mat, 3)$Statistic
  a_pak = rankCluster::q_anova(sizes[[1]], dat, c_mat, 3, w$theta, w$psi)$Statistic
  diff_man[i] = (rfd$Wald.Type.Statistic[1] - w_man)
  diff_pak[i] = (rfd$Wald.Type.Statistic[1] - w_pak)
  diff_anman[i] = (rfd$ANOVA.Type.Statistic[1] - a_man)
  diff_anpak[i] = (rfd$ANOVA.Type.Statistic[1] - a_pak)
  #w_emp = g(sizes[[1]]) * t(phat) %*% t(c_mat) %*% MASS::ginv(c_mat %*% emp_var %*% t(c_mat)) %*% c_mat %*% phat
}

rowMeans(rbind(diff_man, diff_pak, diff_anman, diff_anpak))

w = rankCluster::weight_fun(dat)
st = Sys.time()
d_w = d_a = r_w = r_a = logical(nsim)
P = matrix(0, ncol = nsim, nrow = length(sizes[[1]]))
c_mat = contrMat(sizes[[1]], type = "GrandMean")
for(i in 1:nsim){
  dat = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = "norm", corstruct = "independent", rho = 0, params = list(mean = 0, sd = 1))
  w = rankCluster::weight_fun(dat)
  # Datensatz in ein Format, das rankFD lesen kann
  #grp = rep(c("a", "b", "d"), times = lapply(sizes[[2]], length))
  #dat_fd = data.frame(val = unlist(dat), grp = grp)
  
  # Teststatistiken und Freiheitsgrade 
  #rfd = rankFD::rankFD(val ~ grp, dat_fd, effect = "unweighted", hypothesis = "H0p", rounds = 10)
  P[,i] = rankCluster::rel_eff(dat)
  w = rankCluster::weight_fun(dat)
  d_w[i] = wald_debug(sizes[[1]], dat, w$theta, w$psi, c_mat)$rej
  d_a[i] = anova_debug(sizes[[1]], dat, w$theta, w$psi, c_mat, f_2 = (g(sizes[[1]]) - length(sizes[[1]])))$rej
  #r_w[i] = rfd$Wald.Type.Statistic[3] < 0.05
  #r_a[i] = rfd$ANOVA.Type.Statistic[4] < 0.05
  print(i)
}
stt = Sys.time() - st

mean(d_w)
mean(d_a)
mean(r_w)
mean(r_a)
rowMeans(P)
stt

mean(diff_man)
mean(diff_pak)

nsim = 1e3
settings = samples2
res = matrix(0, nrow = nrow(settings), ncol = 3)
for(k in 1:nrow(settings)){
    d_w = d_a = d_t = logical(nsim)
    sets = settings[k,]
    for(i in 1:nsim){
      sizes = rankCluster::nm_gen2(nn = sets$nn, n_i = sets$n_i, m_ij = sets$m_ij, each_s = sets$each_s, both_s = sets$both_s, identical_s = sets$identical_s, identical_c = sets$identical_c)
      #sizes = rankCluster::nm_gen2(nn, n_i, m_ij, each_s, both_s, identical_s, identical_c)
      c_mat = contrMat(sizes[[1]], type = "Tukey")
      dat = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = "norm", corstruct = "exchangeable", rho = 0.4, params = list(mean = 0, sd = 1))
      w = rankCluster::weight_fun(dat)
      d_w[i] = rankCluster::q_wald(sizes[[1]], dat, c_mat, w$theta, w$psi)$rej
      d_a[i] = rankCluster::q_anova(sizes[[1]], dat, c_mat, w$theta, w$psi, f_2 = (g(sizes[[1]]) - length(sizes[[1]])))$rej
      d_t[i] = rankCluster::max_T(sizes[[1]], dat, cont = c_mat, theta = w$theta, psi = w$psi, alpha = 0.05)$rej
    }
    res[k,] = c(mean(d_w), mean(d_a), mean(d_t))
}
