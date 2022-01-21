library(devtools)
install_github("spruenke/rankCluster")
library(rankCluster)
library(multcomp)
library(rankFD)
library(nparcomp)

# Eigenschaften für Datenerstellung <-> Unwichtig zum Lesen ---------------


nn = 3 # 3 Gruppen
n_i = 15 # Größenordnung Sample size je Gruppe (Abweichung durch Imbalancing)
m_ij = 4# 1 Observation je Cluster

each_s = T
both_s = T
identical_s = F
identical_c = T

# Erstellen der Sample- und Clustersizes
sizes = rankCluster::nm_gen2(nn, n_i, m_ij, each_s, both_s, identical_s, identical_c)

# Kontrastmatrix
c_mat = contrMat(sizes[[1]], type = "GrandMean") %*% diag(1, length(sizes[[1]]))
#c_mat = diag(1,3) - 1/3
rownames(c_mat) = NULL


# Daten wie in "Severely Imbalanced" --------------------------------------

nsim = 1e3
qw_vec = qa_vec = qw_test_vec = qa_test_vec = numeric(nsim)

for(i in 1:nsim){
    # Generieren von Daten
    dat = rankCluster::h_0_f(sizes[[1]], sizes[[2]], dist = "norm", corstruct = "independent", rho = 0, params = list(mean = 0, sd = 1))
    
    # Datensatz in ein Format, das rankFD lesen kann
    grp = rep(c("a", "b", "d"), times = lapply(sizes[[2]], length))
    dat_fd = data.frame(val = unlist(dat), grp = grp)
    
    # Teststatistiken und Freiheitsgrade 
    w = rankCluster::weight_fun(dat)
    rfd = rankFD::rankFD(val ~ grp, dat_fd, effect = "unweighted", hypothesis = "H0p", rounds = 10)
    p_hat = rankCluster::rel_eff(dat)
    qw = rankCluster::q_wald(sizes[[1]], dat, c_mat, type = "unweighted")
    qa = rankCluster::q_anova(sizes[[1]], dat, c_mat, type = "unweighted", f_2 = (sum(sizes[[1]]) - length(sizes[[1]])))
    qw_test = wald_debug(sizes[[1]], dat, w$theta, w$psi, c_mat)
    qa_test = anova_debug(sizes[[1]], dat, w$theta, w$psi, c_mat, f_2 = (sum(sizes[[1]]) - length(sizes[[1]])))
    qw_vec[i] = qw$Statistic
    qa_vec[i] = qa$Statistic
    qw_test_vec[i] = qw_test$Statistic
    qa_test_vec[i] = qa_test$Statistic

}

all(round(qw_vec, 10) == round(qw_test_vec, 10))
all(round(qa_vec, 10) == round(qa_test_vec, 10))