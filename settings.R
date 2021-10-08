# number of Samples nn = 3, 5, 10, 25
# Identical Sample Size n_i = 3, 5, 10, 15, 25, 50, 100, 1000

# Identical Cluster Size m_ij = 3, 5, 10, 15, 20, 25, 40

# One Large, One Small Cluster m_ij = 10, 15, 20, 25; m_ik = 3; m_ih = 40

# One Large, One Small Sample n_i = 10, 15, 25, 50; n_j = 3; n_k = 150

# Identical Cluster Size m_ij = 3, 5, 10, 15, 20, 25, 40

# One Large, One Small Cluster m_ij = 10, 15, 20, 25; m_ik = 3; m_ih = 40

# number of samples
nn = c(3, 5, 10)
each_s = c(F, T)
both_s = c(F, T)

# Identical Sample Sizes --------------------------------------------------
    n_i = c(3, 5, 10, 15, 20, 25, 50, 100)

  # Identical Cluster Size

      m_ij = c(3, 5, 10, 15, 20, 25, 40)
      nm_1 = expand.grid(nn, n_i, m_ij)
      nm_1$each_s = F
      nm_1$both_s = F
      nm_1$identical_s = T
      nm_1$identical_c = T
      nm_1$grp = 1
      colnames(nm_1)[1:3] = c("nn", "n_i", "m_ij")

  # Large/Small Cluster
      m_ij = c(10, 15, 20, 25)
      nm_2 = expand.grid(nn, n_i, m_ij, each_s, both_s)
      nm_2$identical_s = T
      nm_2$identical_c = F
      colnames(nm_2)[1:5] = c("nn", "n_i", "m_ij", "each_s", "both_s")
      # Remove invalid condition
      nm_2 = nm_2[-which(which(nm_2$each_s == T) %in% which(nm_2$both_s == F)),]
      nm_2$grp = 2


# Large/Small Sample Size -------------------------------------------------

    n_i = c(10, 15, 25, 50)

  # Identical Cluster Size

      m_ij = c(3, 5, 10, 15, 20, 25, 40)
      nm_3 = expand.grid(nn, n_i, m_ij)
      nm_3$each_s = F
      nm_3$both_s = F
      nm_3$identical_s = F
      nm_3$identical_c = T
      nm_3$grp = 3
      colnames(nm_3)[1:3] = c("nn", "n_i", "m_ij")

  # Large/Small Cluster
      m_ij = c(10, 15, 20, 25)
      nm_4 = expand.grid(nn, n_i, m_ij, each_s, both_s)
      nm_4$identical_s = F
      nm_4$identical_c = F
      colnames(nm_4)[1:5] = c("nn", "n_i", "m_ij", "each_s", "both_s")
      nm_4 = nm_4[-which(which(nm_4$each_s == T) %in% which(nm_4$both_s == F)),]
      nm_4$grp = 4


# Bind --------------------------------------------------------------------

    samples = rbind(nm_1, nm_2, nm_3, nm_4)

rm(nm_1, nm_2, nm_3, nm_4, both_s, each_s, m_ij, n_i, nn)
