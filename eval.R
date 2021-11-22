rm(list = ls())
library(car)
library(ggplot2)
library(ggpubr)

load("norm_results.RData")

# No Correlation
no_cor = results[[1]]

  # grp 1, 2, 3, 4
  no_cor_1 = subset(no_cor, grp == 1)
  val = c(no_cor_1$wald, no_cor_1$anv, no_cor_1$maxt)
  no_cor_11 = as.data.frame(rbind(no_cor_1[,c(1:3)], no_cor_1[,c(1:3)], no_cor_1[,c(1:3)]))
  no_cor_11$val = val
  no_cor_11$typ = rep(c("Wald", "ANOVA", "MCTP"), each = nrow(no_cor_1))
  # nn= 2, 3, 4, 5
  p1 = ggplot(data = subset(no_cor_11, nn == 2))  +
    geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
    #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
    scale_colour_discrete(name = "Sample Size") +
    scale_linetype_discrete(name = "Procedure") +
    labs(title ="Type-I Error for 2 samples", x = "Clustersize", y = "Type-I error")
  
  p2 = ggplot(data = subset(no_cor_11, nn == 3))  +
    geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
    #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
    scale_colour_discrete(name = "Sample Size") +
    scale_linetype_discrete(name = "Procedure") +
    labs(title ="Type-I Error for 3 samples", x = "Clustersize", y = "Type-I error")
  
  p3 = ggplot(data = subset(no_cor_11, nn == 4))  +
    geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
    #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
    scale_colour_discrete(name = "Sample Size") +
    scale_linetype_discrete(name = "Procedure") +
    labs(title ="Type-I Error for 4 samples", x = "Clustersize", y = "Type-I error")
  
  p4 = ggplot(data = subset(no_cor_11, nn == 5))  +
    geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
    #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
    scale_colour_discrete(name = "Sample Size") +
    scale_linetype_discrete(name = "Procedure") +
    labs(title ="Type-I Error for 5 samples", x = "Clustersize", y = "Type-I error")
  
  p_nocor_11 = ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="right")
  