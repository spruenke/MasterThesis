rm(list = ls())
library(car)
library(ggplot2)
library(ggpubr)


dists = c("norm", "pois", "beta", "binom")
grps = c(1:4)
res_sets = expand.grid(grps, dists, stringsAsFactors = F)
names_sets = expand.grid(c("no_cor", "mid_cor", "sev_cor"), grps, dists, stringsAsFactors = F)
names_list = paste0(names_sets[,3], "_", names_sets[,2], "_", names_sets[,1])
date_code = format(Sys.Date(), "%d%m")
#load("norm_results.RData")


# Weighted ----------------------------------------------------------------

res = c()
for(i in 1:nrow(res_sets)){
# grp 1, 2, 3, 4
  load(paste0("Results/weighted_", res_sets[i,2], "_results.RData"))
    A = list()
    for(j in 1:3){
      dat = results[[j]]
      dat_1 = subset(dat, grp == res_sets[i,1])
      val = c(dat_1$wald, dat_1$anv, dat_1$maxt)
      dat_11 = as.data.frame(rbind(dat_1[,c(1:3)], dat_1[,c(1:3)], dat_1[,c(1:3)]))
      dat_11$val = val
      dat_11$typ = rep(c("Wald", "ANOVA", "MCTP"), each = nrow(dat_1))
      # nn= 2, 3, 4, 5
      p1 = ggplot(data = subset(dat_11, nn == 2))  +
        geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
        #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
        scale_colour_discrete(name = "Sample Size") +
        scale_linetype_discrete(name = "Procedure") +
        labs(title ="Type-I Error of Hypothesis Tests with 2 samples", x = "Clustersize", y = "Type-I error") +
        theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      
      p2 = ggplot(data = subset(dat_11, nn == 3))  +
        geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
        #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
        scale_colour_discrete(name = "Sample Size") +
        scale_linetype_discrete(name = "Procedure") +
        labs(title ="Type-I Error of Hypothesis Tests with 3 samples", x = "Clustersize", y = "Type-I error") +
        theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      
      p3 = ggplot(data = subset(dat_11, nn == 4))  +
        geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
        #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
        scale_colour_discrete(name = "Sample Size") +
        scale_linetype_discrete(name = "Procedure") +
        labs(title ="Type-I Error of Hypothesis Tests with 4 samples", x = "Clustersize", y = "Type-I error") +
        theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      
      p4 = ggplot(data = subset(dat_11, nn == 5))  +
        geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
        #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
        scale_colour_discrete(name = "Sample Size") +
        scale_linetype_discrete(name = "Procedure") +
        labs(title ="Type-I Error of Hypothesis Tests with 5 samples", x = "Clustersize", y = "Type-I error")
      
      p_dat_11 = ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="right") +
        theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      A[[j]] = p_dat_11
      rm(p1, p2, p3, p4)
    }
  res = c(res, A)
}
names(res) = names_list

for(k in 1:length(res)){
  pdf(file = paste0("figures/weighted_", names_list[k], ".pdf"), width = 16, height = 9)
  print(res[[k]])
  dev.off()
}


# Unweighted --------------------------------------------------------------

res = c()
for(i in 1:nrow(res_sets)){
  # grp 1, 2, 3, 4
  load(paste0("Results/unweighted_results_", res_sets[i,2], "_", date_code, ".RData"))
  A = list()
  for(j in 1:3){
    dat = as.data.frame(results[[j]])
    dat_1 = subset(dat, grp == res_sets[i,1])
    val = c(dat_1$wald, dat_1$anv, dat_1$maxt)
    dat_11 = as.data.frame(rbind(dat_1[,c(1:3)], dat_1[,c(1:3)], dat_1[,c(1:3)]))
    dat_11$val = val
    dat_11$typ = rep(c("Wald", "ANOVA", "MCTP"), each = nrow(dat_1))
    # nn= 2, 3, 4, 5
    p1 = ggplot(data = subset(dat_11, nn == 2))  +
      geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
      #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
      scale_colour_discrete(name = "Sample Size") +
      scale_linetype_discrete(name = "Procedure") +
      labs(title ="Type-I Error of Hypothesis Tests with 2 samples", x = "Clustersize", y = "Type-I error") +
      theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 14), axis.title = element_text(size = 14))
    
    p2 = ggplot(data = subset(dat_11, nn == 3))  +
      geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
      #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
      scale_colour_discrete(name = "Sample Size") +
      scale_linetype_discrete(name = "Procedure") +
      labs(title ="Type-I Error of Hypothesis Tests with 3 samples", x = "Clustersize", y = "Type-I error") +
      theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 14), axis.title = element_text(size = 14))
    
    p3 = ggplot(data = subset(dat_11, nn == 4))  +
      geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
      #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
      scale_colour_discrete(name = "Sample Size") +
      scale_linetype_discrete(name = "Procedure") +
      labs(title ="Type-I Error of Hypothesis Tests with 4 samples", x = "Clustersize", y = "Type-I error") +
      theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 14), axis.title = element_text(size = 14))
    
    p4 = ggplot(data = subset(dat_11, nn == 5))  +
      geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
      #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
      scale_colour_discrete(name = "Sample Size") +
      scale_linetype_discrete(name = "Procedure") +
      labs(title ="Type-I Error of Hypothesis Tests with 5 samples", x = "Clustersize", y = "Type-I error") +
      theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 14), axis.title = element_text(size = 14))
    
    p_dat_11 = ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="right")
    A[[j]] = p_dat_11
    rm(p1, p2, p3, p4)
  }
  res = c(res, A)
}
names(res) = names_list

for(k in 1:length(res)){
  pdf(file = paste0("figures/unweighted_", names_list[k], ".pdf"), width = 16, height = 9)
  print(res[[k]])
  dev.off()
}


# Mild Unbalanced ---------------------------------------------------------

dists = c("norm", "pois", "beta", "binom")
grps = c(5:6)
res_sets = expand.grid(grps, dists, stringsAsFactors = F)
names_sets = expand.grid(c("no_cor", "mid_cor", "sev_cor"), grps, dists, stringsAsFactors = F)
names_list = paste0(names_sets[,3], "_", names_sets[,2], "_", names_sets[,1])
#load("norm_results.RData")


# Weighted ----------------------------------------------------------------

res2 = c()
for(i in 1:nrow(res_sets)){
  # grp 1, 2, 3, 4
  load(paste0("Results/weighted_", res_sets[i,2], "mildU_results.RData"))
  A = list()
  for(j in 1:3){
    dat = results[[j]]
    dat_1 = subset(dat, grp == res_sets[i,1])
    val = c(dat_1$wald, dat_1$anv, dat_1$maxt)
    dat_11 = as.data.frame(rbind(dat_1[,c(1:3)], dat_1[,c(1:3)], dat_1[,c(1:3)]))
    dat_11$val = val
    dat_11$typ = rep(c("Wald", "ANOVA", "MCTP"), each = nrow(dat_1))
    # nn= 2, 3, 4, 5
    p1 = ggplot(data = subset(dat_11, nn == 2))  +
      geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
      #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
      scale_colour_discrete(name = "Sample Size") +
      scale_linetype_discrete(name = "Procedure") +
      labs(title ="Type-I Error of Hypothesis Tests with 2 samples", x = "Clustersize", y = "Type-I error") +
      theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
    
    p2 = ggplot(data = subset(dat_11, nn == 3))  +
      geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
      #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
      scale_colour_discrete(name = "Sample Size") +
      scale_linetype_discrete(name = "Procedure") +
      labs(title ="Type-I Error of Hypothesis Tests with 3 samples", x = "Clustersize", y = "Type-I error") +
      theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
    
    p3 = ggplot(data = subset(dat_11, nn == 4))  +
      geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
      #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
      scale_colour_discrete(name = "Sample Size") +
      scale_linetype_discrete(name = "Procedure") +
      labs(title ="Type-I Error of Hypothesis Tests with 4 samples", x = "Clustersize", y = "Type-I error") +
      theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
    
    p4 = ggplot(data = subset(dat_11, nn == 5))  +
      geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
      #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
      scale_colour_discrete(name = "Sample Size") +
      scale_linetype_discrete(name = "Procedure") +
      labs(title ="Type-I Error of Hypothesis Tests with 5 samples", x = "Clustersize", y = "Type-I error") +
      theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
    
    p_dat_11 = ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="right")
    A[[j]] = p_dat_11
    rm(p1, p2, p3, p4)
  }
  res2 = c(res2, A)
}
names(res2) = names_list

for(k in 1:length(res)){
  pdf(file = paste0("figures/mildU_weighted_", names_list[k], ".pdf"), width = 16, height = 9)
  print(res2[[k]])
  dev.off()
}


# Unweighted --------------------------------------------------------------

res2 = c()
for(i in 1:nrow(res_sets)){
  # grp 1, 2, 3, 4
  load(paste0("Results/unweighted_", res_sets[i,2], "mildU_results.RData"))
  A = list()
  for(j in 1:3){
    dat = results[[j]]
    dat_1 = subset(dat, grp == res_sets[i,1])
    val = c(dat_1$wald, dat_1$anv, dat_1$maxt)
    dat_11 = as.data.frame(rbind(dat_1[,c(1:3)], dat_1[,c(1:3)], dat_1[,c(1:3)]))
    dat_11$val = val
    dat_11$typ = rep(c("Wald", "ANOVA", "MCTP"), each = nrow(dat_1))
    # nn= 2, 3, 4, 5
    p1 = ggplot(data = subset(dat_11, nn == 2))  +
      geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
      #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
      scale_colour_discrete(name = "Sample Size") +
      scale_linetype_discrete(name = "Procedure") +
      labs(title ="Type-I Error of Hypothesis Tests with 2 samples", x = "Clustersize", y = "Type-I error")
    
    p2 = ggplot(data = subset(dat_11, nn == 3))  +
      geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
      #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
      scale_colour_discrete(name = "Sample Size") +
      scale_linetype_discrete(name = "Procedure") +
      labs(title ="Type-I Error of Hypothesis Tests with 3 samples", x = "Clustersize", y = "Type-I error")
    
    p3 = ggplot(data = subset(dat_11, nn == 4))  +
      geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
      #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
      scale_colour_discrete(name = "Sample Size") +
      scale_linetype_discrete(name = "Procedure") +
      labs(title ="Type-I Error of Hypothesis Tests with 4 samples", x = "Clustersize", y = "Type-I error")
    
    p4 = ggplot(data = subset(dat_11, nn == 5))  +
      geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
      #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
      scale_colour_discrete(name = "Sample Size") +
      scale_linetype_discrete(name = "Procedure") +
      labs(title ="Type-I Error of Hypothesis Tests with 5 samples", x = "Clustersize", y = "Type-I error")
    
    p_dat_11 = ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="right")
    A[[j]] = p_dat_11
    rm(p1, p2, p3, p4)
  }
  res2 = c(res2, A)
}
names(res2) = names_list

for(k in 1:length(res)){
  pdf(file = paste0("figures/mildU_unweighted_", names_list[k], ".pdf"), width = 16, height = 9)
  print(res2[[k]])
  dev.off()
}
