rm(list = ls())
library(car)
library(ggplot2)
library(ggpubr)
source("write_table.R")




# Norm 1 Sev Cor Unweighted -----------------------------------------------

load("results_norm1.RData")
dat_1 = as.data.frame(results_norm1)

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

p_dat_11 = ggarrange(p1, p2, p3, p4, ncol=1, nrow=4, common.legend = TRUE, legend="top") +
   theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))


pdf(file = "unweighted_norm_1_sev_cor.pdf", width = 10, height = 15)
print(p_dat_11)
dev.off()
sink(file = "norm_1.txt")
write_longtable(dat_1, 1)
sink()


# Pois 1 Mild Cor Weighted ------------------------------------------------------------------

load("results_pois1.RData")
dat_1 = as.data.frame(results_pois1)

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

p_dat_11 = ggarrange(p1, p2, p3, p4, ncol=1, nrow=4, common.legend = TRUE, legend="top") +
   theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))


pdf(file = "weighted_pois_1_mid_cor.pdf", width = 10, height = 15)
print(p_dat_11)
dev.off()
sink(file = "pois_1.txt")
write_longtable(dat_1, 1)
sink()


# Beta 2 No Cor Unweighted ------------------------------------------------

load("results_beta2.RData")
dat_1 = as.data.frame(results_beta2)

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

p_dat_11 = ggarrange(p1, p2, p3, p4, ncol=1, nrow=4, common.legend = TRUE, legend="top") +
   theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))


pdf(file = "unweighted_beta_2_no_cor.pdf", width = 10, height = 15)
print(p_dat_11)
dev.off()
sink(file = "beta_2.txt")
write_longtable(dat_1, 2)
sink()


# Beta 4 No Cor Unweighted ------------------------------------------------------------------

load("results_beta4.RData")
dat_1 = as.data.frame(results_beta4)

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

p_dat_11 = ggarrange(p1, p2, p3, p4, ncol=1, nrow=4, common.legend = TRUE, legend="top") +
   theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))


pdf(file = "unweighted_beta_4_no_cor.pdf", width = 10, height = 15)
print(p_dat_11)
dev.off()
sink(file = "beta_4.txt")
write_longtable(dat_1, 4)
sink()

# Norm 5 Mild Cor Unweighted ----------------------------------------------


load("results_norm5.RData")
dat_1 = as.data.frame(results_norm5)

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

p_dat_11 = ggarrange(p1, p2, p3, p4, ncol=1, nrow=4, common.legend = TRUE, legend="top") +
   theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))


pdf(file = "unweighted_norm_5_mid_cor.pdf", width = 10, height = 15)
print(p_dat_11)
dev.off()

sink(file = "norm_5.txt")
write_longtable(dat_1, 5)
sink()


# Power -------------------------------------------------------------------

   #-- Norm
   load("pow_list_norm.RData")
   pow_list_test = pow_list_norm
   load("pow_list_norm2.RData")
   pow_list_norm = rbind(pow_list_test, pow_list_norm)
   rm(pow_list_test)
   pow_norm_pic = list()
   for(i in 1:length(unique(pow_list_norm$mu))){
      dat = as.data.frame(pow_list_norm)
      dat_1 = subset(dat, mu == unique(dat$mu)[i])
      
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
         labs(title ="Power of Hypothesis Tests with 2 samples", x = "Clustersize", y = "Power") +
         theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      
      p2 = ggplot(data = subset(dat_11, nn == 3))  +
         geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
         #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
         scale_colour_discrete(name = "Sample Size") +
         scale_linetype_discrete(name = "Procedure") +
         labs(title ="Power of Hypothesis Tests with 3 samples", x = "Clustersize", y = "Power") +
         theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      
      p3 = ggplot(data = subset(dat_11, nn == 4))  +
         geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
         #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
         scale_colour_discrete(name = "Sample Size") +
         scale_linetype_discrete(name = "Procedure") +
         labs(title ="Power of Hypothesis Tests with 4 samples", x = "Clustersize", y = "Power") +
         theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      
      p4 = ggplot(data = subset(dat_11, nn == 5))  +
         geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
         #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
         scale_colour_discrete(name = "Sample Size") +
         scale_linetype_discrete(name = "Procedure") +
         labs(title ="Power of Hypothesis Tests with 5 samples", x = "Clustersize", y = "Power")
      
      p_dat_11 = ggarrange(p1, p2, p3, p4, ncol=1, nrow=4, common.legend = TRUE, legend="top") +
         theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      
      pow_norm_pic[[i]] = p_dat_11
      
      pdf(file = paste0("norm_1_sev_mu_", unique(dat$mu)[i], ".pdf"), width = 10, height = 15)
      print(p_dat_11)
      dev.off()
      
      sink(file = paste0("norm_1_sev_mu_", unique(dat$mu)[i], ".txt"))
      write_longtable(dat_1, 1)
      sink()
   }
   
   #-- Pois
   load("pow_list_pois.RData")
   pow_list_test = pow_list_pois
   load("pow_list_pois2.RData")
   pow_list_pois = rbind(pow_list_test, pow_list_pois)
   rm(pow_list_test)
   pow_pois_pic = list()
   for(i in 1:length(unique(pow_list_pois$lambda))){
      dat = as.data.frame(pow_list_pois)
      dat_1 = subset(dat, lambda == unique(dat$lambda)[i])
      
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
         labs(title ="Power of Hypothesis Tests with 2 samples", x = "Clustersize", y = "Power") +
         theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      
      p2 = ggplot(data = subset(dat_11, nn == 3))  +
         geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
         #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
         scale_colour_discrete(name = "Sample Size") +
         scale_linetype_discrete(name = "Procedure") +
         labs(title ="Power of Hypothesis Tests with 3 samples", x = "Clustersize", y = "Power") +
         theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      
      p3 = ggplot(data = subset(dat_11, nn == 4))  +
         geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
         #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
         scale_colour_discrete(name = "Sample Size") +
         scale_linetype_discrete(name = "Procedure") +
         labs(title ="Power of Hypothesis Tests with 4 samples", x = "Clustersize", y = "Power") +
         theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      
      p4 = ggplot(data = subset(dat_11, nn == 5))  +
         geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = typ), size = 1) +
         #geom_line(aes(x = m_ij, y = val, colour = as.factor(n_i), linetype = "dashed"))+
         scale_colour_discrete(name = "Sample Size") +
         scale_linetype_discrete(name = "Procedure") +
         labs(title ="Power of Hypothesis Tests with 5 samples", x = "Clustersize", y = "Power")
      
      p_dat_11 = ggarrange(p1, p2, p3, p4, ncol=1, nrow=4, common.legend = TRUE, legend="top") +
         theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      
      pow_pois_pic[[i]] = p_dat_11
      
      pdf(file = paste0("pois_5_mid_lambda_", unique(dat$lambda)[i], ".pdf"), width = 10, height = 15)
      print(p_dat_11)
      dev.off()
      
      sink(file = paste0("pois_5_mid_lambda_", unique(dat$lambda)[i], ".txt"))
      write_longtable(dat_1, 1)
      sink()
   }
   