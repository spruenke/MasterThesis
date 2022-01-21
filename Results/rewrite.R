rm(list = ls())
date_code = format(Sys.Date(), "%d%m")
load(paste0("Results/unweighted_results_norm_", date_code, ".RData"))
results = final_results_norm

save(file = paste0("Results/unweighted_results_norm_", date_code, ".RData"), results)

load(paste0("Results/unweighted_results_beta_", date_code, ".RData"))
results = final_results_beta
save(file = paste0("Results/unweighted_results_beta_", date_code, ".RData"), results)

load(paste0("Results/unweighted_results_pois_", date_code, ".RData"))
results = final_results_pois
save(file = paste0("Results/unweighted_results_pois_", date_code, ".RData"), results)

load(paste0("Results/unweighted_results_binom_", date_code, ".RData"))
results = final_results_binom
save(file = paste0("Results/unweighted_results_binom_", date_code, ".RData"), results)