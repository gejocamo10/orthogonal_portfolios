setwd("C:/Users/geral/OneDrive - Universidad del Pacífico/Research/Academic/orthogonal_portfolios")
# setwd("C:/Users/gj.camposm/OneDrive - Universidad del Pacífico/Research/Academic/orthogonal_portfolios")

# library
library(tidyverse)
library(ggplot2)
library(mvtnorm)
library(gridExtra)


# functions
source("code/00_aer_paper_general.r")
source("code/00_aer_paper_rules_2.r")

# load data
eem  <- read.csv("data_clean/EEM.US_risk_ports_clean.csv")[, -1]
eemo <- read.csv("data_clean/EEMO.US_risk_ports_clean.csv")[, -1]
eemv <- read.csv("data_clean/EEMV.US_risk_ports_clean.csv")[, -1]
fem  <- read.csv("data_clean/FEM.US_risk_ports_clean.csv")[, -1]
ieem <- read.csv("data_clean/IEEM.LN_risk_ports_clean.csv")[, -1]
psrm <- read.csv("data_clean/PSRM.LN_risk_ports_clean.csv")[, -1]
# vwo  <- read.csv("data_clean/VWO.US_risk_ports_clean.csv")[, -1]

# etfs <- c("eem", "eemo", "eemv", "fem", "ieem", "psrm", "vwo")
etfs <- c("eem", "eemo", "eemv", "fem", "ieem", "psrm")
n_etfs <- length(etfs)

scale_factor <- 10^0
data_list <- list()
media_list <- list()
covariance_list <- list()
data_sim_list <- list() 

n <- 6
t <- 10^5
k<-10^3
for (i in 1:n_etfs){
  data_list[[i]] <- eval(parse(text = etfs[i]))
  media_list[[i]] <- mu_est(scale_factor * data_list[[i]])
  covariance_list[[i]] <- sigma_est(scale_factor * data_list[[i]])
  data_sim_loc <- rmvnorm(n = t,
                          mean = media_list[[i]],
                          sigma = covariance_list[[i]])
  
  indices <- rep(1:(t/k), each = k)
  
  data_sim_list_loc <- split(data_sim_loc, indices)
  
  data_sim_list[[i]] <- lapply(data_sim_list_loc, function(x) matrix(x, ncol = 6))
  }

# evaluate fixed rules in simulation
h_vector <- seq(100, 500, 50) # sensibilizar con varios h
gamma <- 3
etf_sim_rets <- list()
final_list <- list()
count <- 1

for (h in h_vector){ #h
  cat("we are in h =", h, "\n")
  results_etf <- NULL
  for (i in 1:n_etfs){ #etf
    results <- NULL
    for(ell in 1: 10^2){#sim
      data_sim_loc <- data_sim_list[[i]][[ell]]
      some_rets <- NULL
      for (j in h:(k - 1)){#ventana
        #cat(j,"\n")
        bracket <- (j - h + 1):(j)
        mu_loc      <- mu_est(data_sim_loc[bracket, ]) 
        sigma_loc   <- sigma_est(data_sim_loc[bracket, ]) 
        #
        # get rules wo corrections
        omega_kz_loc <- omega_kz_est(t = h, gamma = gamma,
                                     mu = mu_loc, sigma = sigma_loc)
        omega_q_loc   <- omega_q_est(t = h, gamma = gamma,
                                     mu = mu_loc, sigma = sigma_loc)
        omega_m_loc   <- omega_m_est(t = h, gamma = gamma,
                                     mu = mu_loc, sigma = sigma_loc)
        omega_qs_loc  <- omega_qs_est(t = h, gamma = gamma,
                                      mu = mu_loc, sigma = sigma_loc)
        omega_cbr_loc <- omega_cbr_est(t = h, gamma = gamma,
                                       mu = mu_loc, sigma = sigma_loc)
        # get rules with corrections
        omega_kz_wcor_loc <- omega_kz_est(t = h, gamma = gamma, mu = mu_loc,
                                          sigma = sigma_loc, theta_cor = TRUE)
        omega_q_wcor_loc   <- omega_q_est(t = h, gamma = gamma, mu = mu_loc,
                                          sigma = sigma_loc, theta_cor = TRUE)
        omega_m_wcor_loc   <- omega_m_est(t = h, gamma = gamma, mu = mu_loc,
                                          sigma = sigma_loc, theta_cor = TRUE)
        omega_qs_wcor_loc  <- omega_qs_est(t = h, gamma = gamma, mu = mu_loc,
                                           sigma = sigma_loc, theta_cor = TRUE)
        omega_cbr_wcor_loc <- omega_cbr_est(t = h, gamma = gamma, mu = mu_loc,
                                            sigma = sigma_loc, theta_cor = TRUE)
        # get utility for next point
        rets_loc <- c(
          t(omega_cbr_loc) %*% data_sim_loc[j + 1,],
          t(omega_qs_loc) %*% data_sim_loc[j + 1,],
          t(omega_kz_loc) %*% data_sim_loc[j + 1,],
          t(omega_m_loc) %*% data_sim_loc[j + 1,],
          t(omega_q_loc) %*% data_sim_loc[j + 1,],
          t(omega_cbr_wcor_loc) %*% data_sim_loc[j + 1,],
          t(omega_qs_wcor_loc) %*% data_sim_loc[j + 1,],
          t(omega_kz_wcor_loc) %*% data_sim_loc[j + 1,],
          t(omega_m_wcor_loc) %*% data_sim_loc[j + 1,],
          t(omega_q_wcor_loc) %*% data_sim_loc[j + 1,]
        )
        some_rets <- rbind(some_rets, rets_loc)
        }#cada ventana movil
      results <- rbind(results, apply(some_rets, 2, eu_est, gamma = 3))
      colnames(results) <- c("cbr", "qs", "kz", "m", "q", "cbr_wcor", "qs_wcor",
                             "kz_wcor", "m_wcor", "q_wcor")
      }#cada ell
    etf_sim_rets[[i]] <- results
    write.csv(etf_sim_rets[[i]], file =
                paste0("data_simulation/retornos_gauss_", etfs[i], "_", h,
                       "_portfolio_profits.csv"))
    results_etf <- rbind(results_etf, apply(results, 2, mean))
    
  }#cada etf
  final_list[[count]] <- results_etf
  write.csv(round(results_etf * 100, 3),
            file = paste("data_simulation/retornos_gauss_h_", h, ".csv", sep = ""),
            row.names = FALSE)
  count <- count + 1

  }# cada h

save.image(file = "code/environment_gauss.RData")