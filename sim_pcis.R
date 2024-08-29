rm(list = ls())
library(pci2s)
library(dplyr)


expit <- function(x) exp(x) / (1 + exp(x))

param_grid <- expand.grid(N = 2000,
                          buy = seq(0, 2, 0.5),
                          # bunc = c(0, 0.1, 1),
                          bunc = 0.5,
                          bay = c(0, 0.2))
NSIM <- 1000

sim_pcis_once <- function(N, buy, bunc, bay) {
  U <- runif(N)
  X <- runif(N)
  
  A <- rbinom(N, 1, expit(-3 + 5 * U + 1 * X))
  
  Y <- rexp(N, 0.2 + buy * U + 0.2 * X + bay * A) ## additive hazard: 0.2
  
  D <- as.numeric(Y < 5)
  Y[D == 0] <- 5
  
  ## make negative control variables
  Z1 <- rnorm(N, 2 * bunc * U + 0.5 * X, sd = 0.5)
  Z2 <- rnorm(N, bunc * U + 2 * X, sd = 0.2)
  W1 <- rnorm(N, bunc * U + 0.2 * X, sd = 0.1)
  W2 <- rnorm(N, 4 * bunc * U + X, sd = 0.25)
  Z <- cbind(Z1, Z2); W <- cbind(W1, W2)
  
  
  pciah_results <- p2sls.ah(Y = Y, D = D, A = A, X = X,
                           W = W, Z = Z)
  
  naive_ah_model <- lin_ah(time = Y, event = D, covariates = cbind(A, X),
                           variance = T)
  naive_est <- naive_ah_model$ESTIMATE[1]
  naive_se <- as.numeric(naive_ah_model$SE[1])
  
  nc_adjusted_model <- lin_ah(time = Y, event = D, covariates = cbind(A, X, W, Z),
                              variance = T)
  nc_adjusted_est <- nc_adjusted_model$ESTIMATE[1]
  nc_adjusted_se <- as.numeric(nc_adjusted_model$SE[1])
  
  return(c(pci_est = pciah_results$ESTIMATE[1],
           pci_se = as.numeric(pciah_results$SE[1]),
           naive_est = naive_est,
           naive_se = naive_se,
           nc_adjusted_est = nc_adjusted_est,
           nc_adjusted_se = nc_adjusted_se))
}


for (i in 1:nrow(param_grid)) {
  print(i)
  N <- param_grid$N[i]
  buy <- param_grid$buy[i]
  bunc <- param_grid$bunc[i]
  bay <- param_grid$bay[i]
  
  sim_results <- lapply(1:NSIM,
                        function(kk) {
                          print(sprintf("Scenario: %s; Replication: %s",
                                        i, kk))
                          set.seed(kk)
                          once_results <- try(sim_pcis_once(N, buy, bunc, bay),
                                              silent = T)
                          if (class(once_results) == "try-error") {
                            return(NA)
                          } else {
                            return(once_results)
                          }
                        })
  
  saveRDS(sim_results, 
          file = sprintf("results/sim_pciah_N_%s_buy_%s_bunc_%s_bay_%s.rds",
                         N, buy, bunc, bay))
}

