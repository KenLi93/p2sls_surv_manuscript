rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

NSIM <- 1000
param_grid <- expand.grid(N = 2000,
                          buy = seq(0, 2, 0.5),
                          bunc = c(0, 0.1, 0.5, 1),
                          bay = c(0, 0.2)) %>%
  mutate(pci_bias = NA, pci_cover = NA, pci_ci_len = NA,
         naive_bias = NA, naive_cover = NA, naive_ci_len = NA,
         adjusted_bias = NA, adjusted_cover = NA, adjusted_ci_len = NA)

all_sim_results <- lapply(1:nrow(param_grid),
                          function(i) {
                            N <- param_grid$N[i]
                            buy <- param_grid$buy[i]
                            bunc <- param_grid$bunc[i]
                            bay <- param_grid$bay[i]
                            sim_results <- readRDS(sprintf("results/sim_pciah_N_%s_buy_%s_bunc_%s_bay_%s.rds",
                                                           N, buy, bunc, bay)) %>%
                              bind_rows() %>%
                              mutate(id = 1:NSIM)
                            
                            all_est_df <- sim_results %>%
                              select(id, pci_est, naive_est, nc_adjusted_est) %>%
                              pivot_longer(cols = c(pci_est, naive_est, nc_adjusted_est),
                                           names_to = "method",
                                           values_to = "est") %>%
                              mutate(method = factor(method, 
                                                     levels = c("pci_est", "naive_est", "nc_adjusted_est"),
                                                     labels = c("PCI", "Naive", "Fully adjusted"))) 
                            
                            all_se_df <- sim_results %>%
                              select(id, pci_se, naive_se, nc_adjusted_se) %>%
                              pivot_longer(cols = c(pci_se, naive_se, nc_adjusted_se),
                                           names_to = "method",
                                           values_to = "se") %>%
                              mutate(method = factor(method, 
                                                     levels = c("pci_se", "naive_se", "nc_adjusted_se"),
                                                     labels = c("PCI", "Naive", "Fully adjusted"))) 
                            
                            results_df <- left_join(all_est_df, all_se_df, by = c("id", "method")) %>%
                              mutate(N = N, bunc = bunc, buy = buy, bay = bay) %>%
                              mutate(buy = factor(buy, levels = seq(0, 2, 0.5),
                                                  labels = seq(0, 2, 0.5)),
                                     bias = est - bay,
                                     cover = as.numeric(est - qnorm(0.975) * se < bay &
                                                          est + qnorm(0.975) * se > bay))
                            
                            return(results_df)
                          }) %>%
  bind_rows()

method_palette <- c("#D92321","#1749FF", "#FF6F1B","#810094","#378252","#FF5EBF","#3700A5")
for (ibunc in c(0, 0.1, 0.5, 1)) {
  for (ibay in c(0, 0.2)) {
    nc_label <- ifelse(ibunc == 0, "Irrelevant NC",
                       ifelse(ibunc == 0.1, "Weak NC", "Valid NC"))
    plotdata <- all_sim_results %>% 
      filter(bunc == ibunc, bay == ibay) 
    bias_plot <- 
      ggplot(data = plotdata,
             aes(x = buy, y = bias, color = method)) +
      geom_boxplot() +
      # ggtitle(bquote(.(nc_label) ~ ", " ~ beta[A] == .(ibay))) +
      ggtitle("") +
      theme_pubr() +
      scale_color_manual(values = method_palette) +
      guides(color = "none") +
      xlab(expression(beta[U])) + ylab("Bias") +
      theme(panel.border = element_rect(colour = "black", fill=NA),
            panel.grid.major.y = element_line(),
            panel.grid.minor.y = element_line(),
            plot.title = element_text(hjust = 0.5, size = 18),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 18)) 
    ggsave(filename = sprintf("plots/bias_bunc_%s_bay_%s.png",
                              ibunc * 10, ibay * 10),
           plot = bias_plot,
           width = 5.5, height = 4.5)
    
    
    coverage_data <- plotdata %>%
      group_by(method, buy) %>%
      summarise(cover_rate = mean(cover))
    
    coverage_plot <- 
      ggplot(data = coverage_data,
             aes(x = buy, y = cover_rate, fill = method)) +
      geom_bar(stat = "identity",
               position = position_dodge()) +
      ylim(0, 1) +
      geom_hline(yintercept = 0.95, linetype = 2, linewidth = 1) +
      # ggtitle(bquote(.(nc_label) ~ ", " ~ beta[A] == .(ibay))) +
      ggtitle("") +
      theme_pubr() +
      scale_fill_manual(values = method_palette) +
      guides(fill = "none") +
      xlab(expression(beta[U])) + ylab("Coverage of 95% CI") +
      theme(panel.border = element_rect(colour = "black", fill=NA),
            panel.grid.major.y = element_line(),
            panel.grid.minor.y = element_line(),
            plot.title = element_text(hjust = 0.5, size = 18),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 18)) 
    
    ggsave(filename = sprintf("plots/coverage_bunc_%s_bay_%s.png",
                              ibunc * 10, ibay * 10),
           plot = coverage_plot,
           width = 5.5, height = 4.5)
    
  }
}

png("legend_simulation.png", width = 950, height = 50)
par(mar = c(0, 0, 0, 0))
plot.new()
legend(x = 0.5, y = 0.5, xjust = 0.5, yjust = 0.5,
       legend = c("P2SLS-Surv", "Naïve", "Fully adjusted"),
       fill = method_palette[1:3],
       cex = 3, ncol = 3, bty = "n")

dev.off()
