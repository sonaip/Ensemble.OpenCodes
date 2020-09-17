
library(tidyverse)
library(cowplot)


#---- Read and prepare the data ----

data <- readRDS('rrawData.rds')
dur <- data$conds
data_e1 <- data$exp1
data_e2 <- data$exp2 

means <- group_by(dur, sequence) %>% summarize(ari_mean=mean(duration), 
                                               geom_mean=(prod(duration))^(1/5),
                                               harm_mean=1/mean(1/duration),
                                               weight_mean=sum(duration/sum(duration)*duration))

dat_e1_means <- rowMeans(matrix(data_e1$alpha, 3, 16))
dat_e2_means <- rowMeans(matrix(data_e2$alpha, 3, 15))
dat_e1_se <- apply(matrix(data_e1$alpha, 3, 16), 1, sd) / sqrt(15)
dat_e2_se <- apply(matrix(data_e2$alpha, 3, 15), 1, sd) / sqrt(15)

# ---- Average data model fits ----

# Arithmetic mean
ari_simulation <- function(par, N=100000) {
  amean <- matrix(rep(0,3*N), 3,N)
  for(i in 1:3) {
    durations <- filter(dur, sequence==i)$duration
    noisy_dur <- matrix(durations + rnorm(5*N, 0, par[1]*durations), 5, N)
    amean[i,] <- colMeans(noisy_dur) + rnorm(N, par[2], par[3])
  }
  return(amean)
}

# Arithmetic mean fit helper functions
ari_sim_fit_e1 <- function(par) {
  return(sum((rowMeans(ari_simulation(par)-dat_e1_means)^2)))
}

ari_sim_fit_e2 <- function(par) {
  return(sum((rowMeans(ari_simulation(par)-dat_e2_means)^2)))
}

# I think the result of optimization here is no better than the simple solution below (other
# than a possible slight improvement by random chance since it is a stochastic simulation)
#par_a_e1 <- optim(c(0.1, mean(dat_e1_means)-mean(means$ari_mean), 0), ari_sim_fit_e1)
#par_a_e2 <- optim(c(0.1, mean(dat_e2_means)-mean(means$ari_mean), 0), ari_sim_fit_e2)

# Calculate the predictions of the best fit of the arithmetic mean
ameans_e1 <- rowMeans(ari_simulation(c(0, mean(dat_e1_means)-mean(means$ari_mean), 0)))
ameans_e2 <- rowMeans(ari_simulation(c(0, mean(dat_e2_means)-mean(means$ari_mean), 0)))

# Grid search to confirm that solution is optimal
# afitval <- array(0, dim=c(11,11,11))
# 
# p1 <- seq(0, 0.5, 0.05)
# p2 <- seq(-100, 0, 10)
# p3 <- seq(0, 100, 10)
# for(i in 1:length(p1)) {
#   for(j in 1:length(p2)) {
#     for(k in 1:length(p3)) {
#       afitval[i,j,k] <- ari_sim_fit_e1(c(p1[i], p2[j], p3[k]))
#     }
#   }
# }

# Geometric mean version 1
geom_simulation <- function(par, N=100000) {
  gmean <- matrix(rep(0,3*N), 3,N)
  for(i in 1:3) {
    durations <- filter(dur, sequence==i)$duration
    noisy_dur <- matrix(log(durations) + rnorm(5*N, 0, par[1]), 5, N)
    gmean[i,] <- exp(colMeans(noisy_dur)) + rnorm(N, par[2], par[3])
  }
  return(gmean)
}

# Geometric mean fit helper functions
geom_sim_fit_e1 <- function(par) {
  return(sum((rowMeans(geom_simulation(par)-dat_e1_means)^2)))
}

geom_sim_fit_e2 <- function(par) {
  return(sum((rowMeans(geom_simulation(par)-dat_e2_means)^2)))
}

# Again, I think the result of optimization here is no better than the simple solution below
#par_g_e1 <- optim(c(0.05, mean(dat_e1_means)-mean(means$ari_mean), 0), geom_sim_fit_e1)
#par_g_e2 <- optim(c(0.05, mean(dat_e1_means)-mean(means$ari_mean), 0), geom_sim_fit_e2)

# Calculate the predictions of the best fit of the geometric mean
gmeans_e1 <- rowMeans(geom_simulation(c(0.1, mean(dat_e1_means)-mean(means$geom_mean), 0)))
gmeans_e2 <- rowMeans(geom_simulation(c(0.1, mean(dat_e2_means)-mean(means$geom_mean), 0)))

# Geometric mean version 2
geom_simulation_v2 <- function(par, N=100000) {
  gmean <- matrix(rep(0,3*N), 3,N)
  for(i in 1:3) {
    durations <- filter(dur, sequence==i)$duration
    noisy_dur <- matrix(log(durations) + rnorm(5*N, par[2], par[1]), 5, N)
    gmean[i,] <- exp(colMeans(noisy_dur))
  }
  return(gmean)
}

# Geometric mean v2 fit helper functions
geom_sim_fit_v2_e1 <- function(par) {
  return(sum((rowMeans(geom_simulation_v2(par)-dat_e1_means)^2)))
}

geom_sim_fit_v2_e2 <- function(par) {
  return(sum((rowMeans(geom_simulation_v2(par)-dat_e2_means)^2)))
}

# Again, I think the result of optimization here is no better than the simple solution below
#par_g_e1 <- optim(c(0.05, log(mean(dat_e1_means)/mean(means$geom_mean)), 0), geom_sim_fit_v2_e1)
#par_g_e2 <- optim(c(0.05, log(mean(dat_e2_means)/mean(means$geom_mean)), 0), geom_sim_fit_v2_e2)

#gmeans_v2_e1 <- rowMeans(geom_simulation_v2(c(0.05, log(mean(dat_e1_means)/mean(means$geom_mean)), 0)))
#gmeans_v2_e2 <- rowMeans(geom_simulation_v2(c(0.05, log(mean(dat_e2_means)/mean(means$geom_mean)), 0)))

# Grid search to confirm that solution is optimal
# gfitval <- array(0, dim=c(11,11,11))
# 
# p1 <- seq(0, 0.1, 0.01)
# p2 <- seq(-0.1, 0, 0.01)
# p3 <- seq(0, 1, 0.1)
# for(i in 1:length(p1)) {
#   for(j in 1:length(p2)) {
#     for(k in 1:length(p3)) {
#       gfitval[i,j,k] <- geom_sim_fit_v2_e1(c(p1[i], p2[j], p3[k]))
#     }
#   }
# }

# Geometric mean v2 fit helper functions with no noise
geom_sim_nonoise_fit_v2_e1 <- function(par) {
  return(sum((rowMeans(geom_simulation_v2(c(0,par,0))-dat_e1_means)^2)))
}

geom_sim_nonoise_fit_v2_e2 <- function(par) {
  return(sum((rowMeans(geom_simulation_v2(c(0,par,0))-dat_e2_means)^2)))
}

# Optimization to find best fit of geometric mean v2 
fit <- optim(0, geom_sim_nonoise_fit_v2_e1)
fit2 <- optim(0, geom_sim_nonoise_fit_v2_e2)

# Calculate the predictions of the best fit of geometric mean v2
gmeans_v2_e1 <- rowMeans(geom_simulation_v2(c(0, fit$par, 0)))
gmeans_v2_e2 <- rowMeans(geom_simulation_v2(c(0, fit2$par, 0)))

# Harmonic mean
harm_simulation <- function(par, N=100000) {
  hmean <- matrix(rep(0,3*N), 3,N)
  for(i in 1:3) {
    durations <- filter(dur, sequence==i)$duration
    noisy_dur <- matrix(durations + rnorm(5*N, 0, par[1]*durations), 5, N)
    hmean[i,] <- 1/colMeans(1/noisy_dur) + rnorm(N, par[2], par[3])
  }
  return(hmean)
}

# Harmonic mean fit helper functions
harm_sim_fit_e1 <- function(par) {
  return(sum((rowMeans(harm_simulation(par)-dat_e1_means)^2)))
}

harm_sim_fit_e2 <- function(par) {
  return(sum((rowMeans(harm_simulation(par)-dat_e2_means)^2)))
}

# Grid search to find best parameters for harmonic mean 
# Here optimization seems to be necessary, since dependence on parameters is more complex
# hfitval <- array(0, dim=c(11,11,11))
# 
# p1 <- seq(0, 0.2, 0.02)
# p2 <- seq(-60, 140, 20)
# p3 <- seq(0, 100, 10)
# for(i in 1:length(p1)) {
#   for(j in 1:length(p2)) {
#     for(k in 1:length(p3)) {
#       hfitval[i,j,k] <- harm_sim_fit_e1(c(p1[i], p2[j], p3[k]))
#     }
#   }
# }
# 
# Find the best parameters based on the grid search
# par_coords <- which(hfitval==min(hfitval), arr.ind=TRUE)
# par <- c(p1[par_coords[1]], p2[par_coords[2]], p3[par_coords[3]])
# hmeans_e1 <- rowMeans(harm_simulation(par))
# # 
# 
# hfitval <- array(0, dim=c(11,11,11))
#
# Grid search for harmonic mean for experiment 2
# p1 <- seq(0, 0.2, 0.02)
# p2 <- seq(-60, 140, 20)
# p3 <- seq(0, 100, 10)
# for(i in 1:length(p1)) {
#   for(j in 1:length(p2)) {
#     for(k in 1:length(p3)) {
#       hfitval[i,j,k] <- harm_sim_fit_e2(c(p1[i], p2[j], p3[k]))
#     }
#   }
# }
# 
# Find best parameters of grid search for harmonic mean for exp 2
# par_coords <- which(hfitval==min(hfitval), arr.ind=TRUE)
# par <- c(p1[par_coords[1]], p2[par_coords[2]], p3[par_coords[3]])
# hmeans_e2 <- rowMeans(harm_simulation(par))

# Load results of the above commented simulation + optimization from a file
load('hmeans.rdata')

# Weighted mean
weight_simulation <- function(par, N=100000) {
  wmean <- matrix(rep(0,3*N), 3,N)
  for(i in 1:3) {
    durations <- filter(dur, sequence==i)$duration
    weights <- durations/sum(durations)
    noisy_dur <- matrix(weights*durations + rnorm(5*N, 0, par[1]*durations), 5, N)
    wmean[i,] <- colSums(noisy_dur) + rnorm(N, par[2], par[3])
  }
  return(wmean)
}

# Weighted mean fit helper functions
weight_sim_fit_e1 <- function(par) {
  return(sum((rowMeans(weight_simulation(par)-dat_e1_means)^2)))
}

weight_sim_fit_e2 <- function(par) {
  return(sum((rowMeans(weight_simulation(par)-dat_e2_means)^2)))
}

# Again, I think the result of optimization here is no better than the simple solution below
#par_w_e1 <- optim(c(0.1, mean(dat_e1_means)-mean(means$weight_mean), 0), weight_sim_fit_e1)
#par_w_e2 <- optim(c(0.1, mean(dat_e2_means)-mean(means$weight_mean), 0), weight_sim_fit_e2)

# Calculate predictions of best fit of weighted mean
wmeans_e1 <- rowMeans(weight_simulation(c(0, mean(dat_e1_means)-mean(means$weight_mean), 0)))
wmeans_e2 <- rowMeans(weight_simulation(c(0, mean(dat_e2_means)-mean(means$weight_mean), 0)))


# Grid search to confirm that solution is optimal
# wfitval <- array(0, dim=c(11,11,11))
# p1 <- seq(0, 0.2, 0.02)
# p2 <- seq(-200, 0, 20)
# p3 <- seq(0, 100, 10)
# for(i in 1:length(p1)) {
#   for(j in 1:length(p2)) {
#     for(k in 1:length(p3)) {
#       wfitval[i,j,k] <- weight_sim_fit_e1(c(p1[i], p2[j], p3[k]))
#     }
#   }
# }


# Plot model prediction figure (based on fit to average data) for Exp 1
fig_mod_pred_e1 <- ggplot(data.frame(set=factor(c(1,2,3)), pred_a=ameans_e1, pred_g1=gmeans_e1, pred_g2=gmeans_v2_e1, 
                  pred_h=hmeans_e1, pred_w=wmeans_e1, dat=dat_e1_means, dat_se=dat_e1_se),
                  aes(x=set, group=1)) + geom_point(aes(y=dat), size = 3) +
  geom_errorbar(aes(ymin=dat_e1_means-dat_e1_se, ymax=dat_e1_means+dat_e1_se), width=0.2) +
  geom_line(aes(y=pred_a, color="blue")) + geom_line(aes(y=pred_g2, color="red")) + 
  geom_line(aes(y=pred_h, color="orange")) + geom_line(aes(y=pred_w, color="purple")) +
  theme_classic() + ggtitle('Auditory') + labs(x='Set', y='Observed and predicted PSE (ms)') +
  scale_colour_manual(name = '', 
                      values = c('blue'='blue', 'red'='red', 
                                 'orange'='orange', 'purple'='purple'), 
                      labels = c('blue'='Arithmetic', 
                                 'red'='Geometric', 'orange'='Harmonic',
                                 'purple'='Weighted')) + 
  theme(legend.position = c(0.8, 0.9), legend.title = element_blank()) +
  ylim(c(500, 900))

# Save model prediction figure for Exp 1
ggsave('figures/mod_pred_e1.png', fig_mod_pred_e1, width=4.5, height=3)

# Plot model prediction figure (based on fit to average data) for Exp 2
fig_mod_pred_e2 <- ggplot(data.frame(set=factor(c(1,2,3)), pred_a=ameans_e2, pred_g1=gmeans_e2, pred_g2=gmeans_v2_e2, 
                  pred_h=hmeans_e2,  pred_w=wmeans_e2, dat=dat_e2_means, dat_se=dat_e2_se), 
                  aes(x=set, group=1)) + geom_point(aes(y=dat), size = 3) +
  geom_errorbar(aes(ymin=dat_e2_means-dat_e2_se, ymax=dat_e2_means+dat_e2_se), width=0.2) + 
  geom_line(aes(y=pred_a, color="blue")) + geom_line(aes(y=pred_g2, color="red")) + 
  geom_line(aes(y=pred_h, color="orange")) + geom_line(aes(y=pred_w, color="purple")) + 
  theme_classic() + ggtitle('Visual') + labs(x='Set', y='Observed and predicted PSE (ms)') +
  scale_colour_manual(name = 'Model', 
                      values = c('blue'='blue', 'red'='red', 
                                 'orange'='orange', 'purple'='purple'), 
                      labels = c('blue'='Arithmetic',
                                 'red'='Geometric', 'orange'='Harmonic', 
                                 'purple'='Weighted'))+ 
  theme(legend.position = c(0.8, 0.9), legend.title = element_blank()) + 
  ylim(c(500, 900))

# Save model prediction figure for Exp 2
ggsave('figures/mod_pred_e2.png', fig_mod_pred_e2, width=4.5, height=3)

# Combine model prediction figures into one and save
fig7= plot_grid(fig_mod_pred_e1, fig_mod_pred_e2, nrow = 1, labels = c("a","b"))
fig7
ggsave('figures/fig7.png', fig7, width = 8, height = 4, dpi = 600)
ggsave('figures/fig7.pdf', fig7, width = 8, height = 4, dpi = 600)

# ---- Individual participant model fits ----

# Prepare data structures for individual participant fits
apred_e1 <- matrix(rep(0,3*16), 16,3)
apred_e2 <- matrix(rep(0,3*16), 16,3)
gpred_e1 <- matrix(rep(0,3*16), 16,3)
gpred_e2 <- matrix(rep(0,3*16), 16,3)
gpred_v2_e1 <- matrix(rep(0,3*16), 16,3)
gpred_v2_e2 <- matrix(rep(0,3*16), 16,3)
hpred_e1 <- matrix(rep(0,3*16), 16,3)
hpred_e2 <- matrix(rep(0,3*16), 16,3)
wpred_e1 <- matrix(rep(0,3*16), 16,3)
wpred_e2 <- matrix(rep(0,3*16), 16,3)

# The commented code below does individual participant fitting
# for (i in 1:16) {
#   
#   print(i)
#   
#   dat_e1 <- filter(data_e1, subject==i)$alpha
#   dat_e2 <- filter(data_e2, subject==i)$alpha
#   
#   apred_e1[i,] <- rowMeans(ari_simulation(c(0.1, mean(dat_e1)-mean(means$ari_mean), 0)))
#   apred_e2[i,] <- rowMeans(ari_simulation(c(0.1, mean(dat_e2)-mean(means$ari_mean), 0)))
#   
#   gpred_e1[i,] <- rowMeans(geom_simulation(c(0.05, mean(dat_e1)-mean(means$geom_mean), 0)))
#   gpred_e2[i,] <- rowMeans(geom_simulation(c(0.05, mean(dat_e2)-mean(means$geom_mean), 0)))
# 
#   gpred_v2_e1[i,] <- rowMeans(geom_simulation_v2(c(0.05, log(mean(dat_e1)/mean(means$geom_mean)), 0)))
#   gpred_v2_e2[i,] <- rowMeans(geom_simulation_v2(c(0.05, log(mean(dat_e2)/mean(means$geom_mean)), 0)))
#   
#   harm_sim_fit_isub_e1 <- function(par) {
#     return(sum((rowMeans(harm_simulation(par)-dat_e1)^2)))
#   }
#   
#   harm_sim_fit_isub_e2 <- function(par) {
#     return(sum((rowMeans(harm_simulation(par)-dat_e2)^2)))
#   }
#   
#   hfitval <- array(0, dim=c(21,21,21))
#   
#   p1 <- seq(0, 0.4, 0.02)
#   p2 <- seq(-200, 200, 20)
#   p3 <- seq(0, 100, 5)
#   for(m in 1:length(p1)) {
#     for(n in 1:length(p2)) {
#       for(l in 1:length(p3)) {
#         hfitval[m,n,l] <- harm_sim_fit_e1(c(p1[m], p2[n], p3[l]))
#       }
#     }
#   }
#   
#   par_coords <- which(hfitval==min(hfitval), arr.ind=TRUE)
#   par <- c(p1[par_coords[1]], p2[par_coords[2]], p3[par_coords[3]])
#   hpred_e1[i,]  <- rowMeans(harm_simulation(par))
#   
#   p1 <- seq(0, 0.4, 0.02)
#   p2 <- seq(-200, 200, 20)
#   p3 <- seq(0, 100, 5)
#   for(m in 1:length(p1)) {
#     for(n in 1:length(p2)) {
#       for(l in 1:length(p3)) {
#         hfitval[m,n,l] <- harm_sim_fit_e2(c(p1[m], p2[n], p3[l]))
#       }
#     }
#   }
#   
#   par_coords <- which(hfitval==min(hfitval), arr.ind=TRUE)
#   par <- c(p1[par_coords[1]], p2[par_coords[2]], p3[par_coords[3]])
#   hpred_e2[i,]  <- rowMeans(harm_simulation(par))
#   
#   wpred_e1[i,] <- rowMeans(weight_simulation(c(0.1, mean(dat_e1)-mean(means$weight_mean), 0)))
#   wpred_e2[i,] <- rowMeans(weight_simulation(c(0.1, mean(dat_e2)-mean(means$weight_mean), 0)))
#   
# } 

# load the predictions which were generated by the commented, individual participant fitting,
# loop above
load('model_predictions.rdata')

# Plot model prediction figure based on averaging over individual participant fits for Exp1
ggplot(data.frame(set=c(1,2,3), pred_a=colMeans(apred_e1), pred_g1=colMeans(gpred_e1), pred_g2=colMeans(gpred_v2_e1), 
                  pred_h=colMeans(hpred_e1), pred_w=colMeans(wpred_e1), dat=dat_e1_means), aes(x=set)) +
  geom_point(aes(y=dat)) + geom_line(aes(y=pred_a, color="blue")) + 
  geom_line(aes(y=pred_g1, color="green")) + geom_line(aes(y=pred_g2, color="red")) + 
  geom_line(aes(y=pred_h, color="orange")) + geom_line(aes(y=pred_w, color="purple")) +
  theme_bw() + ggtitle('Experiment 1 - Auditory') +
  scale_colour_manual(name = 'Model', 
                      values = c('blue'='blue','green'='green', 'red'='red', 
                                 'orange'='orange', 'purple'='purple'), 
                      labels = c('blue'='Arithmetic', 'green'='Geometric', 
                                 'red'='Geometric v2', 'orange'='Harmonic',
                                 'purple'='Weighted'))

# Plot model prediction figure based on averaging over individual participant fits for Exp2
ggplot(data.frame(set=c(1,2,3), pred_a=colMeans(apred_e2), pred_g1=colMeans(gpred_e2), pred_g2=colMeans(gpred_v2_e2), 
                  pred_h=colMeans(hpred_e2), pred_w=colMeans(wpred_e2), dat=dat_e2_means), aes(x=set)) + 
  geom_point(aes(y=dat)) + geom_line(aes(y=pred_a, color="blue")) + 
  geom_line(aes(y=pred_g1, color="green")) + geom_line(aes(y=pred_g2, color="red")) + 
  geom_line(aes(y=pred_h, color="orange")) + geom_line(aes(y=pred_w, color="purple")) + 
  theme_bw() + ggtitle('Experiment 2 - Visual') + 
  scale_colour_manual(name = 'Model', 
                      values = c('blue'='blue','green'='green', 'red'='red', 
                                 'orange'='orange', 'purple'='purple'), 
                      labels = c('blue'='Arithmetic', 'green'='Geometric', 
                                 'red'='Geometric v2', 'orange'='Harmonic', 
                                 'purple'='Weighted'))
