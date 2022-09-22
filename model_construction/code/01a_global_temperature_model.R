library(MASS)
library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
bayesplot_theme_update(text = element_text(size = 16, family = "sans"))
theme_update(text = element_text(size = 16, family = "sans"))
library(tidybayes)
library(broom.mixed)
library(patchwork)

##################################################
# Read all relevant data
##################################################
df <- read_csv('../input_data/gridmet_midwest_tavg_1979-2020.csv')

##################################################
# Model
##################################################
# Exploratory plots
p1 <- ggplot(df, aes(x = tavg_mw)) + 
  geom_histogram(bins=20) +
  labs(x = "average temperature (C)", y="")

ggsave(filename = '../figures/tavg_mw_raw.png',
       plot = p1)

# Frequentist model
tavg_model_freq <- fitdistr(df$tavg_mw, densfun = "normal")
tavg_model_freq

# Bayesian model
tavg_model <- stan_glm(tavg_mw ~ 1,
                       data = df,
                       family = gaussian,
                       chains = 3, iter = 10000*2, 
                       cores = 3, seed = 84735)

prior_summary(tavg_model)

# Effective sample size ratio and Rhat
neff_ratio(tavg_model)
rhat(tavg_model)

# Trace & density plots of parallel chains
mcmc_trace(tavg_model, size = 0.1)
mcmc_dens_overlay(tavg_model)

# Posterior predictive checks
p1 <- pp_check(tavg_model, nreps = 1000) + 
  labs(x="average temperature (C)") +
  theme_grey()

ggsave('../figures/tavg_mw_bayes_fit.png',
       plot = p1,
       width = 12, height = 6, units="in")

# Approximate parameter posterior
tavg_model_posterior <- as.data.frame(tavg_model)
colnames(tavg_model_posterior)[1] <- "intercept"

# Intercept
intcp_fit <- fitdistr(tavg_model_posterior$intercept,
                      densfun = "normal")

p_intcp <- ggplot() + 
  geom_density(data = tavg_model_posterior,
               aes(x = intercept, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(tavg_model_posterior$intercept),
                                        max(tavg_model_posterior$intercept))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = intcp_fit$estimate['mean'],
                            sd = intcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Mean", y="",
       subtitle=paste("Normal(", signif(intcp_fit$estimate['mean'], 3),
                      ",", signif(intcp_fit$estimate['sd'], 3), ")", sep="")) 

# Sigma
sigma_fit <- fitdistr(tavg_model_posterior$sigma,
                      densfun = "gamma")

p_sigma <- ggplot() + 
  geom_density(data = tavg_model_posterior,
               aes(x = sigma, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(tavg_model_posterior$sigma),
                                        max(tavg_model_posterior$sigma))),
                aes(x = x),
                fun = dgamma,
                args = list(shape = sigma_fit$estimate['shape'],
                            rate = sigma_fit$estimate['rate']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Sigma", y="",
       subtitle=paste("Gamma(", signif(sigma_fit$estimate['shape'], 3),
                      ",", signif(sigma_fit$estimate['rate'], 3), ")", sep=""))

ggsave('../figures/tavg_mw_bayes_posterior.png',
       plot = p_intcp | p_sigma,
       width = 12, height = 6, units="in")
