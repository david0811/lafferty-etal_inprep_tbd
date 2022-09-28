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
df_gridmet <- read_csv('../input_data/gridmet_midwest_tavg_1979-2020.csv')
df_nclim <- read_csv('../input_data/nclimgrid_midwest_tavg_1895-2021.csv')

# nclimgrid vs gridmet
ggplot() + 
  geom_line(data=df_nclim, aes(x=year, y=tavg_mw), color='blue') + 
  geom_smooth(data=df_nclim, aes(x=year, y=tavg_mw), color='blue', method='lm') + 
  geom_line(data=df_gridmet, aes(x=year, y=tavg_mw), color='orange') +
  geom_smooth(data=df_gridmet, aes(x=year, y=tavg_mw), color='orange', method='lm')

# detrend
nclim_tavg_mw_linmod <- lm(tavg_mw ~ year, data=df_nclim)
df_nclim$tavg_mw_dt <- resid(nclim_tavg_mw_linmod)

gridmet_tavg_mw_linmod <- lm(tavg_mw ~ year, data=df_gridmet)
df_gridmet$tavg_mw_dt <- resid(gridmet_tavg_mw_linmod)

# now...
ggplot() + 
  geom_line(data=df_nclim, aes(x=year, y=tavg_mw_dt), color='blue') + 
  geom_line(data=df_gridmet, aes(x=year, y=tavg_mw_dt), color='orange')

ggplot() + 
  geom_density(data=df_nclim, aes(tavg_mw_dt), color='blue') + 
  geom_density(data=df_gridmet, aes(tavg_mw_dt), color='orange')

# NClimGrid shows considerably larger variations than gridMET in 
# historical average temperatures. We need to use NClimGrid in the price module
# so in order to keep the model internally consistent we will calibrate 
# all modules that depend on Midwest Tavg against NClimGrid anomalies

##################################################
# Model
##################################################
# Exploratory plots
p1 <- ggplot(df_nclim, aes(x = tavg_mw_dt)) + 
  geom_histogram(bins=20) +
  labs(x = "Midwest temperature anomaly (C)", y="")
p1

ggsave(filename = '../figures/tavg_mw_raw.png',
       plot = p1)

# Frequentist model
tavg_model_freq <- fitdistr(df_nclim$tavg_mw_dt, densfun = "normal")
tavg_model_freq

# Bayesian model
tavg_model <- stan_glm(tavg_mw_dt ~ 1,
                       data = df_nclim,
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
p <- pp_check(tavg_model, nreps = 1000) + 
  labs(x="Midwest temperature anomaly (C)", y="Density",
       subtitle = "1000 Posterior Predictive Draws") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")
p

ggsave('../figures/tavg_mw_bayes_fit.png',
       plot = p,
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
