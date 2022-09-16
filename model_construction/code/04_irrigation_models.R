library(MASS)
library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
bayesplot_theme_update(text = element_text(size = 16, family = "sans"))
theme_update(text = element_text(size = 16, family = "sans"))
library(tidybayes)
library(broom.mixed)
library(modelr)
library(bayesrules)
library(patchwork)

##################################################
# Read all relevant data
##################################################

# gridmet state obs
df_state_obs_maize <- read_csv('../input_data/gridmet_state_weather_variables_maize_irr_weighted.csv')
df_state_obs_soy <- read_csv('../input_data/gridmet_state_weather_variables_soy_irr_weighted.csv')

# USDA water applied
df_water_applied_maize <- read_csv('../input_data/usda_maize_water_applied_2013-2018.csv')
df_water_applied_soy <- read_csv('../input_data/usda_soy_water_applied_2013-2018.csv')

##################################################
# Water Applied
##################################################

#################
## Maize
#################
df_maize <- inner_join(df_state_obs_maize, df_water_applied_maize, 
                       by=c('year', 'state' = 'state_fips_code'))

# Exploratory plots
p1 <- ggplot(df_maize, aes(x = prcp, y = log(water_applied))) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Precip", y = "log(Water Applied)",
       title = "Maize")

p2 <- ggplot(df_maize, aes(x = EDD, y = log(water_applied))) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "EDD", y = "log(Water Applied)")

ggsave(filename = '../figures/water_applied_maize_raw.png',
       plot = p1 + p2 & scale_y_continuous(limits=c(4.7, 6.2)))

# OLS model
water_applied_maize_model_freq <- lm(log(water_applied) ~ prcp + EDD, data = df_maize)
summary(water_applied_maize_model_freq)

# Bayesian model
water_applied_maize_model <- stan_glm(log(water_applied) ~ prcp + EDD, data = df_maize,
                                      family = gaussian(),
                                      # prior_intercept = normal(0, 100),
                                      # prior = normal(0, 100),
                                      # prior_aux = exponential(0.0001),
                                      chains = 3, iter = 10000*2, 
                                      cores = 3, seed = 84735)

prior_summary(water_applied_maize_model)

# Effective sample size ratio and Rhat
neff_ratio(water_applied_maize_model)
rhat(water_applied_maize_model)

# Trace & density plots of parallel chains
mcmc_trace(water_applied_maize_model, size = 0.1)
mcmc_dens_overlay(water_applied_maize_model)

# Posterior predictive checks
pp_check(water_applied_maize_model, nreps = 100) + 
  xlab("log Water Applied (mm/acre)") +
  theme_grey()

p1 <- ppc_intervals(
  y = df_maize$water_applied,
  yrep = exp(posterior_predict(water_applied_maize_model)),
  x = df_maize$EDD,
  prob = 0.95) +
  labs(x = "EDD", y = "Water Applied (mm/acre)",
       title = "Maize",
       subtitle = "95% posterior predictive intervals") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")

p2 <- ppc_intervals(
  y = df_maize$water_applied,
  yrep = exp(posterior_predict(water_applied_maize_model)),
  x = df_maize$prcp,
  prob = 0.95) +
  labs(x = "Precip (mm)", y = "Water Applied (mm/acre)") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")

ggsave('../figures/water_applied_maize_bayes_fit.png',
       plot = p1 | p2,
       width = 12, height = 6, units="in")

# Approximate parameter posterior
water_applied_maize_model_posterior <- as.data.frame(water_applied_maize_model)
colnames(water_applied_maize_model_posterior)[1] <- "intercept"

# Precip param
prcp_fit <- fitdistr(water_applied_maize_model_posterior$prcp,
         densfun = "normal")

p_prcp <- ggplot() + 
  geom_density(data = water_applied_maize_model_posterior,
               aes(x = prcp, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(water_applied_maize_model_posterior$prcp),
                                        max(water_applied_maize_model_posterior$prcp))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = prcp_fit$estimate['mean'],
                            sd = prcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Precip Coefficient", y="",
       subtitle=paste("Normal(", signif(prcp_fit$estimate['mean'], 3),
                      ",", signif(prcp_fit$estimate['sd'], 3), ")", sep="")) 

# EDD param
edd_fit <- fitdistr(water_applied_maize_model_posterior$EDD,
                      densfun = "normal")

p_edd <- ggplot() + 
  geom_density(data = water_applied_maize_model_posterior,
               aes(x = EDD, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(water_applied_maize_model_posterior$EDD),
                                        max(water_applied_maize_model_posterior$EDD))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = edd_fit$estimate['mean'],
                            sd = edd_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="EDD Coefficient", y="",
       subtitle=paste("Normal(", signif(edd_fit$estimate['mean'], 3),
                      ",", signif(edd_fit$estimate['sd'], 3), ")", sep="")) 

# Intercept
intcp_fit <- fitdistr(water_applied_maize_model_posterior$intercept,
                    densfun = "normal")

p_intcp <- ggplot() + 
  geom_density(data = water_applied_maize_model_posterior,
               aes(x = intercept, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(water_applied_maize_model_posterior$intercept),
                                        max(water_applied_maize_model_posterior$intercept))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = intcp_fit$estimate['mean'],
                            sd = intcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Intercept", y="",
       subtitle=paste("Normal(", signif(intcp_fit$estimate['mean'], 3),
                      ",", signif(intcp_fit$estimate['sd'], 3), ")", sep="")) 

# Sigma
sigma_fit <- fitdistr(water_applied_maize_model_posterior$sigma,
                      densfun = "normal")

p_sigma <- ggplot() + 
  geom_density(data = water_applied_maize_model_posterior,
               aes(x = sigma, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(water_applied_maize_model_posterior$sigma),
                                        max(water_applied_maize_model_posterior$sigma))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = sigma_fit$estimate['mean'],
                            sd = sigma_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Sigma", y="",
       subtitle=paste("Normal(", signif(sigma_fit$estimate['mean'], 3),
                      ",", signif(sigma_fit$estimate['sd'], 3), ")", sep="")) 

ggsave('../figures/water_applied_maize_bayes_posterior.png',
       plot = (p_prcp | p_edd) / (p_intcp | p_sigma),
       width = 12, height = 6, units="in")

#################
## Soy
#################
df_soy <- inner_join(df_state_obs_soy, df_water_applied_soy, 
                     by=c('year', 'state' = 'state_fips_code'))

# Visualize data
p1 <- ggplot(df_soy, aes(x = prcp, y = log(water_applied))) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Precip", y = "log(Water Applied)",
       title = "Soy")

p2 <- ggplot(df_soy, aes(x = EDD, y = log(water_applied))) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "EDD", y = "log(Water Applied)")

ggsave(filename = '../figures/water_applied_soy_raw.png',
       plot = p1 + p2 & scale_y_continuous(limits=c(4.7, 6.1)))

# OLS model
water_applied_soy_model_freq <- lm(log(water_applied) ~ prcp + EDD, data = df_soy)
summary(water_applied_soy_model_freq)

# Bayesian model
water_applied_soy_model <- stan_glm(log(water_applied) ~ prcp + EDD, data = df_soy,
                                    family = gaussian(),
                                    # prior_intercept = normal(0, 100, autoscale = T),
                                    # prior = normal(0, 100, autoscale = T),
                                    # prior_aux = exponential(0.0001, autoscale = T),
                                    chains = 3, iter = 10000*2, 
                                    cores = 3, seed = 84735)

prior_summary(water_applied_soy_model)

# Effective sample size ratio and Rhat
neff_ratio(water_applied_soy_model)
rhat(water_applied_soy_model)

# Trace & density plots of parallel chains
mcmc_trace(water_applied_soy_model, size = 0.1)
mcmc_dens_overlay(water_applied_soy_model)

# Posterior predictive checks
pp_check(water_applied_soy_model, nreps = 100) + 
  xlab("log Water Applied (mm/acre)") +
  theme_grey()

p1 <- ppc_intervals(
  y = df_soy$water_applied,
  yrep = exp(posterior_predict(water_applied_soy_model)),
  x = df_soy$EDD,
  prob = 0.95) +
  labs(x = "EDD", y = "Water Applied (mm/acre)",
       title = "Soy",
       subtitle = "95% posterior predictive intervals") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")

p2 <- ppc_intervals(
  y = df_soy$water_applied,
  yrep = exp(posterior_predict(water_applied_soy_model)),
  x = df_soy$prcp,
  prob = 0.95) +
  labs(x = "Precip (mm)", y = "Water Applied (mm/acre)") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")

ggsave('../figures/water_applied_soy_bayes_fit.png',
       plot = p1 | p2,
       width = 12, height = 6, units="in")

# Summarize posterior
tidy(water_applied_soy_model, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.99)
