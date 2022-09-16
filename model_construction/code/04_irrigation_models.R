library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
bayesplot_theme_update(text = element_text(size = 16, family = "sans"))
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

# Visualize data
ggplot(df_maize, aes(x = prcp, y = log(water_applied))) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(df_maize, aes(x = EDD, y = log(water_applied))) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE)

# OLS model
water_applied_maize_model_freq <- lm(log(water_applied) ~ prcp + EDD, data = df_maize)
summary(water_applied_maize_model_freq)

# Bayesian model
water_applied_maize_model <- stan_glm(log(water_applied) ~ prcp + EDD, data = df_maize,
                                      family = gaussian(),
                                      prior_intercept = normal(0, 100),
                                      prior = normal(0, 100),
                                      prior_aux = exponential(0.0001),
                                      chains = 3, iter = 10000*2, 
                                      cores = 3, seed = 84735)

prior_summary(water_applied_maize_model)

# Effective sample size ratio and Rhat
neff_ratio(water_applied_maize_model)
rhat(water_applied_maize_model)

# Trace & density plots of parallel chains
mcmc_trace(water_applied_maize_model, size = 0.1)
mcmc_dens_overlay(water_applied_maize_model)

# Summarize posterior
tidy(water_applied_maize_model, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.99)

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

ggsave('../',
       plot = p1 / p2)

#################
## Soy
#################
df_soy <- inner_join(df_state_obs_soy, df_water_applied_soy, 
                     by=c('year', 'state' = 'state_fips_code'))

# Visualize data
ggplot(df_soy, aes(x = prcp, y = log(water_applied))) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(df_soy, aes(x = EDD, y = log(water_applied))) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE)

# OLS model
water_applied_soy_model_freq <- lm(log(water_applied) ~ prcp + EDD, data = df_soy)
summary(water_applied_soy_model_freq)

# Bayesian model
water_applied_soy_model <- stan_glm(log(water_applied) ~ prcp + EDD, data = df_soy,
                                    family = gaussian(),
                                    prior_intercept = normal(0, 100, autoscale = T),
                                    prior = normal(0, 100, autoscale = T),
                                    prior_aux = exponential(0.0001, autoscale = T),
                                    chains = 3, iter = 10000*2, 
                                    cores = 3, seed = 84735)

prior_summary(water_applied_soy_model)

# Effective sample size ratio and Rhat
neff_ratio(water_applied_soy_model)
rhat(water_applied_soy_model)

# Trace & density plots of parallel chains
mcmc_trace(water_applied_soy_model, size = 0.1)
mcmc_dens_overlay(water_applied_soy_model)

# Summarize posterior
tidy(water_applied_soy_model, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.99)

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

p1 / p2
