library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
library(tidybayes)
library(broom.mixed)
library(modelr)
library(bayesrules)

##################################################
# Read all relevant data
##################################################

# gridmet county obs
df_county_obs <- read_csv('../input_data/gridmet_county_weather_variables_1979-2020.csv')
df_county_obs$state <- df_county_obs$fips %>% substr(1,2) # add state column

df_county_obs_mw <- filter(df_county_obs, state %in% c('17','19','29')) # IL, IA, MO

##################################################
# County GDD
##################################################

# Visualize data
ggplot(df_county_obs_mw, aes(x = tavg, y = GDD)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", se = FALSE)

# OLS model
GDD_model_freq <- lm(GDD ~ tavg, data = df_county_obs_mw)
summary(GDD_model_freq)

# Bayesian model
GDD_model <- stan_glm(GDD ~ tavg, data = df_county_obs_mw,
                     family = gaussian,
                     # prior_intercept = normal(-1000, 100),
                     # prior = normal(150, 100),
                     # prior_aux = exponential(0.0008),
                     chains = 3, iter = 10000*2, 
                     cores = 3, seed = 84735)

prior_summary(GDD_model)

# Effective sample size ratio and Rhat
neff_ratio(GDD_model)
rhat(GDD_model)

# Trace & density plots of parallel chains
mcmc_trace(GDD_model, size = 0.1)
mcmc_dens_overlay(GDD_model)

# Summarize posterior
tidy(GDD_model, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.99)

# Plot predictive
df_county_obs_mw %>%
  data_grid(tavg = seq_range(tavg, n = 101)) %>%
  add_predicted_draws(GDD_model) %>%
  ggplot(aes(x = tavg, y = GDD)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = "#08519C") +
  geom_point(data = df_county_obs_mw, size = 0.1) +
  scale_fill_brewer()

# Posterior predictive check
pp_check(GDD_model, nreps = 100) + 
  xlab("County GDD") +
  theme_grey()

##################################################
# County EDD
##################################################
# Visualize data
ggplot(df_county_obs_mw, aes(x = tavg, y = log(EDD))) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", se = FALSE)

# Bayesian model
EDD_model <- stan_glm(log(EDD) ~ tavg, data = df_county_obs_mw,
                      family = gaussian(),
                      prior_intercept = normal(0, 100),
                      prior = normal(0, 100),
                      prior_aux = exponential(0.0008),
                      chains = 3, iter = 1000*2, 
                      cores = 3, seed = 84735)

prior_summary(EDD_model)

# Effective sample size ratio and Rhat
neff_ratio(EDD_model)
rhat(EDD_model)

# Trace & density plots of parallel chains
mcmc_trace(EDD_model, size = 0.1)
mcmc_dens_overlay(EDD_model)

# Summarize posterior
tidy(EDD_model, effects = c("fixed", "aux"),
     conf.int = TRUE, conf.level = 0.99)

# Plot predictive
df_county_obs_mw %>%
  data_grid(tavg = seq_range(tavg, n = 101)) %>%
  add_predicted_draws(EDD_model, tranform=TRUE) %>%
  ggplot(aes(x = tavg, y = log(EDD))) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = "#08519C") +
  geom_point(data = df_county_obs_mw, size = 0.1) +
  scale_fill_brewer()

# Posterior predictive checks
pp_check(EDD_model, nreps = 50) + 
  xlab("County EDD") +
  theme_grey()