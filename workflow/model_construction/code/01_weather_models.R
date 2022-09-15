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

df_county_obs <- read_csv('../input_data/gridmet_county_weather_variables_1979-2020.csv')
df_mw_obs <- read_csv('../input_data/gmfd_midwest_tavg_1948-2016.csv')

##################################################
# County GDD
##################################################
# Visualize data
ggplot(df_county_obs, aes(x = tavg, y = GDD)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", se = FALSE)

# Bayesian model
GDD_model <- stan_glm(GDD ~ tavg, data = df_county_obs,
                     family = gaussian,
                     prior_intercept = normal(-1000, 100),
                     prior = normal(150, 100),
                     prior_aux = exponential(0.0008),
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
df_county_obs %>%
  data_grid(tavg = seq_range(tavg, n = 101)) %>%
  add_predicted_draws(GDD_model) %>%
  ggplot(aes(x = tavg, y = GDD)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = "#08519C") +
  geom_point(data = df_county_obs, size = 0.1) +
  scale_fill_brewer()

# Posterior predictive check
pp_check(GDD_model, nreps = 100) + 
  xlab("County GDD") +
  theme_grey()

##################################################
# County EDD
##################################################
# Visualize data
ggplot(df_county_obs, aes(x = tavg, y = log(EDD))) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", se = FALSE)

# Bayesian model
EDD_model <- stan_glm(EDD ~ tavg, data = df_county_obs,
                      family = gaussian(link='log'),
                      prior_intercept = normal(0, 100),
                      prior = normal(0, 10), 
                      prior_aux = exponential(0.0008),
                      chains = 4, iter = 5000*2, seed = 84735)

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
df_county_obs %>%
  data_grid(tavg = seq_range(tavg, n = 101)) %>%
  add_predicted_draws(EDD_model) %>%
  ggplot(aes(x = tavg, y = EDD)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = "#08519C") +
  geom_point(data = df_county_obs, size = 0.1) +
  scale_fill_brewer()

# Posterior predictive check
pp_check(EDD_model, nreps = 50) + 
  xlab("County EDD") +
  theme_grey()
