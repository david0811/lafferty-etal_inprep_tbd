library(MASS)
library(tidyverse)
library(brms)
library(bayesplot)
theme_update(text = element_text(size = 16, family = "sans"))
bayesplot_theme_update(text = element_text(size = 16, family = "sans"))
library(tidybayes)
library(broom.mixed)
library(patchwork)
library(fixest)
library(modelr)

##################################################
# Read all relevant data
##################################################

# gridmet county obs
df_county_obs <- read_csv('../input_data/gridmet_county_weather_variables_1979-2020.csv')
df_county_obs$state <- df_county_obs$fips %>% substr(1,2) # add state column

##################################################
# County EDD
##################################################
df_county_obs <- df_county_obs[df_county_obs$EDD > 0, ] # remove 0 EDDs

# Visualize data
p1 <- ggplot(df_county_obs, aes(x = tavg, y = EDD)) + 
  geom_point(size = 0.5) + 
  geom_smooth() + 
  labs(x='County average temperature (C)', y='County EDD')

p2 <- ggplot(df_county_obs, aes(x = tavg, y = log(EDD))) + 
  geom_point(size = 0.5) + 
  geom_smooth(method='lm') + 
  labs(x='County average temperature (C)', y='County log(EDD)')

ggsave(filename = '../figures/county_edd_raw.png',
       plot = p1 | p2)

# OLS model
EDD_model_freq <- lm(log(EDD) ~ tavg, data = df_county_obs)
summary(EDD_model_freq)

# Bayesian model
EDD_model <- stan_glm(log(EDD) ~ tavg, data = df_county_obs,
                     family = gaussian,
                     chains = 3, iter = 10000*2, 
                     cores = 3, seed = 84735)

EDD_model <- brm(bf(log(EDD) ~ tavg, sigma ~ tavg), 
                 data = df_county_obs,
                 family = gaussian(),
                 chains = 3, iter = 10000*2,
                 cores = 3, seed = 84735)

prior_summary(EDD_model)

# Effective sample size ratio and Rhat
neff_ratio(EDD_model)
rhat(EDD_model)

# Trace & density plots of parallel chains
mcmc_trace(EDD_model, size = 0.1)
mcmc_dens_overlay(EDD_model)

# Plot predictive
sampling <- seq(1, nrow(df_county_obs), 10)
p1 <- df_county_obs %>%
  data_grid(tavg = seq_range(tavg, n = 101)) %>%
  add_predicted_draws(EDD_model) %>%
  ggplot(aes(x = tavg, y = EDD)) +
  stat_lineribbon(aes(y = exp(.prediction)), .width = c(.99, .95, .8, .5), color = "#08519C") +
  geom_point(data = df_county_obs[sampling,], size = 0.1) +
  scale_fill_brewer() +
  labs(x='County average temperature (C)', y='County EDD')
p1

ggsave(filename = '../figures/county_edd_bayes_fit.png',
       plot = p1)

# Posterior predictive check
pp_check(EDD_model, ndraws = 100) + 
  labs(x="County EDD", y='Density',
       subtitle = '100 Posterior Predictive Draws')

# Approximate parameter posterior
EDD_model_posterior <- as.data.frame(EDD_model)

# tavg
tavg_fit <- fitdistr(EDD_model_posterior$b_tavg,
                      densfun = "normal")

p_tavg <- ggplot() + 
  geom_density(data = EDD_model_posterior,
               aes(x = b_tavg, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(EDD_model_posterior$b_tavg),
                                        max(EDD_model_posterior$b_tavg))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = tavg_fit$estimate['mean'],
                            sd = tavg_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="tavg", y="",
       subtitle=paste("Normal(", signif(tavg_fit$estimate['mean'], 3),
                      ",", signif(tavg_fit$estimate['sd'], 3), ")", sep=""))

# sigma_tavg
sigma_tavg_fit <- fitdistr(EDD_model_posterior$b_sigma_tavg,
                     densfun = "normal")

p_sigma_tavg <- ggplot() + 
  geom_density(data = EDD_model_posterior,
               aes(x = b_sigma_tavg, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(EDD_model_posterior$b_sigma_tavg),
                                        max(EDD_model_posterior$b_sigma_tavg))),
                aes(x = x),
                fun = dnorm,
                args = list(sd = sigma_tavg_fit$estimate['sd'],
                            mean = sigma_tavg_fit$estimate['mean']),
                colour="black", lty="dashed", size=1) + 
  labs(x="sigma_tavg", y="",
       subtitle=paste("Normal(", signif(sigma_tavg_fit$estimate['sd'], 3),
                      ",", signif(sigma_tavg_fit$estimate['mean'], 3), ")", sep="")) 

# Intercept
intcp_fit <- fitdistr(EDD_model_posterior$b_Intercept,
                      densfun = "normal")

p_intcp <- ggplot() + 
  geom_density(data = EDD_model_posterior,
               aes(x = b_Intercept, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(EDD_model_posterior$b_Intercept),
                                        max(EDD_model_posterior$b_Intercept))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = intcp_fit$estimate['mean'],
                            sd = intcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Intercept", y="",
       subtitle=paste("Normal(", signif(intcp_fit$estimate['mean'], 3),
                      ",", signif(intcp_fit$estimate['sd'], 3), ")", sep="")) 

# sigma Intercept
sigma_intcp_fit <- fitdistr(EDD_model_posterior$b_sigma_Intercept,
                      densfun = "normal")

p_sigma_intcp <- ggplot() + 
  geom_density(data = EDD_model_posterior,
               aes(x = b_sigma_Intercept, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(EDD_model_posterior$b_sigma_Intercept),
                                        max(EDD_model_posterior$b_sigma_Intercept))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = sigma_intcp_fit$estimate['mean'],
                            sd = sigma_intcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="sigma_Intercept", y="",
       subtitle=paste("Normal(", signif(sigma_intcp_fit$estimate['mean'], 3),
                      ",", signif(sigma_intcp_fit$estimate['sd'], 3), ")", sep="")) 

ggsave('../figures/county_edd_bayes_posterior.png',
       plot = (p_tavg | p_intcp) / (p_sigma_tavg | p_sigma_intcp),
       width = 12, height = 6, units="in")
